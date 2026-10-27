(in-package #:ntop.e2e.test)

(cl-weave:describe
 "route process readiness headings"
 (cl-weave:it
  "recognizes the default Name(n) process heading"
  (multiple-value-bind (ready-p observed-heading diagnostic)
      (ntop.e2e::%route-process-widget-ready-p
       (criterion-test-grid 12 50 "1234 sshd"))
    (cl-weave:expect ready-p :to-be t)
    (cl-weave:expect observed-heading :to-equal "Name(n)")
    (cl-weave:expect diagnostic :to-equal
                     "Expected process heading Name(n); observed Name(n).")))
 (cl-weave:it
  "recognizes Command(n) when --process-command was launched"
  (multiple-value-bind (ready-p observed-heading diagnostic)
      (ntop.e2e::%route-process-widget-ready-p
       (criterion-test-grid 12 50 "1234 /bin/sleep"
                            "1;38;5;2;49" "PID Command(n) CPU%(c)")
       (list "--process-command"))
    (declare (ignore diagnostic))
    (cl-weave:expect ready-p :to-be t)
    (cl-weave:expect observed-heading :to-equal "Command(n)")))
 (cl-weave:it
  "reports the observed heading when it differs from the expected heading"
  (multiple-value-bind (ready-p observed-heading diagnostic)
      (ntop.e2e::%route-process-widget-ready-p
       (criterion-test-grid 12 50 "1234 sshd")
       (list "--process-command"))
    (cl-weave:expect ready-p :to-be nil)
    (cl-weave:expect observed-heading :to-equal "Name(n)")
    (cl-weave:expect (search "Command(n)" diagnostic)
                     :to-satisfy (function plusp))
    (cl-weave:expect (search "Name(n)" diagnostic)
                     :to-satisfy (function plusp))))
 (cl-weave:it
  "classifies interactive readiness timeouts by the failing side"
  (let* ((route (ntop.e2e::make-route-case
                 :id "readiness-test" :kind :additional :comparable-p nil))
         (diagnostic
           "Expected process heading Command(n); observed Name(n).")
         (directory (ntop.e2e::%make-e2e-temporary-directory)))
    (unwind-protect
         (dolist (side-case
                  (list (list :reference :reference-error)
                        (list :candidate :candidate-error)))
           (let* ((side (first side-case))
                  (classification (second side-case))
                  (failed-side (list :readiness-status :manual
                                    :readiness-result :timeout
                                    :readiness-diagnostic diagnostic))
                  (reference (when (eq side :reference) failed-side))
                  (candidate (when (eq side :candidate) failed-side))
                  (result
                    (ntop.e2e::%route-capture-subcase
                     route nil 1 directory
                     (lambda ()
                       (ntop.e2e::%route-all-expected-actual
                        route "btm-self" nil reference candidate))))
                  (route-status
                    (ntop.e2e::%route-result-status route (list result))))
             (cl-weave:expect (getf result :classification)
                              :to-equal classification)
             (cl-weave:expect (getf result :status) :to-equal :fail)
             (cl-weave:expect
              (search "observed Name(n)" (getf result :actual))
              :to-satisfy (function plusp))
             (cl-weave:expect
              (search "observed Name(n)" (getf result :cause))
              :to-satisfy (function plusp))
             (cl-weave:expect
              (ntop.e2e::%route-classification-group-count
               (list result) :harness-error)
              :to-equal 1)
             (cl-weave:expect route-status :to-equal :fail)
             (cl-weave:expect
              (ntop.e2e::%route-results-exit-code
               (list (list :status route-status)))
              :to-equal 1)))
      (uiop:delete-directory-tree directory :validate t))))
 (cl-weave:it
  "classifies owned-child readiness timeouts and fails unresolved probe limits"
  (let* ((route
           (find "sleep-child" (ntop.e2e::route-catalog)
                 :key (function ntop.e2e::route-id)
                 :test (function string=)))
         (side-symbol (find-symbol "%ROUTE-RUN-SLEEP-CHILD-SIDE" "NTOP.E2E"))
         (original-side (symbol-function side-symbol))
         (mode :timeout)
         (diagnostic
           "Expected process heading Command(n); observed Name(n).")
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (subcase (first (ntop.e2e::%route-subcases-expanded route)))
         (result nil))
    (unwind-protect
         (progn
           (setf (symbol-function side-symbol)
                 (lambda (program ignored-route ignored-subcase prefix rows columns
                          rate timeout config-path evidence-directory)
                   (declare (ignore ignored-route ignored-subcase prefix rows columns
                                    rate timeout config-path evidence-directory))
                   (if (string= program "ntop")
                       (if (eq mode :timeout)
                           (list :status :manual :passed nil
                                 :readiness-result :timeout
                                 :readiness-diagnostic diagnostic
                                 :actual diagnostic :cause diagnostic)
                           (list :status :manual :passed nil
                                 :actual "owned PID and marker were not on one row"
                                 :cause "process-row probe limitation"))
                       (list :passed t :actual "owned child visible"))))
           (setf result
                 (ntop.e2e::%route-capture-subcase
                  route subcase 1 directory
                  (lambda ()
                    (ntop.e2e::%route-run-sleep-child-pair
                     route subcase "owned child readiness" "btm-self"
                     "btm" "ntop"
                     (namestring (merge-pathnames "reference" directory))
                     (namestring (merge-pathnames "candidate" directory))
                     24 80 250 1 nil directory))))
           (cl-weave:expect (getf result :classification)
                            :to-equal :candidate-error)
           (cl-weave:expect (getf result :status) :to-equal :fail)
           (cl-weave:expect
            (ntop.e2e::%route-classification-group-count
             (list result) :harness-error)
            :to-equal 1)
           (cl-weave:expect
            (ntop.e2e::%route-results-exit-code
             (list (list :status :fail)))
            :to-equal 1)
           (cl-weave:expect (search "observed Name(n)" (getf result :actual))
                            :to-satisfy (function plusp))
           (setf mode :probe-limit)
           (setf result
                 (ntop.e2e::%route-capture-subcase
                  route subcase 2 directory
                  (lambda ()
                    (ntop.e2e::%route-run-sleep-child-pair
                     route subcase "owned child probe limit" "btm-self"
                     "btm" "ntop"
                     (namestring (merge-pathnames "reference" directory))
                     (namestring (merge-pathnames "candidate" directory))
                     24 80 250 1 nil directory))))
           (let ((route-status
                   (ntop.e2e::%route-result-status route (list result))))
             (cl-weave:expect (ntop.e2e::%route-subresult-status result) :to-equal :fail)
             (cl-weave:expect (getf result :classification) :to-equal :candidate-failure)
             (cl-weave:expect route-status :to-equal :fail)
             (cl-weave:expect
              (ntop.e2e::%route-report-exit-code
               (list (list :status route-status :subresults (list result))))
              :to-equal 1)))
      (setf (symbol-function side-symbol) original-side)
      (uiop:delete-directory-tree directory :validate t)))))
(cl-weave:describe
 "structural modal transition checks"
 (cl-weave:it
  "classifies Escape from an expanded widget by frame geometry"
  (let* ((route
          (find "key-36" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase (list :context "expanded widget"))
         (expanded (ntop.e2e:make-terminal-grid :rows 12 :cols 40))
         (dashboard (ntop.e2e:make-terminal-grid :rows 12 :cols 40))
         (still-expanded (ntop.e2e:make-terminal-grid :rows 12 :cols 40)))
    (dolist (grid (list expanded still-expanded))
      (ntop.e2e.test::criterion-test-write grid 0 0 "0" "┌")
      (ntop.e2e.test::criterion-test-write grid 0 39 "0" "┐")
      (ntop.e2e.test::criterion-test-write grid 11 0 "0" "└")
      (ntop.e2e.test::criterion-test-write grid 11 39 "0" "┘"))
    (dolist (grid (list dashboard))
      (ntop.e2e.test::criterion-test-write grid 0 0 "0" "┌")
      (ntop.e2e.test::criterion-test-write grid 0 19 "0" "┐")
      (ntop.e2e.test::criterion-test-write grid 5 0 "0" "└")
      (ntop.e2e.test::criterion-test-write grid 5 19 "0" "┘"))
    (cl-weave:expect
     (ntop.e2e::%route-expanded-widget-transition-p dashboard expanded)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-expanded-widget-restored-p dashboard dashboard)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-expanded-widget-restored-p still-expanded dashboard)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-modal-action-state-p route subcase expanded dashboard)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-modal-action-state-p
      route subcase expanded still-expanded)
     :to-be nil)))
 (cl-weave:it
  "compares expanded-widget Escape by frame state when other cells differ"
  (let* ((route
          (find "key-36" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase (list :context "expanded widget" :probe :modal))
         (before (ntop.e2e:make-terminal-grid :rows 12 :cols 40))
         (reference-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 40))
         (candidate-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 40)))
    (labels ((frame (grid top left bottom right)
               (ntop.e2e.test::criterion-test-write grid top left "0" "┌")
               (ntop.e2e.test::criterion-test-write grid top right "0" "┐")
               (ntop.e2e.test::criterion-test-write grid bottom left "0" "└")
               (ntop.e2e.test::criterion-test-write grid bottom right "0" "┘")))
      (frame before 0 0 11 39)
      (frame reference-grid 0 0 5 19)
      (frame candidate-grid 0 20 5 39))
    (let ((reference
            (list :grid reference-grid :initial-grid before
                  :projection (ntop.e2e:project-criterion-grid reference-grid)
                  :initial-projection (ntop.e2e:project-criterion-grid before)))
          (candidate
            (list :grid candidate-grid :initial-grid before
                  :projection (ntop.e2e:project-criterion-grid candidate-grid)
                  :initial-projection (ntop.e2e:project-criterion-grid before))))
      (cl-weave:expect
       (not (ntop.e2e::criterion-projections-equal-p
             (getf reference :projection) (getf candidate :projection)))
       :to-be t)
      (multiple-value-bind (passed difference)
          (ntop.e2e::%route-all-expected-actual
           route "ntop" subcase reference candidate)
        (cl-weave:expect passed :to-be t)
        (cl-weave:expect difference :to-be nil)))))
 (cl-weave:it
  "closes process search with either reference or candidate control labels"
  (let* ((route
          (find "key-36" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase (list :context "process search"))
         (closed (criterion-test-grid 12 60 ""))
         (reference-search (criterion-test-grid 12 60 ""))
         (candidate-search (criterion-test-grid 12 60 "")))
    (criterion-test-write reference-search 6 4 "0" "Case(F1) Whole(F2) Regex(F3) > ")
    (criterion-test-write candidate-search 6 4 "0" "Case Whole Regex > ")
    (cl-weave:expect
     (ntop.e2e::%route-modal-action-state-p
      route subcase reference-search closed)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-modal-action-state-p
      route subcase candidate-search closed)
     :to-be t))))
(cl-weave:describe
 "route read completion predicates"
 (cl-weave:it
  "returns after one complete state while output continues"
  (let* ((script
           (concatenate (quote string)
             "import os,time;os.write(1,bytes([82,69,65,68,89,27,91]));"
             "time.sleep(0.15);os.write(1,bytes([51,49,109]));"
             "time.sleep(0.25);os.write(1,bytes([68,79,78,69]));"
             "[time.sleep(0.05) or os.write(1,bytes([46])) for _ in range(50)]"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 4d0))
         (started (ntop.e2e::%now))
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait
             (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session
                  :quiet-seconds 1d0
                  :timeout-seconds 2d0
                  :initial-quiet-seconds 0.1d0
                  :completion-predicate (lambda (octets)
                    (let ((text (ntop.e2e::%octets-to-text octets)))
                      (and (search "READY" text)
                           (not (search "DONE" text)))))
                  :completion-samples-required 1))
           (cl-weave:expect
            (search "READY" (ntop.e2e::%octets-to-text output))
            :to-satisfy (function identity))
           (cl-weave:expect
            (ntop.e2e::%route-incomplete-escape-suffix-p output)
            :to-be nil)
           (cl-weave:expect
            (search "DONE" (ntop.e2e::%octets-to-text output))
            :to-be nil)
           (cl-weave:expect
            (< (ntop.e2e::%ticks-to-seconds
                (- (ntop.e2e::%now) started))
               0.8d0)
            :to-be t)
           (cl-weave:expect
            (sb-ext:process-alive-p (ntop.e2e::e2e-session-process session))
            :to-be t))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "loads the widget state reader used by route actions"
  (cl-weave:expect
   (fboundp (quote ntop.e2e::%route-read-widget-state))
   :to-be-truthy))
 (cl-weave:it
  "distinguishes the search prompt from regular help guidance"
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p
    "│Type to search, Esc to close")
   :to-be nil)
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p
    "Search:   Type to search, Esc to close")
   :to-be t)
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p "Search: |")
   :to-be t)))
(cl-weave:describe
 "owned child PID markers and safe cancellation"
 (cl-weave:it
  "requires the PID-specific marker on the owned process row"
  (let* ((pid 12345)
         (matching
           (criterion-test-grid
            12 80 (format nil "~d ntop-owned-~d" pid pid)))
         (generic-marker
           (criterion-test-grid 12 80 (format nil "~d ntop-owned" pid)))
         (different-marker
           (criterion-test-grid
            12 80 (format nil "~d ntop-owned-54321" pid))))
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-row-p matching pid)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-row-p generic-marker pid)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-row-p different-marker pid)
     :to-be nil)))
 (cl-weave:it
  "only sends a safe cancellation input for an owned dialog"
  (let* ((pid 12345)
         (cancel-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 80))
         (simple-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 80))
         (unsafe-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 80))
         (wrong-pid-grid (ntop.e2e:make-terminal-grid :rows 12 :cols 80))
         (simple-input nil)
         (unsafe-error nil)
         (wrong-pid-error nil))
    (dolist (grid (list cancel-grid simple-grid unsafe-grid))
      (criterion-test-write
       grid 2 2 "38;5;7;49"
       (format nil "Kill process ~s with PID ~d?" "sleep" pid))
      (criterion-test-write grid 4 2 "38;5;7;49" "Confirm Kill Process"))
    (criterion-test-write cancel-grid 6 2 "38;5;7;49" "0: Cancel")
    (criterion-test-write
     wrong-pid-grid 2 2 "38;5;7;49"
     (format nil "Kill process ~s with PID ~d?" "sleep" 54321))
    (cl-weave:expect
     (ntop.e2e::%route-owned-dialog-pid-matches-p cancel-grid pid)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-owned-dialog-pid-matches-p wrong-pid-grid pid)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-owned-dialog-safe-cancel-input cancel-grid pid)
     :to-equal (ntop.e2e::%route-safe-dialog-click-input cancel-grid))
    (criterion-test-write simple-grid 6 2 "38;5;7;49"
                          "Press ENTER to confirm")
    (cl-weave:expect
     (ntop.e2e::%route-simple-kill-confirmation-p simple-grid pid)
     :to-be t)
    (setf simple-input
          (ntop.e2e::%route-owned-dialog-safe-cancel-input simple-grid pid))
    (cl-weave:expect
     simple-input
     :to-equal (ntop.e2e::%route-key-input "Escape"))
    (cl-weave:expect
     (ntop.e2e::%route-simple-kill-confirmation-p wrong-pid-grid pid)
     :to-be nil)
    (handler-case
        (ntop.e2e::%route-owned-dialog-safe-cancel-input wrong-pid-grid pid)
      (error (condition)
        (setf wrong-pid-error condition)))
    (cl-weave:expect (not (null wrong-pid-error)) :to-be t)
    (criterion-test-write unsafe-grid 6 2 "38;5;7;49" "1: Yes")
    (handler-case
        (ntop.e2e::%route-owned-dialog-safe-cancel-input unsafe-grid pid)
      (error (condition)
        (setf unsafe-error condition)))
    (cl-weave:expect (not (null unsafe-error)) :to-be t))))

(cl-weave:describe
 "btm-self mode applicability metadata"
 (cl-weave:it
  "selects extension probes from subcase metadata"
  (cl-weave:expect
   (ntop.e2e::%route-btm-self-mode-na-p
    "btm-self" (list (cons 1 (list :probe :extension))))
   :to-be t))
 (cl-weave:it
  "selects restore probes from subcase metadata"
  (cl-weave:expect
   (ntop.e2e::%route-btm-self-mode-na-p
    "btm-self" (list (cons 1 (list :probe :restore))))
   :to-be t))
 (cl-weave:it
  "does not select extension probes in comparison mode"
  (cl-weave:expect
   (ntop.e2e::%route-btm-self-mode-na-p
    "ntop" (list (cons 1 (list :probe :extension))))
   :to-be nil))
 (cl-weave:it
  "does not select unrelated behavior probes"
  (cl-weave:expect
   (ntop.e2e::%route-btm-self-mode-na-p
    "btm-self" (list (cons 1 (list :probe :cpu-frequency))))
   :to-be nil))
 (cl-weave:it
  "requires every selected subcase to be mode-only"
  (cl-weave:expect
   (ntop.e2e::%route-btm-self-mode-na-p
    "btm-self"
    (list (cons 1 (list :probe :extension))
          (cons 2 (list :probe :cpu-frequency))))
   :to-be nil)))
