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
  "classifies owned-child readiness timeouts and keeps probe-limit manual results"
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
                     "reference"
                     (namestring (merge-pathnames "candidate" directory))
                     24 80 250 1 nil directory))))
           (let ((route-status
                   (ntop.e2e::%route-result-status route (list result))))
             (progn
               (cl-weave:expect (getf result :status) :to-equal :manual)
               (cl-weave:expect
                (first (getf result :evidence))
                :to-equal
                (namestring
                 (merge-pathnames "reference-manual.md" directory))))
             (cl-weave:expect (getf result :classification) :to-be nil)
             (cl-weave:expect route-status :to-equal :manual)
             (cl-weave:expect
              (ntop.e2e::%route-results-exit-code
               (list (list :status route-status)))
              :to-equal 0)))
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
         (subcase (list :context "expanded widget"))
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
   :to-be-truthy)))

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
 "owned child readiness"
 (cl-weave:it
  "accepts an already visible owned PID marker without extending the deadline"
  (let* ((pid 12345)
         (deadline
           (+ (ntop.e2e::%now)
              (ntop.e2e::%seconds-to-ticks 30d0)))
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0 deadline 30d0))
         (raw
           (ntop.e2e::%route-octets
            (format nil "~c[1;1H~d ntop-owned-~d" #\Escape pid pid))))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
      (cl-weave:expect result :to-equal :ready)
      (cl-weave:expect (string= diagnostic "Owned PID 12345 marker row is visible.") :to-be t)
      (cl-weave:expect
       (ntop.e2e::e2e-session-deadline session)
       :to-equal deadline))))
 (cl-weave:it
  "returns marker-row and owned-dialog timeouts at the readiness deadline"
  (let* ((ntop.e2e::*route-process-readiness-timeout-seconds* 0d0)
         (pid 12345)
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0
                                        (ntop.e2e::%now) 0d0))
         (raw
           (make-array 0 :element-type '(unsigned-byte 8)
                       :adjustable t :fill-pointer 0)))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
      (cl-weave:expect result :to-equal :timeout)
      (cl-weave:expect
       (string= diagnostic
                "Owned PID 12345 marker row not ready before timeout.")
       :to-be t)
      (cl-weave:expect
       (search "marker row not ready before timeout" diagnostic)
       :to-satisfy (function plusp)))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-dialog session raw 12 80 pid)
      (cl-weave:expect result :to-equal :timeout)
      (cl-weave:expect
       (string= diagnostic
                "Owned PID 12345 kill dialog not ready before timeout.")
       :to-be t))))
 (cl-weave:it
  "waits for an exact PID dialog after marker selection"
  (let* ((pid 12345)
         (marker (format nil "~d ntop-owned-~d" pid pid))
         (grid (criterion-test-grid 12 80 marker))
         (session
           (ntop.e2e::%make-e2e-session
            nil nil nil nil 0
            (+ (ntop.e2e::%now) (ntop.e2e::%seconds-to-ticks 30d0))
            30d0))
         (raw
           (make-array 0 :element-type '(unsigned-byte 8)
                       :adjustable t :fill-pointer 0))
         (frames
           (list
            (ntop.e2e::%route-octets
             (format nil "~c[3;3HKill process sleep with PID 54321?"
                     #\Escape))
            (ntop.e2e::%route-octets
             (format nil "~c[3;3HKill process sleep with PID ~d?"
                     #\Escape pid))))
         (read-count 0)
         (read-symbol (find-symbol "%ROUTE-READ-INTERVAL" "NTOP.E2E"))
         (original-read-interval (symbol-function read-symbol)))
    (criterion-test-write grid 10 2 "38;5;0;48;5;12" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p grid pid)
     :to-be t)
    (criterion-test-write grid 10 2 "38;5;7;49" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p grid pid)
     :to-be nil)
    (unwind-protect
         (progn
           (setf (symbol-function read-symbol)
                 (lambda (ignored-session target ignored-seconds)
                   (declare (ignore ignored-session ignored-seconds))
                   (incf read-count)
                   (ntop.e2e::%append-octets target (pop frames))))
           (multiple-value-bind (result diagnostic)
               (ntop.e2e::%route-wait-owned-dialog
                session raw 12 80 pid)
             (cl-weave:expect result :to-equal :ready)
             (cl-weave:expect
              diagnostic
              :to-equal "Owned PID 12345 kill dialog is visible.")
             (cl-weave:expect read-count :to-equal 2)
             (cl-weave:expect
              (ntop.e2e::%route-owned-dialog-pid-matches-p
               (ntop.e2e::%route-grid raw 12 80) pid)
              :to-be t)))
      (setf (symbol-function read-symbol) original-read-interval))))
 (cl-weave:it
  "reports observed child and dialog state after confirmation input"
  (progn
   (cl-weave:expect
    (ntop.e2e::%route-sleep-child-action-actual 62070 t t)
    :to-equal
    "owned PID 62070 after confirmation input; child alive yes; exact-PID dialog open")
   (cl-weave:expect
    (ntop.e2e::%route-sleep-child-action-actual 62070 nil nil)
    :to-equal
    "owned PID 62070 after confirmation input; child alive no; exact-PID dialog closed"))))
