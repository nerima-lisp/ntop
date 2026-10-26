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
                     "btm" "ntop" "reference" "candidate"
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
                     "btm" "ntop" "reference" "candidate"
                     24 80 250 1 nil directory))))
           (let ((route-status
                   (ntop.e2e::%route-result-status route (list result))))
             (cl-weave:expect (getf result :status) :to-equal :manual)
             (cl-weave:expect (getf result :classification) :to-be nil)
             (cl-weave:expect route-status :to-equal :manual)
             (cl-weave:expect
              (ntop.e2e::%route-results-exit-code
               (list (list :status route-status)))
              :to-equal 0)))
      (setf (symbol-function side-symbol) original-side)
      (uiop:delete-directory-tree directory :validate t)))))
