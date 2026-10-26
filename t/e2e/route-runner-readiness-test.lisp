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
  "retains readiness timeout diagnostics in the paired report"
  (let* ((route (ntop.e2e::make-route-case
                 :id "readiness-test" :kind :extension :comparable-p nil))
         (reference
           (list :readiness-status :manual
                 :readiness-result :timeout
                 :readiness-diagnostic
                 "Expected process heading Command(n); observed Name(n)."))
         (candidate
           (list :readiness-status :manual
                 :readiness-result :timeout
                 :readiness-diagnostic
                 "Expected process heading Command(n); observed Name(n).")))
    (multiple-value-bind (passed difference cause status)
        (ntop.e2e::%route-all-expected-actual
         route "btm-self" nil reference candidate)
      (let ((report
              (ntop.e2e::%route-paired-result
               "readiness timeout" "Processes rows visible" passed
               (ntop.e2e::%route-interactive-actual
                difference reference candidate)
               cause difference reference candidate)))
        (cl-weave:expect status :to-equal :manual)
        (cl-weave:expect (getf report :passed) :to-be nil)
        (cl-weave:expect (search "observed Name(n)" (getf report :actual))
                         :to-satisfy (function plusp))
        (cl-weave:expect (search "observed Name(n)" (getf report :cause))
                         :to-satisfy (function plusp)))))))
