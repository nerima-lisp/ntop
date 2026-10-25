(in-package #:ntop.e2e)

(defun %route-all-expected-actual (route mode subcase reference candidate)
  (let* ((difference (%route-projection-first-difference
                      (getf reference :projection) (getf candidate :projection)))
         (comparable-p (route-comparable-p route))
         (reference-state (%route-action-state-p route mode subcase reference :reference))
         (candidate-state (%route-action-state-p route mode subcase candidate :candidate)))
    (cond
     ((and comparable-p difference)
      (if (and (member (route-id route)
                       (list "key-9" "key-10" "key-11")
                       :test (function string=))
               (search "Move widget selection" (route-action route))
               (or (eq reference-state :manual)
                   (eq candidate-state :manual)))
          (values nil difference
                  "selected-widget focus was ambiguous; verify the border manually"
                  :manual)
        (values nil difference
                (if (string= mode "btm-self")
                    "btm-self condition-A projections differ"
                  "condition-A reference and candidate differ"))))
     ((and (eq reference-state :manual) (eq candidate-state :manual))
      (values nil nil
              (cond
               ((string= (route-id route) "mouse-74")
                (concatenate (quote string)
                             "The Processes pane showed No data even with a live "
                             "harness-owned child; no rows or selected sort marker "
                             "were available to compare."))
               ((string= (route-id route) "mouse-75")
                (concatenate (quote string)
                             "condition-A stayed equal after the BTM CPU click; verify "
                             "the selected series manually."))
               ((string= (route-id route) "mouse-77")
                "The BTM screen had one Battery panel and no visible selection tabs.")
               ((and (member (route-id route) '("key-9" "key-10" "key-11")
                             :test #'string=)
                     (search "Move widget selection" (route-action route)))
                (if (string= (route-id route) "key-9")
                    (format nil "~a ~a ~a ~a"
                            "The default 60x220 BTM layout showed several widgets."
                            "Ctrl+Right, Shift+Right, L, and D produced no captured character"
                            "or ANSI-style change, and the cursor stayed hidden."
                            "Selected-widget focus could not be observed.")
                    (concatenate 'string
                                 "Input reached BTM; condition-A cells and attributes did not "
                                 "expose focus, and the cursor stayed hidden.")))
               ((and (string= (route-id route) "key-38")
                     (%route-widget-no-data-p
                      (getf reference :initial-grid) "Processes")
                     (%route-widget-no-data-p
                      (getf candidate :initial-grid) "Processes")
                     (null (%route-grid-headers
                            (getf reference :initial-grid)))
                     (null (%route-grid-headers
                            (getf candidate :initial-grid))))
                (format nil "~a ~a"
                        "Both BTM Processes panes showed No data and no sort headers."
                        "The c CPU-sort order could not be observed."))
               (t
                (%route-manual-reason route subcase reference candidate)))
              :manual))
     ((or (eq reference-state :manual) (eq candidate-state :manual))
      (values nil difference "the expected route state is not measurable on at least one side"
              :manual))
     ((if (and (eq (route-kind route) :extension) (not comparable-p))
          candidate-state (and reference-state candidate-state))
      (values t difference
              (if comparable-p "condition-A projections match"
                "approved §D extension deviation")))
     ((and (string= mode "ntop") (null reference-state) (null candidate-state))
      (values nil difference
              (if difference
                  (concatenate 'string
                               "The condition-A projection did not establish the expected "
                               "state. Verify the route manually.")
                (concatenate 'string
                             "Both condition-A projections matched, but neither saved frame "
                             "showed the expected transition. Verify the documented "
                             "before/action/after procedure manually."))
              :manual))
     (t (values nil difference
                (if reference-state
                    "The expected transition was observed on the reference, but not the candidate."
                  (concatenate 'string
                               "The expected transition was observed on the candidate, "
                               "but not the reference.")))))))

(defun %route-run-interactive-pair (route subcase description mode btm ntop
                                    reference-prefix candidate-prefix rows
                                    columns rate timeout config-path
                                    evidence-directory)
  (let* ((reference (%route-run-interactive-side
                     btm route subcase reference-prefix rows columns rate timeout
                     config-path evidence-directory))
         (candidate (%route-run-interactive-side
                     ntop route subcase candidate-prefix rows columns rate timeout
                     config-path evidence-directory))
         (passed nil) (difference nil) (cause nil) (status nil))
    (multiple-value-setq (passed difference cause status)
      (%route-all-expected-actual route mode subcase reference candidate))
    (when passed
      (multiple-value-bind (reference-ok reference-actual)
          (%route-content-check route subcase reference)
        (multiple-value-bind (candidate-ok candidate-actual)
            (%route-content-check route subcase candidate)
          (unless (and reference-ok candidate-ok)
            (setf passed nil
                  cause (format nil "reference ~a; candidate ~a"
                                reference-actual candidate-actual))))))
    (let ((result (%route-paired-result
                   description (route-expected route) passed
                   (format nil "reference/candidate condition-A ~a"
                           (if difference "differs" "matches"))
                   cause difference reference candidate)))
      (when (eq status :manual)
        (let ((manual (%route-manual-result route subcase evidence-directory
                                            cause reference-prefix)))
          (setf (getf result :status) :manual
                (getf result :expected) (getf manual :expected)
                (getf result :cause) (getf manual :cause)
                (getf result :evidence)
                (append (getf result :evidence) (getf manual :evidence)))))
      result)))

(defun %route-run-control-pair
    (route description base-args candidate-args mode btm ntop reference-prefix
     candidate-prefix timeout)
  (let* ((reference (%route-run-cli-side btm base-args reference-prefix timeout))
         (candidate (%route-run-cli-side ntop candidate-args candidate-prefix
                                         timeout))
         (reference-output (%route-cli-output reference))
         (candidate-output (%route-cli-output candidate))
         (reference-valid (%route-control-output-valid-p route btm reference))
         (candidate-valid (%route-control-output-valid-p route ntop candidate))
         (equal-p (and (eql (getf reference :exit-code)
                            (getf candidate :exit-code))
                       (string= reference-output candidate-output)))
         (passed (and reference-valid candidate-valid
                      (or (not (string= mode "btm-self")) equal-p))))
    (%route-paired-result
     description (route-expected route) passed
     (format nil "btm: ~a; candidate: ~a"
             (getf reference :actual) (getf candidate :actual))
     (cond ((not reference-valid)
            "btm control lacked the expected help or version structure")
           ((not candidate-valid)
            "candidate lacked the expected help or version structure")
           ((and (string= mode "btm-self") (not equal-p))
            "btm-self CLI output differs")
           (t "both CLI controls have the expected structure"))
     nil reference candidate)))

(defun %route-run-terminal-pair (route subcase description btm ntop
                                 reference-prefix candidate-prefix rows columns
                                 timeout config-path evidence-directory)
  (let* ((reference (%route-run-terminal-side
                     btm route subcase reference-prefix rows columns timeout
                     config-path evidence-directory))
         (candidate (%route-run-terminal-side
                     ntop route subcase candidate-prefix rows columns timeout
                     config-path evidence-directory))
         (passed (and (getf reference :passed) (getf candidate :passed))))
    (%route-paired-result
     description (route-expected route) passed
     (format nil "reference ~a; candidate ~a"
             (getf reference :actual) (getf candidate :actual))
     (if passed "terminal settings match the PTY baseline"
         "terminal settings were not restored or a child did not exit")
     nil reference candidate)))

(defun %route-run-exit-pair (route subcase description mode btm ntop
                             reference-prefix candidate-prefix rows columns rate
                             timeout config-path evidence-directory)
  (let* ((reference (%route-run-exit-side
                     btm route subcase reference-prefix rows columns rate timeout
                     config-path evidence-directory))
         (candidate (%route-run-exit-side
                     ntop route subcase candidate-prefix rows columns rate timeout
                     config-path evidence-directory))
         (reference-code (getf reference :exit-code))
         (candidate-code (getf candidate :exit-code))
         (passed (and (getf reference :passed) (getf candidate :passed)
                      (or (not (string= mode "btm-self"))
                          (eql reference-code candidate-code)))))
    (%route-paired-result
     description "the documented quit input exits the child with status 0"
     passed
     (format nil "reference ~a; candidate ~a"
             (getf reference :actual) (getf candidate :actual))
     (if passed "both children exited successfully"
         "a quit input did not produce the expected exit")
     nil reference candidate)))

(defun %route-run-resize-pair (route subcase description btm ntop
                               reference-prefix candidate-prefix rate timeout
                               config-path evidence-directory)
  (let* ((reference (%route-run-resize-side
                     btm route subcase reference-prefix rate timeout config-path
                     evidence-directory))
         (candidate (%route-run-resize-side
                     ntop route subcase candidate-prefix rate timeout config-path
                     evidence-directory))
         (reference-grid (getf reference :grid))
         (candidate-grid (getf candidate :grid))
         (reference-initial (getf reference :initial-grid))
         (candidate-initial (getf candidate :initial-grid))
         (difference (%route-projection-first-difference
                      (getf reference :projection)
                      (getf candidate :projection)))
         (sizes-p
           (and (= (grid-width reference-initial) 80)
                (= (grid-height reference-initial) 24)
                (= (grid-width candidate-initial) 80)
                (= (grid-height candidate-initial) 24)
                (= (grid-width reference-grid) 120)
                (= (grid-height reference-grid) 36)
                (= (grid-width candidate-grid) 120)
                (= (grid-height candidate-grid) 36)))
         (passed (and sizes-p (null difference))))
    (%route-paired-result
     description (route-expected route) passed
     (format nil "reference ~a; candidate ~a"
             (getf reference :actual) (getf candidate :actual))
     (cond ((not sizes-p) "PTY dimensions did not change from 80 by 24 to 120 by 36")
           (difference "condition-A reference and candidate differ")
           (t "both resized frames are 120 by 36 and condition-A matches"))
     difference reference candidate)))

(defun %route-run-sleep-child-pair (route subcase description mode btm ntop
                                    reference-prefix candidate-prefix rows
                                    columns rate timeout config-path
                                    evidence-directory)
  (declare (ignore mode))
  (let* ((reference (%route-run-sleep-child-side
                     btm route subcase reference-prefix rows columns rate timeout
                     config-path evidence-directory))
         (candidate (%route-run-sleep-child-side
                     ntop route subcase candidate-prefix rows columns rate timeout
                     config-path evidence-directory))
         (manual (or (eq :manual (getf reference :status))
                     (eq :manual (getf candidate :status))))
         (passed (and (getf reference :passed) (getf candidate :passed)))
         (result (%route-paired-result
                  description (route-expected route) (and passed (not manual))
                  (format nil "reference ~a; candidate ~a"
                          (getf reference :actual) (getf candidate :actual))
                  (if passed "each side proved the expected owned-child state"
                      "a side failed the expected safe state for its owned child")
                  nil reference candidate)))
    (when manual
      (let* ((cause (format nil "reference ~a; candidate ~a"
                            (getf reference :actual) (getf candidate :actual)))
             (manual-result (%route-manual-result
                             route subcase evidence-directory cause
                             reference-prefix)))
        (setf (getf result :status) :manual
              (getf result :expected) (getf manual-result :expected)
              (getf result :cause) cause
              (getf result :evidence)
              (append (getf result :evidence)
                      (getf manual-result :evidence)))))
    result))

(defun %route-run-exception (route subcase description ntop prefix rows
                             columns timeout)
  (declare (ignore description))
  (let* ((result (%route-run-exception-side prefix rows columns timeout))
         (passed (getf result :passed)))
    (list :passed passed
          :input
          (format nil
                  "Scope: exported ntop.app.infrastructure:call-with-terminal-session; ~a"
                  (%route-input-description route subcase))
          :expected (route-expected route)
          :actual (getf result :actual)
          :cause (if passed
                     "call-with-terminal-session restored termios as the error escaped"
                     (format nil "infrastructure child ~a failed" ntop))
          :first-difference nil
          :evidence (%route-pair-evidence result nil))))
