(in-package #:ntop.e2e)

(defun %route-all-expected-actual (route mode subcase reference candidate)
  (let* ((comparable-p (route-comparable-p route))
         (difference
           (and comparable-p
                (%route-projection-first-difference
                 (getf reference :projection)
                 (getf candidate :projection))))
         (reference-state
           (%route-action-state-p route mode subcase reference :reference))
         (candidate-state
           (%route-action-state-p route mode subcase candidate :candidate)))
    (loop for side in (list :reference :candidate)
          for result in (list reference candidate)
          when (eq :manual (getf result :readiness-status))
            do (error 'route-side-capture-error
                      :side side
                      :cause (or (getf result :readiness-diagnostic)
                                 (getf result :cause)
                                 "Process-data readiness timed out.")
                      :result result))
    (cond
      ((and (not comparable-p) candidate-state)
       (values t nil "candidate expected state was observed" nil))
      ((and comparable-p difference)
       (values nil difference
               "condition-A reference and candidate differ" nil))
      ((and reference-state candidate-state)
       (values t nil "expected states and condition-A match" nil))
      (t
       (values nil difference
               (cond
                 ((or (eq reference-state :manual)
                      (eq candidate-state :manual))
                  "state observer could not determine the expected result")
                 ((not reference-state)
                  "reference expected state was not observed")
                 (t "candidate expected state was not observed"))
               nil)))))

(defun %route-interactive-actual (difference reference candidate)
  (if (or (eq :manual (getf reference :readiness-status))
          (eq :manual (getf candidate :readiness-status)))
      (format nil "readiness timeout; reference ~a; candidate ~a"
              (or (getf reference :readiness-diagnostic)
                  (getf reference :actual)
                  "diagnostic unavailable")
              (or (getf candidate :readiness-diagnostic)
                  (getf candidate :actual)
                  "diagnostic unavailable"))
      (format nil "reference/candidate condition-A ~a"
              (if difference "differs" "matches"))))

(defun %route-run-interactive-pair (route subcase description mode btm ntop
                                    reference-prefix candidate-prefix rows
                                    columns rate timeout config-path
                                    evidence-directory)
  (let* ((reference
          (%route-capture-side
           :reference #'%route-run-interactive-side btm route subcase
           reference-prefix rows columns rate timeout config-path
           evidence-directory))
         (candidate
          (%route-capture-side
           :candidate #'%route-run-interactive-side ntop route subcase
           candidate-prefix rows columns rate timeout config-path
           evidence-directory))
         (applicability-result
           (%route-fixture-applicability-result
            route subcase mode reference candidate)))
    (when applicability-result
      (return-from %route-run-interactive-pair applicability-result))
    (let* ((reference-state
             (%route-action-state-p route mode subcase reference :reference))
           (candidate-state
             (%route-action-state-p route mode subcase candidate :candidate))
           (passed nil)
           (difference nil)
           (cause nil))
      (multiple-value-setq (passed difference cause)
        (%route-all-expected-actual route mode subcase reference candidate))
      (when passed
        (multiple-value-bind (reference-ok reference-actual)
            (%route-content-check route subcase reference)
          (multiple-value-bind (candidate-ok candidate-actual)
              (%route-content-check route subcase candidate)
            (unless (and reference-ok candidate-ok)
              (setf passed nil
                    reference-state (and reference-state reference-ok)
                    candidate-state (and candidate-state candidate-ok)
                    cause (format nil "reference ~a; candidate ~a"
                                  reference-actual candidate-actual))))))
      (setf (getf reference :passed) (eq reference-state t)
            (getf candidate :passed) (eq candidate-state t))
      (%route-paired-result
       description (route-expected route) passed
       (%route-interactive-actual difference reference candidate)
       cause difference reference candidate))))

(defun %route-run-control-pair
    (route description base-args candidate-args mode btm ntop reference-prefix
           candidate-prefix timeout)
  (let* ((reference (%route-capture-side
                     :reference (function %route-run-cli-side) btm base-args
                     reference-prefix timeout))
         (candidate (%route-capture-side
                     :candidate (function %route-run-cli-side) ntop candidate-args
                     candidate-prefix timeout))
         (reference-output (%route-cli-output reference))
         (candidate-output (%route-cli-output candidate))
         (reference-valid (%route-control-output-valid-p route btm reference))
         (candidate-valid (%route-control-output-valid-p route ntop candidate))
         (equal-p (and (eql (getf reference :exit-code)
                            (getf candidate :exit-code))
                       (string= reference-output candidate-output)))
         (passed (and reference-valid candidate-valid
                      (or (not (string= mode "btm-self")) equal-p)))
         (difference (when (and (string= mode "btm-self") (not equal-p))
                       "btm-self CLI output differs")))
    (setf (getf reference :passed) reference-valid
          (getf candidate :passed) candidate-valid)
    (%route-paired-result
     description (route-expected route) passed
     (format nil "btm: ~a; candidate: ~a"
             (getf reference :actual) (getf candidate :actual))
     (cond ((not reference-valid)
            "btm control lacked the expected help or version structure")
           ((not candidate-valid)
            "candidate lacked the expected help or version structure")
           (difference difference)
           (t "both CLI controls have the expected structure"))
     difference reference candidate)))

(defun %route-run-terminal-pair (route subcase description btm ntop
                                       reference-prefix candidate-prefix rows columns
                                       timeout config-path evidence-directory)
  (let* ((reference
          (%route-capture-side
           :reference #'%route-run-terminal-side btm route subcase
           reference-prefix rows columns timeout config-path evidence-directory))
         (candidate
          (%route-capture-side
           :candidate #'%route-run-terminal-side ntop route subcase
           candidate-prefix rows columns timeout config-path evidence-directory))
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
  (let* ((reference
          (%route-capture-side
           :reference #'%route-run-exit-side btm route subcase reference-prefix
           rows columns rate timeout config-path evidence-directory))
         (candidate
          (%route-capture-side
           :candidate #'%route-run-exit-side ntop route subcase candidate-prefix
           rows columns rate timeout config-path evidence-directory))
         (reference-code (getf reference :exit-code))
         (candidate-code (getf candidate :exit-code))
         (passed (and (getf reference :passed) (getf candidate :passed)
                      (or (not (string= mode "btm-self"))
                          (eql reference-code candidate-code))))
         (difference (when (and (string= mode "btm-self")
                                (not (eql reference-code candidate-code)))
                       "btm-self child exit statuses differ")))
    (%route-paired-result
     description "the documented quit input exits the child with status 0"
     passed
     (format nil "reference ~a; candidate ~a"
             (getf reference :actual) (getf candidate :actual))
     (if passed "both children exited successfully"
       "a quit input did not produce the expected exit")
     difference reference candidate)))

(defun %route-run-resize-pair (route subcase description btm ntop
                                     reference-prefix candidate-prefix rate timeout
                                     config-path evidence-directory)
  (let* ((reference
          (%route-capture-side
           :reference (function %route-run-resize-side) btm route subcase reference-prefix
           rate timeout config-path evidence-directory))
         (candidate
          (%route-capture-side
           :candidate (function %route-run-resize-side) ntop route subcase candidate-prefix
           rate timeout config-path evidence-directory))
         (reference-grid (getf reference :grid))
         (candidate-grid (getf candidate :grid))
         (reference-initial (getf reference :initial-grid))
         (candidate-initial (getf candidate :initial-grid))
         (difference (%route-projection-first-difference
                      (getf reference :projection)
                      (getf candidate :projection)))
         (reference-size-p
          (and (= (grid-width reference-initial) 80)
               (= (grid-height reference-initial) 24)
               (= (grid-width reference-grid) 120)
               (= (grid-height reference-grid) 36)))
         (candidate-size-p
          (and (= (grid-width candidate-initial) 80)
               (= (grid-height candidate-initial) 24)
               (= (grid-width candidate-grid) 120)
               (= (grid-height candidate-grid) 36)))
         (sizes-p (and reference-size-p candidate-size-p))
         (passed (and sizes-p (null difference))))
    (setf (getf reference :passed) reference-size-p
          (getf candidate :passed) candidate-size-p)
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
  (let* ((reference
          (%route-capture-side
           :reference #'%route-run-sleep-child-side btm route subcase
           reference-prefix rows columns rate timeout config-path
           evidence-directory))
         (candidate
          (%route-capture-side
           :candidate #'%route-run-sleep-child-side ntop route subcase
           candidate-prefix rows columns rate timeout config-path
           evidence-directory))
         (manual
          (and (or (eq :manual (getf reference :status))
                   (eq :manual (getf candidate :status)))
               (or (eq :manual (getf reference :status))
                   (getf reference :passed))
               (or (eq :manual (getf candidate :status))
                   (getf candidate :passed))))
         (passed (and (getf reference :passed) (getf candidate :passed)))
         (result
          (%route-paired-result
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
                      (getf manual-result :evidence)))
        (remf result :classification)))
    result))

(defun %route-run-exception (route subcase description ntop prefix rows
                                   columns timeout)
  (declare (ignore description))
  (let* ((result (%route-capture-side
                  :candidate #'%route-run-exception-side prefix rows columns
                  timeout))
         (passed (getf result :passed)))
    (list :passed passed
          :classification (unless passed :candidate-failure)
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
