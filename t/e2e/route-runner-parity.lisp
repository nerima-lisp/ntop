(in-package #:ntop.e2e)

(defun %route-extension-state-p (route subcase candidate)
  (let* ((action (route-action route))
         (initial (getf candidate :initial-grid))
         (steps (getf candidate :action-grids))
         (projections (mapcar #'project-criterion-grid steps)))
    (cond
      ((string= action "Cycle selected widget view")
       (let* ((context (or (getf subcase :context) ""))
              (expected-count (if (string= context "Temperatures") 3 2)))
         (and (= (length steps) expected-count)
              (criterion-projections-equal-p
               (project-criterion-grid initial) (car (last projections)))
              (loop with previous = (project-criterion-grid initial)
                    for projection in projections
                    always (prog1
                               (not (criterion-projections-equal-p
                                     previous projection))
                             (setf previous projection))))))
      ((string= action "Scroll history backward")
       (and (= (length steps) 1)
            (%route-grid-first-difference initial (first steps))))
      ((member action '("Scroll history forward" "Return graph to current")
               :test #'string=)
       (and (= (length steps) 2)
            (%route-grid-first-difference initial (first steps))
            (%route-grid-first-difference (first steps) (second steps))
            (criterion-projections-equal-p
             (project-criterion-grid initial) (car (last projections)))))
      (t t))))

(defun %route-action-state-p (route mode subcase side side-name)
  (let* ((action (route-action route))
         (before (getf side :initial-projection))
         (after (getf side :projection))
         (changed (and before after
                       (not (criterion-projections-equal-p before after))))
         (stable (and before after
                      (criterion-projections-equal-p before after))))
    (cond
      ((eq (route-kind route) :extension)
       (or (and (string= mode "btm-self")
                (not (route-comparable-p route)))
           (%route-extension-state-p route subcase side)))
      ((and (eq (route-kind route) :key)
            (string= action "Documented horizontal movement"))
       (if (string= mode "btm-self") stable
           (if (eq side-name :candidate) changed stable)))
      ((eq (route-kind route) :search-field)
       (%route-search-token-visible-p route subcase (getf side :grid)))
      ((eq (route-kind route) :key) changed)
      ((eq (route-kind route) :mouse)
       (if (member action
                   (list "Ignore other buttons" "Ignore pointer motion")
                   :test (function string=))
           stable
           changed))
      ((eq (route-kind route) :flag)
       (let ((name (ntop.app.domain:parity-flag-snake-case
                    (route-entry route))))
         (cond
           ((member name (list "disable_keys" "disable_click" "free_arc")
                    :test (function string=))
            stable)
           ((string= name "basic")
            (%route-basic-mode-visible-p (getf side :grid)))
           (t
            (and (getf side :grid)
                 (getf side :projection)
                 (getf side :evidence))))))
      ((and (eq (route-kind route) :additional)
            (string= (route-id route) "basic-mode"))
       (%route-basic-mode-visible-p (getf side :grid)))
      (t nil))))

(defun %route-all-expected-actual (route mode subcase reference candidate)
  (let* ((difference (%route-projection-first-difference
                      (getf reference :projection)
                      (getf candidate :projection)))
         (comparable-p (route-comparable-p route))
         (action-p
           (if (and (eq (route-kind route) :extension)
                    (not comparable-p))
               (%route-action-state-p route mode subcase candidate :candidate)
               (and (%route-action-state-p
                     route mode subcase reference :reference)
                    (%route-action-state-p
                     route mode subcase candidate :candidate)))))
    (cond
      ((not action-p)
       (values nil difference
               "expected route state was not observed on the reference and candidate"))
      ((and comparable-p difference)
       (values nil difference
               (if (string= mode "btm-self")
                   "btm-self condition-A projections differ"
                   "condition-A reference and candidate differ")))
      (t
       (values t difference
               (if comparable-p
                   "condition-A projections match"
                   "approved §D extension deviation"))))))

(progn
  (defun %route-basic-mode-visible-p (grid)
    (let ((text (%route-grid-text grid)))
      (and (every (lambda (label)
                    (search label text :test (function char-equal)))
                  (list "CPU" "Memory" "Network"))
           (null (%route-grid-headers grid)))))
  (defun %route-content-check (route subcase side)
    (case (route-kind route)
      (:column
       (multiple-value-bind (passed expected-p)
           (%route-column-visible-p route (getf side :grid))
         (values passed
                 (format nil "column header ~s was ~a"
                         (ntop.app.domain:parity-column-header
                          (route-entry route))
                         (if expected-p "present" "absent")))))
      (:search-field
       (let ((visible-p (%route-search-token-visible-p
                         route subcase (getf side :grid))))
         (values visible-p
                 (format nil "search query for ~s was ~a"
                         (getf subcase :input)
                         (if visible-p "accepted without syntax error"
                             "not visible or rejected")))))
      (:additional
       (if (string= (route-id route) "basic-mode")
           (values (%route-basic-mode-visible-p (getf side :grid))
                   "basic mode shows CPU, Memory, Network, and no process table")
           (values t "")))
      (otherwise (values t "")))))

(defun %route-pair-evidence (reference candidate)
  (remove nil
          (mapcar (lambda (side)
                    (let ((path (getf side :evidence)))
                      (when path
                        (if (pathnamep path) (namestring path) path))))
                  (list reference candidate))))

(defun %route-paired-result (description expected passed actual cause difference
                             reference candidate)
  (list :passed passed
        :input description
        :expected expected
        :actual actual
        :cause cause
        :first-difference difference
        :evidence (%route-pair-evidence reference candidate)))

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
                  cause (format nil "reference ~a; candidate ~a"
                                reference-actual candidate-actual))))))
    (%route-paired-result
     description (route-expected route) passed
     (format nil "reference/candidate condition-A ~a"
             (if difference "differs" "matches"))
     cause difference reference candidate)))

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
         (passed (and (getf reference :passed) (getf candidate :passed))))
    (%route-paired-result
     description (route-expected route) passed
     (format nil "reference ~a; candidate ~a"
             (getf reference :actual) (getf candidate :actual))
     (if passed "each side proved the expected owned-child state"
         "a side failed the expected safe state for its owned child")
     nil reference candidate)))

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

(defun %route-grid-first-difference (left right)
  (when (and left right)
    (loop for row below (min (grid-height left) (grid-height right))
          do (loop for column below (min (grid-width left) (grid-width right))
                   for left-cell = (grid-cell-at left row column)
                   for right-cell = (grid-cell-at right row column)
                   unless (and (char= (cell-character left-cell)
                                     (cell-character right-cell))
                               (equalp (cell-foreground left-cell)
                                       (cell-foreground right-cell))
                               (equalp (cell-background left-cell)
                                       (cell-background right-cell))
                               (eql (cell-bold-p left-cell)
                                    (cell-bold-p right-cell)))
                     do (return-from %route-grid-first-difference
                          (format nil "row ~d column ~d: reference ~s; candidate ~s"
                                  (1+ row) (1+ column)
                                  (list (cell-character left-cell)
                                        (cell-foreground left-cell)
                                        (cell-background left-cell)
                                        (cell-bold-p left-cell))
                                  (list (cell-character right-cell)
                                        (cell-foreground right-cell)
                                        (cell-background right-cell)
                                        (cell-bold-p right-cell))))))
    (unless (and (= (grid-height left) (grid-height right))
                 (= (grid-width left) (grid-width right)))
      (format nil "grid dimensions differ: ~d x ~d vs ~d x ~d"
              (grid-width left) (grid-height left)
              (grid-width right) (grid-height right)))))

(defun %route-grid-headers (grid)
  (loop for row below (grid-height grid)
        for text = (criterion-row-text grid row)
        when (criterion-process-header-row-p
              grid row (criterion-projection-frames grid))
          collect text))

(defun %route-column-visible-p (route grid)
  (let* ((entry (route-entry route))
         (header (ntop.app.domain:parity-column-header entry))
         (condition (ntop.app.domain:parity-column-condition entry))
         (expected-p (not (search "optional" condition
                                  :test #'char-equal))))
    (values (if expected-p
                (some (lambda (row) (search header row))
                      (%route-grid-headers grid))
                (not (some (lambda (row) (search header row))
                           (%route-grid-headers grid))))
            expected-p)))

(defun %route-search-token-visible-p (route subcase grid)
  (declare (ignore route))
  (let* ((query (%route-search-query (getf subcase :input)))
         (text (and grid (%route-grid-text grid)))
         (prompt (format nil "> ~a" query))
         (errors
           (list "Invalid numeric value"
                 "Missing numeric value"
                 "Invalid process search field"
                 "Expected a process search expression"
                 "Missing value for process search field"
                 "Invalid duration value"
                 "Unknown duration unit"
                 "Invalid process search query"
                 "Missing closing quotation"
                 "Missing closing parentheses"
                 "Missing expression")))
    (and text
         (search prompt text)
         (not (some (lambda (message)
                      (search message text :test (function char-equal)))
                    errors)))))

(defun %route-marker-input (marker grid)
  (let* ((frames (criterion-projection-frames grid))
         (point
           (case marker
             (:basic-mode-right-arrow
              (loop for row below (grid-height grid)
                    for column = (position (code-char 9658)
                                           (criterion-row-text grid row))
                    when column return (list column row)))
             ((:focus-cpu :focus-memory)
              (let ((target (if (eq marker :focus-cpu) "CPU" "Memory")))
                (loop for (top bottom left right) in frames
                      when (search target (criterion-row-text grid top)
                                   :test #'char-equal)
                        return (list (floor (+ left right) 2)
                                     (floor (+ top bottom) 2))))))))
    (unless point
      (error "Could not locate safe route target ~s in the initial frame."
             marker))
    (format nil "~c[<0;~d;~dM" #\Escape (1+ (first point))
            (1+ (second point)))))

(defun %route-manual-result (route subcase directory)
  (let ((path (merge-pathnames
               (format nil "~a-manual.md" (route-id route)) directory))
        (input (getf subcase :input)))
    (%write-text
     path
     (format nil
             (concatenate 'string
                          "# ~a~%~%Procedure: Start the candidate monitor, then "
                          "run `~a` in a second terminal. Compare powermetrics' average for each "
                          "cluster with the candidate display for that cluster; require values "
                          "within 10%.~%~%"
                          "Expected: ~a~%~%Reason: ~a~%~%"
                          "Result: Not run automatically. Record observed "
                          "cluster averages and the comparison here.~%")
             (route-action route) input
             (route-expected route) (route-manual-reason route)))
    (list :status :manual
          :passed nil
          :input (%route-input-description route subcase)
          :expected (route-expected route)
          :actual "Manual procedure documented; result pending."
          :cause (route-manual-reason route)
          :evidence (list (namestring path)))))

(defun %route-wait-owned-child (process timeout-seconds)
  (let ((deadline (+ (%now) (%seconds-to-ticks timeout-seconds))))
    (loop while (sb-ext:process-alive-p process)
          do (when (>= (%now) deadline)
               (error "Harness-owned child ~d did not exit."
                      (sb-ext:process-pid process)))
             (sleep 0.05d0))
    (sb-ext:process-exit-code process)))

(defun %route-substring-count (text needle)
  (loop with start = 0
        for position = (search needle text :start2 start)
        while position
        count t
        do (setf start (1+ position))))
