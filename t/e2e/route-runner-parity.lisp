(in-package #:ntop.e2e)

(defun %route-extension-state-p (route subcase candidate)
  (let* ((action (route-action route)) (initial (getf candidate :initial-grid))
         (steps (getf candidate :action-grids))
         (projections (mapcar (function project-criterion-grid) steps)))
    (cond
      ((string= action "Cycle selected widget view")
       (let* ((context (or (getf subcase :context) ""))
              (count (if (string= context "Temperatures") 3 2))
              (start (project-criterion-grid initial)))
         (and (= (length steps) count)
              (criterion-projections-equal-p start (car (last projections)))
              (loop with previous = start
                    for projection in projections
                    always (prog1 (not (criterion-projections-equal-p previous projection))
                             (setf previous projection))))))
      ((string= action "Scroll history backward")
       (and (= (length steps) 1)
            (%route-grid-first-difference initial (first steps))))
      ((member action (quote ("Scroll history forward" "Return graph to current"))
               :test (function string=))
       (and (= (length steps) 2)
            (%route-grid-first-difference initial (first steps))
            (%route-grid-first-difference (first steps) (second steps))
            (criterion-projections-equal-p
             (project-criterion-grid initial) (car (last projections)))))
      (t t))))

(defun %route-action-state-p (route mode subcase side side-name)
  (let* ((action (route-action route))
         (before (getf side :initial-projection)) (after (getf side :projection))
         (grid (getf side :grid)) (initial (getf side :initial-grid))
         (text (and grid (%route-grid-text grid)))
         (initial-text (and initial (%route-grid-text initial)))
         (changed (and before after (not (criterion-projections-equal-p before after))))
         (stable (and before after (criterion-projections-equal-p before after))))
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
       (%route-search-token-visible-p (getf subcase :input) grid))
      ((eq (route-kind route) :column)
 (if (%route-widget-no-data-p grid "Processes")
     :manual
     (and (%route-column-visible-p route initial)
          (%route-column-visible-p route grid))))
      ((eq (route-kind route) :key)
       (let ((headers (%route-grid-headers grid)) (old (%route-grid-headers initial)))
         (cond
           ((string= action "Open help")
            (and text (search "1 - General" text)
                 (not (search "1 - General" initial-text))))
           ((string= action "Open help search")
            (and text initial-text (search "Esc to close" text)
                 (search "1 - General" initial-text)))
           ((string= action "Open process search")
            (and text initial-text (search "Case(F1)" text) (search "> " text)
                 (not (search "Case(F1)" initial-text))))
           ((string= action "Enter search text")
 (if (string-equal (or (getf subcase :context) "") "help search")
     (and initial-text
          (search "Search:" initial-text)
          text
          (search (format nil "Search: ~a"
                          (%route-search-query
                           (%route-key-input (getf subcase :input))))
                  text))
     (and initial-text
          (search "> " initial-text)
          (%route-search-token-visible-p
           (%route-key-input (getf subcase :input)) grid))))
           ((string= action "Open process sort list")
 (and initial-text text
      (not (search "Sort By" initial-text :test (function char-equal)))
      (search "Sort By" text :test (function char-equal))))
           ((string= action "Toggle process command display")
 (if (or (%route-widget-no-data-p initial "Processes")
         (%route-widget-no-data-p grid "Processes"))
     :manual
     (and (some (lambda (row) (search "Name(n)" row)) old)
          (notany (lambda (row) (search "Name(n)" row)) headers)
          (some (lambda (row) (search "Command(n)" row)) headers))))
           ((string= action "Toggle memory values")
 (if (or (%route-widget-no-data-p initial "Processes")
         (%route-widget-no-data-p grid "Processes"))
     :manual
     (or (and (some (lambda (row) (search "Mem%(m)" row)) old)
              (some (lambda (row) (search "Mem(m)" row)) headers))
         (and (some (lambda (row) (search "Mem(m)" row)) old)
              (some (lambda (row) (search "Mem%(m)" row)) headers)))))
           ((and (string= action "Toggle kernel threads")
      (not (member :linux *features*)))
 (if (or (%route-widget-no-data-p initial "Processes")
         (%route-widget-no-data-p grid "Processes"))
     :manual
     (and stable headers)))
           (t :manual))))
      ((eq (route-kind route) :mouse)
 (cond
   ((member action (quote ("Ignore other buttons" "Ignore pointer motion"))
            :test (function string=))
    stable)
   ((member (route-id route) (quote ("mouse-71" "mouse-72"))
            :test (function string=))
    :manual)
   ((%route-mouse-target-unavailable-p subcase initial) :manual)
   (t changed)))
      ((and (eq (route-kind route) :flag)
            (member (ntop.app.domain:parity-flag-snake-case (route-entry route))
                    '("disable_keys" "disable_click" "free_arc") :test #'string=))
       stable)
      ((or (and (eq (route-kind route) :flag)
                (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                         "basic"))
           (and (eq (route-kind route) :additional)
                (string= (route-id route) "basic-mode")))
       (and (%route-basic-mode-visible-p initial)
            (%route-basic-mode-visible-p grid)))
      ((eq (route-kind route) :flag)
       (and grid (getf side :projection) (getf side :evidence)))
      (t nil))))

(defun %route-all-expected-actual (route mode subcase reference candidate)
  (let* ((difference (%route-projection-first-difference
                     (getf reference :projection) (getf candidate :projection)))
         (comparable-p (route-comparable-p route))
         (reference-state (%route-action-state-p route mode subcase reference :reference))
         (candidate-state (%route-action-state-p route mode subcase candidate :candidate)))
    (cond
      ((and comparable-p difference)
       (values nil difference
               (if (string= mode "btm-self") "btm-self condition-A projections differ"
                   "condition-A reference and candidate differ")))
      ((and (eq reference-state :manual) (eq candidate-state :manual))
       (values nil nil
               (format nil "~a in ~a lacks a measurable starting state in this run."
                       (route-action route) (or (getf subcase :context) "default"))
               :manual))
      ((or (eq reference-state :manual) (eq candidate-state :manual))
 (values nil difference "the expected route state is not measurable on at least one side"
         :manual))
      ((if (and (eq (route-kind route) :extension) (not comparable-p))
           candidate-state (and reference-state candidate-state))
       (values t difference
               (if comparable-p "condition-A projections match"
                   "approved §D extension deviation")))
      (t (values nil difference
                "expected route state was not observed on the reference and candidate")))))
(defun %route-basic-mode-visible-p (grid)
  (let ((text (and grid (%route-grid-text grid))))
    (and text
         (every (lambda (label) (search label text :test #'char-equal))
                '("RAM" "RX:" "TX:" "Total RX:" "Total TX:"
                  "Disks" "Temperature"))
         (null (%route-grid-headers grid)))))
(defun %route-content-check (route subcase side)
  (case (route-kind route)
    (:column
     (multiple-value-bind (passed expected-p)
         (%route-column-visible-p route (getf side :grid))
       (values passed
               (format nil "column header ~s was ~a"
                       (ntop.app.domain:parity-column-header (route-entry route))
                       (if expected-p "present" "absent")))))
    (:search-field
     (let ((visible-p (%route-search-token-visible-p
                       (getf subcase :input) (getf side :grid))))
       (values visible-p
               (format nil "search query for ~s was ~a" (getf subcase :input)
                       (if visible-p "accepted without syntax error"
                           "not visible or rejected")))))
    (:additional
     (if (string= (route-id route) "basic-mode")
         (values (%route-basic-mode-visible-p (getf side :grid))
                 "basic mode shows RAM, RX/TX totals, Disks, Temperature")
         (values t "")))
    (otherwise (values t ""))))
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
         (headers (%route-grid-headers grid))
         (text (and grid (%route-grid-text grid)))
         (expected-p
           (notany (lambda (term) (search term condition :test #'char-equal))
                   '("optional" "gpu feature" "may hide after grouping"
                     "when not grouped"))))
    (values (and text headers (search "Processes" text :test #'char-equal)
                 (if expected-p
                     (some (lambda (row) (search header row)) headers)
                     (not (some (lambda (row) (search header row)) headers))))
            expected-p)))
(defun %route-search-token-visible-p (input grid)
  (let* ((query (%route-search-query input))
         (text (and grid (%route-grid-text grid)))
         (prompt (format nil "> ~a" query))
         (errors '("Invalid numeric value" "Missing numeric value"
                   "Invalid process search field"
                   "Expected a process search expression"
                   "Missing value for process search field"
                   "Invalid duration value" "Unknown duration unit"
                   "Invalid process search query" "Missing closing quotation"
                   "Missing closing parentheses" "Missing expression")))
    (and text (search prompt text)
         (not (some (lambda (message)
                      (search message text :test #'char-equal))
                    errors)))))
(defun %route-marker-input (marker grid &optional route subcase rows columns)
  (if (eq marker :route-mouse-action)
      (%route-mouse-action-inputs route subcase grid rows columns)
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
                                     :test (function char-equal))
                          return (list (floor (+ left right) 2)
                                       (floor (+ top bottom) 2))))))))
      (unless point (error "Could not locate safe route target ~s in the initial frame." marker))
      (format nil "~c[<0;~d;~dM" #\Escape (1+ (first point))
              (1+ (second point))))))
(defun %route-manual-result (route subcase directory
                               &optional reason prefix)
  (let* ((id (route-id route)) (kind (route-kind route))
         (input (%route-input-description route subcase))
         (context (or (getf subcase :context) "default"))
         (reason (or reason (route-manual-reason route) input))
         (path (if prefix (pathname (format nil "~a-manual.md" prefix))
                   (merge-pathnames (format nil "~a-manual.md" id) directory)))
         (procedure
 (cond
   ((string= id "mouse-71")
    "Show at least two widgets; click an unselected widget and verify its selected border changes.")
   ((string= id "mouse-72")
    "Start basic mode, click the next-widget arrow, and verify the selected-widget marker moves.")
   ((and (eq kind :mouse) (string-equal context "CPU legend"))
    "Use a BTM screen with named CPU-series labels; click one visible series and verify selection.")
   ((and (eq kind :mouse) (string-equal context "temperature table"))
    (concatenate (quote string)
                 "Use a host with temperature sensor rows; click the Sensor "
                 "header or row and verify the result."))
   ((and (eq kind :mouse) (string-equal context "disk table"))
    "Use a host with mounted disks; click a Disk header or row and verify sorting or selection.")
   ((and (eq kind :mouse) (string-equal context "battery widget"))
    (concatenate (quote string)
                 "Configure a Battery widget on a host with a detected "
                 "battery; click each available tab and verify selection."))
   ((and (eq kind :mouse) (member context (quote ("tables" "process table"))
                                  :test (function string-equal)))
    (concatenate (quote string)
                 "Use a host where BTM shows live process rows; wheel over "
                 "the table and verify its scroll position."))
   ((and (eq kind :mouse) (string= id "mouse-73"))
    (concatenate (quote string)
                 "Use a host where BTM shows live process rows; select the "
                 "harness-owned PID and verify its highlight changes."))
   ((and (eq kind :flag)
         (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                  "disable_advanced_kill"))
    (concatenate (quote string)
                 "Use visible process rows and a disposable child; verify the child "
                 "stays alive and the dialog offers only Yes/No."))
   ((and (eq kind :flag)
         (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                  "read_only"))
    (concatenate (quote string)
                 "Use visible process rows and a disposable child; verify the child "
                 "stays alive and no kill dialog opens."))
   ((string= id "cpu-frequency")
    (concatenate (quote string) "Run sudo powermetrics --samplers cpu_power "
                 "-i 1000 -n 5 beside btm; compare each cluster average within 10%."))
   ((string= id "basic-mode")
    "Run btm --basic; verify RAM, RX/TX totals, Disks and Temperature; no processes.")
   ((string= id "sleep-child")
    (concatenate (quote string) "Run btm --default-widget-type proc; start sleep 120, "
                 "select its PID, send SIGTERM, and verify only it exits."))
   ((eq kind :key)
    (format nil "~a: ~a; send input and inspect the result." input
            (cond
              ((search "search" input :test (function char-equal))
               "open search, enter a nonempty query, set its cursor")
              ((search "disk" input :test (function char-equal))
               "show disk rows and record sort")
              ((search "temperature" input :test (function char-equal))
               "show readings and record sort")
              ((search "process" input :test (function char-equal))
               "show two live rows and choose a valid selection")
              (t "record a visible, non-boundary start state"))))
   ((eq kind :column)
    (format nil (concatenate (quote string)
                 "~a: run where BTM displays live process rows, then inspect "
                 "header ~s and its condition.")
            input (ntop.app.domain:parity-column-header (route-entry route))))
   ((eq kind :search-field)
    (format nil "~a: enter ~s in field ~a; check syntax errors."
            input (getf subcase :input)
            (ntop.app.domain:parity-search-field-field (route-entry route))))
   ((eq kind :mouse)
    (format nil "~a: record selection, perform the gesture once, inspect result."
            input))
   (t (format nil "Run ~a and inspect its documented state." input))))
         (expected
           (cond
             ((string= id "basic-mode")
              "RAM, RX/TX totals, Disks and Temperature are visible; no process table.")
             ((string= id "sleep-child")
              "Only the selected disposable child exits after SIGTERM.")
             (t (format nil "~a; expected: ~a" input (route-expected route))))))
    (%write-text path
                 (format nil "# ~a~%~%Procedure: ~a~%~%Expected: ~a~%~%Reason: ~a~%~%"
                         id procedure expected reason))
    (list :status :manual :expected expected :cause reason
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
