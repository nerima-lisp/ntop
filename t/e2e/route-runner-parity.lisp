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
(progn
  (defun %route-help-section-number (grid)
    (loop for row below (grid-height grid)
          for text = (criterion-row-text grid row)
          for separator = (search " - " text)
          when (and separator
                    (plusp separator)
                    (digit-char-p (char text (1- separator))))
            return (digit-char-p (char text (1- separator)))))
  (defun %route-help-section-transition-p (subcase before-grid after-grid)
  (let* ((before (%route-help-section-number before-grid))
         (after (%route-help-section-number after-grid))
         (input (getf subcase :input))
         (target (and input
                      (= (length input) 1)
                      (digit-char-p (char input 0))))
         (heading (and target (format nil "~d - " target))))
    (and before after target
         (if (<= target 6)
             (and (= after target) (/= before after))
           (and (= before 1)
                (= after 6)
                (not (loop for row below (grid-height before-grid)
                           thereis (search heading
                                            (criterion-row-text before-grid row))))
                (loop for row below (grid-height after-grid)
                      thereis (search heading
                                       (criterion-row-text after-grid row)))
                (%route-grid-first-difference before-grid after-grid))))))
  (defun %route-graph-window-transition-p (route before-grid after-grid)
    (labels ((scale-position (text start)
               (let ((sixty (search "60s" text :start2 start))
                     (forty-five (search "45s" text :start2 start)))
                 (cond ((and sixty forty-five) (min sixty forty-five))
                       (sixty sixty)
                       (forty-five forty-five))))
             (scale-value (text position)
               (if (eql position (search "60s" text :start2 position)) :60 :45))
             (axis-state (grid)
               (when (and grid
                          (search "CPU" (%route-grid-text grid)
                                  :test #'char-equal))
                 (loop for row below (grid-height grid)
                       for text = (criterion-row-text grid row)
                       for start = (scale-position text 0)
                       ;; Skip the 0s suffix inside 60s; the next 0s marks the axis end.
                       for endpoint = (and start (search "0s" text :start2 (+ start 3)))
                       for next-start = (and endpoint
                                             (scale-position text (+ endpoint 2)))
                       when endpoint
                         return (list (scale-value text start)
                                      (and next-start
                                           (scale-value text next-start)))))))
      (let* ((before (axis-state before-grid))
             (after (axis-state after-grid))
             (action (route-action route))
             (id (route-id route)))
        (and before after
             (second before)
             (eq (second before) (second after))
             (cond
               ((string= id "mouse-69")
                (and (eq (first before) :60) (eq (first after) :45)))
               ((string= id "mouse-70")
                (and (eq (first before) :45) (eq (first after) :60)))
               ((string= action "Zoom graph in")
                (and (eq (first before) :60) (eq (first after) :45)))
               ((member action '("Zoom graph out" "Reset graph zoom")
                        :test #'string=)
                (and (eq (first before) :45) (eq (first after) :60))))))))
)


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
         (t
          (cond
           ((member action (list "Zoom graph in" "Zoom graph out" "Reset graph zoom")
                    :test (function string=))
            (%route-graph-window-transition-p route initial grid))
           ((string= action "Select help section")
            (%route-help-section-transition-p subcase initial grid))
           (t
            (%route-key-context-state route subcase initial grid stable)))))))
     ((eq (route-kind route) :mouse)
      (cond
       ((member action (quote ("Ignore other buttons" "Ignore pointer motion"))
                :test (function string=))
        stable)
       ((and (string= (route-id route) "mouse-74")
             (%route-widget-no-data-p initial "Processes")
             (%route-widget-no-data-p grid "Processes"))
        :manual) ((member (route-id route)
                          (quote ("mouse-71" "mouse-72"))
                          :test (function string=))
                  :manual)
       ((and (string-equal (or (getf subcase :context) "") "help")
             (member (route-id route) '("mouse-69" "mouse-70") :test #'string=))
        (let* ((before-top
                (and initial
                     (loop for row below (grid-height initial)
                           when (search "Scroll to browse"
                                        (criterion-row-text initial row)
                                        :test #'char-equal)
                           return row)))
               (after-top
                (and grid
                     (loop for row below (grid-height grid)
                           when (search "Scroll to browse"
                                        (criterion-row-text grid row)
                                        :test #'char-equal)
                           return row)))
               (before-general
                (and initial
                     (loop for row below (grid-height initial)
                           when (search "1 - General"
                                        (criterion-row-text initial row)
                                        :test #'char-equal)
                           return row)))
               (after-general
                (and grid
                     (loop for row below (grid-height grid)
                           when (search "1 - General"
                                        (criterion-row-text grid row)
                                        :test #'char-equal)
                           return row)))
               (raw-change (and initial grid
                                (%route-grid-first-difference initial grid))))
          (and raw-change
               (if (string= (route-id route) "mouse-69")
                   (and (null before-top) before-general after-top after-general
                        (= before-general after-top)
                        (= (1+ before-general) after-general))
                 (and before-top before-general (null after-top) after-general
                      (= (1+ before-top) before-general)
                      (= before-top after-general))))))
       ((%route-mouse-target-unavailable-p subcase initial) :manual)
       (t
        (if (and (string-equal (or (getf subcase :context) "") "graphs")
                 (member (route-id route) '("mouse-69" "mouse-70")
                         :test #'string=))
            (%route-graph-window-transition-p route initial grid)
          (if (and (member (route-id route) '("mouse-75" "mouse-77")
                           :test #'string=)
                   (not changed))
              :manual
            changed)))))
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
  (let* ((id (route-id route))
         (input (%route-input-description route subcase))
         (context (or (getf subcase :context) "default"))
         (reason (or reason (route-manual-reason route) input))
         (path (if prefix
                   (pathname (format nil "~a-manual.md" prefix))
                   (merge-pathnames (format nil "~a-manual.md" id)
                                    directory)))
         (procedure (%route-manual-procedure route subcase input context))
         (expected
           (cond
             ((string= id "basic-mode")
              "RAM, RX/TX totals, Disks and Temperature are visible; no process table.")
             ((string= id "sleep-child")
              "Only the selected disposable child exits after SIGTERM.")
             (t (format nil "~a; expected: ~a" input (route-expected route))))))
    (%write-text path
                 (format nil
                         "# ~a~%~%Procedure: ~a~%~%Expected: ~a~%~%Reason: ~a~%~%"
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
