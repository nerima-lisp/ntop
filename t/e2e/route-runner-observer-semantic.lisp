(in-package #:ntop.e2e)

(progn
  (defun %route-observer-grid-changed-p (before after)
    (not (null (%route-grid-first-difference before after))))
  (defun %route-observer-tree-shape (grid)
    (let ((rows (%route-observer-process-rows grid)))
      (when rows
        (list (length rows)
              (loop for row in rows
                    sum (if (or (search "├─" row) (search "└─" row)) 1 0))
              (loop for row in rows sum (if (search "+ " row) 1 0))))))
  (defun %route-observer-tree-transition-p (action before after)
    (and before after
         (cond
           ((string= action "Toggle process tree")
            (/= (second before) (second after)))
           ((string= action "Toggle process tree branch")
            (or (/= (first before) (first after))
                (/= (third before) (third after))))
           (t nil))))
  (defun %route-observer-tree-horizontal-selection-p
      (route subcase initial grid)
    (let* ((action (route-action route))
           (id (if (string= action "Move left within widget") "key-8" "key-9"))
           (probe-route (make-route-case :id id :action action)))
      (eq t (%route-key-context-state probe-route subcase initial grid nil))))
  (defun %route-observer-tree-p (route subcase initial grid)
    (let* ((action (route-action route))
           (before (%route-observer-tree-shape initial))
           (after (%route-observer-tree-shape grid)))
      (cond
       ((member action
                (quote ("Go to first row" "Go to last row"
                                          "Move to previous row" "Move to next row"
                                          "Page up" "Page down" "Half-page up" "Half-page down"))
                :test (function string=))
        (and before after
             (plusp (second before))
             (plusp (second after))
             (multiple-value-bind (old old-last)
                 (%route-observer-process-selection-index initial)
               (multiple-value-bind (new new-last)
                   (%route-observer-process-selection-index grid)
                 (%route-observer-navigation-index-p
                  action old new old-last new-last)))))
       ((member action (quote ("Move left within widget"
                               "Move right within widget"))
                :test (function string=))
        (and before after
             (plusp (second before))
             (plusp (second after))
             (%route-observer-tree-horizontal-selection-p
              route subcase initial grid)))
       (t (%route-observer-tree-transition-p action before after)))))
)

(progn
  (defun %route-observer-sort-state (grid)
    (let* ((header (first (%route-grid-headers grid)))
           (labels (quote ("CPU%(c)" "Mem%(m)" "Mem(m)" "PID(p)" "Count"
                           "Name(n)" "Command(n)" "GPU%" "GMem%" "GMem"))))
      (when header
        (loop for label in labels
              for start = (search label header)
              for end = (and start (+ start (length label)))
              for marker = (and end (< end (length header)) (char header end))
              when (and marker (find marker "▲▼"))
                return (list label marker)))))
  (defun %route-observer-sort-transition-p (action before after)
    (let* ((old-label (first before))
           (new-label (first after))
           (old-marker (second before))
           (new-marker (second after))
           (targets
            (cond ((search "CPU" action :test (function char-equal))
                   (quote ("CPU%(c)")))
                  ((search "memory" action :test (function char-equal))
                   (quote ("Mem%(m)" "Mem(m)")))
                  ((search "PID" action :test (function char-equal))
                   (quote ("PID(p)" "Count")))
                  ((search "name" action :test (function char-equal))
                   (quote ("Name(n)" "Command(n)")))
                  ((search "GPU utilization" action :test (function char-equal))
                   (quote ("GPU%")))
                  ((search "GPU memory" action :test (function char-equal))
                   (quote ("GMem%" "GMem"))))))
      (cond ((string= action "Reverse sort order")
             (and old-label new-label (equal old-label new-label)
                  (find old-marker "▲▼") (find new-marker "▲▼")
                  (not (char= old-marker new-marker))))
            (targets
             (and (member new-label targets :test (function string=))
                  (find new-marker "▲▼")
                  (or (not (member old-label targets :test (function string=)))
                      (not (char= old-marker new-marker))))))))
  (defun %route-observer-sort-p (route initial grid)
    (let* ((action (route-action route))
           (text (and grid (%route-grid-text grid))))
      (if (string= action "Open process sort list")
          (and text (search "Sort By" text :test (function char-equal))
               (search "CPU" text :test (function char-equal)))
          (%route-observer-sort-transition-p
           action
           (%route-observer-sort-state initial)
           (%route-observer-sort-state grid)))))
)

(progn
  (defun %route-observer-process-selection-index (grid)
    (let* ((frame (%route-observer-frame-with-title grid "Processes"))
           (header
            (and frame
                 (loop for row from (1+ (first frame)) below (second frame)
                       when (criterion-process-header-row-p grid row (list frame))
                         return row))))
      (when header
        (let* ((styles
                (loop for row from (1+ header) below (second frame)
                      for text = (criterion-row-text grid row)
                      for column = (position-if-not
                                    (lambda (character) (char= character #\Space)) text)
                      unless (string= (string-trim '(#\Space) text) "")
                        collect (cell-background (grid-cell-at grid row column))))
               (normal
                (first
                 (sort (remove-duplicates (copy-list styles) :test (function equalp))
                       (function >)
                       :key (lambda (style)
                              (count style styles :test (function equalp))))))
               (outliers
                (loop for style in styles
                      for index from 0
                      unless (equalp style normal)
                        collect index)))
          (when (= (length outliers) 1)
            (values (first outliers) (1- (length styles))))))))
  (defun %route-observer-navigation-index-p
      (action before after before-last after-last)
    (and before after before-last after-last
         (cond
           ((string= action "Go to first row") (zerop after))
           ((string= action "Go to last row") (= after after-last))
           ((string= action "Move to previous row")
            (= after (max 0 (1- before))))
           ((string= action "Move to next row")
            (= after (min after-last (1+ before))))
           ((member action (quote ("Page up" "Half-page up")) :test (function string=))
            (or (< after before) (and (zerop before) (zerop after))))
           ((member action (quote ("Page down" "Half-page down"))
                    :test (function string=))
            (or (> after before) (and (= before before-last) (= after after-last)))))))
  (defun %route-observer-table-header (grid title)
    (let* ((frame (%route-observer-frame-with-title grid title))
           (row (and frame (1+ (first frame))))
           (text (and row (criterion-row-text grid row))))
      (when text
        (remove-if (lambda (character) (find character "▲▼")) text))))
  (defun %route-observer-table-p (route subcase initial grid)
    (let* ((title (%route-observer-table-title route subcase))
           (rows (and title (%route-observer-table-rows grid title)))
           (action (route-action route))
           (target (%route-observer-table-sort-target action))
           (navigation-p
            (member action
                    (quote
                    ("Go to first row"
                     "Go to last row"
                     "Move to previous row"
                     "Move to next row"
                     "Page up"
                     "Page down"
                     "Half-page up"
                     "Half-page down"))
                    :test (function string=)))
           (passed
            (and rows
                 (not (%route-widget-no-data-p grid title))
                 (cond
                  (navigation-p
                   (multiple-value-bind (before before-last)
                       (%route-observer-process-selection-index initial)
                     (multiple-value-bind (after after-last)
                         (%route-observer-process-selection-index grid)
                       (%route-observer-navigation-index-p
                        action before after before-last after-last))))
                  ((string= action "Toggle process table tab")
                   (%route-observer-table-header-transition-p
                    (%route-observer-table-header initial title)
                    (%route-observer-table-header grid title)))
                  (target
                   (%route-observer-table-sort-transition-p
                    target
                    (%route-observer-table-sort-state initial title target)
                    (%route-observer-table-sort-state grid title target)))
                  (t nil)))))
      (values passed (list :table title :rows (length rows) :action action)
              (unless passed "table-specific state transition was not observed"))))
)
(progn
  (defun %route-observer-table-header-transition-p (before after)
    (and before after (not (equal before after))))
  (defun %route-observer-label-style (grid label)
    (when grid
      (loop for row below (grid-height grid)
            for text = (criterion-row-text grid row)
            for column = (search label text :test (function char-equal))
            when column
              return
                (let ((cell (grid-cell-at grid row column)))
                  (list (copy-tree (cell-foreground cell))
                        (copy-tree (cell-background cell))
                        (cell-bold-p cell))))))
  (defun %route-observer-search-option-transition-p (route before after)
    (let* ((label
            (cdr (assoc (route-id route)
                        (quote (("key-25" . "Case")
                                ("key-26" . "Whole")
                                ("key-27" . "Regex")))
                        :test (function string=))))
           (old (and label (%route-observer-label-style before label)))
           (new (and label (%route-observer-label-style after label))))
      (and old new (not (equalp old new)))))
  (defun %route-observer-help-search-rows (grid)
    (when grid
      (loop for row below (grid-height grid)
            for text = (criterion-row-text grid row)
            for separator = (search " - " text)
            unless (or (null separator)
                       (search "Search:" text :test (function char-equal))
                       (%route-help-section-heading-line-p text separator))
              collect (string-trim " " text))))
  (defun %route-observer-search-result-rows-match-p (query rows)
    (and (stringp query)
         rows
         (every (lambda (row)
                  (search query row :test (function char-equal)))
                rows)))
  (defun %route-observer-search-results-p (route subcase initial grid)
    (let* ((help-p (eq (getf subcase :fixture) :help))
           (query (if help-p
                      "process"
                      (nth-value 0 (%route-observer-prompt-state grid))))
           (before-query
            (if help-p
                "process"
                (nth-value 0 (%route-observer-prompt-state initial))))
           (rows (if help-p
                     (%route-observer-help-search-rows grid)
                     (%route-observer-process-rows grid)))
           (before-rows
            (if help-p
                (%route-observer-help-search-rows initial)
                (%route-observer-process-rows initial)))
           (visible-p
            (if help-p
                (%route-help-search-query-visible-p grid query)
                (%route-search-token-visible-p query grid)))
           (before-visible-p
            (if help-p
                (%route-help-search-query-visible-p initial before-query)
                (%route-search-token-visible-p before-query initial)))
           (transition-p
            (or (%route-observer-search-option-transition-p route initial grid)
                (and before-rows rows (not (equal before-rows rows)))))
           (matches-p (%route-observer-search-result-rows-match-p query rows)))
      (and visible-p before-visible-p matches-p transition-p)))
  (defun %route-observer-help-section-range (grid)
    (when grid
      (let ((sections
              (loop for row below (grid-height grid)
                    for text = (criterion-row-text grid row)
                    for separator = (search " - " text)
                    for number =
                      (and separator
                           (%route-help-section-heading-line-p text separator)
                           (digit-char-p (char text (1- separator))))
                    when number collect number)))
        (when sections
          (cons (apply (function min) sections)
                (apply (function max) sections))))))
  (defun %route-observer-help-navigation-p (route subcase before after)
    (let* ((action (route-action route))
           (old (%route-observer-help-section-range before))
           (new (%route-observer-help-section-range after)))
      (and old new
           (cond
             ((string= action "Select help section")
              (%route-help-section-transition-p subcase before after))
             ((string= action "Go to first row") (= (car new) 1))
             ((string= action "Go to last row") (= (cdr new) 9))
             ((member action (quote ("Move to previous row" "Page up"
                                     "Half-page up"))
                      :test (function string=))
              (< (car new) (car old)))
             ((member action (quote ("Move to next row" "Page down"
                                     "Half-page down"))
                      :test (function string=))
              (> (cdr new) (cdr old)))
             (t nil)))))
)
(progn
  (defun %route-observer-table-sort-target (action)
    (cond
     ((search "Select disk column" action :test (function char-equal))
      (let* ((index (digit-char-p (char action (1- (length action)))))
             (labels (quote ("Disk(d)" "Mount(m)" "Used(u)" "Free(n)"
                             "Total(t)" "Used%(p)" "R/s(r)" "W/s(w)"))))
        (and index (nth index labels))))
     ((string= action "Sort temperatures by name") "Sensor(s)")
     ((string= action "Sort temperatures by value") "Temp(t)")))
  (defun %route-observer-table-sort-state (grid title target)
    (let* ((frame (%route-observer-frame-with-title grid title))
           (row (and frame (1+ (first frame))))
           (header (and row (criterion-row-text grid row)))
           (start (and header (search target header)))
           (end (and start (+ start (length target))))
           (marker (and end (< end (length header)) (char header end))))
      (when (and marker (find marker "▲▼"))
        (list target marker))))
  (defun %route-observer-table-sort-transition-p (target before after)
    (and target after
         (equal (first after) target)
         (find (second after) "▲▼")
         (or (not (equal (first before) target))
             (not (char= (second before) (second after)))))))

(defun %route-observer-frame-with-title (grid title)
  (when grid
    (find-if
     (lambda (frame)
       (search title (criterion-row-text grid (first frame)) :test #'char-equal))
     (criterion-projection-frames grid))))

(defun %route-observer-table-rows (grid title)
  (let ((frame (%route-observer-frame-with-title grid title)))
    (when frame
      (loop for row from (1+ (first frame)) below (second frame)
            for text = (criterion-row-text grid row)
            unless (string= (string-trim '(#\Space) text) "")
              collect text))))

(defun %route-observer-process-rows (grid)
  (when grid
    (let* ((frames (criterion-projection-frames grid))
           (frame
            (find-if
             (lambda (candidate)
               (search "Processes" (criterion-row-text grid (first candidate)) :test #'char-equal))
             frames))
           (header
            (and frame
                 (loop for row from (1+ (first frame)) below (second frame)
                       when (criterion-process-header-row-p grid row (list frame))
                         return row))))
      (when (and frame header)
        (loop for row from (1+ header) below (second frame)
              for text = (criterion-row-text grid row)
              unless (string= (string-trim '(#\Space) text) "")
                collect text)))))

(defun %route-observer-prompt-state (grid)
  (when grid
    (loop for row below (grid-height grid)
          for line = (criterion-row-text grid row)
          for process-start = (search "> " line)
          for help-start = (search "Search:" line :test #'char-equal)
          for start = (or process-start help-start)
          when start
            do (let* ((prefix-length
                       (if process-start
                           2
                           (length "Search:")))
                      (tail
                       (string-right-trim '(#\Space)
                                          (string-left-trim '(#\Space)
                                                            (subseq line (+ start prefix-length)))))
                      (cursor (position #\| tail)))
                 (if cursor
                     (return
                      (values (concatenate 'string (subseq tail 0 cursor) (subseq tail (1+ cursor)))
                              cursor
                              line))
                     (return (values tail nil line)))))))

(defun %route-observer-table-title (route subcase)
  (let* ((context (or (getf subcase :context) ""))
         (action (route-action route))
         (type (%route-widget-type context)))
    (cond
      ((or (equal type "proc") (search "process" action :test #'char-equal)) "Processes")
      ((or (equal type "disk") (search "disk" action :test #'char-equal)) "Disks")
      ((or (equal type "temp") (search "temperature" action :test #'char-equal)) "Temperatures")
      ((or (equal type "cpu") (search "CPU series" action :test #'char-equal)) "CPU")
      ((equal type "battery") "Battery"))))

(defun %route-help-navigation-state-p (route subcase before-grid after-grid)
  (when (and (string-equal (or (getf subcase :context) "") "help")
             before-grid after-grid
             (member (route-action route)
                     (list "Go to first row" "Go to last row"
                           "Move to previous row" "Move to next row"
                           "Page up" "Page down" "Half-page up" "Half-page down")
                     :test (function string=)))
    (%route-grid-first-difference before-grid after-grid)))

  (defun %route-modal-action-state-p (route subcase before-grid after-grid)
  (let* ((id (route-id route))
         (action (route-action route))
         (context (or (getf subcase :context) ""))
         (before (%route-grid-text before-grid))
         (after (%route-grid-text after-grid)))
    (cond
      ((and (string= id "key-35")
            (string= action "Accept current choice"))
       (cond
         ((string-equal context "help search")
          (and (%route-help-search-query-visible-p before-grid "process")
               (search "Help" after :test #'char-equal)
               (not (search "Search: process" after :test #'char-equal))))
         ((member context '("process search" "process sort" "kill dialog")
                  :test #'string-equal)
          nil)
         (t nil)))
      ((and (string= id "key-36")
            (string= action "Close current modal"))
       (cond
         ((string-equal context "process search")
          (and (search "Case(F1)" before :test #'char-equal)
               (search "> " before)
               (not (search "Case(F1)" after :test #'char-equal))))
         ((string-equal context "help search")
          (and (%route-help-search-query-visible-p before-grid "process")
               (search "Help" after :test #'char-equal)
               (not (search "Search: process" after :test #'char-equal))))
         ((string-equal context "process sort")
          (and (search "Sort By" before :test #'char-equal)
               (not (search "Sort By" after :test #'char-equal))))
         ((string-equal context "expanded widget")
          (and (search "Esc to go back" before :test #'char-equal)
               (not (search "Esc to go back" after :test #'char-equal))
               (every (lambda (label) (search label after :test #'char-equal))
                      '("CPU" "Memory" "Network"))))
         ((member context '("kill dialog" "help") :test #'string-equal)
          nil)
         (t nil)))
      (t nil))))
