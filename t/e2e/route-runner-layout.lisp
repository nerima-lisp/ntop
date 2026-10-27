(in-package #:ntop.e2e)

(defun %route-mouse-action-inputs (route subcase grid rows columns)
  (let* ((context (or (getf subcase :context) ""))
         (action (route-action route))
         (gesture (getf subcase :gesture))
         (wheel-p (member gesture '("wheel-up" "wheel-down") :test #'string=)))
    (labels ((point (label)
               (loop for y below (grid-height grid)
                     for span = (first (criterion-label-spans
                                        (criterion-row-text grid y) label))
                     when span return (cons (car span) y)))
             (frame-for-label (label)
               (loop for y below (grid-height grid)
                     for span = (first (criterion-label-spans
                                        (criterion-row-text grid y) label))
                     when span
                       do (let ((frame
                                  (find-if
                                   (lambda (bounds) (= y (first bounds)))
                                   (criterion-projection-frames grid))))
                            (when frame (return frame)))))
             (frame-center (frame)
               (when frame
                 (cons (floor (+ (1+ (third frame)) (1- (fourth frame))) 2)
                       (floor (+ (1+ (first frame)) (1- (second frame))) 2))))
             (process-table-point ()
               (let* ((top (loop for row below (grid-height grid)
                                 for text = (criterion-row-text grid row)
                                 when (search "Processes" text :test #'char-equal)
                                   return row))
                      (frame (and top
                                  (find-if
                                   (lambda (bounds) (= top (first bounds)))
                                   (criterion-projection-frames grid)))))
                 (frame-center frame)))) (let* ((target
               (cond
                 ((and wheel-p
                       (or (string-equal context "tables")
                           (string-equal context "process table")))
                  (process-table-point))
                 ((and wheel-p (string-equal context "graphs"))
                  (frame-center (frame-for-label "CPU")))
                 ((and wheel-p (string-equal context "help"))
                  (frame-center (frame-for-label "Help")))
                 ((string= action "Select process row")
                  (let ((header (point "PID")))
                    (and header (cons (car header) (1+ (cdr header))))))
                 ((string= action "Select process sort") (point "Name"))
                 ((string= action "Select CPU series") (point "AVG"))
                 ((string= action "Select table row or column")
                  (point (if (string-equal context "temperature table") "Sensor" "Disk")))
                 ((string= action "Select battery") (point "Battery"))
                 ((string-equal context "help") (point "Help"))
                 ((string-equal context "graphs") (point "CPU"))
                 ((string-equal context "tables") (point "PID"))
                 ((string-equal context "kill dialog") (or (point "Cancel") (point "No")))
                 (t (point "Memory")))))
        (unless target
          (unless (%route-mouse-target-unavailable-p subcase grid)
            (error "No visible target for route ~a in ~s." (route-id route) context))
          (return-from %route-mouse-action-inputs nil))
        (let ((x (1+ (min (1- columns) (car target))))
              (y (1+ (min (1- rows) (cdr target)))))
          (flet ((sgr (code suffix)
                   (format nil "~c[<~d;~d;~d~c" #\Escape code x y suffix)))
            (cond
              ((string= gesture "wheel-up") (list (sgr 64 #\M)))
              ((string= gesture "wheel-down") (list (sgr 65 #\M)))
              ((string= gesture "left-button-down") (list (sgr 0 #\M)))
              ((string= gesture "right/middle/other button")
               (list (sgr 2 #\M) (sgr 1 #\M)))
              ((string= gesture "move/drag/release")
               (list (sgr 35 #\M) (sgr 2 #\M) (sgr 34 #\M) (sgr 2 #\m)
                     (sgr 1 #\M) (sgr 33 #\M) (sgr 1 #\m)))
              (t (error "Unknown route mouse gesture: ~s" gesture))))))))
)

(defun %route-widget-type (context)
  (cond
    ((member context '("Memory" "memory widget") :test #'string-equal) "mem")
    ((member context '("Temperatures" "temperature table")
             :test #'string-equal) "temp")
    ((member context '("Disks" "disk table") :test #'string-equal) "disk")
    ((member context '("Network" "network table") :test #'string-equal) "net")
    ((member context '("Processes" "process table" "tables" "process tree"
                       "process sort" "process sort list" "process search"
                       "search cursor" "kill dialog")
             :test #'string-equal) "proc")
    ((member context '("battery widget" "battery tabs") :test #'string-equal)
     "battery")
    ((string-equal context "CPU legend") "cpu")
    ((string-equal context "graphs") "cpu")))

(defun %route-config-layout (route subcase)
  (let ((context (or (getf subcase :context) "")))
    (cond
      ((member context (quote ("battery widget" "battery tabs"))
               :test (function string-equal))
       (quote (((battery nil :default)))))
      ((string-equal context "CPU legend")
       (quote (((cpu nil :default)))))
      ((and (eq (route-kind route) :key)
            (member (route-id route) (quote ("key-9" "key-11"))
                    :test (function string=)))
       (quote (((cpu 2 :default) (mem 1))
               ((net 2) (disk 2) (temp 1))
               ((proc 2)))))
      (t *btm-layout*))))

(defun %route-widget-no-data-p (grid title)
  (let* ((top (and grid
                   (loop for row below (grid-height grid)
                         for text = (criterion-row-text grid row)
                         when (search title text :test (function char-equal))
                           return row)))
         (frame (and top
                     (find-if (lambda (bounds) (= top (first bounds)))
                              (criterion-projection-frames grid)))))
    (and frame
         (loop for row from (first frame) to (second frame)
               thereis (search "No data" (criterion-row-text grid row)
                               :test (function char-equal))))))

(defun %route-mouse-target-unavailable-p (subcase grid)
  (let* ((context (or (getf subcase :context) ""))
         (text (and grid (%route-grid-text grid))))
    (cond
      ((member context (quote ("tables" "process table"))
               :test (function string-equal))
       (or (%route-widget-no-data-p grid "Processes")
           (let* ((frames (and grid (criterion-projection-frames grid)))
                  (frame (find-if
                          (lambda (frame)
                            (search "Processes"
                                    (criterion-row-text grid (first frame))
                                    :test (function char-equal)))
                          frames))
                  (header (and frame
                               (loop for row from (1+ (first frame))
                                     below (second frame)
                                     when (criterion-process-header-row-p
                                           grid row (list frame))
                                       return row))))
             (and frame
                  header
                  (not (loop for row from (1+ header) below (second frame)
                             thereis
                             (loop for column from (1+ (third frame))
                                   below (fourth frame)
                                   thereis
                                   (not (char= (cell-character
                                                (grid-cell-at grid row column))
                                               #\Space)))))))))
      ((string-equal context "temperature table")
       (%route-widget-no-data-p grid "Temperatures"))
      ((string-equal context "disk table")
       (%route-widget-no-data-p grid "Disks"))
      ((string-equal context "CPU legend")
       (and text
            (search "CPU" text :test (function char-equal))
            (not (search "AVG" text :test (function char-equal)))))
      ((string-equal context "battery widget")
       (or (not (and text (search "Battery" text :test (function char-equal))))
           (%route-widget-no-data-p grid "Battery")))
      (t nil))))

(defun %route-grid (octets rows columns)
  (let ((grid (make-terminal-grid :rows rows :cols columns)))
    (parse-ansi (%octets-to-text octets)
                grid
                (lambda (value) value)
                (lambda (condition) (error condition)))))

(defun %route-grid-after-output (raw output rows columns)
  (%route-grid (concatenate '(vector (unsigned-byte 8)) raw output)
               rows columns))

(defun %route-expanded-widget-p (grid)
  (and grid
       (member (list 0 (1- (grid-height grid)) 0 (1- (grid-width grid)))
               (criterion-frame-spans grid)
               :test #'equal)))

(defun %route-expanded-widget-transition-p (before-grid after-grid)
  (and before-grid
       (%route-expanded-widget-p after-grid)
       (not (equal (criterion-frame-spans before-grid)
                   (criterion-frame-spans after-grid)))))

(defun %route-expanded-widget-restored-p (grid initial-grid)
  (and grid initial-grid
       (not (%route-expanded-widget-p grid))
       (equal (criterion-frame-spans grid)
              (criterion-frame-spans initial-grid))))

(defun %route-grid-text (grid)
  (with-output-to-string (stream)
    (dotimes (row (grid-height grid))
      (dotimes (column (grid-width grid))
        (write-char (cell-character (grid-cell-at grid row column)) stream))
      (terpri stream))))

(defun %route-projection-first-difference (left right)
  (when (and left right (not (criterion-projections-equal-p left right)))
    (let ((left-cells (aref left 2))
          (right-cells (aref right 2)))
      (loop for row below (min (aref left 1) (aref right 1))
            do (loop for column below (min (aref left 0) (aref right 0))
                     for left-cell =
                       (find-if (lambda (cell)
                                  (and (= row (aref cell 1))
                                       (= column (aref cell 2))))
                                left-cells)
                     for right-cell =
                       (find-if (lambda (cell)
                                  (and (= row (aref cell 1))
                                       (= column (aref cell 2))))
                                right-cells)
                     unless (equalp left-cell right-cell)
                       do (return-from %route-projection-first-difference
                            (format nil "row ~d column ~d: reference ~s; candidate ~s"
                                    (1+ row) (1+ column)
                                    left-cell right-cell))))
      (format nil "projection dimensions or cell count differ: ~d x ~d vs ~d x ~d"
              (aref left 0) (aref left 1) (aref right 0) (aref right 1)))))
(defun %route-help-section-heading-line-p (text separator)
  (when (>= separator 2)
    (let ((border (char text (- separator 2))))
      (or (member border '(#\│ #\┃ #\║) :test (function char=))
          (char= border (code-char 124))))))
(defun %route-help-section-number (grid &optional target)
  (loop for row below (grid-height grid)
        for text = (criterion-row-text grid row)
        for separator = (search " - " text)
        for number = (and separator
                          (%route-help-section-heading-line-p text separator)
                          (digit-char-p (char text (1- separator))))
        when (and number (or (null target) (eql number target)))
          return (values number row)))
(defun %route-help-section-transition-p (subcase before-grid after-grid)
  (let* ((input (getf subcase :input))
         (gesture (getf subcase :gesture))
         (target (and input
                      (= (length input) 1)
                      (digit-char-p (char input 0))))
         (changed (and before-grid after-grid
                       (not (string= (%route-grid-text before-grid)
                                     (%route-grid-text after-grid))))))
    (multiple-value-bind (before before-row)
        (%route-help-section-number before-grid)
      (multiple-value-bind (after after-row)
          (%route-help-section-number after-grid target)
        (cond
          ((member gesture (list "wheel-up" "wheel-down")
                   :test (function string=))
           (and before after changed
                (if (string= gesture "wheel-down")
                    (or (> after before)
                        (and (= after before) (< after-row before-row)))
                    (or (< after before)
                        (and (= after before) (> after-row before-row))))))
          (target
           (and (not (eql before target))
                (eql after target)
                changed
                t))
          (t nil))))))
(defun %route-graph-window-transition-p
    (subcase before-grid after-grid &optional expected-before expected-after)
  (labels ((scale-position (text start)
             (loop with cursor = start
                   for suffix = (position (code-char 115) text :start cursor)
                   while suffix
                   do (let ((digit (1- suffix)))
                        (loop while (and (>= digit cursor)
                                         (digit-char-p (char text digit)))
                              do (decf digit))
                        (when (< digit (1- suffix))
                          (return (1+ digit)))
                        (setf cursor (1+ suffix)))))
           (scale-value (text position)
             (let ((suffix (and position
                                (position (code-char 115) text :start position))))
               (and suffix (parse-integer text :start position :end suffix))))
           (axis-state (grid)
             (when (and grid
                        (search "CPU" (%route-grid-text grid) :test (function char-equal)))
               (loop for row below (grid-height grid)
                     for text = (criterion-row-text grid row)
                     for start = (scale-position text 0)
                     for first-end = (and start
                                          (position (code-char 115) text :start start))
                     for endpoint = (and first-end
                                         (search "0s" text :start2 (1+ first-end)))
                     for next-start = (and endpoint
                                           (scale-position text (+ endpoint 2)))
                     when endpoint
                       return (list (scale-value text start)
                                    (and next-start
                                         (scale-value text next-start)))))))
    (let* ((before (axis-state before-grid))
           (after (axis-state after-grid))
           (gesture (getf subcase :gesture))
           (expected-before (or expected-before (first before))))
      (and (member gesture (list "wheel-up" "wheel-down") :test (function string=))
           before
           after
           (integerp (first before))
           (integerp (first after))
           (integerp (second before))
           (eql (first before) expected-before)
           (eql (second before) (second after))
           (if expected-after
               (eql (first after) expected-after)
               (if (string= gesture "wheel-up")
                   (< (first after) (first before))
                   (> (first after) (first before))))))))
