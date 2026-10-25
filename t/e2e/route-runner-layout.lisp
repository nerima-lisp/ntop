(in-package #:ntop.e2e)

(defun %route-mouse-action-inputs (route subcase grid rows columns)
  (let* ((context (or (getf subcase :context) "")) (action (route-action route))
         (gesture (getf subcase :gesture)))
    (labels ((point (label)
                    (loop for y below (grid-height grid)
                          for span =
                          (first (criterion-label-spans (criterion-row-text grid y) label))
                          when span return (cons (car span) y))))
      (let* ((target
              (cond ((string= action "Select process row")
                     (let ((h (point "PID"))) (and h (cons (car h) (1+ (cdr h))))))
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
        (unless target (unless
                           (%route-mouse-target-unavailable-p subcase grid)
                         (error "No visible target for route ~a in ~s."
                                (route-id route) context))
          (return-from %route-mouse-action-inputs nil))
        (let ((x (1+ (min (1- columns) (car target))))
              (y (1+ (min (1- rows) (cdr target)))))
          (flet ((sgr (code suffix) (format nil "~c[<~d;~d;~d~c" #\Escape code x y suffix)))
            (cond ((string= gesture "wheel-up") (list (sgr 64 #\M)))
                  ((string= gesture "wheel-down") (list (sgr 65 #\M)))
                  ((string= gesture "left-button-down") (list (sgr 0 #\M)))
                  ((string= gesture "right/middle/other button") (list (sgr 2 #\M) (sgr 1 #\M)))
                  ((string= gesture "move/drag/release")
                   (list (sgr 35 #\M) (sgr 2 #\M) (sgr 34 #\M) (sgr 2 #\m)
                         (sgr 1 #\M) (sgr 33 #\M) (sgr 1 #\m)))
                  (t (error "Unknown route mouse gesture: ~s" gesture)))))))))

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
            (member (route-id route) '("key-9" "key-11") :test #'string=))
       (quote (((cpu 2 :default) (mem nil))
               ((net 2) (disk 2) (temp nil))
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
