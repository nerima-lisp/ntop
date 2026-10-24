(in-package #:ntop.e2e)

(defmacro define-criterion-vocabulary (&body rules)
  `(defparameter *criterion-vocabulary* ',rules))

(define-criterion-vocabulary
  (:border-characters "─│┌┐└┘├┤┬┴┼")
  (:frame-corner-pairs (#\┌ #\┐) (#\└ #\┘))
  (:frame-divider-pairs (#\├ #\┤) (#\├ #\┼)
                        (#\┼ #\┤) (#\┼ #\┼))
  (:title-row-markers "─┌┐")
  (:fixed-title-labels "CPU" "Memory" "Network" "Disks"
                       "Temperatures" "Processes")
  (:dynamic-title-exclusions (:cpu-load-averages "CPU" 3))
  (:process-header-anchor-pairs ("Count" "Name(n)")
                                ("PID" "Name(n)"))
  (:process-header-labels "Count" "PID" "Name(n)" "CPU%(c)"
                          "Mem%(m)" "R/s" "W/s" "T.Read" "T.Write"
                          "User" "State" "Time" "Priority"))

(defun criterion-rule-values (key)
  (cdr (assoc key *criterion-vocabulary*)))

(defun criterion-character-in-p (character characters)
  (find character characters :test #'char=))

(defun criterion-word-boundary-p (text position)
  (or (zerop position)
      (>= position (length text))
      (let ((character (char text position))
            (previous-character (char text (1- position))))
        (or (char= character #\Space)
            (char= previous-character #\Space)
            (criterion-character-in-p
             character
             (first (criterion-rule-values :border-characters)))
            (criterion-character-in-p
             previous-character
             (first (criterion-rule-values :border-characters)))
            (find character "()")
            (find previous-character "()")))))

(defun criterion-label-spans (text label)
  (loop with position = 0
        for start = (search label text :start2 position)
        while start
        for end = (+ start (length label))
        when (and (criterion-word-boundary-p text start)
                  (criterion-word-boundary-p text end))
          collect (cons start end)
        do (setf position (1+ start))))

(defun criterion-title-row-p (text)
  (loop for character across text
        thereis (criterion-character-in-p
                 character
                 (first (criterion-rule-values :title-row-markers)))))

(defun criterion-next-word-span (text position)
  (let ((length (length text))
        (border-characters
          (first (criterion-rule-values :border-characters))))
    (loop while (and (< position length)
                     (or (char= (char text position) #\Space)
                         (criterion-character-in-p
                          (char text position) border-characters)))
          do (incf position))
    (let ((start position))
      (loop while (and (< position length)
                       (not (char= (char text position) #\Space))
                       (not (criterion-character-in-p
                             (char text position) border-characters)))
            do (incf position))
      (and (< start position) (cons start position)))))

(defun criterion-numeric-token-p (token)
  (and (plusp (length token))
       (some (lambda (character) (char<= #\0 character #\9)) token)
       (every (lambda (character)
                (or (char<= #\0 character #\9)
                    (find character ".+-")))
              token)))

(defun criterion-title-exclusion-spans (text)
  (loop for (rule-name label count)
          in (criterion-rule-values :dynamic-title-exclusions)
        when (eq rule-name :cpu-load-averages)
          append
          (loop for label-span in (criterion-label-spans text label)
                append
                (loop with position = (cdr label-span)
                      repeat count
                      for span = (criterion-next-word-span text position)
                      while (and span
                                 (criterion-numeric-token-p
                                  (subseq text (car span) (cdr span))))
                      collect span
                      do (setf position (cdr span))))))

(defun criterion-span-excluded-p (span exclusions)
  (some (lambda (excluded)
          (and (< (car span) (cdr excluded))
               (< (car excluded) (cdr span))))
        exclusions))

(defun criterion-title-columns (text row frames)
  (when (and (criterion-title-row-p text)
             (some (lambda (frame) (= row (first frame))) frames))
    (let ((exclusions (criterion-title-exclusion-spans text)))
      (loop for frame in frames
            when (= row (first frame))
            append
            (loop for label in (criterion-rule-values :fixed-title-labels)
                  append
                  (loop for span in (criterion-label-spans text label)
                        when (and (> (car span) (third frame))
                                  (<= (cdr span) (fourth frame))
                                  (not (criterion-span-excluded-p
                                        span exclusions)))
                        append
                        (loop for column from (car span) below (cdr span)
                              collect column)))))))

(defun criterion-process-header-span (text eligible-p)
  (when eligible-p
    (let ((spans
           (loop for label in (criterion-rule-values :process-header-labels)
                 append (criterion-label-spans text label))))
      (when (some (lambda (pair)
                    (every (lambda (label)
                             (criterion-label-spans text label))
                           pair))
                  (criterion-rule-values :process-header-anchor-pairs))
        (cons (apply #'min (mapcar #'car spans))
              (apply #'max (mapcar #'cdr spans)))))))

(defun criterion-row-text (grid row)
  (let ((text (make-string (grid-width grid))))
    (dotimes (column (grid-width grid))
      (setf (char text column)
            (cell-character (grid-cell-at grid row column))))
    text))

(defun criterion-frame-pairs (text corner-pair)
  (destructuring-bind (left-corner right-corner) corner-pair
    (loop with position = 0
          for left = (position left-corner text :start position)
          while left
          for right = (position right-corner text :start (1+ left))
          while right
          collect (cons left right)
          do (setf position (1+ right)))))

(defun criterion-frame-bottom (grid top left right corner-pair)
  (destructuring-bind (left-corner right-corner) corner-pair
    (or (loop for row from (1+ top) below (grid-height grid)
              when (and (char= (cell-character
                                (grid-cell-at grid row left))
                               left-corner)
                        (char= (cell-character
                                (grid-cell-at grid row right))
                               right-corner))
                return row)
        (1- (grid-height grid)))))

(defun criterion-frame-spans (grid)
  (let ((corner-pairs (criterion-rule-values :frame-corner-pairs)))
    (loop for row below (grid-height grid)
          append
          (loop for (left . right)
                  in (criterion-frame-pairs
                      (criterion-row-text grid row) (first corner-pairs))
                collect (list row
                              (criterion-frame-bottom
                               grid row left right (second corner-pairs))
                              left
                              right)))))
(defun criterion-frame-contains-p (outer inner)
  (destructuring-bind (outer-top outer-bottom outer-left outer-right) outer
    (destructuring-bind (inner-top inner-bottom inner-left inner-right) inner
      (and (< outer-top inner-top)
           (< inner-bottom outer-bottom)
           (< outer-left inner-left)
           (< inner-right outer-right)))))
(defun criterion-projection-frames (grid)
  (let* ((frames (criterion-frame-spans grid))
         (terminal-frame
           (find (list 0
                       (1- (grid-height grid))
                       0
                       (1- (grid-width grid)))
                 frames
                 :test #'equal))
         (frames-without-terminal
           (remove terminal-frame frames :test #'equal)))
    (append (when terminal-frame (list terminal-frame))
            (remove-if
             (lambda (frame)
               (some (lambda (container)
                       (criterion-frame-contains-p container frame))
                     frames-without-terminal))
             frames-without-terminal))))
(defun criterion-frame-divider-row-p (grid row left right)
  (some (lambda (pair)
          (and (char= (cell-character (grid-cell-at grid row left))
                      (first pair))
               (char= (cell-character (grid-cell-at grid row right))
                      (second pair))))
        (criterion-rule-values :frame-divider-pairs)))
(defun criterion-process-header-row-p (grid row frames)
  (let ((text (criterion-row-text grid row)))
    (loop for (top bottom left right) in frames
          thereis
          (and (= row (1+ top))
               (< row bottom)
               (search "Processes" (criterion-row-text grid top))
               (some
                (lambda (pair)
                  (let ((spans
                         (mapcar
                          (lambda (label)
                            (find-if
                             (lambda (span)
                               (and (< left (car span))
                                    (<= (cdr span) right)))
                             (criterion-label-spans text label)))
                          pair)))
                    (and (every #'identity spans)
                         (every
                          (lambda (span)
                            (loop for column from (car span) below (cdr span)
                                  always (cell-bold-p
                                          (grid-cell-at grid row column))))
                          spans))))
                (criterion-rule-values :process-header-anchor-pairs))))))
(defun criterion-border-cell-p (grid row column character frames)
  (and (criterion-character-in-p
        character (first (criterion-rule-values :border-characters)))
       (some (lambda (frame)
               (destructuring-bind (top bottom left right) frame
                 (and (<= top row bottom)
                      (<= left column right)
                      (or (= row top)
                          (= row bottom)
                          (= column left)
                          (= column right)
                          (criterion-frame-divider-row-p
                           grid row left right))))) frames)))
(defun criterion-color-vector (color)
  (let ((kind (car color))
        (value (cdr color)))
    (vector (ecase kind
              (:default "default")
              (:indexed "indexed")
              (:rgb "rgb"))
            (case kind
              (:default nil)
              (:indexed value)
              (:rgb (coerce value 'vector))))))
(defun project-criterion-grid (grid)
  (let ((entries nil)
        (frames (criterion-projection-frames grid)))
    (dotimes (row (grid-height grid))
      (let* ((text (criterion-row-text grid row))
             (title-columns (criterion-title-columns text row frames))
             (header-span
               (criterion-process-header-span
                text
                (criterion-process-header-row-p grid row frames))))
        (dotimes (column (grid-width grid))
          (let* ((cell (grid-cell-at grid row column))
                 (character (cell-character cell))
                 (category
                   (cond
                     ((criterion-border-cell-p grid row column character frames)
                      "border")
                     ((and header-span
                           (<= (car header-span) column)
                           (< column (cdr header-span)))
                      "process-header")
                     ((member column title-columns) "title"))))
            (when category
              (push (vector category row column (string character)
                            (criterion-color-vector (cell-foreground cell))
                            (criterion-color-vector (cell-background cell))
                            (if (cell-bold-p cell) 1 0))
                    entries))))))
    (vector (grid-width grid)
            (grid-height grid)
            (coerce (nreverse entries) 'vector))))