(in-package #:ntop.e2e)

(defun parse-ansi-text (text grid)
  (unless (stringp text)
    (parse-fail text 0 "input must be decoded text"))
  (let* ((row 0)
         (column 0)
         (foreground '(:default))
         (background '(:default))
         (bold-p nil)
         (saved-row 0)
         (saved-column 0)
         (scroll-top 0)
         (scroll-bottom (1- (grid-height grid)))
         (width (grid-width grid))
         (height (grid-height grid))
         (cells (grid-cells grid)))
    (labels ((make-cell (character)
               (%make-terminal-cell character foreground background bold-p))
             (blank-cell ()
               (make-cell #\Space))
             (clear-range (target-row start end)
               (when (and (<= 0 target-row) (< target-row height))
                 (loop for target-column from (max 0 start) to (min (1- width)
                                                                    end)
                       do (setf (aref cells target-row target-column) (blank-cell)))))
             (clear-row (target-row)
               (clear-range target-row 0 (1- width)))
             (scroll-up (count)
               (dotimes (step (min count (1+ (- scroll-bottom scroll-top))))
                 (loop for target-row from scroll-top below scroll-bottom
                       do (loop for target-column below width
                                do (setf (aref cells target-row target-column) (aref
                                                                                cells
                                                                                (1+
                                                                                 target-row)
                                                                                target-column))))
                 (clear-row scroll-bottom)))
             (scroll-down (count)
               (dotimes (step (min count (1+ (- scroll-bottom scroll-top))))
                 (loop for target-row downfrom scroll-bottom above scroll-top
                       do (loop for target-column below width
                                do (setf (aref cells target-row target-column) (aref
                                                                                cells
                                                                                (1-
                                                                                 target-row)
                                                                                target-column))))
                 (clear-row scroll-top)))
             (line-feed ()
               (if (= row scroll-bottom)
                   (scroll-up 1)
                   (when (< row (1- height))
                     (incf row))))
             (reverse-index ()
               (if (= row scroll-top)
                   (scroll-down 1)
                   (when (plusp row)
                     (decf row))))
             (set-cursor (new-row new-column)
               (setf row (min (1- height) (max 0 new-row))
                     column (min (1- width) (max 0 new-column))))
             (put-character (character)
               (when (>= column width)
                 (setf column 0)
                 (line-feed))
               (setf (aref cells row column) (make-cell character))
               (incf column))
             (erase-display (mode)
               (case mode
                 (0
                   (clear-range row column (1- width))
                   (loop for target-row from (1+ row) below height
                         do (clear-row target-row)))
                 (1
                   (loop for target-row below row
                         do (clear-row target-row))
                   (clear-range row 0 column))
                 ((2 3)
                  (loop for target-row below height
                        do (clear-row target-row)))))
             (erase-line (mode)
               (case mode
                 (0 (clear-range row column (1- width)))
                 (1 (clear-range row 0 column))
                 (2 (clear-row row))))
             (move-tab (count forward-p)
               (dotimes (step (min count width))
                 (setf column (if forward-p
                                  (min (1- width) (* 8 (1+ (floor column 8))))
                                  (max 0 (* 8 (1- (ceiling column 8))))))))
             (apply-sgr (parameters position)
               (let ((parameters (or parameters '(0))))
                 (loop with index = 0
                       while (< index (length parameters))
                       for code = (or (nth index parameters) 0)
                       for rule = (assoc code *sgr-rule-data*)
                       do (cond
                            ((member code '(38 48))
                             (multiple-value-bind (color next-index)
                                 (parse-extended-color text
                                                       parameters
                                                       index
                                                       position)
                               (if (= code 38)
                                   (setf foreground color)
                                   (setf background color))
                               (setf index next-index)))
                            (rule
                             (let ((action (second rule)))
                               (case (first action)
                                 (:reset
                                  (setf foreground '(:default)
                                        background '(:default)
                                        bold-p nil))
                                 (:bold
                                  (setf bold-p (second action)))
                                 (:foreground
                                  (setf foreground (cons (second action)
                                                         (third action))))
                                 (:background
                                  (setf background (cons (second action)
                                                         (third action)))))
                               (incf index)))
                            (t (incf index))))))
             (apply-csi (raw final position)
               (let* ((private-p
                       (and (plusp (length raw)) (find (char raw 0) "?<>!")))
                      (parameters
                       (parse-csi-parameters text
                                             (if private-p
                                                 (subseq raw 1)
                                                 raw)
                                             position)))
                 (unless private-p
                   (case final
                     (#\m (apply-sgr parameters position))
                     (#\A
                      (set-cursor (- row (max 1 (parameter-at parameters 0 1)))
                                  column))
                     (#\B
                      (set-cursor (+ row (max 1 (parameter-at parameters 0 1)))
                                  column))
                     (#\C
                      (set-cursor row
                                  (+ (min column (1- width))
                                     (max 1 (parameter-at parameters 0 1)))))
                     (#\D
                      (set-cursor row
                                  (- (min column (1- width))
                                     (max 1 (parameter-at parameters 0 1)))))
                     ((#\E #\F)
                      (set-cursor
                       (if (char= final #\E)
                           (+ row (max 1 (parameter-at parameters 0 1)))
                           (- row (max 1 (parameter-at parameters 0 1))))
                       0))
                     ((#\G #\`)
                      (set-cursor row
                                  (1- (max 1 (parameter-at parameters 0 1)))))
                     (#\d
                      (set-cursor (1- (max 1 (parameter-at parameters 0 1)))
                                  column))
                     ((#\H #\f)
                      (set-cursor (1- (max 1 (parameter-at parameters 0 1)))
                                  (1- (max 1 (parameter-at parameters 1 1)))))
                     (#\J (erase-display (parameter-at parameters 0 0)))
                     (#\K (erase-line (parameter-at parameters 0 0)))
                     (#\X
                      (clear-range row
                                   column
                                   (+ column
                                      (max 1 (parameter-at parameters 0 1))
                                      -1)))
                     (#\@
                      (let ((count
                             (min width (max 1 (parameter-at parameters 0 1)))))
                        (loop for target-column downfrom (1- width) above (+
                                                                           column
                                                                           count
                                                                           -1)
                              do (setf (aref cells row target-column) (aref
                                                                       cells
                                                                       row
                                                                       (-
                                                                        target-column
                                                                        count))))
                        (clear-range row column (+ column count -1))))
                     (#\P
                      (let ((count
                             (min width (max 1 (parameter-at parameters 0 1)))))
                        (loop for target-column from column below (- width
                                                                     count)
                              do (setf (aref cells row target-column) (aref
                                                                       cells
                                                                       row
                                                                       (+
                                                                        target-column
                                                                        count))))
                        (clear-range row (- width count) (1- width))))
                     ((#\S #\T)
                      (let ((count (max 1 (parameter-at parameters 0 1))))
                        (if (char= final #\S)
                            (scroll-up count)
                            (scroll-down count))))
                     (#\r
                      (let ((top (parameter-at parameters 0 1))
                            (bottom (parameter-at parameters 1 height)))
                        (when (and (<= 1 top bottom height) (< top bottom))
                          (setf scroll-top (1- top)
                                scroll-bottom (1- bottom)
                                row 0
                                column 0))))
                     (#\s
                      (setf saved-row row
                            saved-column (min column (1- width))))
                     (#\u (set-cursor saved-row saved-column))
                     (#\I (move-tab (max 1 (parameter-at parameters 0 1)) t))
                     (#\Z (move-tab (max 1 (parameter-at parameters 0 1)) nil))))))
             (consume-csi (start)
               (multiple-value-bind (raw final next-position)
                   (parse-csi-sequence text start)
                 (apply-csi raw final start)
                 next-position))
             (consume-control-string (start)
               (loop for position from start below (length text)
                     for character = (char text position)
                     do (cond
                          ((or (char= character #\Bell)
                               (= (char-code character) #x9c))
                           (return-from consume-control-string
                             (1+ position)))
                          ((and (char= character #\Escape)
                                (< (1+ position) (length text))
                                (char= (char text (1+ position)) #\\))
                           (return-from consume-control-string
                             (+ position 2))))
                     finally (parse-fail text
                                         start
                                         "unterminated control string")))
             (reset-screen ()
               (setf row 0
                     column 0
                     foreground '(:default)
                     background '(:default)
                     bold-p nil
                     saved-row 0
                     saved-column 0
                     scroll-top 0
                     scroll-bottom (1- height))
               (loop for target-row below height
                     do (clear-row target-row)))
             (consume-escape (start)
               (when (>= start (length text))
                 (parse-fail text start "incomplete escape sequence"))
               (let ((character (char text start)))
                 (case character
                   (#\[ (consume-csi (1+ start)))
                   (#\] (consume-control-string (1+ start)))
                   ((#\P #\X #\^ #\_) (consume-control-string (1+ start)))
                   (#\D
                     (line-feed)
                     (1+ start))
                   (#\M
                     (reverse-index)
                     (1+ start))
                   (#\E
                     (line-feed)
                     (setf column 0)
                     (1+ start))
                   (#\7
                     (setf saved-row row
                           saved-column (min column (1- width)))
                     (1+ start))
                   (#\8
                     (set-cursor saved-row saved-column)
                     (1+ start))
                   (#\c
                     (reset-screen)
                     (1+ start))
                   ((#\( #\) #\* #\+ #\# #\%)
                     (when (>= (1+ start) (length text))
                       (parse-fail text start "incomplete escape selector"))
                     (+ start 2))
                   ((#\H #\= #\> #\\) (1+ start))
                   (otherwise
                    (parse-fail text start "unsupported escape sequence"))))))
      (loop with position = 0
            while (< position (length text))
            do (let* ((character (char text position))
                      (code (char-code character)))
                 (setf position (cond
                                  ((char= character #\Escape)
                                   (consume-escape (1+ position)))
                                  ((= code #x9b) (consume-csi (1+ position)))
                                  ((= code #x9d)
                                   (consume-control-string (1+ position)))
                                  ((= code #x9c) (1+ position))
                                  ((char= character #\Return)
                                    (setf column 0)
                                    (1+ position))
                                  ((char= character #\Newline)
                                    (line-feed)
                                    (1+ position))
                                  ((char= character #\Backspace)
                                    (setf column (max 0 (1- column)))
                                    (1+ position))
                                  ((char= character #\Tab)
                                    (move-tab 1 t)
                                    (1+ position))
                                  ((< code #x20) (1+ position))
                                  (t
                                    (put-character character)
                                    (1+ position))))))
      grid)))

(defun parse-ansi (text grid on-success on-failure)
  (let ((failure
         (handler-case (progn
                         (parse-ansi-text text grid)
                         nil)
           (ansi-parse-error (condition)
             condition))))
    (if failure
        (funcall on-failure failure)
        (funcall on-success grid))))
