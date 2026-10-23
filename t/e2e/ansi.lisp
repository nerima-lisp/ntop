(in-package #:ntop.e2e)

(define-condition ansi-parse-error
  (error)
  ((position :initarg :position :reader ansi-parse-error-position)
   (text :initarg :text :reader ansi-parse-error-text)
   (reason :initarg :reason :reader ansi-parse-error-reason))
  (:report
   (lambda (condition stream)
     (format stream
             "ANSI parse error at character ~d: ~a"
             (ansi-parse-error-position condition)
             (ansi-parse-error-reason condition)))))

(defstruct
    (terminal-cell
     (:constructor %make-terminal-cell (character foreground background bold-p))
     (:conc-name cell-))
  character
  foreground
  background
  bold-p)

(defstruct
    (terminal-grid (:constructor %make-terminal-grid (width height cells))
                   (:conc-name grid-)
                   (:copier nil))
  width
  height
  (cells nil :type (array * (* *))))

(defun default-cell ()
  (%make-terminal-cell #\Space '(:default) '(:default) nil))

(defun make-terminal-grid (&key rows cols)
  (unless (and (integerp rows) (plusp rows))
    (error "ROWS must be a positive integer."))
  (unless (and (integerp cols) (plusp cols))
    (error "COLS must be a positive integer."))
  (let ((cells (make-array (list rows cols))))
    (dotimes (row rows)
      (dotimes (column cols)
        (setf (aref cells row column) (default-cell))))
    (%make-terminal-grid cols rows cells)))

(defun grid-cell-at (grid row column)
  (aref (grid-cells grid) row column))

(defun (setf grid-cell-at) (cell grid row column)
  (setf (aref (grid-cells grid) row column) cell))

(defun copy-terminal-grid (grid)
  (let ((cells (make-array (list (grid-height grid) (grid-width grid)))))
    (dotimes (row (grid-height grid))
      (dotimes (column (grid-width grid))
        (let* ((cell (grid-cell-at grid row column))
               (copy (copy-terminal-cell cell)))
          (setf (cell-foreground copy) (copy-tree (cell-foreground cell))
                (cell-background copy) (copy-tree (cell-background cell))
                (aref cells row column) copy))))
    (%make-terminal-grid (grid-width grid) (grid-height grid) cells)))

(defun parse-fail (text position reason)
  (error 'ansi-parse-error :position position :text text :reason reason))

(defmacro define-sgr-rule (&rest rules)
  (cons 'list
        (mapcar
         (lambda (rule)
           (list 'list (first rule) (list 'quote (rest rule))))
         rules)))

(defparameter *sgr-rule-data*
  (append
   (define-sgr-rule (0 :reset)
                    (1 :bold t)
                    (22 :bold nil)
                    (39 :foreground :default nil)
                    (49 :background :default nil))
   (define-sgr-rule (30 :foreground :indexed 0)
                    (31 :foreground :indexed 1)
                    (32 :foreground :indexed 2)
                    (33 :foreground :indexed 3)
                    (34 :foreground :indexed 4)
                    (35 :foreground :indexed 5)
                    (36 :foreground :indexed 6)
                    (37 :foreground :indexed 7)
                    (90 :foreground :indexed 8)
                    (91 :foreground :indexed 9)
                    (92 :foreground :indexed 10)
                    (93 :foreground :indexed 11)
                    (94 :foreground :indexed 12)
                    (95 :foreground :indexed 13)
                    (96 :foreground :indexed 14)
                    (97 :foreground :indexed 15)
                    (40 :background :indexed 0)
                    (41 :background :indexed 1)
                    (42 :background :indexed 2)
                    (43 :background :indexed 3)
                    (44 :background :indexed 4)
                    (45 :background :indexed 5)
                    (46 :background :indexed 6)
                    (47 :background :indexed 7)
                    (100 :background :indexed 8)
                    (101 :background :indexed 9)
                    (102 :background :indexed 10)
                    (103 :background :indexed 11)
                    (104 :background :indexed 12)
                    (105 :background :indexed 13)
                    (106 :background :indexed 14)
                    (107 :background :indexed 15))))

(defun ascii-digit-p (character)
  (and (char<= #\0 character #\9)))

(defun parse-csi-sequence (text start)
  (loop for position from start below (length text)
        for code = (char-code (char text position))
        do (cond
             ((<= #x40 code #x7e)
              (return-from parse-csi-sequence
                (values (subseq text start position)
                        (char text position)
                        (1+ position))))
             ((<= #x20 code #x3f))
             (t (parse-fail text position "invalid CSI byte")))
        finally (parse-fail text start "unterminated CSI sequence")))

(defun parse-csi-parameters (text raw position)
  (when (zerop (length raw))
    (return-from parse-csi-parameters
      nil))
  (let ((parameters nil)
        (field-start 0))
    (loop for index from 0 to (length raw)
          for delimiter-p = (or (= index (length raw))
                                (char= (char raw index) #\;))
          do (if delimiter-p
                 (let ((field (subseq raw field-start index)))
                   (push
                    (if (zerop (length field))
                        nil
                        (progn
                          (unless (every #'ascii-digit-p field)
                            (parse-fail text
                                        position
                                        "non-numeric CSI parameter"))
                          (parse-integer field)))
                    parameters)
                   (setf field-start (1+ index)))
                 (unless (ascii-digit-p (char raw index))
                   (parse-fail text position "non-numeric CSI parameter"))))
    (nreverse parameters)))

(defun parameter-at (parameters index default)
  (or (nth index parameters) default))

(defun parse-extended-color (text parameters index position)
  (let ((mode (nth (1+ index) parameters)))
    (case mode
      (5
       (let ((value (nth (+ index 2) parameters)))
         (unless (and (integerp value) (<= 0 value 255))
           (parse-fail text position "invalid indexed color"))
         (values (cons :indexed value) (+ index 3))))
      (2
       (let ((red (nth (+ index 2) parameters))
             (green (nth (+ index 3) parameters))
             (blue (nth (+ index 4) parameters)))
         (unless
             (every
              (lambda (value)
                (and (integerp value) (<= 0 value 255)))
              (list red green blue))
           (parse-fail text position "invalid RGB color"))
         (values (cons :rgb (list red green blue)) (+ index 5))))
      (otherwise (parse-fail text position "invalid extended color mode")))))
