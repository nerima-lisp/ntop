(in-package #:ntop.ui)

(defstruct
    (table-column
     (:constructor %make-table-column
                   (id header
                       width-kind
                       width
                       max-percent
                       hidden-p
                       sortable-p
                       value-function)))
  id
  header
  width-kind
  width
  max-percent
  hidden-p
  sortable-p
  value-function)

(defun make-table-column (&key id
                               header
                               (width-kind :soft)
                               width
                               max-percent
                               hidden-p
                               sortable-p
                               value-function)
  (check-type header string)
  (unless (member width-kind '(:hard :soft))
    (error "Unknown table column width kind ~S." width-kind))
  (when width
    (unless (and (integerp width) (not (minusp width)))
      (error "A table column width must be a non-negative integer or NIL.")))
  (when (and (eq width-kind :hard) (null width))
    (error "A hard table column requires a width."))
  (when max-percent
    (unless (and (numberp max-percent) (<= 0 max-percent 1))
      (error "A soft width limit must be between zero and one.")))
  (when (and (eq width-kind :hard) max-percent)
    (error "A hard table column cannot have a percentage limit."))
  (when (and value-function (not (functionp value-function)))
    (error "A table column value function must be a function or NIL."))
  (%make-table-column id
                      header
                      width-kind
                      width
                      max-percent
                      (not (null hidden-p))
                      (not (null sortable-p))
                      value-function))

(defmacro define-table-columns (name &body definitions)
  "Define NAME as a list of declarative table columns."
  `(defparameter ,name
     (list
      ,@(mapcar
         (lambda (definition)
           (unless (and (listp definition) (>= (length definition) 3))
             (error "A table column definition needs an ID, header and width kind."))
           (destructuring-bind (id header width-kind &rest options) definition
             `(make-table-column :id
                                 ,id
                                 :header
                                 ,header
                                 :width-kind
                                 ,width-kind
                                 ,@options)))
         definitions))))

(defstruct table-column-layout
  id
  x
  width)

(defun measure-table-text (text)
  (check-type text string)
  (loop for character across text
        sum (cl-tty-kit:char-width character)))

(defun truncate-table-text (text width)
  (check-type text string)
  (check-type width (integer 0 *))
  (with-output-to-string (output)
    (loop with used = 0
          for character across text
          for cell-width = (cl-tty-kit:char-width character)
          while (<= (+ used cell-width) width)
          do (write-char character output) (incf used cell-width))))

(defun table-column-cell-text (column row)
  (let ((value
         (if (table-column-value-function column)
             (funcall (table-column-value-function column) row)
             (getf row (table-column-id column)))))
    (typecase value
      (null "")
      (string value)
      (t (princ-to-string value)))))

(defun table-column-header-width (column)
  (+ (measure-table-text (table-column-header column))
     (if (table-column-sortable-p column)
         1
         0)))

(defun table-column-desired-width (column rows)
  (if (eq (table-column-width-kind column) :hard)
      (table-column-width column)
      (loop with desired = (table-column-header-width column)
            for row in rows
            for width = (measure-table-text (table-column-cell-text column row))
            when (> width desired)
              do (setf desired width)
            finally (return (max desired (or (table-column-width column) 0))))))

(defun table-column-initial-width (column rows total-width remaining-width)
  (let ((desired (table-column-desired-width column rows)))
    (if (eq (table-column-width-kind column) :hard)
        desired
        (let* ((header-width (table-column-header-width column))
               (percentage-limit (table-column-max-percent column)))
          (when (> header-width remaining-width)
            (return-from table-column-initial-width
              nil))
          (let ((soft-limit
                 (if percentage-limit
                     (max header-width (ceiling (* percentage-limit total-width)))
                     desired)))
            (min soft-limit desired remaining-width))))))

(defun compute-table-layout (columns total-width &key right-to-left-p rows)
  (check-type total-width (integer 0 *))
  (let* ((column-list (coerce columns 'list))
         (row-list (coerce rows 'list))
         (ordered
          (if right-to-left-p
              (reverse column-list)
              column-list))
         (accepted nil)
         (remaining-width total-width))
    (when (> (length column-list) 100)
      (error "A table cannot contain more than 100 columns."))
    (dolist (column ordered)
      (unless (table-column-hidden-p column)
        (let ((width
               (table-column-initial-width column row-list total-width remaining-width)))
          (when (or (null width) (zerop width) (> width remaining-width))
            (return))
          (push (cons column width) accepted)
          (setf remaining-width (max 0 (- remaining-width width 1))))))
    (setf accepted (nreverse accepted))
    (when right-to-left-p
      (setf accepted (nreverse accepted)))
    (when accepted
      (let* ((count (length accepted))
             (per-column (floor remaining-width count))
             (leading-extra (mod remaining-width count)))
        (loop for entry in accepted
              for index from 0
              do (incf (cdr entry)
                       (+ per-column
                          (if (< index leading-extra)
                              1
                              0))))))
    (loop with x = 0
          for (column . width) in accepted
          collect (make-table-column-layout :id
                                            (table-column-id column)
                                            :x
                                            x
                                            :width
                                            width)
          do (incf x (1+ width)))))

(defun cpu-legend-region (total-width &key left-p)
  (check-type total-width (integer 0 *))
  (let ((legend-width (floor (* total-width 3) 20)))
    (if (< legend-width 6)
        (values 0 0 0 total-width)
        (if left-p
            (values 0 legend-width legend-width (- total-width legend-width))
            (values (- total-width legend-width)
                    legend-width
                    0
                    (- total-width legend-width))))))

(defun cpu-legend-label (kind width)
  (check-type width (integer 1 *))
  (cond
    ((eq kind :all) "All")
    ((eq kind :average) "AVG")
    ((and (integerp kind) (not (minusp kind)))
     (if (< width 5)
         (write-to-string kind)
         (format nil "CPU~D" kind)))
    (t (error "Unknown CPU legend entry ~S." kind))))
