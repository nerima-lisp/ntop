(in-package #:ntop.e2e)

(defparameter *golden-categories*
  '("border" "title" "process-header"))

(progn
  (defparameter *golden-widths*
    #(40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220))
  (defparameter *golden-heights*
    #(12 18 24 30 36 42 48 54 60))
  (defun golden-dimensions ()
    (loop for width across *golden-widths*
          append (loop for height across *golden-heights*
                       collect (list width height)))))

(defmacro define-golden-comparison-cases (&body cases)
  `(cl-weave:it-each ,cases
                     "compares criterion-A projections for ~A"
                     (case-name left right expected)
                     (declare (ignore case-name))
                     (cl-weave:expect
                      (ntop.e2e::criterion-projections-equal-p left right)
                      :to-be
                      expected)))

(defun %valid-projection-color-p (color)
  (and (vectorp color)
       (= (length color) 2)
       (let ((kind (aref color 0))
             (value (aref color 1)))
         (cond
           ((and (stringp kind) (string= kind "default"))
            (or (null value)
                (and (vectorp value)
                     (not (stringp value))
                     (zerop (length value)))))
           ((and (stringp kind) (string= kind "indexed"))
            (and (integerp value) (<= 0 value 255)))
           ((and (stringp kind) (string= kind "rgb"))
            (and (vectorp value)
                 (= (length value) 3)
                 (every
                  (lambda (component)
                    (and (integerp component) (<= 0 component 255)))
                  value)))
           (t nil)))))

(defun %valid-projection-cell-p (cell width height)
  (and (vectorp cell)
       (= (length cell) 7)
       (stringp (aref cell 0))
       (member (aref cell 0) *golden-categories* :test #'string=)
       (integerp (aref cell 1))
       (<= 0 (aref cell 1))
       (< (aref cell 1) height)
       (integerp (aref cell 2))
       (<= 0 (aref cell 2))
       (< (aref cell 2) width)
       (stringp (aref cell 3))
       (= (length (aref cell 3)) 1)
       (%valid-projection-color-p (aref cell 4))
       (%valid-projection-color-p (aref cell 5))
       (member (aref cell 6) '(0 1))))

(defun %valid-criterion-projection-p (projection width height)
  (and (vectorp projection)
       (= (length projection) 3)
       (eql (aref projection 0) width)
       (eql (aref projection 1) height)
       (vectorp (aref projection 2))
       (every
        (lambda (cell)
          (%valid-projection-cell-p cell width height))
        (aref projection 2))))

(progn
  (defun %projection-fixed-cells (projection)
    (remove-if
     (lambda (cell)
       (string= (aref cell 0) "process-header"))
     (aref projection 2)))
  (defun %process-header-field-attributes (column-id role cells)
    (let* ((attributes
            (mapcar (lambda (cell)
                      (and cell
                           (vector (aref cell 4)
                                   (aref cell 5)
                                   (aref cell 6))))
                    cells))
           (unique-attributes
            (remove-duplicates attributes :test #'equalp)))
      (if (and (= (length unique-attributes) 1)
               (first unique-attributes))
          (let ((attribute (first unique-attributes)))
            (vector column-id role
                    (aref attribute 0)
                    (aref attribute 1)
                    (aref attribute 2)))
        (vector column-id :invalid))))
  (defun %process-header-attribute-cells (projection)
    (let* ((entries (aref projection 2))
           (rows
            (sort
             (remove-duplicates
              (loop for entry across entries
                    when (string= (aref entry 0) "process-header")
                    collect (aref entry 1))
              :test #'=)
             #'<))
           (column-ids
            (criterion-rule-values :process-header-column-ids))
           (sort-markers
            (first (criterion-rule-values :process-header-sort-markers))))
      (loop for row in rows
            append
            (let* ((cells
                    (sort
                     (loop for entry across entries
                           when (and (string= (aref entry 0) "process-header")
                                     (= (aref entry 1) row))
                           collect entry)
                     #'<
                     :key (lambda (cell) (aref cell 2))))
                   (first-column
                    (reduce #'min cells :key (lambda (cell) (aref cell 2))))
                   (last-column
                    (reduce #'max cells :key (lambda (cell) (aref cell 2))))
                   (text
                    (make-string (1+ (- last-column first-column))
                                 :initial-element #\Space)))
              (dolist (cell cells)
                (setf (char text (- (aref cell 2) first-column))
                      (char (aref cell 3) 0)))
              (loop for (label column-id) in column-ids
                    append
                    (loop for span in (criterion-label-spans text label)
                          append
                          (let* ((label-cells
                                  (loop for column from (+ first-column (car span))
                                        below (+ first-column (cdr span))
                                        collect
                                        (find column cells
                                              :key (lambda (entry) (aref entry 2))
                                              :test #'=)))
                                 (marker-position (cdr span))
                                 (marker-cell
                                  (and (< marker-position (length text))
                                       (find (char text marker-position)
                                             sort-markers
                                             :test #'char=)
                                       (find (+ first-column marker-position)
                                             cells
                                             :key (lambda (entry) (aref entry 2))
                                             :test #'=)))
                                 (marker-cells
                                  (when marker-cell (list marker-cell))))
                            (append
                             (list (%process-header-field-attributes
                                    column-id :label label-cells))
                             (when marker-cell
                               (list (%process-header-field-attributes
                                      column-id :sort-marker marker-cells)))))))))))
  (defun %same-process-header-attribute-key-p (left right)
    (and (= (length left) 5)
         (= (length right) 5)
         (eq (aref left 0) (aref right 0))
         (eq (aref left 1) (aref right 1))))
  (defun %shared-process-header-attribute-cells (left right)
    (loop for left-cell in left
          for right-cell = (find left-cell right
                                 :test #'%same-process-header-attribute-key-p)
          when right-cell
          collect (cons left-cell right-cell)))
  (defun %process-header-attributes-equal-p (left right)
    (and (notany (lambda (cell) (eq (aref cell 1) :invalid)) left)
         (notany (lambda (cell) (eq (aref cell 1) :invalid)) right)
         (every (lambda (pair)
                  (equalp (subseq (car pair) 2)
                          (subseq (cdr pair) 2)))
                (%shared-process-header-attribute-cells left right)))))

(defun criterion-projections-equal-p (left right)
  (and (vectorp left)
       (>= (length left) 2)
       (vectorp right)
       (>= (length right) 2)
       (let ((width (aref left 0))
             (height (aref left 1)))
         (and (integerp width)
              (integerp height)
              (plusp width)
              (plusp height)
              (%valid-criterion-projection-p left width height)
              (%valid-criterion-projection-p right width height)
              (equalp (%projection-fixed-cells left)
                      (%projection-fixed-cells right))
              (%process-header-attributes-equal-p
               (%process-header-attribute-cells left)
               (%process-header-attribute-cells right))))))

(defun %golden-pathname (directory width height)
  (merge-pathnames (format nil "~d-~d.json" width height)
                   (uiop:ensure-directory-pathname (pathname directory))))

(defun %read-criterion-golden (pathname width height)
  (with-open-file (stream pathname :direction :input :external-format :utf-8)
    (let ((projection
           (json-kit:read-json stream
                               :array-type
                               :vector
                               :max-input-length
                               (* 4 1024 1024)
                               :timeout-seconds
                               5d0)))
      (unless (%valid-criterion-projection-p projection width height)
        (error "Invalid criterion-A projection in ~a." pathname))
      projection)))

(defun compare-goldens (&key left right (scope "condition-a"))
  (unless (string= scope "condition-a")
    (error "Unsupported golden comparison scope: ~a." scope))
  (let ((mismatches nil)
        (compared 0))
    (dolist (dimension (golden-dimensions))
      (destructuring-bind (width height) dimension
        (let ((left-projection
               (%read-criterion-golden (%golden-pathname left width height)
                                       width
                                       height))
              (right-projection
               (%read-criterion-golden (%golden-pathname right width height)
                                       width
                                       height)))
          (incf compared)
          (unless (equalp left-projection right-projection)
            (push (format nil "~dx~d" width height) mismatches)))))
    (when mismatches
      (error "Criterion-A golden sets differ at ~{~a~^, ~}."
             (nreverse mismatches)))
    (format t "Compared ~d criterion-A golden projections.~%" compared)
    0))
