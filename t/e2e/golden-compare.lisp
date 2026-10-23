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
           ((and (stringp kind) (string= kind "default")) (null value))
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
              (equalp left right)))))

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
