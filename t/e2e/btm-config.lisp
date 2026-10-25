(in-package #:ntop.e2e)

(defmacro define-btm-layout (&rest rows)
  `(quote ,rows))

(defparameter *btm-layout*
  (define-btm-layout ((cpu nil))
                     ((mem 4) (:column 3 ((temp nil) (disk nil))))
                     ((net nil) (proc nil :default))))

(defun %write-btm-widget (stream widget nested-p)
  (destructuring-bind (type &optional ratio marker) widget
    (format stream
            (if nested-p "[[row.child.child]]~%" "[[row.child]]~%"))
    (when ratio
      (format stream "ratio = ~d~%" ratio))
    (format stream "type = ~s~%" (string-downcase (symbol-name type)))
    (when marker
      (format stream "default = true~%"))))

(defun %write-btm-layout-child (stream child)
  (if (eq (first child) :column)
      (destructuring-bind (kind ratio widgets) child
        (declare (ignore kind))
        (format stream "[[row.child]]~%ratio = ~d~%" ratio)
        (dolist (widget widgets)
          (%write-btm-widget stream widget t)))
      (%write-btm-widget stream child nil)))

(defun btm-config-string (rate)
  (unless (and (integerp rate) (plusp rate))
    (error "RATE must be a positive integer."))
  (let* ((row-count (length *btm-layout*))
         (row-ratios
           (if (= row-count 3)
               (list 3 4 3)
               (make-list row-count :initial-element 1))))
    (with-output-to-string (stream)
      (format stream
              "[flags]~%basic = false~%dot_marker = false~%rate = ~d~%~%"
              rate)
      (format stream
              "[processes]~%case_sensitive = false~%default_grouped = true~%~%")
      (loop for row in *btm-layout*
            for row-ratio in row-ratios
            do (progn
                 (format stream "[[row]]~%ratio = ~d~%" row-ratio)
                 (dolist (child row)
                   (%write-btm-layout-child stream child))
                 (terpri stream))))))
