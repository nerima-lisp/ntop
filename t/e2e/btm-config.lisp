(in-package #:ntop.e2e)

(defmacro define-btm-layout (&rest rows)
  `(quote ,rows))

(defparameter *btm-layout*
  (define-btm-layout ((cpu 2) (mem 1))
                     ((net 2) (disk 2) (temp 1))
                     ((proc 2 :default))))
;; Match the 1:1:2 row sizing from the §3.4 reference layout.
(defparameter *btm-row-ratios* (quote (1 1 2)))

(defun %write-btm-widget (stream widget)
  (destructuring-bind (type &optional ratio marker) widget
    (format stream "[[row.child]]~%")
    (when ratio
      (format stream "ratio = ~d~%" ratio))
    (format stream "type = ~s~%" (string-downcase (symbol-name type)))
    (when marker
      (format stream "default = true~%"))))

(defun btm-config-string (rate &optional tree-mode-p)
  (unless (and (integerp rate) (plusp rate))
    (error "RATE must be a positive integer."))
  (let* ((row-count (length *btm-layout*))
         (row-ratios
           (if (= row-count (length *btm-row-ratios*))
               *btm-row-ratios*
               (make-list row-count :initial-element 1))))
    (with-output-to-string (stream)
      (format stream
              "[flags]~%basic = false~%dot_marker = false~%rate = ~d~%~%"
              rate)
      (format stream
              "[processes]~%case_sensitive = false~%default_grouped = ~a~%"
              (if tree-mode-p "false" "true"))
      (when tree-mode-p
        (format stream "default_tree = true~%"))
      (terpri stream)
      (loop for row in *btm-layout*
            for row-ratio in row-ratios
            do (progn
                 (format stream "[[row]]~%ratio = ~d~%" row-ratio)
                 (dolist (child row)
                   (%write-btm-widget stream child))
                 (terpri stream))))))
