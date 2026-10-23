(in-package #:ntop.e2e)

(defmacro define-btm-layout (&rest rows)
  `(quote ,rows))

(defparameter *btm-layout*
  (define-btm-layout ((cpu 2) (mem nil))
                     ((net 2) (disk 2) (temp nil))
                     ((proc 2 :default))))

(defun btm-config-string (rate)
  (unless (and (integerp rate) (plusp rate))
    (error "RATE must be a positive integer."))
  (with-output-to-string (stream)
    (format stream
            "[flags]~%basic = false~%dot_marker = false~%rate = ~d~%~%"
            rate)
    (format stream
            "[processes]~%case_sensitive = false~%default_grouped = true~%~%")
    (loop for
      row
      in
      *btm-layout*
      do
      (format stream "[[row]]~%")
      (loop for
        (type ratio marker)
        in
        row
        do
        (format stream
                "[[row.child]]~%type = ~s~%"
                (string-downcase (symbol-name type)))
        (when ratio
          (format stream "ratio = ~d~%" ratio))
        (when marker
          (format stream "default = true~%")))
      (terpri stream))))
