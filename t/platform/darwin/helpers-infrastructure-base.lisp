(in-package #:ntop.test)

(defmacro define-case-table (suite cases format-control bindings &body body)
  "Expand CASES into a cl-weave it-each group named by SUITE."
  `(cl-weave:describe ,suite
                      (cl-weave:it-each ,cases ,format-control ,bindings ,@body)))

(defun %make-octets (values)
  "Build a vector of unsigned 8-bit bytes from VALUES."
  (make-array (length values)
              :element-type
              (quote (unsigned-byte 8))
              :initial-contents
              values))

(defun %encode-unsigned-octets (value width)
  "Encode VALUE as WIDTH little-endian unsigned 8-bit bytes."
  (let ((octets (make-array width :element-type '(unsigned-byte 8))))
    (dotimes (index width octets)
      (setf (aref octets index) (ldb (byte 8 (* index 8)) value)))))