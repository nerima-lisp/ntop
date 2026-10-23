(in-package #:ntop.gpu.domain)

(defun gpu-display-name (name index count short-names-p)
  (if short-names-p
      (if (= count 1)
          "GPU"
          (format nil "GPU~D" index))
      name))

(defun gpu-memory-percent (used-bytes total-bytes)
  (when
      (and (integerp used-bytes)
           (integerp total-bytes)
           (<= 0 used-bytes total-bytes)
           (plusp total-bytes))
    (* 100 (/ used-bytes total-bytes))))
