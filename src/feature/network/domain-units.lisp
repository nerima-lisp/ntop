(in-package #:ntop.network.domain)

(defparameter +network-prefix-scales+
  '((:decimal (:none 1) (:kilo 1000) (:mega 1000000) (:giga 1000000000)
              (:tera 1000000000000))
    (:binary (:none 1) (:kibi 1024) (:mebi 1048576) (:gibi 1073741824)
             (:tebi 1099511627776))))

(defun %network-prefix-system (binary-prefix-p)
  (if binary-prefix-p :binary :decimal))

(defun %network-prefix (quantity prefix-system)
  (let ((magnitude (abs (ntop.shared.domain:quantity-value quantity))))
    (loop with selected-prefix = :none
          for (prefix threshold) in (cdr (assoc prefix-system
                                                +network-prefix-scales+))
          when (>= magnitude threshold)
            do (setf selected-prefix prefix)
          finally (return selected-prefix))))

(defun network-scale-quantity (quantity binary-prefix-p)
  (when quantity
    (let ((prefix-system (%network-prefix-system binary-prefix-p)))
      (ntop.shared.domain:scale-quantity
       quantity prefix-system (%network-prefix quantity prefix-system)))))

(defun network-axis-scaling (logarithmic-p binary-prefix-p)
  (cond
    ((not logarithmic-p) :linear)
    (binary-prefix-p :log2)
    (t :log10)))
