(in-package #:ntop.shared.domain)

(defun counter-rate-per-second (previous current elapsed-seconds)
  (check-type previous optional-counter)
  (check-type current optional-counter)
  (check-type elapsed-seconds real)
  (unless (plusp elapsed-seconds)
    (error "Elapsed seconds must be positive: ~S" elapsed-seconds))
  (when (and previous current)
    (if (< current previous)
        0
        (/ (- current previous) elapsed-seconds))))
