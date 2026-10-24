(in-package #:ntop.shared.domain)

(defgeneric read-monotonic-time (clock on-time on-unavailable))

(defgeneric read-unix-time (clock on-time on-unavailable))
