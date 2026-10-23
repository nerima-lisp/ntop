(in-package #:ntop.app.infrastructure)

(defclass system-clock ()
  ())

(defun make-system-clock ()
  (make-instance 'system-clock))

(defun %report-monotonic-time (read-time on-time on-unavailable)
  (multiple-value-bind (time condition)
      (handler-case (multiple-value-bind (seconds nanoseconds)
                        (funcall read-time)
                      (values (+ seconds (/ nanoseconds 1000000000d0)) nil))
        (error (condition)
          (values nil condition)))
    (if condition
        (funcall on-unavailable condition)
        (funcall on-time time))))

(defmethod ntop.shared.domain:read-monotonic-time ((clock system-clock) on-time
                                                   on-unavailable)
  (declare (ignore clock))
  (%report-monotonic-time
   (lambda ()
     (sb-unix:clock-gettime sb-unix:clock-monotonic))
   on-time
   on-unavailable))
