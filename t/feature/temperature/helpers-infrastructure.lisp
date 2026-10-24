(in-package #:ntop.test)

(defun %temperature-test-collect (collector)
  "Capture the sample or unavailable reason returned by a temperature collector."
  (let ((result nil))
    (ntop.shared.application:collect-temperature-sample collector
                                                        (lambda (sample)
                                                          (setf result (list :value sample)))
                                                        (lambda (reason)
                                                          (setf result (list :unavailable reason))))
    result))
(defclass temperature-fixture-clock ()
  ((time :initarg :time :accessor %temperature-fixture-clock-time)
   (unavailable-reason :initarg :unavailable-reason
                       :initform nil
                       :reader %temperature-fixture-clock-unavailable-reason)))

(defun %temperature-test-sensors (result)
  "Return the sensors from a successfully collected temperature sample."
  (ntop.temperature.domain:temperature-sample-sensors (second result)))
(defmethod ntop.shared.domain:read-monotonic-time
    ((clock temperature-fixture-clock) on-time on-unavailable)
  (let ((reason (%temperature-fixture-clock-unavailable-reason clock)))
    (if reason
        (funcall on-unavailable reason)
        (funcall on-time (%temperature-fixture-clock-time clock)))))
