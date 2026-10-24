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

(defun %temperature-test-sensors (result)
  "Return the sensors from a successfully collected temperature sample."
  (ntop.temperature.domain:temperature-sample-sensors (second result)))
