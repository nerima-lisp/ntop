(in-package #:ntop.battery.infrastructure)

(defclass battery-collector ()
  ((clock :initarg :clock :reader %battery-collector-clock)))

(defun make-battery-collector (clock)
  (make-instance 'battery-collector :clock clock))

(defgeneric read-battery-devices (collector on-value on-unavailable))

(defun %collect-battery-sample-from-devices
    (collector devices on-sample on-unavailable)
  (if (null devices)
      (funcall on-unavailable :not-present)
      (ntop.shared.domain:read-monotonic-time
       (%battery-collector-clock collector)
       (lambda (sampled-at)
         (funcall on-sample
                  (ntop.battery.domain:make-battery-sample
                   :sampled-at sampled-at
                   :devices devices)))
       on-unavailable)))

(defmethod ntop.shared.application:collect-battery-sample
    ((collector battery-collector) on-sample on-unavailable)
  (labels ((normalize-unavailable-reason (arguments)
             (if (= (length arguments) 1)
                 (case (first arguments)
                   (:no-battery :not-present)
                   (:invalid-data :read-failed)
                   (otherwise
                    (if (typep
                         (first arguments)
                         'ntop.shared.application:collector-unavailability-reason)
                        (first arguments)
                        :read-failed)))
                 :read-failed))
           (report-unavailable (&rest arguments)
             (funcall on-unavailable
                      (normalize-unavailable-reason arguments))))
    (read-battery-devices
     collector
     (lambda (devices)
       (%collect-battery-sample-from-devices
        collector devices on-sample #'report-unavailable))
     #'report-unavailable)))

#-(or darwin linux)
(defmethod read-battery-devices
    ((collector battery-collector) on-value on-unavailable)
  (declare (ignore collector on-value))
  (funcall on-unavailable :unsupported-platform))
