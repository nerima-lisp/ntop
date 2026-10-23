(in-package #:ntop.disk.infrastructure)

(defclass disk-collector ()
  ((clock :initarg :clock :reader %disk-collector-clock)))

(defun make-disk-collector (clock)
  (make-instance 'disk-collector :clock clock))

(defgeneric read-disk-values (collector on-value on-unavailable))

(defun %disk-unavailable-reason (reason)
  (if (member reason
              '(:not-present :disabled
                             :permission-denied
                             :unsupported-platform
                             :private-api-unavailable
                             :read-failed)
              :test
              #'eq)
      reason
      :read-failed))

#-(or darwin linux)
(defmethod read-disk-values ((collector disk-collector) on-value on-unavailable)
  (declare (ignore collector on-value))
  (funcall on-unavailable :unsupported-platform))

(defun %collect-disk-sample-from-values (collector values
                                                   on-sample
                                                   on-unavailable)
  (ntop.shared.domain:read-monotonic-time (%disk-collector-clock collector)
                                          (lambda (sampled-at)
                                            (funcall on-sample
                                                     (ntop.disk.domain:make-disk-sample
                                                      :sampled-at
                                                      sampled-at
                                                      :mounts
                                                      (getf values :mounts)
                                                      :devices
                                                      (getf values :devices))))
                                          on-unavailable))

(defmethod ntop.shared.application:collect-disk-sample ((collector
                                                         disk-collector)
                                                        on-sample
                                                        on-unavailable)
  (flet ((report-unavailable (reason)
           (funcall on-unavailable (%disk-unavailable-reason reason))))
    (read-disk-values collector
                      (lambda (values)
                        (%collect-disk-sample-from-values collector
                                                          values
                                                          on-sample
                                                          #'report-unavailable))
                      #'report-unavailable)))
