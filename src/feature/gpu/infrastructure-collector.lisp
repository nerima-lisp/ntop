(in-package #:ntop.gpu.infrastructure)

(defclass gpu-collector ()
  ((clock :initarg :clock :reader %gpu-collector-clock)
   (config :initarg :config :reader %gpu-collector-config)))

(defun make-gpu-collector (clock config)
  (make-instance 'gpu-collector :clock clock :config config))

(defgeneric read-gpu-device-data (collector on-value on-unavailable))

(defun %gpu-devices-from-rows (rows short-names-p)
  (loop for row in rows
        for index from 0
        for name = (ntop.gpu.domain:gpu-display-name (getf row :name)
                                                     index
                                                     (length rows)
                                                     short-names-p)
        collect (apply #'ntop.gpu.domain:make-gpu-device
                       (append (list :name name)
                               (loop for (key value) on row by #'cddr
                                     unless (eq key :name)
                                       append (list key value))))))

(defun %collect-gpu-sample-from-rows (collector rows on-sample on-unavailable)
  (let* ((config (%gpu-collector-config collector))
         (devices
          (%gpu-devices-from-rows rows
                                  (ntop.shared.application:application-config-short-gpu-names-p
                                   config))))
    (ntop.shared.domain:read-monotonic-time (%gpu-collector-clock collector)
                                            (lambda (sampled-at)
                                              (funcall on-sample
                                                       (ntop.gpu.domain:make-gpu-sample
                                                        :sampled-at
                                                        sampled-at
                                                        :devices
                                                        devices)))
                                            (lambda (reason)
                                              (declare (ignore reason))
                                              (funcall on-unavailable
                                                       :read-failed)))))

(defmethod ntop.shared.application:collect-gpu-sample ((collector gpu-collector)
                                                       on-sample
                                                       on-unavailable)
  (if (ntop.shared.application:application-config-disable-gpu-p
       (%gpu-collector-config collector))
      (funcall on-unavailable :disabled)
      (read-gpu-device-data collector
                            (lambda (rows)
                              (%collect-gpu-sample-from-rows collector
                                                             rows
                                                             on-sample
                                                             on-unavailable))
                            on-unavailable)))
