(in-package #:ntop.gpu.infrastructure)

#+darwin
(defmethod read-gpu-device-data ((collector gpu-collector)
                                 on-value
                                 on-unavailable)
  (declare (ignore collector))
  (ntop.platform.darwin:darwin-read-gpu-device-data
   on-value
   on-unavailable))
