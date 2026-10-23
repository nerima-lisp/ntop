(in-package #:ntop.network.infrastructure)

#+darwin
(defmethod read-network-interface-data ((collector network-collector)
                                        on-value
                                        on-unavailable)
  (declare (ignore collector))
  (ntop.platform.darwin:darwin-read-network-interface-data
   on-value
   on-unavailable))
