(define-ntop-package #:ntop.network.domain (:documentation "Network metrics and calculations.")
  (:export
   ;; WP0-4: network sample types.
   #:network-interface #:make-network-interface #:network-interface-name
   #:network-interface-rx-bytes #:network-interface-tx-bytes #:network-interface-rx-packets
   #:network-interface-tx-packets #:network-sample #:make-network-sample
   #:network-sample-interfaces
   ;; F4: feature domain behavior.
   #:network-total-bytes #:network-total-packets #:network-rate
   #:network-packet-rate-per-second #:network-average-packet-size
   #:network-scale-quantity #:network-axis-scaling
   ))

(define-ntop-package #:ntop.network.infrastructure (:documentation "Network OS access.")
  (:export
   ;; F4: feature data collection.
   #:network-collector #:make-network-collector #:read-network-interface-data
   ))

(define-ntop-package #:ntop.network.presentation (:documentation "Network views.")
  (:export
   ;; F4: feature presentation.
   #:network-presentation-state #:make-network-presentation-state
   #:network-presentation-state-baseline-sample
   ))
