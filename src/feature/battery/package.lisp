(define-ntop-package #:ntop.battery.domain (:documentation "Battery metrics and calculations.")
  (:export
   ;; WP0-4: battery sample types.
   #:battery-device #:make-battery-device #:battery-device-name #:battery-device-charge-percent
   #:battery-device-power-watts #:battery-device-state #:battery-device-seconds-to-full
   #:battery-device-seconds-to-empty #:battery-device-health-percent #:battery-sample
   #:make-battery-sample #:battery-sample-devices
   ;; F7: feature domain behavior.
   #:battery-charge-percent #:battery-health-percent #:battery-power-watts
   #:battery-state-from-signals #:battery-time-remaining-seconds
   #:battery-estimated-time-seconds
   ))

(define-ntop-package #:ntop.battery.infrastructure (:documentation "Battery OS access.")
  (:export
   ;; F7: feature data collection.
   #:make-battery-collector
   ))

(define-ntop-package #:ntop.battery.presentation (:documentation "Battery views.")
  (:export
   ;; F7: feature presentation.
   ))
