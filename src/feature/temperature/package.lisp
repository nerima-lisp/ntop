(define-ntop-package #:ntop.temperature.domain (:documentation "Temperature values and units.")
  (:export
   ;; WP0-4: temperature sample types.
   #:temperature-sensor #:make-temperature-sensor #:temperature-sensor-name
   #:temperature-sensor-temperature-celsius #:temperature-sample #:make-temperature-sample
   #:temperature-sample-sensors
   ;; F6: feature domain behavior.
   #:temperature-sensor-labels
   ))

(define-ntop-package #:ntop.temperature.infrastructure
    (:documentation "Temperature OS access.")
  (:export
   ;; F6: feature data collection.
   #:temperature-source #:make-temperature-source
   #:temperature-source-identity #:temperature-source-name #:temperature-source-handle
   #:temperature-collector #:make-temperature-collector
   #:make-darwin-temperature-collector #:make-linux-temperature-collector
   ))

(define-ntop-package #:ntop.temperature.presentation
    (:documentation "Temperature views.")
  (:export
   ;; F6: feature presentation.
   #:*temperature-table-columns* #:make-temperature-table-config
   ))
