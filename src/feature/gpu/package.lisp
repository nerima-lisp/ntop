(define-ntop-package #:ntop.gpu.domain (:documentation "GPU metrics and calculations.")
  (:export
   ;; WP0-4: GPU sample types.
   #:gpu-device #:make-gpu-device #:gpu-device-name #:gpu-device-usage-percent
   #:gpu-device-memory-used-bytes #:gpu-device-memory-total-bytes #:gpu-device-temperature-celsius
   #:gpu-sample #:make-gpu-sample #:gpu-sample-devices
   ;; F3: feature domain behavior.
   #:gpu-display-name #:gpu-memory-percent
   ))

(define-ntop-package #:ntop.gpu.infrastructure (:documentation "GPU operating-system access.")
  (:export
   ;; F3: feature data collection.
   #:gpu-collector #:make-gpu-collector #:read-gpu-device-data
   ))

(define-ntop-package #:ntop.gpu.presentation (:documentation "GPU views.")
  (:export
   ;; F3: feature presentation.
   ))
