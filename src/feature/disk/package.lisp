(define-ntop-package #:ntop.disk.domain (:documentation "Disk metrics and calculations.")
  (:export
   ;; WP0-4: disk sample types.
   #:disk-mount #:make-disk-mount #:disk-mount-device-name #:disk-mount-mount-path
   #:disk-mount-filesystem-type #:disk-mount-used-bytes #:disk-mount-free-bytes
   #:disk-mount-total-bytes #:disk-mount-io-key #:disk-device #:make-disk-device
   #:disk-device-device-name #:disk-device-read-bytes #:disk-device-write-bytes #:disk-sample
   #:make-disk-sample #:disk-sample-mounts #:disk-sample-devices
   ;; F5: feature domain behavior.
   #:disk-filesystem-type-supported-p #:disk-io-key-from-device-name
   #:disk-used-percent #:disk-free-percent #:disk-sample-rows #:sort-disk-rows
   ))

(define-ntop-package #:ntop.disk.infrastructure (:documentation "Disk operating-system access.")
  (:export
   ;; F5: feature data collection.
   #:disk-collector #:make-disk-collector
   ))

(define-ntop-package #:ntop.disk.presentation (:documentation "Disk tables and graphs.")
  (:export
   ;; F5: feature presentation.
   #:*disk-table-columns* #:*disk-table-columns-with-free-percent*
   #:make-disk-table-config
   ))
