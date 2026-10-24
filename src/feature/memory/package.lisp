(define-ntop-package #:ntop.memory.domain (:documentation "Memory metrics and calculations.")
  (:export
   ;; WP0-4: memory sample types.
   #:memory-sample #:make-memory-sample #:memory-sample-ram-used-bytes
   #:memory-sample-ram-total-bytes #:memory-sample-swap-used-bytes
   #:memory-sample-swap-total-bytes #:memory-sample-cache-used-bytes
   #:memory-sample-cache-total-bytes #:memory-sample-arc-used-bytes
   #:memory-sample-arc-total-bytes
   ;; F2: feature domain behavior.
   #:memory-used-bytes-from-pages #:memory-cache-used-bytes))

(define-ntop-package #:ntop.memory.infrastructure (:documentation "Memory operating-system access.")
  (:export
   ;; F2: feature data collection.
   #:make-memory-collector
   ))

(define-ntop-package #:ntop.memory.presentation (:documentation "Memory views and gauges.")
  (:export
   ;; F2: feature presentation.
   ))
