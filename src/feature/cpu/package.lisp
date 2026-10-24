(define-ntop-package #:ntop.cpu.domain (:documentation "CPU metrics and calculations.")
  (:export
   ;; WP0-4: CPU sample types.
   #:cpu-core-value #:make-cpu-core-value #:cpu-core-value-core-index #:cpu-core-value-user-ticks
   #:cpu-core-value-nice-ticks #:cpu-core-value-system-ticks #:cpu-core-value-idle-ticks
   #:cpu-core-value-iowait-ticks #:cpu-core-value-irq-ticks #:cpu-core-value-softirq-ticks
   #:cpu-core-value-steal-ticks #:cpu-core-value-guest-ticks #:cpu-core-value-guest-nice-ticks
   #:cpu-core-value-usage-percent #:cpu-core-value-frequency-mhz #:cpu-sample #:make-cpu-sample
   #:cpu-sample-core-values #:cpu-sample-average-usage-percent #:cpu-sample-load-average-1
   #:cpu-sample-load-average-5 #:cpu-sample-load-average-15
   #:cpu-sample-cgroup-average-cpu-percent #:cpu-sample-cgroup-cpu-usage-ns
   #:cpu-sample-cgroup-cpu-quota-micros #:cpu-sample-cgroup-cpu-period-micros
   #:cpu-sample-cgroup-cpu-throttled-ns #:cpu-sample-cgroup-cpu-throttled-periods
   #:cpu-sample-cgroup-cpu-limit-cores
   ;; F1: feature domain behavior.
   #:cpu-core-usage-percent #:cpu-average-usage-percent
   #:cpu-cgroup-average-percent #:cpu-weighted-frequency-mhz))

(define-ntop-package #:ntop.cpu.infrastructure (:documentation "CPU operating-system access.")
  (:export
   ;; F1: feature data collection.
   #:make-cpu-collector))

(define-ntop-package #:ntop.cpu.presentation (:documentation "CPU graphs and gauges.")
  (:export
   ;; F1: feature presentation.
   ))
