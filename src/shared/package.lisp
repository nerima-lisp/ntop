(define-ntop-package #:ntop.shared.domain
  (:documentation "Measurements and time-series history.")
  (:export
   ;; WP0-3: measurement values, clocks, rates and units.
   #:measurement-value #:measurement-counter #:optional-counter #:monotonic-seconds
   #:measurement-available-p #:sample-record #:make-sample-record #:sample-record-sampled-at
   #:define-value-type #:define-sample-type #:counter-rate-per-second
   #:read-monotonic-time #:read-unix-time
   #:quantity #:make-quantity #:quantity-value #:quantity-unit #:unit-definition
   #:convert-quantity #:scaled-quantity #:make-scaled-quantity #:scaled-quantity-value
   #:scaled-quantity-unit #:scaled-quantity-prefix #:scaled-quantity-symbol #:scale-quantity
   ;; WP2: retained time-series history.
   #:history-sample #:history-sample-sampled-at #:history-sample-value #:history-store
   #:make-history-store #:history-store-series-count #:history-store-capacity
   #:history-store-retention-seconds #:history-store-sample-interval-seconds
   #:history-store-zoom-step-seconds #:history-store-window-width-seconds #:history-paused-p
   #:history-append #:history-prune #:history-pause #:history-resume #:history-scroll-older
   #:history-scroll-newer #:history-go-current #:history-current-window #:history-set-window-width
   #:history-zoom-in #:history-zoom-out #:history-reset-zoom #:history-slice))

(define-ntop-package #:ntop.shared.application (:documentation "Configuration and collector ports.")
  (:export
   ;; WP0-4: configuration values and collector ports.
   #:define-config #:application-config #:make-application-config #:application-config-rate-ms
   #:application-config-retention-seconds #:application-config-time-window-seconds
   #:application-config-zoom-window-seconds #:application-config-hide-time-p
   #:application-config-auto-hide-time-p #:application-config-theme
   #:application-config-default-widget-type #:application-config-default-widget-count
   #:application-config-temperature-unit #:application-config-memory-legend-position
   #:application-config-enable-cache-memory-p #:application-config-short-gpu-names-p
   #:application-config-data-unit #:application-config-binary-prefix-p
   #:application-config-network-legend-position #:application-config-network-use-log-p
   #:application-config-network-use-old-legend-p #:application-config-network-show-packets-p
   #:application-config-network-start-zeroed-p #:application-config-network-interface-filter
   #:application-config-default-cpu-entry #:application-config-hide-average-cpu-p
   #:application-config-cpu-left-legend-p #:application-config-unnormalized-cpu-p
   #:application-config-current-usage-p #:application-config-process-regex-p
   #:application-config-process-case-sensitive-p #:application-config-process-whole-word-p
   #:application-config-process-get-threads-p #:application-config-process-hide-kernel-threads-p
   #:application-config-process-tree-collapse-p #:application-config-process-show-command-p
   #:application-config-process-default-memory-value-p
   #:application-config-process-default-grouped-p #:application-config-process-default-tree-p
   #:application-config-process-columns #:application-config-process-sort-column
   #:application-config-process-sort-order #:application-config-disable-advanced-kill-p
   #:application-config-battery-p #:application-config-disable-click-p
   #:application-config-disable-keys-p #:application-config-read-only-p
   #:application-config-basic-p #:application-config-expanded-p
   #:application-config-show-table-scroll-position-p #:application-config-disable-gpu-p
   #:application-config-dot-marker-p #:application-config-widget-rows #:define-collector
   #:collector-unavailability-reason
   #:normalize-collector-unavailability-reason
   #:collect-cpu-sample #:collect-memory-sample #:collect-gpu-sample #:collect-network-sample
   #:collect-disk-sample #:collect-temperature-sample #:collect-battery-sample
   #:collect-process-sample
   ))
