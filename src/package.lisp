;; Keep the org-required CL-only use list uniform across project packages.
(defmacro define-ntop-package (name &body clauses)
  `(defpackage ,name (:use #:cl) ,@clauses))

(define-ntop-package #:ntop.shared.domain (:documentation "Measurements and time-series history.")
  (:export
   ;; WP0-3: measurement values, clocks, rates and units.
   #:measurement-value #:measurement-counter #:optional-counter #:monotonic-seconds
   #:measurement-available-p #:sample-record #:make-sample-record #:sample-record-sampled-at
   #:define-value-type #:define-sample-type #:counter-rate-per-second #:read-monotonic-time
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
   #:history-zoom-in #:history-zoom-out #:history-reset-zoom #:history-slice
   ))

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
   #:collect-cpu-sample #:collect-memory-sample #:collect-gpu-sample #:collect-network-sample
   #:collect-disk-sample #:collect-temperature-sample #:collect-battery-sample
   #:collect-process-sample
   ))

(define-ntop-package #:ntop.platform.darwin
  (:documentation "Darwin system calls and bindings.")
  (:export
   ;; WP0-7: FFI declarations, sysctl, mach, CoreFoundation and IOKit.
   #:define-darwin-ffi #:decode-little-endian-unsigned #:decode-little-endian-signed
   #:decode-c-string #:decode-mach-timebase #:mach-ticks-to-nanoseconds #:sysctl-read-by-name
   #:sysctl-read-mib #:sysctl-read-integer-by-name #:sysctl-read-string-by-name
   #:call-with-mach-host-port #:mach-timebase-info #:mach-vm-deallocate
   #:mach-read-load-average #:mach-read-host-vm-statistics64
   #:darwin-error-string #:cf-null-reference-p #:call-with-owned-cf-reference
   #:with-owned-cf-reference #:cf-value-to-lisp #:cf-dictionary-get-value
   #:call-with-iokit-services #:iokit-registry-properties
   ;; F1, infrastructure-ioreport.lisp: IOReport bindings.
   ;; F3, infrastructure-ioaccel.lisp: IOAccelerate bindings.
   #:darwin-read-gpu-device-data
   ;; F4, infrastructure-ifmib.lisp: interface MIB bindings.
   #:decode-ifmibdata #:darwin-read-network-interface-data
   ;; F5, infrastructure-fs.lisp: filesystem and block-storage bindings.
   #:decode-darwin-statfs-buffer #:decode-darwin-volume-space-used
   #:read-darwin-file-system-stats #:read-darwin-volume-space-used
   #:read-darwin-block-storage-stats
   ;; F6, infrastructure-iohid.lisp: IOHID bindings.
   #:load-darwin-iohid-api
   ;; F7, infrastructure-power.lisp: power bindings.
   ;; F8a, infrastructure-libproc.lisp: libproc bindings.
   #:read-darwin-process-rows))

(define-ntop-package #:ntop.platform.linux
  (:documentation "Linux procfs and sysfs access.")
  (:export
   ;; WP9: Linux platform bindings.
   #:*linux-root* #:with-linux-root #:read-linux-proc-stat #:read-linux-proc-loadavg
   #:read-linux-proc-meminfo #:read-linux-cgroup-data #:read-linux-process-stat
   #:read-linux-process-status #:read-linux-process-io #:read-linux-process-cmdline
   #:read-linux-process-tasks #:read-proc-mounts #:read-proc-diskstats #:read-linux-statvfs
   #:read-linux-network-statistics #:read-linux-cpu-frequency-khz
   #:enumerate-linux-temperature-sources #:read-linux-temperature-values
   #:read-linux-batteries #:read-linux-amd-gpus))

(define-ntop-package #:ntop.ui (:documentation "Shared rendering and input contracts.")
  (:export
 ;; WP0-4: widget and input protocols.
 #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y #:rectangle-width #:rectangle-height
 #:clickable-area #:make-clickable-area #:clickable-area-rectangle #:clickable-area-action
 #:clickable-area-value #:draw-widget #:define-widget #:translate-input
 ;; WP1: formatting and themes.
 #:format-cpu-basic-percent #:format-cpu-title #:format-process-cpu-percent
 #:format-process-memory-percent #:format-memory-percentage #:format-memory-fraction
 #:format-memory-legend #:format-process-binary-bytes #:format-process-decimal-bytes
 #:format-disk-capacity #:format-disk-io-rate #:format-process-io-rate #:format-network-rate
 #:format-network-total #:format-network-legend #:format-disk-io-legend-rate
 #:format-temperature #:format-temperature-graph-value #:format-temperature-axis-labels
 #:format-network-axis-labels #:format-network-log-axis-labels
 #:format-disk-io-axis-labels #:format-disk-io-log-axis-labels
 #:format-process-time #:format-time-axis-labels
 #:presentation-theme #:presentation-theme-style
 ;; WP5: layout.
 #:layout-widgets #:define-layout
 ;; WP6: charts and gauges.
 #:chart-series #:make-chart-series #:chart-series-name #:chart-series-style
 #:chart-series-samples #:chart-config #:make-chart-config #:define-chart-config
 #:chart-config-x-start #:chart-config-x-end #:chart-config-y-min #:chart-config-y-max
 #:chart-config-y-scale #:chart-config-y-labels #:chart-config-x-labels
 #:chart-config-theme-name #:chart-config-legend-position
 #:chart-config-legend-width-ratio #:chart-config-legend-height-ratio
 #:chart-config-dot-marker-p #:*default-chart-config* #:chart-legend-layout
 #:draw-time-chart #:gauge-value #:make-gauge-value #:gauge-value-start-label
 #:gauge-value-inner-label #:gauge-value-ratio #:gauge-value-label-style
 #:gauge-value-gauge-style #:draw-pipe-gauge #:draw-basic-cpu-gauges
 #:draw-basic-memory-gauge
 ;; WP7: tables.
 #:define-table-columns #:table-column #:make-table-column #:table-column-id
 #:table-column-header #:table-column-width-kind #:table-column-width #:table-column-max-percent
 #:table-column-hidden-p #:table-column-sortable-p #:table-column-value-function
 #:table-column-layout #:table-column-layout-id #:table-column-layout-x
 #:table-column-layout-width #:measure-table-text #:truncate-table-text #:compute-table-layout
 #:cpu-legend-region
 #:cpu-legend-label #:draw-table-text #:table-config #:make-table-config #:table-config-columns
 #:table-config-title #:table-config-show-scroll-position-p #:table-config-right-to-left-p
 #:table-config-sort-column #:table-config-sort-order #:table-config-selectable-p
 #:table-config-focused-p #:table-config-show-current-entry-when-unfocused-p #:table-state
 #:make-table-state #:table-state-selected-index #:table-state-scroll-offset
 #:format-table-title #:table-visible-window #:move-table-selection #:draw-table
 ;; WP10: input events.
 #:input-meaning #:make-input-meaning #:input-meaning-p
 #:input-meaning-kind #:input-meaning-key #:input-meaning-text
 #:input-meaning-gesture #:input-meaning-x #:input-meaning-y
 #:input-meaning-modifiers))

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

(define-ntop-package #:ntop.process.domain
    (:documentation "Process records and search language.")
  (:export
   ;; WP0-4: process types and search contract.
   #:process-thread #:make-process-thread #:process-thread-thread-id #:process-thread-name
   #:process-thread-cpu-time-ns #:process-thread-state #:process-thread-priority
   #:process-thread-nice #:process-record #:make-process-record #:process-record-pid
   #:process-record-ppid #:process-record-name #:process-record-command
   #:process-record-cpu-time-ns #:process-record-resident-memory-bytes
   #:process-record-virtual-memory-bytes #:process-record-read-bytes
   #:process-record-write-bytes #:process-record-uid #:process-record-user-name
   #:process-record-state #:process-record-start-time-unix-seconds
   #:process-record-priority #:process-record-nice #:process-record-kernel-thread-p
   #:process-record-threads #:process-record-gpu-usage-percent
   #:process-record-gpu-memory-bytes #:process-record-ps-fallback-cpu-percent
   #:process-signal #:make-process-signal #:process-signal-operating-system
   #:process-signal-name #:process-signal-number #:process-sample
   #:make-process-sample #:process-sample-processes #:process-sample-signals
   #:process-row #:make-process-row #:process-row-p
   #:process-row-count #:process-row-pid #:process-row-ppid #:process-row-name #:process-row-command
   #:process-row-cpu-percent #:process-row-memory-percent
   #:process-row-memory-bytes #:process-row-virtual-memory-bytes
   #:process-row-read-bytes-per-second #:process-row-write-bytes-per-second
   #:process-row-total-read-bytes #:process-row-total-write-bytes
   #:process-row-user-name #:process-row-state #:process-row-elapsed-seconds
   #:process-row-priority #:process-row-nice #:process-row-gpu-usage-percent
   #:process-row-gpu-memory-bytes #:process-row-gpu-memory-percent
   ;; WP3: process search language.
   #:process-search-syntax-error #:make-process-search-syntax-error
   #:process-search-syntax-error-start-offset
   #:process-search-syntax-error-end-offset
   #:process-search-syntax-error-message #:compile-process-search
   ;; WP4: process domain behavior.
   #:process-read-bytes-per-second #:process-write-bytes-per-second
   #:process-cpu-percent #:process-row-from-samples
   #:group-process-domain-rows
   #:process-sort-state #:make-process-sort-state
   #:process-sort-state-key #:process-sort-state-order
   #:process-sort-state-memory-mode #:process-sort-key-from-character
   #:process-sort-select-key #:process-sort-invert #:sort-process-domain-rows
   #:process-tree-state #:make-process-tree-state
   #:process-tree-state-collapsed-p #:process-tree-state-set-collapsed
   #:process-tree-state-toggle #:build-process-tree
   #:process-tree-entry #:process-tree-entry-p #:process-tree-entry-row
   #:process-tree-entry-depth #:process-tree-entry-last-child-p
   #:process-tree-entry-has-children-p #:process-tree-entry-collapsed-p
   #:process-tree-visible-rows))

(define-ntop-package #:ntop.process.application
    (:documentation "Process search and termination.")
  (:export
   ;; WP0-4: agreed use-case contracts.
   ;; WP4: process use cases.
   #:process-search-state #:make-process-search-state #:process-search-state-p
   #:process-search-state-query #:process-search-state-predicate
   #:process-search-state-syntax-error #:compile-process-search-state
   #:find-process-index-by-pid #:find-process-group-index-by-key
   #:process-termination-state #:process-termination-state-p
   #:make-process-termination-state
   #:process-termination-state-selected-signal
   #:process-termination-state-target-pids
   #:process-termination-state-send-status
   #:update-process-termination-targets #:select-process-signal
   #:send-process-termination))

(define-ntop-package #:ntop.process.infrastructure
    (:documentation "Process data and signals.")
  (:export
   ;; F8a: process data collection.
   #:process-collector #:make-process-collector #:send-process-signal))

(define-ntop-package #:ntop.process.presentation (:documentation "Process tables and dialogs.")
  (:export
   ;; F8b: process presentation.
   #:make-process-table-config
   #:process-table-config-sort-state
   #:process-table-config-grouped-p
   #:process-table-config-grouped-by-tab-p
   #:process-table-config-show-command-p
   #:process-table-config-memory-mode
   #:process-table-config-gpu-available-p
   #:process-table-config-focused-p
   #:process-table-config-show-current-entry-when-unfocused-p
   #:process-table-config-show-scroll-position-p
   #:process-table-config-column-ids
   #:process-table-visible-columns #:process-table-sort-options
   #:make-process-search-field-state
   #:process-search-field-state-application-state
   #:process-search-field-state-regex-p
   #:process-search-field-state-case-sensitive-p
   #:process-search-field-state-whole-word-p
   #:transition-process-search-field
   #:make-process-sort-menu-state #:move-process-sort-menu-selection
   #:select-process-sort-menu-choice #:apply-process-sort-menu-choice
   #:open-process-termination-dialog
   #:move-process-termination-dialog-selection
   #:select-process-termination-dialog-choice
   #:confirm-process-termination-dialog))

(define-ntop-package #:ntop.app.domain (:documentation "Application state and parity data.")
  (:export
 ;; WP0-4: application state and actions.
 #:app-state #:make-app-state #:app-state-selected-widget #:app-state-expanded-p
 #:app-state-basic-mode-p #:app-state-paused-p #:app-state-zoom-seconds
 #:app-state-scroll-offset #:app-state-active-dialog #:app-state-widget-v-states
 #:input-context #:make-input-context #:input-context-selected-widget
 #:input-context-active-dialog #:input-context-basic-mode-p
 #:input-context-search-mode-p #:app-action #:make-app-action #:app-action-kind
 #:app-action-value #:*app-action-kinds*
 #:collection-snapshot #:collection-snapshot-p #:make-collection-snapshot
 #:collection-snapshot-sampled-at #:collection-snapshot-cpu-sample
 #:collection-snapshot-memory-sample #:collection-snapshot-gpu-sample
 #:collection-snapshot-network-sample #:collection-snapshot-disk-sample
 #:collection-snapshot-temperature-sample #:collection-snapshot-battery-sample
 #:collection-snapshot-process-sample
 ;; WP0-5: typed btm parity data.
 #:btm-parity-catalog #:parity-entry-kind
 #:parity-key #:parity-key-action #:parity-key-keys
 #:parity-key-contexts #:parity-key-behavior #:parity-key-source
 #:parity-mouse #:parity-mouse-action #:parity-mouse-gesture
 #:parity-mouse-contexts #:parity-mouse-behavior #:parity-mouse-condition
 #:parity-mouse-source
 #:parity-flag #:parity-flag-snake-case #:parity-flag-short
 #:parity-flag-aliases #:parity-flag-value-type #:parity-flag-values
 #:parity-flag-condition #:parity-flag-source #:parity-flag-kebab-case
 #:parity-column #:parity-column-header
 #:parity-column-field #:parity-column-aliases #:parity-column-condition
 #:parity-column-default #:parity-column-slot #:parity-column-source
 #:parity-search-field #:parity-search-field-field
 #:parity-search-field-tokens #:parity-search-field-condition
 #:parity-search-field-implicit #:parity-search-field-source
 #:parity-extension #:parity-extension-action
 #:parity-extension-key #:parity-extension-contexts #:parity-extension-behavior
 #:parity-extension-relation #:parity-extension-source))

(define-ntop-package #:ntop.app.application
    (:documentation "Application event handling.")
  (:export
   ;; WP10: key bindings.
   #:application-keymap-state #:application-keymap-state-p
   #:make-application-keymap-state #:dispatch-application-input
   ;; WP12a: event loop and sampler.
   #:collect-application-snapshot #:collect-application-result
   #:application-collection-result #:application-collection-result-p
   #:application-collection-result-snapshot
   #:application-collection-result-unavailability-reasons
   #:application-unavailability-reasons #:application-unavailability-reasons-p
   #:application-unavailability-reason
   #:run-fixed-rate-sampler
   #:apply-application-action #:run-application-loop
   ;; WP12b: application composition.
   ))

(define-ntop-package #:ntop.app.infrastructure (:documentation "Terminal and sampler lifecycle.")
  (:export
   ;; WP12a: terminal and collection infrastructure.
   #:system-clock #:make-system-clock
   #:start-collection-worker #:collection-worker-channel
   #:stop-collection-worker #:call-with-terminal-session
   #:run-application
   ))

(define-ntop-package #:ntop.app.presentation (:documentation "Screen layout and widgets.")
  (:export
   ;; WP12b: whole-screen presentation.
   ))

(define-ntop-package #:ntop.cli (:documentation "Command-line options and entry point.")
  (:export
   ;; WP13: command-line interface.
   ))

(define-ntop-package #:ntop.test (:documentation "Project test definitions.")
  (:export
   ;; WP0-1: foundation smoke tests.
   ))
