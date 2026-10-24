(define-ntop-package #:ntop.ui (:documentation "Shared rendering and input contracts.")
  (:export
 ;; WP0-4: widget and input protocols.
 #:rectangle #:make-rectangle #:rectangle-x #:rectangle-y #:rectangle-width #:rectangle-height
 #:clickable-area #:make-clickable-area #:clickable-area-rectangle #:clickable-area-action
 #:clickable-area-value #:draw-widget #:define-widget #:translate-input
 ;; WP1: formatting and themes.
 #:format-cpu-basic-percent #:format-cpu-title #:format-percent-value #:format-fixed
 #:format-memory-percentage #:format-memory-fraction
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
 #:chart-series-samples
 #:chart-series-maximum-value #:chart-config #:make-chart-config #:define-chart-config
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
