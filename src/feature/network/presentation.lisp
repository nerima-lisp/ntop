(in-package #:ntop.network.presentation)

(defstruct
    (network-presentation-state (:constructor %make-network-presentation-state (baseline-sample)))
  "Holds the first network sample used by the zero-based total display."
  (baseline-sample nil :type (or null ntop.network.domain:network-sample)))

(defun make-network-presentation-state (&key baseline-sample)
  (when (and baseline-sample (not (typep baseline-sample 'ntop.network.domain:network-sample)))
    (error "BASELINE-SAMPLE must be a network sample or NIL."))
  (%make-network-presentation-state baseline-sample))

(defparameter +network-log-axis-levels+
  '((:decimal 6d0 9d0 12d0 15d0) (:binary 20d0 30d0 40d0 50d0)))

(defparameter +network-log-axis-prefixes+
  '((:decimal "K" "M" "G" "T") (:binary "Ki" "Mi" "Gi" "Ti" "Pi")))

(ntop.ui:define-widget :network
                       (target rectangle state config series)
                       (check-type config ntop.shared.application:application-config)
                       (let* ((samples (%network-samples series))
                              (presentation-state (%network-state state))
                              (baseline
                               (%network-baseline presentation-state
                                                  samples
                                                  (%network-start-zeroed-p config)))
                              (rate-rows (%network-rate-rows samples config))
                              (latest (car (last samples)))
                              (previous (and (cdr samples) (car (last samples 2))))
                              (maximum (%network-maximum-rate rate-rows))
                              (chart-series
                               (%network-chart-series rate-rows
                                                      latest
                                                      previous
                                                      baseline
                                                      rectangle
                                                      config))
                              (theme (ntop.shared.application:application-config-theme config)))
                         (if (ntop.shared.application:application-config-basic-p config)
                             (let ((basic-config
                                    (ntop.shared.application::copy-application-config config)))
                               (setf (ntop.shared.application:application-config-data-unit
                                      basic-config)
                                     :byte)
                               (%draw-network-basic target rectangle latest previous baseline
                                                    basic-config))
                           (let* ((chart-area (%network-content-rectangle rectangle))
                                  (chart-rectangle (%network-chart-rectangle chart-area config))
                                  (chart-config
                                   (%network-chart-config rectangle latest maximum config)))
                             (%draw-network-title target rectangle " Network " theme)
                             (ntop.ui:draw-time-chart target
                                                      chart-rectangle
                                                      chart-config
                                                      chart-series)
                             (when (%network-old-legend-p config)
                               (%draw-network-old-legend target
                                                         (%network-old-legend-rectangle
                                                          chart-area)
                                                         latest
                                                         previous
                                                         baseline
                                                         config))))
                         nil))

(defun %network-state (state)
  (cond
    ((null state) (make-network-presentation-state))
    ((typep state 'network-presentation-state) state)
    (t (error "STATE must be a network-presentation-state or NIL."))))

(defun %network-samples (series)
  (let ((samples (coerce series 'list)))
    (loop with previous-time = nil
          for sample in samples
          for sampled-at = (progn
                             (unless (typep sample 'ntop.network.domain:network-sample)
                               (error "NETWORK series must contain network samples."))
                             (ntop.shared.domain:sample-record-sampled-at sample))
          do (when (and previous-time (< sampled-at previous-time))
               (error "NETWORK samples must be chronological.")) (setf previous-time sampled-at))
    samples))

(defun %network-baseline (state samples start-zeroed-p)
  (when (and start-zeroed-p (null (network-presentation-state-baseline-sample state)))
    (setf (network-presentation-state-baseline-sample state) (first samples)))
  (network-presentation-state-baseline-sample state))

(defun %network-rate-rows (samples config)
  (loop with previous = nil
        with unit = (ntop.shared.application:application-config-data-unit config)
        for sample in samples
        collect (list sample
                      (and previous
                           (ntop.network.domain:network-rate previous sample :rx :unit unit))
                      (and previous
                           (ntop.network.domain:network-rate previous sample :tx :unit unit))
                      previous)
        do (setf previous sample)))

(defun %network-maximum-rate (rate-rows)
  (loop for rate in (loop for row in rate-rows
                          append (list (second row) (third row)))
        when rate
          maximize (ntop.shared.domain:quantity-value rate) into maximum
        finally (return (or maximum 0d0))))

(defun %network-chart-series (rate-rows latest previous baseline rectangle config)
  (let* ((first-time
          (and rate-rows (ntop.shared.domain:sample-record-sampled-at (first (first rate-rows)))))
         (last-time
          (and rate-rows
               (ntop.shared.domain:sample-record-sampled-at (first (car (last rate-rows))))))
         (history-series
          (if rate-rows
              (let ((history-store (%network-rate-history-store rate-rows config)))
                (loop for index below 2
                      collect (ntop.shared.domain:history-slice history-store
                                                                index
                                                                :start
                                                                first-time
                                                                :end
                                                                last-time)))
              (list #() #())))
         (packets-visible
          (%network-packet-legend-visible-p
           (ntop.shared.application:application-config-network-show-packets-p config)
           (ntop.ui:rectangle-width rectangle)))
         (rx-total (%network-total-bytes latest :rx baseline config))
         (tx-total (%network-total-bytes latest :tx baseline config))
         (rx-rate (second (car (last rate-rows))))
         (tx-rate (third (car (last rate-rows))))
         (rx-name
          (%network-legend-name "RX" rx-rate rx-total previous latest :rx packets-visible config))
         (tx-name
          (%network-legend-name "TX" tx-rate tx-total previous latest :tx packets-visible config))
         (theme (ntop.shared.application:application-config-theme config)))
    (list
     (ntop.ui:make-chart-series :name
                                rx-name
                                :style
                                (ntop.ui:presentation-theme-style theme :rx)
                                :samples
                                (first history-series))
     (ntop.ui:make-chart-series :name
                                tx-name
                                :style
                                (ntop.ui:presentation-theme-style theme :tx)
                                :samples
                                (second history-series)))))

(defun %network-rate-history-store (rate-rows config)
  (let* ((first-time (ntop.shared.domain:sample-record-sampled-at (first (first rate-rows))))
         (last-time (ntop.shared.domain:sample-record-sampled-at (first (car (last rate-rows)))))
         (deltas
          (loop for left in rate-rows
                for right in (cdr rate-rows)
                collect (- (ntop.shared.domain:sample-record-sampled-at (first right))
                           (ntop.shared.domain:sample-record-sampled-at (first left)))))
         (sample-interval
          (if deltas
              (apply (function min) deltas)
              (max 0.001d0 (/ (ntop.shared.application:application-config-rate-ms config) 1000d0))))
         (retention (max 1d0 (+ (- last-time first-time) sample-interval)))
         (store
          (ntop.shared.domain:make-history-store 2
                                                 :retention-seconds retention
                                                 :sample-interval-seconds sample-interval
                                                 :initial-window-seconds retention
                                                 :minimum-window-seconds 0.1d0)))
    (dolist (row rate-rows)
      (let ((sample (first row)))
        (loop for index below 2
              for rate in (list (second row) (third row))
              for value = (and rate
                               (ntop.shared.domain:quantity-value rate))
              do (ntop.shared.domain:history-append store index sample value))))
    store))

(defun %network-legend-name (direction rate
                                       total
                                       previous
                                       latest
                                       direction-key
                                       packets-visible
                                       config)
  (let* ((rate-value (and rate (ntop.shared.domain:quantity-value rate)))
         (total-bits (and total (* total 8)))
         (prefix-system (%network-prefix-system config))
         (unit (ntop.shared.application:application-config-data-unit config))
         (main
          (if (and rate-value total-bits)
              (ntop.ui:format-network-legend direction rate-value total-bits prefix-system unit)
              (%network-partial-legend direction rate-value total-bits prefix-system unit)))
         (packet-rate
          (and previous
               latest
               (ntop.network.domain:network-packet-rate-per-second previous latest direction-key)))
         (average
          (and previous
               latest
               (ntop.network.domain:network-average-packet-size previous latest direction-key))))
    (if packets-visible
        (format nil
                "~A Packets: ~Apkt/s Avg: ~A"
                main
                (ntop.ui::format-aligned
                 (if packet-rate
                     (format nil "~D" (round packet-rate))
                     "N/A")
                 8
                 :right)
                (%network-average-packet-size-text average config))
        main)))

(defun %network-partial-legend (direction rate total-bits prefix-system unit)
  (format nil
          "~A: ~A All: ~A"
          direction
          (ntop.ui::format-aligned
           (if rate
               (ntop.ui:format-network-rate rate prefix-system unit)
               "N/A")
           10
           :left)
          (ntop.ui::format-aligned
           (if total-bits
               (ntop.ui:format-network-total total-bits prefix-system)
               "N/A")
           8
           :left)))

(defun %network-average-packet-size-text (quantity config)
  (if (null quantity)
      "N/A"
      (let ((scaled
             (ntop.network.domain:network-scale-quantity quantity
                                                         (eq (%network-prefix-system config)
                                                             :binary))))
        (format nil
                "~A~A"
                (ntop.ui:format-fixed (ntop.shared.domain:scaled-quantity-value scaled) 1)
                (ntop.shared.domain:scaled-quantity-symbol scaled)))))

(defun %network-total-bytes (sample direction baseline config)
  (ntop.network.domain:network-total-bytes sample
                                           direction
                                           :baseline-sample
                                           baseline
                                           :start-zeroed-p
                                           (%network-start-zeroed-p config)))

(defun %network-prefix-system (config)
  (if (ntop.shared.application:application-config-binary-prefix-p config)
      :binary
      :decimal))

(defun %network-old-legend-p (config)
  (ntop.shared.application:application-config-network-use-old-legend-p config))

(defun %network-start-zeroed-p (config)
  (ntop.shared.application:application-config-network-start-zeroed-p config))

(defun %network-packet-legend-visible-p (show-packets-p width)
  (and show-packets-p (> (floor (* width 3) 4) 70)))

(defun %network-content-rectangle (rectangle)
  (ntop.ui:make-rectangle :x
                          (ntop.ui:rectangle-x rectangle)
                          :y
                          (1+ (ntop.ui:rectangle-y rectangle))
                          :width
                          (ntop.ui:rectangle-width rectangle)
                          :height
                          (max 0 (1- (ntop.ui:rectangle-height rectangle)))))

(defun %draw-network-title (target rectangle title theme)
  (%network-write-text target
                       rectangle
                       (ntop.ui:rectangle-x rectangle)
                       (ntop.ui:rectangle-y rectangle)
                       title
                       (ntop.ui:rectangle-width rectangle)
                       (ntop.ui:presentation-theme-style theme :widget-title)))

(defun %network-chart-rectangle (rectangle config)
  (if (%network-old-legend-p config)
      (ntop.ui:make-rectangle :x
                              (ntop.ui:rectangle-x rectangle)
                              :y
                              (ntop.ui:rectangle-y rectangle)
                              :width
                              (ntop.ui:rectangle-width rectangle)
                              :height
                              (max 0 (- (ntop.ui:rectangle-height rectangle) 4)))
      rectangle))

(defun %network-old-legend-rectangle (rectangle)
  (let ((height (min 4 (ntop.ui:rectangle-height rectangle))))
    (ntop.ui:make-rectangle :x
                            (ntop.ui:rectangle-x rectangle)
                            :y
                            (+ (ntop.ui:rectangle-y rectangle)
                               (- (ntop.ui:rectangle-height rectangle) height))
                            :width
                            (ntop.ui:rectangle-width rectangle)
                            :height
                            height)))
