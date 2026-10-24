(in-package #:ntop.temperature.presentation)

(defun %temperature-graph-series (series theme unit)
  (let* ((items (coerce (or series #()) 'list))
         (colors (ntop.ui:presentation-theme-style theme :temperature-colors)))
    (loop for source in items
          for index from 0
          for source-samples = (ntop.ui:chart-series-samples source)
          for samples = (map 'vector
                             (lambda (sample)
                               (let ((value
                                      (ntop.shared.domain:history-sample-value
                                       sample)))
                                 (ntop.shared.domain::%make-history-sample
                                  (ntop.shared.domain:history-sample-sampled-at
                                   sample)
                                  (and value
                                       (coerce
                                        (ntop.ui::format-temperature-value value
                                                                           unit)
                                        'double-float)))))
                             source-samples)
          for latest-value = (when (plusp (length source-samples))
                               (ntop.shared.domain:history-sample-value
                                (aref source-samples
                                      (1- (length source-samples)))))
          collect (ntop.ui:make-chart-series :name
                                             (if latest-value
                                                 (format nil
                                                         "~A: ~A"
                                                         (ntop.ui:chart-series-name
                                                          source)
                                                         (ntop.ui:format-temperature-graph-value
                                                          (ntop.shared.domain:history-sample-value
                                                           (aref source-samples
                                                                 (1-
                                                                  (length
                                                                   source-samples))))
                                                          unit))
                                                 (ntop.ui:chart-series-name
                                                  source))
                                             :style
                                             (or
                                              (ntop.ui:chart-series-style
                                               source)
                                              (and (plusp (length colors))
                                                   (nth
                                                    (mod index (length colors))
                                                    colors)))
                                             :samples
                                             samples))))

(defun %temperature-graph-config (theme config series)
  (let ((unit
         (ntop.shared.application:application-config-temperature-unit config)))
    (multiple-value-bind (y-maximum y-labels)
        (ntop.ui:format-temperature-axis-labels
         (ntop.ui:chart-series-maximum-value series)
         unit)
      (ntop.ui:make-chart-config :y-min
                                 0d0
                                 :y-max
                                 y-maximum
                                 :y-labels
                                 y-labels
                                 :theme-name
                                 theme
                                 :legend-position
                                 :top-right
                                 :legend-width-ratio
                                 9/10
                                 :legend-height-ratio
                                 1/2
                                 :dot-marker-p
                                 (ntop.shared.application:application-config-dot-marker-p
                                  config)))))

(defun %draw-temperature-graph-title (target rectangle theme)
  (let* ((width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle))
         (title " Temperature "))
    (when (and (plusp width) (plusp height))
      (cl-tty-kit:screen-write-string target
                                      (ntop.ui:rectangle-x rectangle)
                                      (ntop.ui:rectangle-y rectangle)
                                      (subseq title
                                              0
                                              (min width (length title)))
                                      :style
                                      (ntop.ui:presentation-theme-style theme
                                                                        :widget-title)))))

(ntop.ui:define-widget :temperature-graph
                       (target rectangle state config series)
                       (declare (ignore state))
                       (let* ((theme (ntop.shared.application:application-config-theme config))
                              (unit
                               (ntop.shared.application:application-config-temperature-unit
                                config))
                              (graph-series
                               (%temperature-graph-series series theme unit))
                              (chart-config
                               (%temperature-graph-config theme
                                                          config
                                                          graph-series))
                              (height (ntop.ui:rectangle-height rectangle)))
                         (when (plusp height)
                           (%draw-temperature-graph-title target
                                                          rectangle
                                                          theme)
                           (ntop.ui:draw-time-chart target
                                                    (ntop.ui:make-rectangle :x
                                                                            (ntop.ui:rectangle-x
                                                                             rectangle)
                                                                            :y
                                                                            (1+
                                                                             (ntop.ui:rectangle-y
                                                                              rectangle))
                                                                            :width
                                                                            (ntop.ui:rectangle-width
                                                                             rectangle)
                                                                            :height
                                                                            (max
                                                                             0
                                                                             (1-
                                                                              height)))
                                                    chart-config
                                                    graph-series))
                         nil))
