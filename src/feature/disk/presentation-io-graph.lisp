(in-package #:ntop.disk.presentation)

(defun %disk-io-series-for-name (sources name direction color name-width)
  (let ((source
         (find name sources :key #'ntop.ui:chart-series-name :test #'equal)))
    (when source
      (let* ((samples (ntop.ui:chart-series-samples source))
             (latest-value
              (when (plusp (length samples))
                (ntop.shared.domain:history-sample-value
                 (aref samples (1- (length samples)))))))
        (ntop.ui:make-chart-series :name
                                   (format nil
                                           "~VA ~A:~A"
                                           name-width
                                           name
                                           direction
                                           (if latest-value
                                               (ntop.ui:format-disk-io-legend-rate
                                                latest-value)
                                               (format nil "~11@A" "N/A")))
                                   :style
                                   color
                                   :samples
                                   samples)))))

(defun %disk-io-series-pair (read-series write-series
                                         name
                                         index
                                         name-width
                                         theme)
  (let* ((read-colors
          (ntop.ui:presentation-theme-style theme :disk-io-read-colors))
         (write-colors
          (ntop.ui:presentation-theme-style theme :disk-io-write-colors))
         (read-color
          (and read-colors (nth (mod index (length read-colors)) read-colors)))
         (write-color
          (and write-colors
               (nth (mod index (length write-colors)) write-colors))))
    (remove nil
            (list
             (%disk-io-series-for-name read-series
                                       name
                                       "R"
                                       read-color
                                       name-width)
             (%disk-io-series-for-name write-series
                                       name
                                       "W"
                                       write-color
                                       name-width)))))

(defun %disk-io-chart-series (series theme)
  (let* ((read-series (coerce (or (getf series :read) #()) 'list))
         (write-series (coerce (or (getf series :write) #()) 'list))
         (names
          (sort
           (remove-duplicates
            (loop for item in (append read-series write-series)
                  for name = (ntop.ui:chart-series-name item)
                  when name
                    collect name)
            :test
            #'equal)
           #'string<))
         (name-width
          (loop for name in names
                maximize (length name) into width
                finally (return (or width 0)))))
    (loop for name in names
          for index from 0
          append (%disk-io-series-pair read-series
                                       write-series
                                       name
                                       index
                                       name-width
                                       theme))))

(defun %disk-io-chart-maximum (series)
  (loop for item in series
        append (coerce (ntop.ui:chart-series-samples item) 'list) into samples
        finally (return
                 (loop for sample in samples
                       for value = (ntop.shared.domain:history-sample-value
                                    sample)
                       when value
                         maximize value into maximum
                       finally (return (or maximum 0d0))))))

(defun %disk-io-chart-config (theme config series)
  (multiple-value-bind (y-max y-labels)
      (ntop.ui:format-disk-io-axis-labels (%disk-io-chart-maximum series))
    (ntop.ui:make-chart-config :y-min
                               0d0
                               :y-max
                               y-max
                               :y-labels
                               y-labels
                               :theme-name
                               theme
                               :legend-width-ratio
                               9/10
                               :legend-height-ratio
                               1/2
                               :dot-marker-p
                               (ntop.shared.application:application-config-dot-marker-p
                                config))))

(defun %draw-disk-io-title (target rectangle theme)
  (let* ((width (ntop.ui:rectangle-width rectangle))
         (visible (subseq " Disk I/O " 0 (min width (length " Disk I/O ")))))
    (when (and (plusp width) (plusp (length visible)))
      (cl-tty-kit:screen-write-string target
                                      (ntop.ui:rectangle-x rectangle)
                                      (ntop.ui:rectangle-y rectangle)
                                      visible
                                      :style
                                      (ntop.ui:presentation-theme-style theme
                                                                        :widget-title)))))

(ntop.ui:define-widget :disk-io-graph
                       (target rectangle state config series)
                       (declare (ignore state))
                       (let* ((theme
                               (string-downcase
                                (string
                                 (ntop.shared.application:application-config-theme
                                  config))))
                              (graph-series
                               (%disk-io-chart-series series theme))
                              (chart-config
                               (%disk-io-chart-config theme config graph-series))
                              (height (ntop.ui:rectangle-height rectangle)))
                         (when (plusp height)
                           (%draw-disk-io-title target rectangle theme)
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
                                                    graph-series)))
                       nil)
