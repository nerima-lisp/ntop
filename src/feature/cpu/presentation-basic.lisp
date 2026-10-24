(in-package #:ntop.cpu.presentation)

(defun %cpu-basic-gauge (label usage style)
  (ntop.ui:make-gauge-value :start-label
                            label
                            :inner-label
                            (if (realp usage)
                                (ntop.ui:format-cpu-basic-percent usage nil)
                                "N/A")
                            :ratio
                            (if (realp usage)
                                (/ (float usage 1d0) 100d0)
                                0d0)
                            :label-style
                            style
                            :gauge-style
                            style))

(defun %cpu-basic-core-label (index)
  (let ((text (write-to-string index)))
    (concatenate 'string
                 text
                 (make-string (max 0 (- 3 (length text)))
                              :initial-element
                              #\Space))))

(defun %cpu-basic-gauges (sample theme)
  (loop for core in (%cpu-cores sample)
        for position from 0
        for index = (or (ntop.cpu.domain:cpu-core-value-core-index core)
                        position)
        for colors = (ntop.ui:presentation-theme-style theme :cpu-colors)
        collect (%cpu-basic-gauge (%cpu-basic-core-label index)
                                  (ntop.cpu.domain:cpu-core-value-usage-percent
                                   core)
                                  (nth (mod index (length colors)) colors))))

(defun %draw-cpu-empty (target rectangle theme)
  (let* ((label "N/A")
         (width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle)))
    (when (and (plusp width) (plusp height))
      (cl-tty-kit:screen-write-string target
                                      (+ (ntop.ui:rectangle-x rectangle)
                                         (floor (max 0 (- width (length label)))
                                                2))
                                      (+ (ntop.ui:rectangle-y rectangle)
                                         (floor height 2))
                                      label
                                      :style
                                      (ntop.ui:presentation-theme-style theme
                                                                        :widget-title)))))

(defun %draw-cpu-basic (target rectangle sample config theme)
  (let* ((cores (%cpu-basic-gauges sample theme))
         (average
          (and sample
               (not
                (ntop.shared.application:application-config-hide-average-cpu-p
                 config))
               (ntop.cpu.domain:cpu-sample-average-usage-percent sample)))
         (show-average-p (realp average))
         (core-height
          (max 0
               (- (ntop.ui:rectangle-height rectangle)
                  (if show-average-p
                      1
                      0))))
         (core-rectangle
          (ntop.ui:make-rectangle :x
                                  (ntop.ui:rectangle-x rectangle)
                                  :y
                                  (ntop.ui:rectangle-y rectangle)
                                  :width
                                  (ntop.ui:rectangle-width rectangle)
                                  :height
                                  core-height)))
    (when cores
      (ntop.ui:draw-basic-cpu-gauges target core-rectangle cores))
    (when show-average-p
      (ntop.ui:draw-pipe-gauge target
                               (ntop.ui:make-rectangle :x
                                                       (1+
                                                        (ntop.ui:rectangle-x
                                                         rectangle))
                                                       :y
                                                       (+
                                                        (ntop.ui:rectangle-y
                                                         rectangle)
                                                        core-height)
                                                       :width
                                                       (max 0
                                                            (-
                                                             (ntop.ui:rectangle-width
                                                              rectangle)
                                                             2))
                                                       :height
                                                       1)
                               (%cpu-basic-gauge "AVG"
                                                 average
                                                 (ntop.ui:presentation-theme-style
                                                  theme
                                                  :avg-cpu))))
    (unless (or cores show-average-p)
      (%draw-cpu-empty target rectangle theme))))

(ntop.ui:define-widget :cpu
                       (target rectangle state config series)
                       (let* ((sample (%cpu-sample series))
                              (theme
                               (ntop.shared.application:application-config-theme
                                config)))
                         (if (ntop.shared.application:application-config-basic-p
                              config)
                             (%draw-cpu-basic target
                                              rectangle
                                              sample
                                              config
                                              theme)
                             (%draw-cpu-graph target
                                              rectangle
                                              state
                                              sample
                                              config
                                              series
                                              theme)))
                       nil)
