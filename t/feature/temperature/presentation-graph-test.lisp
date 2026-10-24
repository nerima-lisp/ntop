(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load
   (merge-pathnames
    "helpers-presentation.lisp"
    (uiop:pathname-directory-pathname
     (asdf:component-pathname
      (asdf:find-component
       "ntop/test"
       "t/feature/temperature/presentation-graph-test"))))))
(cl-weave:describe
 "F6 temperature graph presentation"
 (cl-weave:it
  "converts history values and appends the latest unit-formatted value"
  (let* ((source (%f6-temperature-graph-history "CPU" '(20.0 100.0)))
         (fahrenheit
          (first
           (ntop.temperature.presentation::%temperature-graph-series
            (list source)
            "default"
            :fahrenheit)))
         (kelvin
          (first
           (ntop.temperature.presentation::%temperature-graph-series
            (list source)
            "default"
            :kelvin)))
         (celsius
          (first
           (ntop.temperature.presentation::%temperature-graph-series
            (list source)
            "default"
            :celsius))))
    (cl-weave:expect
     (list (ntop.ui:chart-series-name fahrenheit)
           (ntop.shared.domain:history-sample-value
            (aref (ntop.ui:chart-series-samples fahrenheit) 0))
           (ntop.shared.domain:history-sample-value
            (aref (ntop.ui:chart-series-samples fahrenheit) 1))
           (ntop.ui:chart-series-name kelvin)
           (ntop.shared.domain:history-sample-value
            (aref (ntop.ui:chart-series-samples kelvin) 1))
           (ntop.ui:chart-series-name celsius)
           (ntop.ui:chart-series-style fahrenheit))
     :to-equal
     (list "CPU: 212°F" 68d0 212d0 "CPU: 373K" 373.15d0 "CPU: 100°C"
           (first
            (ntop.ui:presentation-theme-style "default"
                                              :temperature-colors))))))
 (cl-weave:it
  "uses explicit styles and omits a value for empty or unavailable latest samples"
  (let* ((explicit
          (ntop.ui:make-chart-series :name "Custom" :style :custom :samples nil))
         (empty
          (first
           (ntop.temperature.presentation::%temperature-graph-series
            (list explicit)
            "default"
            :celsius)))
         (unavailable
          (first
           (ntop.temperature.presentation::%temperature-graph-series
            (list (%f6-temperature-graph-history "Sensor" '(40.0 nil)))
            "default"
            :celsius))))
    (cl-weave:expect
     (list (ntop.ui:chart-series-name empty)
           (ntop.ui:chart-series-style empty)
           (ntop.ui:chart-series-name unavailable)
           (ntop.shared.domain:history-sample-value
            (aref (ntop.ui:chart-series-samples unavailable) 1)))
     :to-equal
     (list "Custom" :custom "Sensor" nil))))
 (cl-weave:it
  "keeps the Celsius 100-degree ceiling and expands it to cover data"
  (let* ((config (ntop.shared.application:make-application-config))
         (below
          (ntop.temperature.presentation::%temperature-graph-series
           (list (%f6-temperature-graph-history "CPU" '(20.0 75.0)))
           "default"
           :celsius))
         (above
          (ntop.temperature.presentation::%temperature-graph-series
           (list (%f6-temperature-graph-history "CPU" '(20.0 135.2d0)))
           "default"
           :celsius))
         (default-chart
          (ntop.temperature.presentation::%temperature-graph-config "default"
                                                                    config
                                                                    below))
         (expanded-chart
          (ntop.temperature.presentation::%temperature-graph-config "default"
                                                                    config
                                                                    above)))
    (cl-weave:expect
     (list (ntop.ui:chart-config-y-max default-chart)
           (ntop.ui:chart-config-y-labels default-chart)
           (ntop.ui:chart-config-y-max expanded-chart)
           (ntop.ui:chart-config-y-labels expanded-chart))
     :to-equal
     (list 100
           (list "0°C" "50°C" "100°C")
           135.2d0
           (list "0°C" "68°C" "136°C")))))
 (cl-weave:it "uses the configured unit when choosing the shared axis ceiling"
              (let* ((fahrenheit-config
                      (ntop.shared.application:make-application-config
                       :temperature-unit
                       :fahrenheit))
                     (fahrenheit-series
                      (ntop.temperature.presentation::%temperature-graph-series
                       (list
                        (%f6-temperature-graph-history "CPU" '(20.0 110.0)))
                       "default"
                       :fahrenheit))
                     (fahrenheit-chart
                      (ntop.temperature.presentation::%temperature-graph-config
                       "default"
                       fahrenheit-config
                       fahrenheit-series))
                     (kelvin-config
                      (ntop.shared.application:make-application-config
                       :temperature-unit
                       :kelvin))
                     (kelvin-series
                      (ntop.temperature.presentation::%temperature-graph-series
                       (list
                        (%f6-temperature-graph-history "CPU" '(20.0 100.0)))
                       "default"
                       :kelvin))
                     (kelvin-chart
                      (ntop.temperature.presentation::%temperature-graph-config
                       "default"
                       kelvin-config
                       kelvin-series)))
                (cl-weave:expect
                 (list (ntop.ui:chart-config-y-max fahrenheit-chart)
                       (ntop.ui:chart-config-y-labels fahrenheit-chart)
                       (ntop.ui:chart-config-y-max kelvin-chart)
                       (ntop.ui:chart-config-y-labels kelvin-chart))
                 :to-equal
                 (list 230d0
                       (list "0°F" "115°F" "230°F")
                       373.15d0
                       (list "0K" "187K" "374K")))))
 (cl-weave:it
  "keeps the btm legend width and height limits and forwards dot markers"
  (let* ((config
          (ntop.shared.application:make-application-config :dot-marker-p t))
         (series
          (ntop.temperature.presentation::%temperature-graph-series
           (%f6-temperature-graph-fixture-series)
           "default"
           :celsius))
         (chart
          (ntop.temperature.presentation::%temperature-graph-config "default"
                                                                    config
                                                                    series)))
    (cl-weave:expect (ntop.ui:chart-config-legend-width-ratio chart) :to-be 9/10)
    (cl-weave:expect (ntop.ui:chart-config-legend-height-ratio chart) :to-be 1/2)
    (cl-weave:expect (ntop.ui:chart-config-dot-marker-p chart) :to-be t)))
 (cl-weave:it
  "returns a zero data maximum for an empty series and a series of missing values"
  (let* ((empty
          (ntop.temperature.presentation::%temperature-graph-series nil
                                                                    "default"
                                                                    :celsius))
         (missing
          (ntop.temperature.presentation::%temperature-graph-series
           (list (%f6-temperature-graph-history "Sensor" '(nil nil)))
           "default"
           :celsius)))
    (cl-weave:expect
     (ntop.temperature.presentation::%temperature-graph-maximum empty)
     :to-be
     0)
    (cl-weave:expect
     (ntop.temperature.presentation::%temperature-graph-maximum missing)
     :to-be
     0)))
 (cl-weave:it "leaves series unstyled when the theme has no temperature colors"
              (let ((series (%f6-temperature-graph-history "CPU" '(40.0 50.0))))
                (cl-weave:with-mocked-functions
                 (((symbol-function 'ntop.ui:presentation-theme-style)
                   (lambda (theme style)
                     (declare (ignore theme style))
                     nil)))
                 (cl-weave:expect
                  (ntop.ui:chart-series-style
                   (first
                    (ntop.temperature.presentation::%temperature-graph-series
                     (list series)
                     "default"
                     :celsius)))
                  :to-be
                  nil))))
 (cl-weave:it "renders fixture histories to the graph screen golden"
              (let ((config
                     (ntop.shared.application:make-application-config
                      :temperature-unit
                      :celsius)))
                (cl-weave:expect
                 (%f6-temperature-graph-screen-lines
                  (%f6-render-temperature-graph config))
                 :to-equal
                 (%f6-temperature-graph-golden-lines))))
 (cl-weave:it "does not draw outside a zero-height rectangle"
              (let* ((screen (cl-tty-kit:make-screen 12 1))
                     (rectangle
                      (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 0)))
                (ntop.ui:draw-widget :temperature-graph
                                     screen
                                     rectangle
                                     nil
                                     (ntop.shared.application:make-application-config)
                                     nil)
                (cl-weave:expect (cl-tty-kit:screen-row-string screen 0)
                                 :to-equal
                                 "            ")))
 (cl-weave:it-each
  ((0 1) (12 0))
  "does not draw a title in rectangles of size ~A by ~A"
  (width height)
  (let ((screen (cl-tty-kit:make-screen 12 1)))
    (ntop.temperature.presentation::%draw-temperature-graph-title
     screen
     (ntop.ui:make-rectangle :x 0 :y 0 :width width :height height)
     "default")
    (cl-weave:expect (cl-tty-kit:screen-row-string screen 0)
                     :to-equal
                     "            "))))

