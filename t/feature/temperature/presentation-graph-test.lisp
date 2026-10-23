(in-package #:ntop.test)

(progn
  (defun %f6-temperature-graph-history (name values)
    (ntop.ui:make-chart-series :name
                               name
                               :samples
                               (loop for value in values
                                     for index from 0
                                     collect (ntop.shared.domain::%make-history-sample
                                              (float index 1d0)
                                              (and value (float value 1d0))))))
  (defun %f6-temperature-graph-fixture-series ()
    (list (%f6-temperature-graph-history "CPU Package" '(25.0 58.0 74.2))
          (%f6-temperature-graph-history "GPU Core" '(40.0 53.0 61.8))))
  (defun %f6-temperature-graph-screen-lines (screen)
    (loop for row below (cl-tty-kit:screen-height screen)
          collect (string-right-trim '(#\Space)
                                     (cl-tty-kit:screen-row-string screen row))))
  (defun %f6-temperature-graph-golden-lines ()
    (with-open-file
        (stream
         (asdf:system-relative-pathname "ntop/test"
                                        "t/feature/temperature/fixtures/graph-golden.txt"))
      (loop for line = (read-line stream nil)
            while line
            collect line)))
  (defun %f6-render-temperature-graph (config)
    (let* ((screen (cl-tty-kit:make-screen 44 16))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 44 :height 16)))
      (ntop.ui:draw-widget :temperature-graph
                           screen
                           rectangle
                           nil
                           config
                           (%f6-temperature-graph-fixture-series))
      screen))
  (cl-weave:describe "F6 temperature graph presentation"
                     (cl-weave:it
                      "converts history values and appends the latest unit-formatted value"
                      (let* ((source
                              (%f6-temperature-graph-history "CPU"
                                                             '(20.0 100.0)))
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
                        (cl-weave:expect (ntop.ui:chart-series-name fahrenheit)
                                         :to-equal
                                         "CPU: 212°F")
                        (cl-weave:expect
                         (ntop.shared.domain:history-sample-value
                          (aref (ntop.ui:chart-series-samples fahrenheit) 0))
                         :to-be
                         68d0)
                        (cl-weave:expect
                         (ntop.shared.domain:history-sample-value
                          (aref (ntop.ui:chart-series-samples fahrenheit) 1))
                         :to-be
                         212d0)
                        (cl-weave:expect (ntop.ui:chart-series-name kelvin)
                                         :to-equal
                                         "CPU: 373K")
                        (cl-weave:expect
                         (ntop.shared.domain:history-sample-value
                          (aref (ntop.ui:chart-series-samples kelvin) 1))
                         :to-be
                         373.15d0)
                        (cl-weave:expect (ntop.ui:chart-series-name celsius)
                                         :to-equal
                                         "CPU: 100°C")
                        (cl-weave:expect (ntop.ui:chart-series-style fahrenheit)
                                         :to-equal
                                         (first
                                          (ntop.ui:presentation-theme-style
                                           "default"
                                           :temperature-colors)))))
                     (cl-weave:it
                      "uses explicit styles and omits a value for empty or unavailable latest samples"
                      (let* ((explicit
                              (ntop.ui:make-chart-series :name
                                                         "Custom"
                                                         :style
                                                         :custom
                                                         :samples
                                                         nil))
                             (empty
                              (first
                               (ntop.temperature.presentation::%temperature-graph-series
                                (list explicit)
                                "default"
                                :celsius)))
                             (unavailable
                              (first
                               (ntop.temperature.presentation::%temperature-graph-series
                                (list
                                 (%f6-temperature-graph-history "Sensor"
                                                                '(40.0 nil)))
                                "default"
                                :celsius))))
                        (cl-weave:expect (ntop.ui:chart-series-name empty)
                                         :to-equal
                                         "Custom")
                        (cl-weave:expect (ntop.ui:chart-series-style empty)
                                         :to-be
                                         :custom)
                        (cl-weave:expect (ntop.ui:chart-series-name unavailable)
                                         :to-equal
                                         "Sensor")
                        (cl-weave:expect
                         (ntop.shared.domain:history-sample-value
                          (aref (ntop.ui:chart-series-samples unavailable) 1))
                         :to-be
                         nil)))
                     (cl-weave:it
                      "keeps the Celsius 100-degree ceiling and expands it to cover data"
                      (let* ((config
                              (ntop.shared.application:make-application-config))
                             (below
                              (ntop.temperature.presentation::%temperature-graph-series
                               (list
                                (%f6-temperature-graph-history "CPU"
                                                               '(20.0 75.0)))
                               "default"
                               :celsius))
                             (above
                              (ntop.temperature.presentation::%temperature-graph-series
                               (list
                                (%f6-temperature-graph-history "CPU"
                                                               '(20.0 135.2d0)))
                               "default"
                               :celsius))
                             (default-chart
                              (ntop.temperature.presentation::%temperature-graph-config
                               "default"
                               config
                               below))
                             (expanded-chart
                              (ntop.temperature.presentation::%temperature-graph-config
                               "default"
                               config
                               above)))
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-max default-chart)
                         :to-be
                         100)
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-labels default-chart)
                         :to-equal
                         '("0°C" "50°C" "100°C"))
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-max expanded-chart)
                         :to-be
                         135.2d0)
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-labels expanded-chart)
                         :to-equal
                         '("0°C" "68°C" "136°C"))))
                     (cl-weave:it
                      "uses the configured unit when choosing the shared axis ceiling"
                      (let* ((fahrenheit-config
                              (ntop.shared.application:make-application-config
                               :temperature-unit
                               :fahrenheit))
                             (fahrenheit-series
                              (ntop.temperature.presentation::%temperature-graph-series
                               (list
                                (%f6-temperature-graph-history "CPU"
                                                               '(20.0 110.0)))
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
                                (%f6-temperature-graph-history "CPU"
                                                               '(20.0 100.0)))
                               "default"
                               :kelvin))
                             (kelvin-chart
                              (ntop.temperature.presentation::%temperature-graph-config
                               "default"
                               kelvin-config
                               kelvin-series)))
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-max fahrenheit-chart)
                         :to-be
                         230d0)
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-labels fahrenheit-chart)
                         :to-equal
                         '("0°F" "115°F" "230°F"))
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-max kelvin-chart)
                         :to-be
                         373.15d0)
                        (cl-weave:expect
                         (ntop.ui:chart-config-y-labels kelvin-chart)
                         :to-equal
                         '("0K" "187K" "374K"))))
                     (cl-weave:it
                      "keeps the btm legend width and height limits and forwards dot markers"
                      (let* ((config
                              (ntop.shared.application:make-application-config
                               :dot-marker-p
                               t))
                             (series
                              (ntop.temperature.presentation::%temperature-graph-series
                               (%f6-temperature-graph-fixture-series)
                               "default"
                               :celsius))
                             (chart
                              (ntop.temperature.presentation::%temperature-graph-config
                               "default"
                               config
                               series)))
                        (cl-weave:expect
                         (ntop.ui:chart-config-legend-width-ratio chart)
                         :to-be
                         9/10)
                        (cl-weave:expect
                         (ntop.ui:chart-config-legend-height-ratio chart)
                         :to-be
                         1/2)
                        (cl-weave:expect
                         (ntop.ui:chart-config-dot-marker-p chart)
                         :to-be
                         t)))
                     (cl-weave:it
                      "returns a zero data maximum for an empty series and a series of missing values"
                      (let* ((empty
                              (ntop.temperature.presentation::%temperature-graph-series
                               nil
                               "default"
                               :celsius))
                             (missing
                              (ntop.temperature.presentation::%temperature-graph-series
                               (list
                                (%f6-temperature-graph-history "Sensor"
                                                               '(nil nil)))
                               "default"
                               :celsius)))
                        (cl-weave:expect
                         (ntop.temperature.presentation::%temperature-graph-maximum
                          empty)
                         :to-be
                         0)
                        (cl-weave:expect
                         (ntop.temperature.presentation::%temperature-graph-maximum
                          missing)
                         :to-be
                         0)))
                     (cl-weave:it
                      "leaves series unstyled when the theme has no temperature colors"
                      (let ((series
                             (%f6-temperature-graph-history "CPU" '(40.0 50.0))))
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
                     (cl-weave:it
                      "renders fixture histories to the graph screen golden"
                      (let ((config
                             (ntop.shared.application:make-application-config
                              :temperature-unit
                              :celsius)))
                        (cl-weave:expect
                         (%f6-temperature-graph-screen-lines
                          (%f6-render-temperature-graph config))
                         :to-equal
                         (%f6-temperature-graph-golden-lines))))
                     (cl-weave:it
                      "does not draw outside a zero-height rectangle"
                      (let* ((screen (cl-tty-kit:make-screen 12 1))
                             (rectangle
                              (ntop.ui:make-rectangle :x
                                                      0
                                                      :y
                                                      0
                                                      :width
                                                      12
                                                      :height
                                                      0)))
                        (ntop.ui:draw-widget :temperature-graph
                                             screen
                                             rectangle
                                             nil
                                             (ntop.shared.application:make-application-config)
                                             nil)
                        (cl-weave:expect (cl-tty-kit:screen-row-string screen 0)
                                         :to-equal
                                         "            ")))))
