(in-package #:ntop.test)

(cl-weave:describe "chart series and configuration"
                   (cl-weave:it "copies series samples into a vector"
                                (let* ((samples
                                        (%chart-test-samples
                                         '((0d0 1d0) (1d0 2d0))))
                                       (series
                                        (ntop.ui:make-chart-series :name
                                                                   "CPU"
                                                                   :samples
                                                                   samples)))
                                  (cl-weave:expect
                                   (vectorp
                                    (ntop.ui:chart-series-samples series))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (ntop.ui:chart-series-name series)
                                   :to-equal
                                   "CPU")))
                   (cl-weave:it-each ((nil) ("") ("Memory"))
                                     "accepts chart series name ~A"
                                     (name)
                                     (cl-weave:expect
                                      (ntop.ui:chart-series-name
                                       (ntop.ui:make-chart-series :name name))
                                      :to-equal
                                      name))
                   (cl-weave:it-each ((:not-a-number) (1))
                                     "rejects chart series name ~A"
                                     (name)
                                     (cl-weave:expect
                                      (%chart-test-signals-error-p
                                       (lambda ()
                                         (ntop.ui:make-chart-series :name name)))
                                      :to-be
                                      t))
                   (cl-weave:it "rejects non-history samples"
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-chart-series :samples
                                                               '(bad-sample))))
                                 :to-be
                                 t))
                   (cl-weave:it "accepts samples with equal timestamps"
                                (let ((samples
                                       (%chart-test-samples
                                        '((2d0 1d0) (2d0 3d0)))))
                                  (cl-weave:expect (length samples) :to-equal 1)))
                   (cl-weave:it "rejects decreasing sample timestamps"
                                (let ((samples
                                       (vector
                                        (ntop.shared.domain::%make-history-sample
                                         2d0
                                         1d0)
                                        (ntop.shared.domain::%make-history-sample
                                         1d0
                                         2d0))))
                                  (cl-weave:expect
                                   (%chart-test-signals-error-p
                                    (lambda ()
                                      (ntop.ui:make-chart-series :samples
                                                                 samples)))
                                   :to-be
                                   t)))
                   (cl-weave:it-each ((:linear) (:log2) (:log10))
                                     "accepts chart scale ~A"
                                     (scale)
                                     (cl-weave:expect
                                      (ntop.ui:chart-config-y-scale
                                       (ntop.ui:make-chart-config :y-scale
                                                                  scale))
                                      :to-equal
                                      scale))
                   (cl-weave:it "copies chart axis labels"
                                (let* ((labels (vector
                                                "left"
                                                "right"))
                                       (config
                                        (ntop.ui:make-chart-config :x-start
                                                                   0
                                                                   :x-end
                                                                   1
                                                                   :x-labels
                                                                   labels
                                                                   :y-labels
                                                                   '("0" "1"))))
                                  (cl-weave:expect
                                   (ntop.ui:chart-config-x-labels config)
                                   :to-equal
                                   '("left" "right"))
                                  (cl-weave:expect
                                   (ntop.ui:chart-config-y-labels config)
                                   :to-equal
                                   '("0" "1"))))
                   (cl-weave:it "provides a default chart configuration"
                                (cl-weave:expect
                                 (ntop.ui:chart-config-y-min
                                  ntop.ui:*default-chart-config*)
                                 :to-equal
                                 0d0)))

(cl-weave:describe "chart configuration validation"
                   (cl-weave:it-each
                    (((:x-start 0)) ((:x-end 1))
                     ((:x-start 2 :x-end 1))
                     ((:x-start "0" :x-end 1))
                     ((:y-min 1 :y-max 1))
                     ((:y-min 2 :y-max 1))
                     ((:y-min "0" :y-max 1))
                     ((:y-scale :log))
                     ((:y-labels (1 "one")))
                     ((:theme-name 7))
                     ((:legend-position :center))
                     ((:legend-width-ratio -0.1))
                     ((:legend-width-ratio 1.1))
                     ((:legend-height-ratio -1))
                     ((:legend-height-ratio 2))
                     ((:x-labels ("left")))
                     ((:x-labels ("left" 1)))
                     ((:x-labels "left")))
                    "rejects invalid chart configuration ~A"
                    (options)
                    (cl-weave:expect
                     (%chart-test-signals-error-p
                      (lambda ()
                        (apply #'ntop.ui:make-chart-config options)))
                     :to-be
                     t))
                   (cl-weave:it "requires both X bounds together"
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-chart-config :x-start 0)))
                                 :to-be
                                 t)
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-chart-config :x-end 1)))
                                 :to-be
                                 t))
                   (cl-weave:it "allows equal X bounds and zero legend ratios"
                                (let ((config
                                       (ntop.ui:make-chart-config :x-start
                                                                  1
                                                                  :x-end
                                                                  1
                                                                  :legend-width-ratio
                                                                  0
                                                                  :legend-height-ratio
                                                                  0)))
                                  (cl-weave:expect
                                   (ntop.ui:chart-config-x-start config)
                                   :to-equal
                                   1)
                                  (cl-weave:expect
                                   (ntop.ui:chart-config-x-end config)
                                   :to-equal
                                   1)))
                   (cl-weave:it "normalizes dot marker options to booleans"
                                (cl-weave:expect
                                 (ntop.ui:chart-config-dot-marker-p
                                  (ntop.ui:make-chart-config :dot-marker-p
                                                             :enabled))
                                 :to-be
                                 t)
                                (cl-weave:expect
                                 (ntop.ui:chart-config-dot-marker-p
                                  (ntop.ui:make-chart-config))
                                 :to-be
                                 nil)))
