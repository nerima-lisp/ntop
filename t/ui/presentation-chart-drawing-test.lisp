(in-package #:ntop.test)

(cl-weave:describe "time-chart drawing"
                   (cl-weave:it "draws axes, series, and a positioned legend"
                                (let* ((screen (cl-tty-kit:make-screen 30 12))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :name
                                                                    "CPU"
                                                                    :samples
                                                                    (%chart-test-samples
                                                                     '((0d0 0d0)
                                                                       (5d0
                                                                        60d0)
                                                                       (10d0
                                                                        100d0))))))
                                       (config
                                        (ntop.ui:make-chart-config :x-start
                                                                   0d0
                                                                   :x-end
                                                                   10d0
                                                                   :y-min
                                                                   0d0
                                                                   :y-max
                                                                   100d0
                                                                   :y-labels
                                                                   '("0" "50"
                                                                         "100")
                                                                   :x-labels
                                                                   '("10s" "0s")
                                                                   :legend-width-ratio
                                                                   1
                                                                   :legend-height-ratio
                                                                   1)))
                                  (multiple-value-bind (target legend)
                                      (ntop.ui:draw-time-chart screen
                                                               (%chart-test-rectangle
                                                                0
                                                                0
                                                                30
                                                                12)
                                                               config
                                                               series)
                                    (let ((text
                                           (%chart-test-screen-text screen 12)))
                                      (cl-weave:expect (eq target screen)
                                                       :to-be
                                                       t)
                                      (cl-weave:expect
                                       (and legend
                                            (plusp
                                             (ntop.ui:rectangle-width legend)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "CPU" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "└" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "─" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "10s" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "0s" text)))
                                       :to-be
                                       t)))))
                   (cl-weave:it "draws dot markers when enabled"
                                (let* ((screen (cl-tty-kit:make-screen 12 5))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :samples
                                                                    (%chart-test-samples
                                                                     '((0d0 1d0)
                                                                       (1d0 2d0))))))
                                       (config
                                        (ntop.ui:make-chart-config :x-start
                                                                   0d0
                                                                   :x-end
                                                                   1d0
                                                                   :y-min
                                                                   0d0
                                                                   :y-max
                                                                   2d0
                                                                   :dot-marker-p
                                                                   t)))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            12
                                                            5)
                                                           config
                                                           series)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "•"
                                             (%chart-test-screen-text screen 5))))
                                   :to-be
                                   t)))
                   (cl-weave:it "draws an X axis without Y labels"
                                (let ((screen (cl-tty-kit:make-screen 16 5)))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            16
                                                            5)
                                                           (ntop.ui:make-chart-config
                                                            :x-start
                                                            0
                                                            :x-end
                                                            1
                                                            :x-labels
                                                            '("1s" "0s"))
                                                           nil)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "─"
                                             (%chart-test-screen-text screen 5))))
                                   :to-be
                                   t)))
                   (cl-weave:it
                    "skips the X axis when the rectangle is too short"
                    (let ((screen (cl-tty-kit:make-screen 12 2)))
                      (ntop.ui:draw-time-chart screen
                                               (%chart-test-rectangle 0 0 12 2)
                                               (ntop.ui:make-chart-config
                                                :x-start
                                                0
                                                :x-end
                                                1
                                                :x-labels
                                                '("1s" "0s"))
                                               nil)
                      (cl-weave:expect
                       (search "─" (%chart-test-screen-text screen 2))
                       :to-be
                       nil)))
                   (cl-weave:it "clears empty charts and returns no legend"
                                (let ((screen (cl-tty-kit:make-screen 10 4)))
                                  (multiple-value-bind (target legend)
                                      (ntop.ui:draw-time-chart screen
                                                               (%chart-test-rectangle
                                                                0
                                                                0
                                                                10
                                                                4)
                                                               (ntop.ui:make-chart-config)
                                                               nil)
                                    (cl-weave:expect (eq target screen)
                                                     :to-be
                                                     t)
                                    (cl-weave:expect legend :to-be nil)
                                    (cl-weave:expect
                                     (%chart-test-screen-text screen 4)
                                     :to-equal
                                     (format nil
                                             "          ~%          ~%          ~%          ~%")))))
                   (cl-weave:it "does not draw samples with missing values"
                                (let* ((screen (cl-tty-kit:make-screen 10 4))
                                       (samples
                                        (%chart-test-samples
                                         '((0d0 nil) (1d0 nil))))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :samples
                                                                    samples))))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            10
                                                            4)
                                                           (ntop.ui:make-chart-config
                                                            :x-start
                                                            0d0
                                                            :x-end
                                                            1d0
                                                            :y-min
                                                            0
                                                            :y-max
                                                            1)
                                                           series)
                                  (cl-weave:expect
                                   (search "⠀"
                                           (%chart-test-screen-text screen 4))
                                   :to-be
                                   nil)))
                   (cl-weave:it
                    "places a single Y label at the bottom of the plot"
                    (let ((screen (cl-tty-kit:make-screen 8 4)))
                      (ntop.ui::%draw-chart-y-labels screen
                                                     (%chart-test-rectangle 0
                                                                            0
                                                                            8
                                                                            4)
                                                     (%chart-test-rectangle 2
                                                                            0
                                                                            6
                                                                            4)
                                                     (ntop.ui:make-chart-config
                                                      :y-labels
                                                      '("only"))
                                                     4
                                                     nil)
                      (cl-weave:expect
                       (not
                        (null
                         (search "only" (cl-tty-kit:screen-row-string screen 3))))
                       :to-be
                       t)))
                   (cl-weave:it "keeps empty plot widths safe"
                                (let ((screen (cl-tty-kit:make-screen 2 3)))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            2
                                                            3)
                                                           (ntop.ui:make-chart-config
                                                            :y-labels
                                                            '("long label")
                                                            :x-labels
                                                            '("left" "right"))
                                                           nil)
                                  (cl-weave:expect
                                   (length (%chart-test-screen-text screen 3))
                                   :to-equal
                                   9))))
