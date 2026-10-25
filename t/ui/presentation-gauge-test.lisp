(in-package #:ntop.test)

(cl-weave:describe "pipe and basic resource gauges"
                   (cl-weave:it-each
                    ((-1d0 0d0) (0d0 0d0) (0.5d0 0.5d0) (1d0 1d0) (2d0 1d0))
                    "clamps gauge ratio ~A"
                    (ratio expected)
                    (cl-weave:expect
                     (=
                      (ntop.ui:gauge-value-ratio
                       (ntop.ui:make-gauge-value :ratio ratio))
                      expected)
                     :to-be
                     t))
                   (cl-weave:it "requires string labels and a real ratio"
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-gauge-value :ratio 0)))
                                 :to-be
                                 nil)
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-gauge-value :start-label
                                                              1
                                                              :ratio
                                                              0)))
                                 :to-be
                                 t)
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-gauge-value :inner-label
                                                              nil
                                                              :ratio
                                                              0)))
                                 :to-be
                                 t)
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:make-gauge-value :ratio "half")))
                                 :to-be
                                 t))
                   (cl-weave:it-each
                    ((:none "CPU[|||||   50%]") (:bars "[|||||||       ]")
                     (:start-label "CPU             "))
                    "draws a pipe gauge in ~A mode"
                    (mode expected)
                    (let ((screen (cl-tty-kit:make-screen 16 1)))
                      (ntop.ui:draw-pipe-gauge screen
                                               (%chart-test-rectangle 0 0 16 1)
                                               (ntop.ui:make-gauge-value
                                                :start-label
                                                "CPU"
                                                :inner-label
                                                "50%"
                                                :ratio
                                                0.5d0)
                                               :label-limit
                                               mode)
                      (cl-weave:expect (cl-tty-kit:screen-row-string screen 0)
                                       :to-equal
                                       expected)))
                   (cl-weave:it "rejects unknown pipe label modes"
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui:draw-pipe-gauge
                                     (cl-tty-kit:make-screen 4 1)
                                     (%chart-test-rectangle 0 0 4 1)
                                     (ntop.ui:make-gauge-value :ratio 0.5d0)
                                     :label-limit
                                     :all)))
                                 :to-be
                                 t))
                   (cl-weave:it "ignores empty gauge rectangles"
                                (let ((screen (cl-tty-kit:make-screen 4 1)))
                                  (cl-weave:expect
                                   (eq
                                    (ntop.ui:draw-pipe-gauge screen
                                                             (%chart-test-rectangle
                                                              0
                                                              0
                                                              0
                                                              1)
                                                             (ntop.ui:make-gauge-value
                                                              :ratio
                                                              1d0))
                                    screen)
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-row-string screen 0)
                                   :to-equal
                                   "    ")))
                   (cl-weave:it "clips gauge labels to the available cells"
                                (let ((screen (cl-tty-kit:make-screen 2 1)))
                                  (ntop.ui:draw-pipe-gauge screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            2
                                                            1)
                                                           (ntop.ui:make-gauge-value
                                                            :start-label
                                                            "CPU"
                                                            :ratio
                                                            0.5d0)
                                                           :label-limit
                                                           :start-label)
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-row-string screen 0)
                                   :to-equal
                                   "CP")))
                   (cl-weave:it "fills a CPU gauge grid in four columns"
                                (let ((screen (cl-tty-kit:make-screen 48 1))
                                      (gauges
                                       (loop repeat 4
                                             collect (ntop.ui:make-gauge-value
                                                      :start-label
                                                      "CPU"
                                                      :inner-label
                                                      "50%"
                                                      :ratio
                                                      0.5d0))))
                                  (ntop.ui:draw-basic-cpu-gauges screen
                                                                 (%chart-test-rectangle
                                                                  0
                                                                  0
                                                                  48
                                                                  1)
                                                                 gauges)
                                  (cl-weave:expect
                                   (count #\[
                                          (cl-tty-kit:screen-row-string screen
                                                                        0))
                                   :to-equal
                                   4)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "CPU"
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)))
                   (cl-weave:it
                    "uses BTM's Bars labels at 10 cells per CPU column"
                    (let ((screen (cl-tty-kit:make-screen 40 1))
                          (gauges
                           (loop repeat 4
                                 collect (ntop.ui:make-gauge-value :start-label
                                                                   "CPU"
                                                                   :inner-label
                                                                   "50%"
                                                                   :ratio
                                                                   0.5d0))))
                      (ntop.ui:draw-basic-cpu-gauges screen
                                                     (%chart-test-rectangle 0
                                                                            0
                                                                            40
                                                                            1)
                                                     gauges)
                      (cl-weave:expect
                       (count #\[ (cl-tty-kit:screen-row-string screen 0))
                       :to-equal
                       0)
                      (cl-weave:expect
                       (loop for start in '(1 11 21 31)
                             collect
                             (subseq (cl-tty-kit:screen-row-string screen 0)
                                     start
                                     (+ start 3)))
                       :to-equal
                       '("CPU" "CPU" "CPU" "CPU"))))
                   (cl-weave:it "uses inner percentages in narrow CPU gauge columns"
                                (let ((screen (cl-tty-kit:make-screen 36 1))
                                      (gauges
                                       (loop repeat 4
                                             collect (ntop.ui:make-gauge-value
                                                      :start-label
                                                      "CPU"
                                                      :inner-label
                                                      "50%"
                                                      :ratio
                                                      0.5d0))))
                                  (ntop.ui:draw-basic-cpu-gauges screen
                                                                 (%chart-test-rectangle
                                                                  0
                                                                  0
                                                                  36
                                                                  1)
                                                                 gauges)
                                  (cl-weave:expect
                                   (count #\[
                                          (cl-tty-kit:screen-row-string screen
                                                                        0))
                                   :to-equal
                                   0)
                                  (cl-weave:expect
                                   (list
                                    (loop for column in '(1 10 19 28)
                                          collect
                                          (subseq
                                           (cl-tty-kit:screen-row-string screen 0)
                                           column
                                           (+ column 3)))
                                    (search "CPU"
                                            (cl-tty-kit:screen-row-string screen 0)))
                                   :to-equal
                                   '(("50%" "50%" "50%" "50%") nil))))
                   (cl-weave:it-each ((8 2 8) (8 1 4) (0 2 0))
                                     "draws ~A CPU gauges in a ~A-row area"
                                     (gauge-count height expected-visible)
                                     (let* ((screen
                                             (cl-tty-kit:make-screen 40 height))
                                            (gauges
                                             (loop repeat gauge-count
                                                   collect (ntop.ui:make-gauge-value
                                                            :start-label
                                                            "CPU"
                                                            :ratio
                                                            0.5d0))))
                                       (ntop.ui:draw-basic-cpu-gauges screen
                                                                      (%chart-test-rectangle
                                                                       0
                                                                       0
                                                                       40
                                                                       height)
                                                                      gauges)
                                       (cl-weave:expect
                                        (count #\C
                                               (%chart-test-screen-text screen
                                                                        height))
                                        :to-equal
                                        expected-visible)))
                   (cl-weave:it "keeps the RAM label when its value is too wide"
                                (let ((screen (cl-tty-kit:make-screen 20 1)))
                                  (ntop.ui:draw-basic-memory-gauge screen
                                                                   (%chart-test-rectangle
                                                                    0
                                                                    0
                                                                    20
                                                                    1)
                                                                   4
                                                                   8
                                                                   "113.9GiB/128.0GiB"
                                                                   nil)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "RAM["
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (not
                                    (search "113.9GiB/128.0GiB"
                                            (cl-tty-kit:screen-row-string
                                             screen
                                             0)))
                                   :to-be
                                   t)))
                   (cl-weave:it "draws a memory ratio and label"
                                (let ((screen (cl-tty-kit:make-screen 24 1)))
                                  (ntop.ui:draw-basic-memory-gauge screen
                                                                   (%chart-test-rectangle
                                                                    0
                                                                    0
                                                                    24
                                                                    1)
                                                                   4
                                                                   8
                                                                   "4K/8K"
                                                                   nil)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "RAM["
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "4K/8K"
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)))
                   (cl-weave:it "handles zero total memory as an empty gauge"
                                (let ((screen (cl-tty-kit:make-screen 20 1)))
                                  (ntop.ui:draw-basic-memory-gauge screen
                                                                   (%chart-test-rectangle
                                                                    0
                                                                    0
                                                                    20
                                                                    1)
                                                                   0
                                                                   0
                                                                   "0B"
                                                                   nil)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "RAM["
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "0B"
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)))
                   (cl-weave:it-each ((-1 8) (1 -8))
                                     "rejects invalid memory values ~A and ~A"
                                     (used total)
                                     (cl-weave:expect
                                      (%chart-test-signals-error-p
                                       (lambda ()
                                         (ntop.ui:draw-basic-memory-gauge
                                          (cl-tty-kit:make-screen 20 1)
                                          (%chart-test-rectangle 0 0 20 1)
                                          used
                                          total
                                          "value"
                                          nil)))
                                      :to-be
                                      t)))

(cl-weave:describe "pipe gauge boundary sizes"
                   (cl-weave:it "skips gauges with zero height"
                                (let ((screen (cl-tty-kit:make-screen 4 1)))
                                  (ntop.ui:draw-pipe-gauge screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            4
                                                            0)
                                                           (ntop.ui:make-gauge-value
                                                            :ratio
                                                            0.5d0))
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-row-string screen 0)
                                   :to-equal
                                   "    ")))
                   (cl-weave:it
                    "omits the pipe when the start label fills its width"
                    (let ((screen (cl-tty-kit:make-screen 3 1)))
                      (ntop.ui:draw-pipe-gauge screen
                                               (%chart-test-rectangle 0 0 3 1)
                                               (ntop.ui:make-gauge-value
                                                :start-label
                                                "CPU"
                                                :ratio
                                                1d0))
                      (cl-weave:expect (cl-tty-kit:screen-row-string screen 0)
                                       :to-equal
                                       "CPU")))
                   (cl-weave:it "omits the closing bracket when no bars fit"
                                (let ((screen (cl-tty-kit:make-screen 2 1)))
                                  (ntop.ui:draw-pipe-gauge screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            2
                                                            1)
                                                           (ntop.ui:make-gauge-value
                                                            :ratio
                                                            1d0)
                                                           :label-limit
                                                           :bars)
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-row-string screen 0)
                                   :to-equal
                                   "[ ")))
                   (cl-weave:it "handles empty labels with a visible pipe"
                                (let ((screen (cl-tty-kit:make-screen 4 1)))
                                  (ntop.ui:draw-pipe-gauge screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            4
                                                            1)
                                                           (ntop.ui:make-gauge-value
                                                            :ratio
                                                            0d0))
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-row-string screen 0)
                                   :to-equal
                                   "[  ]")))
                   (cl-weave:it "distributes remainder cells across CPU columns"
                                (let ((screen (cl-tty-kit:make-screen 43 1))
                                      (gauges
                                       (loop repeat 4
                                             collect (ntop.ui:make-gauge-value
                                                      :start-label
                                                      "CPU"
                                                      :ratio
                                                      0.5d0))))
                                  (ntop.ui:draw-basic-cpu-gauges screen
                                                                 (%chart-test-rectangle
                                                                  0
                                                                  0
                                                                  43
                                                                  1)
                                                                 gauges)
                                  (cl-weave:expect
                                   (loop for start in '(1 12 23 34)
                                         collect
                                         (subseq
                                          (cl-tty-kit:screen-row-string screen 0)
                                          start
                                          (+ start 3)))
                                   :to-equal
                                   '("CPU" "CPU" "CPU" "CPU"))))
)