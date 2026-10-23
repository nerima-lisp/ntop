(in-package #:ntop.test)

(defun %chart-test-signals-error-p (thunk)
  (handler-case (progn
                  (funcall thunk)
                  nil)
    (error ()
      t)))

(defun %chart-test-samples (entries)
  (let ((store
         (ntop.shared.domain:make-history-store 1
                                                :retention-seconds
                                                100d0
                                                :sample-interval-seconds
                                                1d0
                                                :initial-window-seconds
                                                10d0
                                                :minimum-window-seconds
                                                1d0
                                                :zoom-step-seconds
                                                1d0)))
    (dolist (entry entries)
      (ntop.shared.domain:history-append store
                                         0
                                         (ntop.shared.domain:make-sample-record
                                          :sampled-at
                                          (first entry))
                                         (second entry)))
    (if entries
        (ntop.shared.domain:history-slice store
                                          0
                                          :start
                                          (first (first entries))
                                          :end
                                          (first (car (last entries))))
        #())))

(defun %chart-test-screen-text (screen height)
  (with-output-to-string (output)
    (loop for row below height
          do (write-string (cl-tty-kit:screen-row-string screen row) output) (terpri
                                                                              output))))

(defun %chart-test-rectangle (x y width height)
  (ntop.ui:make-rectangle :x x :y y :width width :height height))

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

(cl-weave:describe "chart legend layout and text"
                   (cl-weave:it-each
                    ((:top-left 2 3) (:top 19 3)
                     (:top-right 37 3)
                     (:left 2 11)
                     (:right 37 11)
                     (:bottom-left 2 20)
                     (:bottom 19 20)
                     (:bottom-right 37 20))
                    "places a chart legend at ~A"
                    (position expected-x expected-y)
                    (let* ((area (%chart-test-rectangle 2 3 40 20))
                           (series
                            (list (ntop.ui:make-chart-series :name "CPU")))
                           (config
                            (ntop.ui:make-chart-config :legend-position
                                                       position
                                                       :legend-width-ratio
                                                       1
                                                       :legend-height-ratio
                                                       1)))
                      (multiple-value-bind (legend visible)
                          (ntop.ui:chart-legend-layout area series config)
                        (cl-weave:expect (ntop.ui:rectangle-x legend)
                                         :to-equal
                                         expected-x)
                        (cl-weave:expect (ntop.ui:rectangle-y legend)
                                         :to-equal
                                         expected-y)
                        (cl-weave:expect visible :to-equal series))))
                   (cl-weave:it-each
                    ((nil 1 1) (:top-right 0 1) (:top-right 1 0))
                    "omits an unusable legend with ~A"
                    (position width-ratio height-ratio)
                    (let* ((area (%chart-test-rectangle 0 0 10 4))
                           (series
                            (list (ntop.ui:make-chart-series :name "CPU")))
                           (config
                            (ntop.ui:make-chart-config :legend-position
                                                       position
                                                       :legend-width-ratio
                                                       width-ratio
                                                       :legend-height-ratio
                                                       height-ratio)))
                      (multiple-value-bind (legend visible)
                          (ntop.ui:chart-legend-layout area series config)
                        (cl-weave:expect legend :to-be nil)
                        (cl-weave:expect visible :to-be nil))))
                   (cl-weave:it "omits legends without named series"
                                (multiple-value-bind (legend visible)
                                    (ntop.ui:chart-legend-layout
                                     (%chart-test-rectangle 0 0 20 8)
                                     (list (ntop.ui:make-chart-series))
                                     ntop.ui:*default-chart-config*)
                                  (cl-weave:expect legend :to-be nil)
                                  (cl-weave:expect visible :to-be nil)))
                   (cl-weave:it
                    "limits visible legend entries to the available height"
                    (let ((series
                           (loop for name in '("A" "B" "C" "D" "E")
                                 collect (ntop.ui:make-chart-series :name name))))
                      (multiple-value-bind (legend visible)
                          (ntop.ui:chart-legend-layout
                           (%chart-test-rectangle 0 0 20 5)
                           series
                           (ntop.ui:make-chart-config :legend-width-ratio
                                                      1
                                                      :legend-height-ratio
                                                      1))
                        (cl-weave:expect (ntop.ui:rectangle-height legend)
                                         :to-equal
                                         5)
                        (cl-weave:expect (length visible) :to-equal 3))))
                   (cl-weave:it-each
                    (("abc" 3 "abc") ("abcdef" 4 "abc…")
                     ("abcdef" 1 "…")
                     ("abcdef" 0 ""))
                    "truncates ~A to width ~A"
                    (text width expected)
                    (cl-weave:expect (ntop.ui::chart-truncate-text text width)
                                     :to-equal
                                     expected))
                   (cl-weave:it "clips text by terminal cell width"
                                (cl-weave:expect
                                 (ntop.ui::chart-text-width "東A")
                                 :to-equal
                                 3)
                                (cl-weave:expect
                                 (ntop.ui::chart-clip-text "A東B" 3)
                                 :to-equal
                                 "A東"))
                   (cl-weave:it "rejects negative text widths"
                                (cl-weave:expect
                                 (%chart-test-signals-error-p
                                  (lambda ()
                                    (ntop.ui::chart-clip-text "x" -1)))
                                 :to-be
                                 t)))

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
                     ((:theme-name :default))
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

(cl-weave:describe "chart ranges, scales, and Braille mapping"
                   (cl-weave:it "uses configured X bounds"
                                (cl-weave:expect
                                 (ntop.ui::%chart-x-range
                                  (ntop.ui:make-chart-config :x-start
                                                             4
                                                             :x-end
                                                             9)
                                  nil)
                                 :to-equal
                                 '(4 9)))
                   (cl-weave:it "infers X bounds from every series"
                                (let ((series
                                       (list
                                        (ntop.ui:make-chart-series :samples
                                                                   (%chart-test-samples
                                                                    '((3d0 1d0)
                                                                      (8d0 2d0))))
                                        (ntop.ui:make-chart-series :samples
                                                                   (%chart-test-samples
                                                                    '((1d0 3d0)
                                                                      (9d0 4d0)))))))
                                  (cl-weave:expect
                                   (ntop.ui::%chart-x-range
                                    (ntop.ui:make-chart-config)
                                    series)
                                   :to-equal
                                   '(1d0 9d0))))
                   (cl-weave:it "returns no X range without samples"
                                (cl-weave:expect
                                 (ntop.ui::%chart-x-range
                                  (ntop.ui:make-chart-config)
                                  (list (ntop.ui:make-chart-series)))
                                 :to-be
                                 nil))
                   (cl-weave:it-each
                    ((:linear 8d0 8d0) (:log2 8d0 3d0)
                     (:log10 100d0 2d0)
                     (:log2 0d0 0d0)
                     (:log10 -5d0 0d0))
                    "scales ~A value ~A"
                    (scale value expected)
                    (cl-weave:expect (ntop.ui::%scale-chart-value value scale)
                                     :to-equal
                                     expected))
                   (cl-weave:it "interpolates values along a segment"
                                (cl-weave:expect
                                 (ntop.ui::%interpolate-chart-value 0d0
                                                                    0d0
                                                                    10d0
                                                                    100d0
                                                                    2.5d0)
                                 :to-equal
                                 25d0))
                   (cl-weave:it
                    "uses the ending value for equal-time interpolation"
                    (cl-weave:expect
                     (ntop.ui::%interpolate-chart-value 2d0 10d0 2d0 20d0 2d0)
                     :to-equal
                     20d0))
                   (cl-weave:it
                    "clips a segment and interpolates both endpoints"
                    (let ((left
                           (ntop.shared.domain::%make-history-sample 0d0 0d0))
                          (right
                           (ntop.shared.domain::%make-history-sample 10d0 100d0)))
                      (cl-weave:expect
                       (multiple-value-list
                        (ntop.ui::%clip-chart-segment left right '(2d0 8d0)))
                       :to-equal
                       '(2d0 20d0 8d0 80d0))))
                   (cl-weave:it "drops segments outside the visible range"
                                (cl-weave:expect
                                 (multiple-value-list
                                  (ntop.ui::%clip-chart-segment
                                   (ntop.shared.domain::%make-history-sample 0d0
                                                                             1d0)
                                   (ntop.shared.domain::%make-history-sample 1d0
                                                                             2d0)
                                   '(2d0 3d0)))
                                 :to-equal
                                 '(nil)))
                   (cl-weave:it-each ((0d0 0 11) (10d0 11 11))
                                     "maps X endpoint ~A to dot column ~A"
                                     (timestamp expected-x expected-y)
                                     (cl-weave:expect
                                      (multiple-value-list
                                       (ntop.ui::%chart-point timestamp
                                                              0d0
                                                              (ntop.ui:make-chart-config
                                                               :y-min
                                                               0
                                                               :y-max
                                                               100)
                                                              '(0d0 10d0)
                                                              6
                                                              3))
                                      :to-equal
                                      (list expected-x expected-y)))
                   (cl-weave:it "maps a single-time range to its right edge"
                                (cl-weave:expect
                                 (multiple-value-list
                                  (ntop.ui::%chart-point 5d0
                                                         50d0
                                                         (ntop.ui:make-chart-config
                                                          :y-min
                                                          0
                                                          :y-max
                                                          100)
                                                         '(5d0 5d0)
                                                         6
                                                         3))
                                 :to-equal
                                 '(11 6)))
                   (cl-weave:it-each ((0d0 11) (100d0 0) (-10d0 11) (110d0 0))
                                     "clamps Y value ~A to dot row ~A"
                                     (value expected-y)
                                     (cl-weave:expect
                                      (nth-value 1
                                                 (ntop.ui::%chart-point 5d0
                                                                        value
                                                                        (ntop.ui:make-chart-config
                                                                         :y-min
                                                                         0
                                                                         :y-max
                                                                         100)
                                                                        '(0d0
                                                                          10d0)
                                                                        6
                                                                        3))
                                      :to-equal
                                      expected-y))
                   (cl-weave:it-each
                    ((0 0 1) (1 0 8)
                     (0 1 2)
                     (1 1 16)
                     (0 2 4)
                     (1 2 32)
                     (0 3 64)
                     (1 3 128))
                    "maps Braille dot (~A, ~A) to bit ~A"
                    (x y expected-bit)
                    (let ((dots (make-array '(2 4) :initial-element 0))
                          (styles (make-array '(1 1) :initial-element nil)))
                      (cl-weave:expect
                       (ntop.ui::%set-chart-dot dots styles x y :test-style)
                       :to-equal
                       expected-bit)
                      (cl-weave:expect (ntop.ui::%cell-braille-bits dots 0 0)
                                       :to-equal
                                       expected-bit)))
                   (cl-weave:it "combines all Braille dot bits in a cell"
                                (let ((dots
                                       (make-array '(2 4) :initial-element 1)))
                                  (cl-weave:expect
                                   (ntop.ui::%cell-braille-bits dots 0 0)
                                   :to-equal
                                   255)))
                   (cl-weave:it
                    "draws a horizontal Braille line including both endpoints"
                    (let ((dots (make-array '(5 1) :initial-element 0))
                          (styles (make-array '(3 1) :initial-element nil)))
                      (ntop.ui::%draw-braille-line dots
                                                   styles
                                                   0
                                                   0
                                                   4
                                                   0
                                                   :line-style)
                      (cl-weave:expect
                       (loop for x below 5
                             sum (aref dots x 0))
                       :to-equal
                       5)))
                   (cl-weave:it "draws a reversed diagonal Braille line"
                                (let ((dots
                                       (make-array '(4 4) :initial-element 0))
                                      (styles
                                       (make-array '(2 1) :initial-element nil)))
                                  (ntop.ui::%draw-braille-line dots
                                                               styles
                                                               3
                                                               0
                                                               0
                                                               3
                                                               :line-style)
                                  (cl-weave:expect
                                   (loop for x below 4
                                         sum (loop for y below 4
                                                   sum (aref dots x y)))
                                   :to-equal
                                   4))))

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
                    "uses bars when CPU gauge columns cannot fit labels"
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
                       4)
                      (cl-weave:expect
                       (search "CPU" (cl-tty-kit:screen-row-string screen 0))
                       :to-be
                       nil)))
                   (cl-weave:it "uses start labels in narrow CPU gauge columns"
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
                                   (not
                                    (null
                                     (search "CPU"
                                             (cl-tty-kit:screen-row-string
                                              screen
                                              0))))
                                   :to-be
                                   t)))
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
                                        (count #\[
                                               (%chart-test-screen-text screen
                                                                        height))
                                        :to-equal
                                        expected-visible)))
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
                                   (count #\[
                                          (cl-tty-kit:screen-row-string screen
                                                                        0))
                                   :to-equal
                                   4))))
