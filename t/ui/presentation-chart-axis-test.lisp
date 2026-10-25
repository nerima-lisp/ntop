(in-package #:ntop.test)

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
                    (let ((dots (make-array '(6 4) :initial-element 0))
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
