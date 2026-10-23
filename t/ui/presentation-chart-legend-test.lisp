(in-package #:ntop.test)

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
