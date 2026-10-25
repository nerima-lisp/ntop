(in-package #:ntop.test)

;; BTM 0.14.9 widget ratios: bottom/src/canvas/widgets/mem_graph.rs:198-201,
;; bottom/src/canvas/widgets/network_graph.rs:85-100,
;; bottom/src/canvas/widgets/temperature_graph.rs:44-49, and
;; bottom/src/canvas/widgets/disk_io_graph.rs:111-114. Vendored legend sizing
;; is in bottom/src/canvas/components/time_series/vendored.rs:604-640.
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
                    ((:memory 3/4 3/4 30 15)
                     (:network 9/10 3/4 36 15)
                     (:temperature 9/10 1/2 36 10)
                     (:disk-io 9/10 1/2 36 10))
                    "~A widget ratios cap the legend at ~D by ~D"
                    (widget width-ratio height-ratio expected-width
                            expected-height-limit)
                    (let* ((long-name (make-string 40 :initial-element #\x))
                           (series
                            (cons (ntop.ui:make-chart-series :name long-name)
                                  (loop repeat 19
                                        collect
                                        (ntop.ui:make-chart-series :name "A"))))
                           (config
                            (ntop.ui:make-chart-config
                             :legend-width-ratio width-ratio
                             :legend-height-ratio height-ratio))
                           (area (%chart-test-rectangle 0 0 40 20)))
                      (multiple-value-bind (legend visible)
                          (ntop.ui:chart-legend-layout area series config)
                        (cl-weave:expect
                         (list (ntop.ui:rectangle-x legend)
                               (ntop.ui:rectangle-width legend)
                               (ntop.ui:rectangle-height legend)
                               (length visible))
                         :to-equal
                         (list (- 40 expected-width)
                               expected-width
                               expected-height-limit
                               (- expected-height-limit 2)))
                        (cl-weave:expect expected-height-limit
                                         :to-equal
                                         (floor (* 20 height-ratio))))))
                   (cl-weave:it
                    "requires room for the shortest legend name at its padding boundary"
                    (multiple-value-bind (legend visible)
                        (ntop.ui:chart-legend-layout
                         (%chart-test-rectangle 0 0 10 4)
                         (list (ntop.ui:make-chart-series :name "A"))
                         (ntop.ui:make-chart-config
                          :legend-width-ratio 1/5
                          :legend-height-ratio 1))
                      (cl-weave:expect legend :to-be nil)
                      (cl-weave:expect visible :to-be nil))
                    (multiple-value-bind (legend visible)
                        (ntop.ui:chart-legend-layout
                         (%chart-test-rectangle 0 0 15 4)
                         (list (ntop.ui:make-chart-series :name "A"))
                         (ntop.ui:make-chart-config
                          :legend-position :top-right
                          :legend-width-ratio 1/5
                          :legend-height-ratio 1))
                      (cl-weave:expect
                       (list (ntop.ui:rectangle-x legend)
                             (ntop.ui:rectangle-y legend)
                             (ntop.ui:rectangle-width legend)
                             (ntop.ui:rectangle-height legend))
                       :to-equal
                       (list 12 0 3 3))
                      (cl-weave:expect
                       (mapcar (lambda (item)
                                 (ntop.ui:chart-series-name item))
                               visible)
                       :to-equal
                       (list "A"))))
                   (cl-weave:it
                    "sizes a legend from visible names, excluding hidden entries"
                    (let ((series
                           (mapcar
                            (lambda (name)
                              (ntop.ui:make-chart-series :name name))
                            '("AB" "CD" "long hidden name"))))
                      (multiple-value-bind (legend visible)
                          (ntop.ui:chart-legend-layout
                           (%chart-test-rectangle 0 0 30 4)
                           series
                           (ntop.ui:make-chart-config
                            :legend-width-ratio 1
                            :legend-height-ratio 1))
                        (cl-weave:expect
                         (list (ntop.ui:rectangle-x legend)
                               (ntop.ui:rectangle-width legend)
                               (length visible)
                               (mapcar #'ntop.ui:chart-series-name visible))
                         :to-equal
                         (list 26 4 2 '("AB" "CD"))))))
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
