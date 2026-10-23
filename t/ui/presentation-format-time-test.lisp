(in-package #:ntop.test)

(cl-weave:describe "process elapsed time formatting"
                   (cl-weave:it-each
                    ((0 "0.000s") (500 "0.500s")
                     (900 "0.900s")
                     (1000 "1.000s")
                     (10000 "10.000s")
                     (59999 "59.999s"))
                    "formats elapsed milliseconds ~A below one minute"
                    (elapsed-milliseconds expected)
                    (cl-weave:expect
                     (ntop.ui::format-process-time
                      (ntop.shared.domain:make-quantity elapsed-milliseconds
                                                        :millisecond))
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((60000 "1m 0.00s") (61000 "1m 1.00s")
                     (61999 "1m 1.99s")
                     (600000 "10m 0.00s")
                     (601000 "10m 1.00s")
                     (3599999 "59m 59.99s"))
                    "formats elapsed milliseconds ~A in minutes"
                    (elapsed-milliseconds expected)
                    (cl-weave:expect
                     (ntop.ui::format-process-time
                      (ntop.shared.domain:make-quantity elapsed-milliseconds
                                                        :millisecond))
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((3600000 "1h 0m 0s") (3600999 "1h 0m 0s")
                     (3601000 "1h 0m 1s")
                     (3660000 "1h 1m 0s")
                     (3661000 "1h 1m 1s")
                     (86399000 "23h 59m 59s"))
                    "formats elapsed milliseconds ~A in hours"
                    (elapsed-milliseconds expected)
                    (cl-weave:expect
                     (ntop.ui::format-process-time
                      (ntop.shared.domain:make-quantity elapsed-milliseconds
                                                        :millisecond))
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((86400000 "1d 0h 0m") (86401000 "1d 0h 0m")
                     (86460000 "1d 0h 1m")
                     (89999000 "1d 0h 59m")
                     (90000000 "1d 1h 0m")
                     (31535999000 "364d 23h 59m"))
                    "formats elapsed milliseconds ~A in days"
                    (elapsed-milliseconds expected)
                    (cl-weave:expect
                     (ntop.ui::format-process-time
                      (ntop.shared.domain:make-quantity elapsed-milliseconds
                                                        :millisecond))
                     :to-equal
                     expected)))

(cl-weave:describe "process time accepts shared duration quantities"
                   (cl-weave:it-each
                    ((1 :second "1.000s") (0.5 :second "0.500s")
                     (900000 :nanosecond "0.000s")
                     (59999999 :nanosecond "0.059s")
                     (120 :second "2m 0.00s")
                     (500 :millisecond "0.500s"))
                    "converts ~A ~A to process time"
                    (value unit expected)
                    (cl-weave:expect
                     (ntop.ui::format-process-time
                      (ntop.shared.domain:make-quantity value unit))
                     :to-equal
                     expected))
                   (cl-weave:it "rejects quantities outside the time dimension"
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (ntop.ui::format-process-time
                                                  (ntop.shared.domain:make-quantity
                                                   1
                                                   :byte))
                                                 :returned)
                                   (error ()
                                     :signaled))
                                 :to-be
                                 :signaled)))
