(in-package #:ntop.test)

(cl-weave:describe "btm presentation axis format values"
                   (cl-weave:it-each
                    ((50 :celsius 100 100 ("0°C" "50°C" "100°C"))
                     (70 :fahrenheit nil 212 ("0°F" "106°F" "212°F"))
                     (50 :celsius 90 90 ("0°C" "45°C" "90°C"))
                     (0 :kelvin nil 373.15d0 ("0K" "187K" "374K")))
                    "formats ~A axis labels in ~A"
                    (max-entry unit upper-limit expected-upper expected-labels)
                    (cl-weave:expect
                     (multiple-value-list
                      (ntop.ui::format-temperature-axis-labels max-entry
                                                               unit
                                                               upper-limit))
                     :to-equal
                     (list expected-upper expected-labels)))
                   (cl-weave:it-each
                    ((0 :decimal :bit 1d0 ("0b" "  0.0" "  0.0" "  0.0"))
                     (1000 :decimal :bit 1500d0 ("0Kb" "  0.5" "  1.0" "  1.5"))
                     (1048576 :binary
                              :byte
                              1572864d0
                              ("0MiB" "  0.5" "  1.0" "  1.5"))
                     (1000000 :decimal
                              :byte
                              1500000d0
                              ("0MB" "  0.5" "  1.0" "  1.5")))
                    "formats network axis for ~A"
                    (max-entry prefix-system
                               data-unit
                               expected-upper
                               expected-labels)
                    (multiple-value-bind (actual-upper actual-labels)
                      (ntop.ui::format-network-axis-labels max-entry
                                                           prefix-system
                                                           data-unit)
                      (cl-weave:expect (= actual-upper expected-upper) :to-be t)
                      (cl-weave:expect actual-labels :to-equal expected-labels)))
                   (cl-weave:it-each
                    ((0 1d0 ("0B/s" "     0.0" "     0.0" "     0.0"))
                     (1024 1536d0 ("0KiB/s" "     0.5" "     1.0" "     1.5"))
                     (1048576 1572864d0
                              ("0MiB/s" "     0.5" "     1.0" "     1.5"))
                     (1073741824 1610612736d0
                                 ("0GiB/s" "     0.5" "     1.0" "     1.5")))
                    "formats disk I/O axis for ~A bytes per second"
                    (max-entry expected-upper expected-labels)
                    (multiple-value-bind (actual-upper actual-labels)
                      (ntop.ui::format-disk-io-axis-labels max-entry)
                      (cl-weave:expect (= actual-upper expected-upper) :to-be t)
                      (cl-weave:expect actual-labels :to-equal expected-labels))))
