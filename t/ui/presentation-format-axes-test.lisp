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

(cl-weave:describe "btm logarithmic axis format values"
  (cl-weave:it-each
   ((0d0 :decimal :bit 6d0 (" 0b" "1Kb" "1Mb"))
    (999d0 :decimal :byte 6d0 (" 0B" "1KB" "1MB"))
    (1d6 :decimal :bit 9d0 (" 0b" "1Kb" "1Mb" "1Gb"))
    (1d9 :decimal :byte 12d0 (" 0B" "1KB" "1MB" "1GB" "1TB"))
    (1d12 :decimal :bit 15d0 (" 0b" "1Kb" "1Mb" "1Gb" "1Tb" "1Pb"))
    (1d16 :decimal :byte 15d0 (" 0B" "1KB" "1MB" "1GB" "1TB" "1PB"))
    (0d0 :binary :byte 20d0 ("  0B" "1KiB" "1MiB"))
    (1d0 :binary :bit 20d0 ("  0b" "1Kib" "1Mib"))
    (1.048576d6 :binary :byte 30d0 ("  0B" "1KiB" "1MiB" "1GiB"))
    (1.073741824d9 :binary :bit 40d0 ("  0b" "1Kib" "1Mib" "1Gib" "1Tib"))
    (1.099511627776d12 :binary :byte 50d0
     ("  0B" "1KiB" "1MiB" "1GiB" "1TiB" "1PiB"))
    (-1d0 :decimal :byte 6d0 (" 0B" "1KB" "1MB")))
   "formats logarithmic network axis for ~A ~A ~A"
   (max-entry prefix-system data-unit expected-upper expected-labels)
   (multiple-value-bind (actual-upper actual-labels)
       (ntop.ui:format-network-log-axis-labels
        max-entry prefix-system data-unit)
     (cl-weave:expect (= actual-upper expected-upper) :to-be t)
     (cl-weave:expect actual-labels :to-equal expected-labels)))
  (cl-weave:it-each
   ((0d0 20d0 ("  0B/s" "1KiB/s" "1MiB/s"))
    (1.048575d6 20d0 ("  0B/s" "1KiB/s" "1MiB/s"))
    (1.048576d6 30d0 ("  0B/s" "1KiB/s" "1MiB/s" "1GiB/s"))
    (1.073741824d9 40d0
     ("  0B/s" "1KiB/s" "1MiB/s" "1GiB/s" "1TiB/s"))
    (1.099511627776d12 50d0
     ("  0B/s" "1KiB/s" "1MiB/s" "1GiB/s" "1TiB/s" "1PiB/s"))
    (1.125899906842624d15 50d0
     ("  0B/s" "1KiB/s" "1MiB/s" "1GiB/s" "1TiB/s" "1PiB/s"))
    (-1d0 20d0 ("  0B/s" "1KiB/s" "1MiB/s")))
   "formats logarithmic disk I/O axis for ~A bytes per second"
   (max-entry expected-upper expected-labels)
   (multiple-value-bind (actual-upper actual-labels)
       (ntop.ui:format-disk-io-log-axis-labels max-entry)
     (cl-weave:expect (= actual-upper expected-upper) :to-be t)
     (cl-weave:expect actual-labels :to-equal expected-labels))))
