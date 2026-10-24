(in-package #:ntop.test)

(cl-weave:describe "btm presentation format values"
                   (cl-weave:it-each
                    ((0.0 t "  0.0%") (7.26 t "  7.3%")
                     (99.96 t "100.0%")
                     (0.0 nil "  0%")
                     (7.6 nil "  8%")
                     (99.6 nil "100%"))
                    "formats CPU usage ~A"
                    (usage show-decimal expected)
                    (cl-weave:expect
                     (ntop.ui::format-cpu-basic-percent usage show-decimal)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((0 "0.0%") (2.56 "2.6%") (100 "100.0%") (0.04 "0.0%"))
                    "formats process CPU usage ~A"
                    (usage expected)
                    (cl-weave:expect (ntop.ui:format-percent-value usage)
                                     :to-equal
                                     expected))
                   (cl-weave:it-each
                    ((0 "0.0%") (2.56 "2.6%") (100 "100.0%") (12.34 "12.3%"))
                    "formats process memory percentage ~A"
                    (usage expected)
                    (cl-weave:expect
                     (ntop.ui:format-percent-value usage)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((0 100 "  0%") (49 100 " 49%")
                     (50 100 " 50%")
                     (100 100 "100%"))
                    "formats memory percentage ~A of ~A bytes"
                    (used total expected)
                    (cl-weave:expect
                     (ntop.ui::format-memory-percentage used total)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((512 1024 "0.5KiB/1.0KiB")
                     (1048576 2097152 "1.0MiB/2.0MiB")
                     (1 1 "1.0B/1.0B")
                     (0 1024 "0.0KiB/1.0KiB"))
                    "formats a memory fraction of ~A / ~A bytes"
                    (used total expected)
                    (cl-weave:expect
                     (ntop.ui::format-memory-fraction used total)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    (("RAM" 512 1024 "RAM: 50%   0.5KiB/1.0KiB")
                     ("Swap" 0 1024 "Swap:  0%   0.0KiB/1.0KiB")
                     ("RAM" 0 0 "RAM:   0%   0.0B/0.0B"))
                    "formats the ~A memory legend"
                    (name used total expected)
                    (cl-weave:expect
                     (ntop.ui::format-memory-legend name used total)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((0.01 0.50 1.00 " CPU ─ 0.01 0.50 1.00 ")
                     (2.25 3.50 5.75 " CPU ─ 2.25 3.50 5.75 "))
                    "formats CPU load averages ~A"
                    (one five fifteen expected)
                    (cl-weave:expect
                     (ntop.ui::format-cpu-title one five fifteen)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((0 "0B" "0B" "0B") (1 "1B" "1B" "1B")
                     (1024 "1KiB" "1KB" "1KB")
                     (1000000 "977KiB" "1MB" "1MB")
                     (1000000000 "954MiB" "1.0GB" "1GB")
                     (1073741824 "1.0GiB" "1.1GB" "1GB"))
                    "formats byte count ~A"
                    (bytes binary decimal disk)
                    (progn
                      (cl-weave:expect
                       (ntop.ui::format-process-binary-bytes bytes)
                       :to-equal
                       binary)
                      (cl-weave:expect
                       (ntop.ui::format-process-decimal-bytes bytes)
                       :to-equal
                       decimal)
                      (cl-weave:expect (ntop.ui::format-disk-capacity bytes)
                                       :to-equal
                                       disk)))
                   (cl-weave:it-each
                    ((0 "0B/s") (999 "999B/s")
                     (1000 "1KB/s")
                     (1000000000 "1.0GB/s")
                     (1260000000 "1.3GB/s"))
                    "formats an I/O rate of ~A bytes per second"
                    (rate expected)
                    (progn
                      (cl-weave:expect (ntop.ui::format-disk-io-rate rate)
                                       :to-equal
                                       expected)
                      (cl-weave:expect (ntop.ui::format-process-io-rate rate)
                                       :to-equal
                                       expected)))
                   (cl-weave:it-each
                    ((0 :decimal :bit "0.0b/s") (1000 :decimal :bit "1.0Kb/s")
                     (1024 :binary :byte "1.0KiB/s")
                     (1000000 :decimal :byte "1.0MB/s"))
                    "formats a network rate using ~A prefixes"
                    (rate prefix-system data-unit expected)
                    (cl-weave:expect
                     (ntop.ui::format-network-rate rate prefix-system data-unit)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((0 :decimal "0.0B") (8000 :decimal "1.0KB")
                     (8192 :binary "1.0KiB")
                     (8000000000 :decimal "1.0GB"))
                    "formats network total ~A bits"
                    (bits prefix-system expected)
                    (cl-weave:expect
                     (ntop.ui::format-network-total bits prefix-system)
                     :to-equal
                     expected))
                   (cl-weave:it
                    "left aligns network legend values to their configured widths"
                    (cl-weave:expect
                     (ntop.ui::format-network-legend "RX"
                                                     1000
                                                     8000
                                                     :decimal
                                                     :bit)
                     :to-equal
                     "RX: 1.0Kb/s    All: 1.0KB   "))
                   (cl-weave:it-each
                    ((0 "     0.0B/s") (1024 "   1.0KiB/s")
                     (1073741824 "   1.0GiB/s"))
                    "right aligns disk I/O legend rate ~A"
                    (rate expected)
                    (cl-weave:expect (ntop.ui::format-disk-io-legend-rate rate)
                                     :to-equal
                                     expected))
                   (cl-weave:it-each
                    ((0 :celsius 0 "°C") (20 :fahrenheit 68 "°F")
                     (0 :kelvin 273.15d0 "K")
                     (-40 :fahrenheit -40 "°F"))
                    "converts temperature ~A to ~A"
                    (celsius unit expected-value expected-unit)
                    (progn
                      (cl-weave:expect
                       (ntop.ui::format-temperature-value celsius unit)
                       :to-be
                       expected-value)
                      (cl-weave:expect (ntop.ui::format-temperature-unit unit)
                                       :to-equal
                                       expected-unit)))
                   (cl-weave:it-each
                    ((37.2 :celsius "38°C") (-0.1 :celsius "0°C")
                     (20 :fahrenheit "68°F")
                     (0 :kelvin "274K"))
                    "formats table temperature ~A in ~A"
                    (celsius unit expected)
                    (cl-weave:expect (ntop.ui::format-temperature celsius unit)
                                     :to-equal
                                     expected))
                   (cl-weave:it-each
                    ((36.3 :celsius "36°C") (20 :fahrenheit "68°F")
                     (0 :kelvin "273K"))
                    "formats graph temperature ~A in ~A"
                    (celsius unit expected)
                    (cl-weave:expect
                     (ntop.ui::format-temperature-graph-value celsius unit)
                     :to-equal
                     expected))
                   (cl-weave:it "provides source citations for formatting rules"
                                (cl-weave:expect
                                 (first
                                  (ntop.ui::format-rule-sources
                                   :cpu-basic-percent))
                                 :to-equal
                                 "bottom 0.14.9 src/canvas/widgets/cpu_basic.rs:159-173")))
