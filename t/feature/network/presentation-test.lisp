(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load
   (merge-pathnames
    "helpers-presentation.lisp"
    (uiop:pathname-directory-pathname
     (asdf:component-pathname
      (asdf:find-component "ntop/test"
                           "t/feature/network/presentation-test"))))))

(cl-weave:describe "network presentation"
                   (cl-weave:it-each ((93 nil) (94 nil) (95 t))
                                     "uses the btm packet detail width threshold at ~A columns"
                                     (width expected)
                                     (cl-weave:expect (%f4-packet-visible-p t width)
                                                      :to-be
                                                      expected))
                   (cl-weave:it-each
                    ((:bit :decimal 1000 "1.0Kb/s") (:byte :decimal 1000 "1.0KB/s")
                     (:bit :binary 1024 "1.0Kib/s")
                     (:byte :binary 1024 "1.0KiB/s"))
                    "uses shared network rate formatting for ~A with ~A prefixes"
                    (unit prefix value expected)
                    (let* ((config
                            (ntop.shared.application:make-application-config :data-unit
                                                                             unit
                                                                             :binary-prefix-p
                                                                             (eq prefix :binary)))
                           (quantity (ntop.shared.domain:make-quantity value unit)))
                      (cl-weave:expect
                       (ntop.network.presentation::%network-rate-text quantity prefix config)
                       :to-equal
                       expected)))
                   (cl-weave:it-each ((1000 :decimal "1.0KB") (1024 :binary "1.0KiB"))
                                     "formats raw byte totals with the shared formatter"
                                     (bytes prefix expected)
                                     (cl-weave:expect
                                      (ntop.network.presentation::%network-total-text bytes prefix)
                                      :to-equal
                                      expected))
                   (cl-weave:it-each ((5 2 "2.5B") (1 2 "0.5B"))
                                     "shows fractional average packet size ~A"
                                     (byte-delta packet-delta expected)
                                     (let* ((screen (cl-tty-kit:make-screen 80 8))
                                            (rectangle
                                             (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
                                            (config
                                             (ntop.shared.application:make-application-config
                                              :basic-p
                                              t
                                              :network-show-packets-p
                                              t))
                                            (samples
                                             (vector (%f4-sample 1d0 100 100 10 10)
                                                     (%f4-sample 3d0
                                                                 (+ 100 byte-delta)
                                                                 100
                                                                 (+ 10 packet-delta)
                                                                 10))))
                                       (ntop.ui:draw-widget :network
                                                            screen
                                                            rectangle
                                                            nil
                                                            config
                                                            samples)
                                       (cl-weave:expect
                                        (not
                                         (null
                                          (search expected (cl-tty-kit:screen-to-string screen))))
                                        :to-be
                                        t)))
                   (cl-weave:it
  "uses shared chart axes and matches BTM legend formatting"
  (let* ((rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 20))
         (latest (aref (%f4-network-presentation-samples) 1))
         (config (ntop.shared.application:make-application-config))
         (chart-config
           (ntop.network.presentation::%network-chart-config rectangle
                                                             latest
                                                             2000d0
                                                             config))
         (packet-legend
           (ntop.network.presentation::%network-legend-name
            "RX" nil 1024 nil nil :rx t config)))
    (cl-weave:expect
     (list (ntop.ui:chart-config-y-scale chart-config)
           (ntop.ui:chart-config-y-max chart-config)
           (ntop.ui:chart-config-legend-width-ratio chart-config)
           (ntop.ui:chart-config-legend-height-ratio chart-config))
     :to-equal
     (quote (:linear 3000d0 9/10 3/4)))
    (cl-weave:expect
     (ntop.network.presentation::%network-legend-name
      "RX" nil 1024 nil nil :rx nil config)
     :to-equal
     "RX: N/A        All: 1.0KB")
    (cl-weave:expect
     (not (null (search "All: 1.0KB    Packets:" packet-legend)))
     :to-be
     t)))
                   (cl-weave:it-each
                    ((:decimal-below 999999d0 nil 6d0 :log10)
                     (:decimal-edge 1000000d0 nil 9d0 :log10)
                     (:binary-below 1048575d0 t 20d0 :log2)
                     (:binary-edge 1048576d0 t 30d0 :log2))
                    "uses btm log axis threshold case ~A"
                    (case-name maximum binary-prefix-p expected-maximum expected-scale)
                    (let* ((rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 20))
                           (latest (aref (%f4-network-presentation-samples) 1))
                           (config
                            (ntop.shared.application:make-application-config :network-use-log-p
                                                                             t
                                                                             :binary-prefix-p
                                                                             binary-prefix-p))
                           (chart-config
                            (ntop.network.presentation::%network-chart-config rectangle
                                                                              latest
                                                                              maximum
                                                                              config)))
                      (cl-weave:expect (ntop.ui:chart-config-y-scale chart-config)
                                       :to-be
                                       expected-scale)
                      (cl-weave:expect (ntop.ui:chart-config-y-max chart-config)
                                       :to-be
                                       expected-maximum)))
                   (cl-weave:it
  "renders byte and packet rates with average packet sizes in basic mode"
  (let* ((screen (cl-tty-kit:make-screen 80 8))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
         (config (%f4-basic-config :packets-p t))
         (samples (%f4-network-presentation-samples)))
    (ntop.ui:draw-widget :network screen rectangle nil config samples)
    (let ((snapshot (cl-tty-kit:screen-to-string screen)))
      (cl-weave:expect
       (mapcar
        (lambda (text) (not (null (search text snapshot))))
        (quote ("550.0/s" "2.0K/s" "2.1KB" "6.0KB"
                "10pkt/s" "55.0B" "200.0B" "Network")))
       :to-equal
       (quote (t t t t t t t t))))
    (cl-weave:expect
     (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
     :to-equal
     (ntop.ui:presentation-theme-style :default :widget-title))))
                   (cl-weave:it
  "uses BTM RX and TX colors for basic rate values without bold"
  (let* ((screen (cl-tty-kit:make-screen 80 8))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
         (config (%f4-basic-config))
         (samples (%f4-network-presentation-samples)))
    (ntop.ui:draw-widget :network screen rectangle nil config samples)
    (cl-weave:expect
     (list (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 5 0))
           (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 5 1)))
     :to-equal
     (quote (((:fg 13)) ((:fg 11)))))))
                   (cl-weave:it "keeps basic text inside a small widget rectangle"
                                (let* ((screen (cl-tty-kit:make-screen 20 8))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 3 :y 1 :width 8 :height 4))
                                       (config (%f4-basic-config)))
                                  (ntop.ui:draw-widget :network
                                                       screen
                                                       rectangle
                                                       nil
                                                       config
                                                       (%f4-network-presentation-samples))
                                  (cl-weave:expect
                                   (let ((snapshot (cl-tty-kit:screen-to-string screen)))
                                     (not
                                      (or (search "Network" snapshot)
                                          (find-if
                                           (lambda (character)
                                             (find character "┌┐└┘─│"))
                                           snapshot))))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (loop for row below 8
                                         always (every
                                                 (lambda (character)
                                                   (char= character #\Space))
                                                 (subseq (cl-tty-kit:screen-row-string screen row)
                                                         11
                                                         20)))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (loop for row from 5 below 8
                                         always (every
                                                 (lambda (character)
                                                   (char= character #\Space))
                                                 (cl-tty-kit:screen-row-string screen row)))
                                   :to-be
                                   t)))
                   (cl-weave:it-each ((2 nil nil) (3 t nil) (4 t t))
  "lays out old packet legend at height ~A and skips empty bounds"
  (height headers-p values-p)
  (let* ((screen (cl-tty-kit:make-screen 80 height))
         (rectangle
           (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height height))
         (config
           (ntop.shared.application:make-application-config
            :network-use-old-legend-p t
            :network-show-packets-p t))
         (samples (%f4-network-presentation-samples)))
    (ntop.ui:draw-widget :network screen rectangle nil config samples)
    (let* ((snapshot (cl-tty-kit:screen-to-string screen))
           (chart-config
             (ntop.network.presentation::%network-chart-config
              (ntop.network.presentation::%network-chart-rectangle rectangle config)
              (aref samples 1) 16000d0 config)))
      (cl-weave:expect
       (list (not (null (search "RX Pkts" snapshot)))
             (list (not (null (search "10pkt/s" snapshot)))
                   (not (null (search "55.0B" snapshot)))
                   (not (null (search "200.0B" snapshot)))
                   (ntop.ui:chart-config-legend-position chart-config)
                   (ntop.ui:chart-config-legend-width-ratio chart-config)))
       :to-equal
       (list headers-p
             (list values-p values-p values-p nil 0))))
    (dolist (extents (quote ((0 3) (3 0))))
      (let* ((empty-width (first extents))
             (empty-height (second extents))
             (empty-screen (cl-tty-kit:make-screen 8 4))
             (empty-rectangle
               (ntop.ui:make-rectangle :x 0 :y 0 :width empty-width :height empty-height)))
        (ntop.network.presentation::%draw-network-old-legend
         empty-screen empty-rectangle
         (aref samples 1) (aref samples 0) nil config)
        (cl-weave:expect
         (loop for line in (%f4-network-screen-lines empty-screen)
               always (string= line ""))
         :to-be
         t)))))
                   (cl-weave:it
  "renders the shared chart, legend, and formatter to the screen golden"
  (let* ((screen (cl-tty-kit:make-screen 50 16))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 50 :height 16))
         (config (ntop.shared.application:make-application-config))
         (samples (vector (%f4-sample 2d0 1000 2000 10 20)))
         (chart-config
           (ntop.network.presentation::%network-chart-config rectangle
                                                             (aref samples 0)
                                                             0d0
                                                             config)))
    (ntop.ui:draw-widget :network screen rectangle nil config samples)
    (let ((rows (%f4-network-screen-lines screen))
          (golden-lines (%f4-network-golden-lines)))
      (cl-weave:expect
       (list (length golden-lines)
             (every (lambda (line) (<= (length line) 50)) golden-lines)
             rows)
       :to-equal
       (list 16 t golden-lines)))
    (cl-weave:expect
     (list (ntop.ui:chart-config-y-max chart-config)
           (ntop.ui:chart-config-legend-width-ratio chart-config)
           (ntop.ui:chart-config-legend-height-ratio chart-config))
     :to-equal
     (quote (1d0 9/10 3/4)))
    (cl-weave:expect
     (list (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 1 0))
           (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 24 2)))
     :to-equal
     (list (ntop.ui:presentation-theme-style :default :widget-title)
           (ntop.ui:presentation-theme-style :default :rx)))))
                   (cl-weave:it "rejects invalid network presentation states"
                                (cl-weave:signals simple-error
                                                  (%f4-state :baseline-sample :invalid))
                                (let* ((screen (cl-tty-kit:make-screen 50 8))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 0 :y 0 :width 50 :height 8))
                                       (config (ntop.shared.application:make-application-config))
                                       (samples (%f4-network-presentation-samples)))
                                  (cl-weave:signals simple-error
                                                    (ntop.ui:draw-widget :network
                                                                         screen
                                                                         rectangle
                                                                         :invalid
                                                                         config
                                                                         samples))))
                   (cl-weave:it "does not render a chart legend for none"
                                ;; BTM 0.14.9: src/canvas/components/time_series/
                                ;; vendored.rs:535-549, 604-640 and 1475-1488
                                ;; skip legend layout and drawing for None.
                                (let* ((screen (cl-tty-kit:make-screen 80 20))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 0 :y 0
                                                               :width 80
                                                               :height 20))
                                       (config
                                        (ntop.shared.application:make-application-config
                                         :network-legend-position :none))
                                       (samples
                                        (%f4-network-presentation-samples))
                                       (chart-config
                                        (ntop.network.presentation::%network-chart-config
                                         rectangle
                                         (aref samples 1)
                                         16000d0
                                         config)))
                                  (cl-weave:expect
                                   (ntop.ui:chart-config-legend-position
                                    chart-config)
                                   :to-be
                                   nil)
                                  (ntop.ui:draw-widget :network
                                                       screen
                                                       rectangle
                                                       nil
                                                       config
                                                       samples)
                                  (let ((rendered
                                          (cl-tty-kit:screen-to-string screen)))
                                    (cl-weave:expect (search "RX" rendered)
                                                     :to-be
                                                     nil)
                                    (cl-weave:expect (search "TX" rendered)
                                                     :to-be
                                                     nil))))
                   (cl-weave:it "captures the first sample as the zero-based display baseline"
                                (let* ((screen (cl-tty-kit:make-screen 80 8))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
                                       (config (%f4-basic-config :start-zeroed-p t))
                                       (state (%f4-state))
                                       (samples (%f4-network-presentation-samples)))
                                  (ntop.ui:draw-widget :network
                                                       screen
                                                       rectangle
                                                       state
                                                       config
                                                       samples)
                                  (cl-weave:expect (%f4-baseline state) :to-be (aref samples 0))
                                  (cl-weave:expect
                                   (not
                                    (null (search "1.1KB" (cl-tty-kit:screen-to-string screen))))
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (not
                                    (null (search "4.0KB" (cl-tty-kit:screen-to-string screen))))
                                   :to-be
                                   t)))
                   (cl-weave:it "preserves explicit baselines and leaves ordinary totals unzeroed"
  (let* ((samples (%f4-network-presentation-samples))
         (baseline (%f4-sample 0d0 500 1000 0 0))
         (zeroed-screen (cl-tty-kit:make-screen 80 8))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
         (zeroed-config (%f4-basic-config :start-zeroed-p t))
         (state (%f4-state :baseline-sample baseline)))
    (ntop.ui:draw-widget :network zeroed-screen rectangle state zeroed-config samples)
    (cl-weave:expect (%f4-baseline state) :to-be baseline)
    (cl-weave:expect
     (not (null (search "1.6KB" (cl-tty-kit:screen-to-string zeroed-screen))))
     :to-be
     t)
    (let* ((ordinary-screen (cl-tty-kit:make-screen 80 8))
           (ordinary-state (%f4-state))
           (ordinary-config (%f4-basic-config)))
      (ntop.ui:draw-widget :network ordinary-screen rectangle
                           ordinary-state ordinary-config samples)
      (cl-weave:expect
       (list (%f4-baseline ordinary-state)
             (not (null (search "2.1KB" (cl-tty-kit:screen-to-string ordinary-screen)))))
       :to-equal
       (quote (nil t))))))
                   (cl-weave:it-each ((6 t nil nil) (7 t nil t) (7 nil nil t) (7 nil t nil))
                                     "matches BTM's height-7 time-label boundary at height ~A"
                                     (height auto-hide-p hide-time-p visible-p)
                                     (let* ((screen (cl-tty-kit:make-screen 50 height))
                                            (rectangle
                                             (ntop.ui:make-rectangle :x
                                                                     0
                                                                     :y
                                                                     0
                                                                     :width
                                                                     50
                                                                     :height
                                                                     height))
                                            (config
                                             (ntop.shared.application:make-application-config
                                              :auto-hide-time-p
                                              auto-hide-p
                                              :hide-time-p
                                              hide-time-p
                                              :time-window-seconds
                                              60))
                                            (samples (vector (%f4-sample 3d0 1000 2000 10 20))))
                                       (ntop.ui:draw-widget :network
                                                            screen
                                                            rectangle
                                                            nil
                                                            config
                                                            samples)
                                       (cl-weave:expect
                                        (and (not (null (search "60s" (%f4-snapshot screen))))
     (not (null (search "0s" (%f4-snapshot screen)))))
                                        :to-be
                                        visible-p)))
                   (cl-weave:it
 "renders packet details beside rates in narrow basic widgets"
 (let* ((screen (cl-tty-kit:make-screen 50 8))
        (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 50 :height 8))
        (config (%f4-basic-config :packets-p t))
        (samples (%f4-network-presentation-samples)))
   (ntop.ui:draw-widget :network screen rectangle nil config samples)
   (let ((snapshot (cl-tty-kit:screen-to-string screen)))
     (cl-weave:expect
      (mapcar
       (lambda (text) (not (null (search text snapshot))))
       '("RX: 550.0/s" "RX Pkt: 10pkt/s" "Total RX: 2.1KB" "Avg RX: 55.0B"))
      :to-equal
      '(t t t t))))
)
                   (cl-weave:it
                    "caps logarithmic axis labels at the last declared prefix"
                    (multiple-value-bind (maximum labels)
                        (ntop.network.presentation::%network-log-axis-data 1d100 :decimal :byte)
                      (cl-weave:expect maximum :to-be 15d0)
                      (cl-weave:expect labels
                                       :to-equal
                                       (quote (" 0B" "1KB" "1MB" "1GB" "1TB")))))
                   (cl-weave:it
                    "omits packet columns from the old legend when packet collection is disabled"
                    (let* ((screen (cl-tty-kit:make-screen 80 4))
                           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 4))
                           (config (ntop.shared.application:make-application-config
                                    :network-use-old-legend-p t))
                           (samples (%f4-network-presentation-samples)))
                      (ntop.network.presentation::%draw-network-old-legend
                       screen rectangle (aref samples 1) (aref samples 0) nil config)
                      (let ((snapshot (cl-tty-kit:screen-to-string screen)))
                        (cl-weave:expect (not (null (search "Total RX" snapshot))) :to-be t)
                        (cl-weave:expect (search "RX Pkts" snapshot) :to-be nil))))
                   (cl-weave:it
 "keeps cumulative totals when the first sample cannot provide rates"
 (let* ((config
         (ntop.shared.application:make-application-config
          :basic-p t
          :network-show-packets-p t))
        (latest (%f4-sample 2d0 1000 2000 10 20))
        (values
         (ntop.network.presentation::%network-legend-values
          latest nil nil config t)))
   (cl-weave:expect (subseq values 0 2) :to-equal '("N/A" "N/A"))
   (cl-weave:expect
    (every (lambda (value) (string= value "N/A")) (subseq values 4 8))
    :to-be
    t)
   (cl-weave:expect
    (every (lambda (value) (not (string= value "N/A"))) (subseq values 2 4))
    :to-be
    t)))
                   (cl-weave:it-each ((0 4) (10 0))
                     "leaves an empty basic widget rectangle blank (~Ax~A)"
                     (width height)
                     (let* ((screen (cl-tty-kit:make-screen 20 8))
                            (rectangle
                             (ntop.ui:make-rectangle :x 2 :y 1 :width width :height height))
                            (config (%f4-basic-config)))
                       (ntop.network.presentation::%draw-network-basic
                        screen rectangle nil nil nil config)
                       (cl-weave:expect
                        (every (lambda (line) (string= line ""))
                               (%f4-network-screen-lines screen))
                        :to-be
                        t)))
                   (cl-weave:it
    "clips basic network output at empty content and render bounds"
    (let* ((config (ntop.shared.application:make-application-config :basic-p t :data-unit :byte))
           (rate (ntop.shared.domain:make-quantity 1000 :byte))
           (screen (cl-tty-kit:make-screen 10 1))
           (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 10 :height 1))
           (packet-config (%f4-basic-config :packets-p t))
           (empty-screen (cl-tty-kit:make-screen 8 3))
           (clip-rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 4 :height 2))
           (clip-config (%f4-basic-config))
           (style (ntop.ui:presentation-theme-style
                   (ntop.shared.application:application-config-theme clip-config) :rx))
           (before (cl-tty-kit:screen-to-string empty-screen)))
      (cl-weave:expect
       (ntop.network.presentation::%network-rate-text rate :decimal config)
       :to-equal "1.0K/s")
      (ntop.network.presentation::%draw-network-basic
       screen rectangle nil nil nil packet-config)
      (let ((snapshot (cl-tty-kit:screen-to-string screen)))
        (cl-weave:expect (search "Network" snapshot) :to-be-truthy)
        (cl-weave:expect (search "RX:" snapshot) :to-be-null))
      (ntop.network.presentation::%network-write-text
       empty-screen clip-rectangle 4 0 "right edge" 4 style)
      (ntop.network.presentation::%network-write-text
       empty-screen clip-rectangle 0 2 "bottom edge" 4 style)
      (cl-weave:expect
       (cl-tty-kit:screen-to-string empty-screen)
       :to-equal before))))
