(in-package #:ntop.test)

(defun %f4-sample (sampled-at rx-bytes tx-bytes rx-packets tx-packets)
  (let ((interface
         (ntop.network.domain:make-network-interface :name
                                                     "en0"
                                                     :rx-bytes
                                                     rx-bytes
                                                     :tx-bytes
                                                     tx-bytes
                                                     :rx-packets
                                                     rx-packets
                                                     :tx-packets
                                                     tx-packets)))
    (ntop.network.domain:make-network-sample :sampled-at sampled-at :interfaces (list interface))))

(defun %f4-network-presentation-samples ()
  (vector (%f4-sample 1d0 1000 2000 10 20) (%f4-sample 3d0 2100 6000 30 40)))

(defun %f4-state (&rest arguments)
  (apply (function ntop.network.presentation:make-network-presentation-state) arguments))

(defun %f4-snapshot (screen)
  (cl-tty-kit:screen-to-string screen))

(defun %f4-basic-config (&key packets-p start-zeroed-p)
  (ntop.shared.application:make-application-config :basic-p
                                                   t
                                                   :network-show-packets-p
                                                   packets-p
                                                   :network-start-zeroed-p
                                                   start-zeroed-p))

(defun %f4-baseline (state)
  (ntop.network.presentation:network-presentation-state-baseline-sample state))

(defun %f4-packet-visible-p (show-packets-p width)
  (ntop.network.presentation::%network-packet-legend-visible-p show-packets-p width))

(defun %f4-network-screen-lines (screen)
  (with-input-from-string (stream (cl-tty-kit:screen-to-string screen))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim '(#\Space #\Return) line))))

(defun %f4-network-golden-lines ()
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      "t/feature/network/goldens/presentation-50x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

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
                    "uses the shared linear axis formatter and network legend constraints"
                    (let* ((rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 20))
                           (latest (aref (%f4-network-presentation-samples) 1))
                           (config (ntop.shared.application:make-application-config))
                           (chart-config
                            (ntop.network.presentation::%network-chart-config rectangle
                                                                              latest
                                                                              2000d0
                                                                              config)))
                      (cl-weave:expect (ntop.ui:chart-config-y-scale chart-config) :to-be :linear)
                      (cl-weave:expect (ntop.ui:chart-config-y-max chart-config) :to-be 3000d0)
                      (cl-weave:expect (ntop.ui:chart-config-legend-width-ratio chart-config)
                                       :to-be
                                       9/10)
                      (cl-weave:expect (ntop.ui:chart-config-legend-height-ratio chart-config)
                                       :to-be
                                       3/4)))
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
                        (cl-weave:expect (not (null (search "4.4Kb/s" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "16.0Kb/s" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "2.1KB" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "6.0KB" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "10pkt/s" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "55.0B" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "200.0B" snapshot))) :to-be t)
                        (cl-weave:expect (not (null (search "Network" snapshot))) :to-be t))
                      (cl-weave:expect (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
                                       :to-equal
                                       (ntop.ui:presentation-theme-style :default :widget-title))))
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
                                   (not
                                    (null (search "Network" (cl-tty-kit:screen-to-string screen))))
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
                             (ntop.ui:make-rectangle :x
                                                     0
                                                     :y
                                                     0
                                                     :width
                                                     80
                                                     :height
                                                     height))
                            (config
                             (ntop.shared.application:make-application-config
                              :network-use-old-legend-p t
                              :network-show-packets-p t))
                            (samples (%f4-network-presentation-samples)))
                       (ntop.ui:draw-widget :network screen rectangle nil config samples)
                       (let* ((snapshot (cl-tty-kit:screen-to-string screen))
                              (chart-config
                               (ntop.network.presentation::%network-chart-config
                                (ntop.network.presentation::%network-chart-rectangle
                                 rectangle config)
                                (aref samples 1) 16000d0 config)))
                         (cl-weave:expect
                          (not (null (search "RX Pkts" snapshot)))
                          :to-be headers-p)
                         (cl-weave:expect
                          (not (null (search "10pkt/s" snapshot)))
                          :to-be values-p)
                         (cl-weave:expect
                          (not (null (search "55.0B" snapshot)))
                          :to-be values-p)
                         (cl-weave:expect
                          (not (null (search "200.0B" snapshot)))
                          :to-be values-p)
                         (cl-weave:expect
                          (ntop.ui:chart-config-legend-position chart-config)
                          :to-be nil)
                         (cl-weave:expect
                          (ntop.ui:chart-config-legend-width-ratio chart-config)
                          :to-be 0))
                       (dolist (extents (quote ((0 3) (3 0))))
                         (let* ((empty-width (first extents))
                                (empty-height (second extents))
                                (empty-screen (cl-tty-kit:make-screen 8 4))
                                (empty-rectangle
                                 (ntop.ui:make-rectangle
                                  :x 0 :y 0 :width empty-width :height empty-height)))
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
                        (cl-weave:expect (length golden-lines) :to-be 16)
                        (cl-weave:expect
                         (every
                          (lambda (line)
                            (<= (length line) 50))
                          golden-lines)
                         :to-be
                         t)
                        (cl-weave:expect rows :to-equal golden-lines))
                      (cl-weave:expect (ntop.ui:chart-config-y-max chart-config) :to-be 1d0)
                      (cl-weave:expect (ntop.ui:chart-config-legend-width-ratio chart-config)
                                       :to-be
                                       9/10)
                      (cl-weave:expect (ntop.ui:chart-config-legend-height-ratio chart-config)
                                       :to-be
                                       3/4)
                      (cl-weave:expect (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 1 0))
                                       :to-equal
                                       (ntop.ui:presentation-theme-style :default :widget-title))
                      (cl-weave:expect (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 21 2))
                                       :to-equal
                                       (ntop.ui:presentation-theme-style :default :rx))))
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
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
                                       (zeroed-config (%f4-basic-config :start-zeroed-p t))
                                       (state (%f4-state :baseline-sample baseline)))
                                  (ntop.ui:draw-widget :network
                                                       zeroed-screen
                                                       rectangle
                                                       state
                                                       zeroed-config
                                                       samples)
                                  (cl-weave:expect (%f4-baseline state) :to-be baseline)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "1.6KB" (cl-tty-kit:screen-to-string zeroed-screen))))
                                   :to-be
                                   t)
                                  (let* ((ordinary-screen (cl-tty-kit:make-screen 80 8))
                                         (ordinary-state (%f4-state))
                                         (ordinary-config (%f4-basic-config)))
                                    (ntop.ui:draw-widget :network
                                                         ordinary-screen
                                                         rectangle
                                                         ordinary-state
                                                         ordinary-config
                                                         samples)
                                    (cl-weave:expect (%f4-baseline ordinary-state) :to-be nil)
                                    (cl-weave:expect
                                     (not
                                      (null
                                       (search "2.1KB"
                                               (cl-tty-kit:screen-to-string ordinary-screen))))
                                     :to-be
                                     t))))
                   (cl-weave:it-each ((6 t nil nil) (7 t nil t) (7 nil nil t) (7 nil t nil))
                                     "honors time-label visibility at widget height ~A"
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
                                        (not (null (search "-60s" (%f4-snapshot screen))))
                                        :to-be
                                        visible-p)))
                   (cl-weave:it "renders packet details beside rates in narrow basic widgets"
                                (let* ((screen (cl-tty-kit:make-screen 50 8))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x 0 :y 0 :width 50 :height 8))
                                       (config (%f4-basic-config :packets-p t))
                                       (samples (%f4-network-presentation-samples)))
                                  (ntop.ui:draw-widget :network screen rectangle nil config samples)
                                  (let ((snapshot (cl-tty-kit:screen-to-string screen)))
                                    (cl-weave:expect (not (null (search "RX: 4.4Kb/s" snapshot)))
                                                     :to-be
                                                     t)
                                    (cl-weave:expect
                                     (not (null (search "RX Pkt: 10pkt/s" snapshot)))
                                     :to-be
                                     t)
                                    (cl-weave:expect
                                     (not (null (search "Total RX: 2.1KB" snapshot)))
                                     :to-be
                                     t)
                                    (cl-weave:expect (not (null (search "Avg RX: 55.0B" snapshot)))
                                                     :to-be
                                                     t)))))
