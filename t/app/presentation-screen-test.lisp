(in-package #:ntop.test)

(defvar *wp12b-screen-widget-calls* nil)
(defvar *wp12b-screen-recording-p* nil)
(defmethod ntop.ui:draw-widget :around (widget-key target rectangle state config series)
  (when *wp12b-screen-recording-p*
    (push (list widget-key rectangle series state config)
          *wp12b-screen-widget-calls*))
  (call-next-method))

(defun %wp12b-screen-history (values)
  (let ((history
         (ntop.shared.domain:make-history-store 1
                                                :retention-seconds
                                                10d0
                                                :sample-interval-seconds
                                                1d0
                                                :initial-window-seconds
                                                10d0
                                                :minimum-window-seconds
                                                1d0)))
    (loop for value in values
          for index from 0
          do (ntop.shared.domain:history-append history
                                                0
                                                (ntop.shared.domain:make-sample-record
                                                 :sampled-at
                                                 (float (1+ index) 1d0))
                                                (float value 1d0)))
    (ntop.shared.domain:history-slice history 0)))
(defun %wp12b-screen-network-sample (sampled-at rx tx)
  (ntop.network.domain:make-network-sample :sampled-at
                                           sampled-at
                                           :interfaces
                                           (list
                                            (ntop.network.domain:make-network-interface
                                             :name
                                             "en0"
                                             :rx-bytes
                                             rx
                                             :tx-bytes
                                             tx
                                             :rx-packets
                                             12
                                             :tx-packets
                                             15))))
(defun %wp12b-screen-previous-process-sample ()
  (ntop.process.domain:make-process-sample :sampled-at                                           2d0
                                           :processes
                                           (list
                                            (ntop.process.domain:make-process-record
                                             :pid
                                             100
                                             :cpu-time-ns
                                             100000000
                                             :read-bytes
                                             6000
                                             :write-bytes
                                             1500))))

(defun %wp12b-screen-fixture-snapshot ()
  (ntop.app.domain:make-collection-snapshot :sampled-at
                                            3d0
                                            :cpu-sample
                                            (ntop.cpu.domain:make-cpu-sample
                                             :sampled-at
                                             3d0
                                             :core-values
                                             (list
                                              (ntop.cpu.domain:make-cpu-core-value
                                               :core-index
                                               0
                                               :usage-percent
                                               35d0
                                               :frequency-mhz
                                               2200d0)
                                              (ntop.cpu.domain:make-cpu-core-value
                                               :core-index
                                               1
                                               :usage-percent
                                               62d0
                                               :frequency-mhz
                                               2100d0))
                                             :average-usage-percent
                                             48d0
                                             :load-average-1
                                             0.25d0
                                             :load-average-5
                                             0.5d0
                                             :load-average-15
                                             0.75d0)
                                            :memory-sample
                                            (ntop.memory.domain:make-memory-sample
                                             :sampled-at
                                             3d0
                                             :ram-used-bytes
                                             (* 5 1024 1024 1024)
                                             :ram-total-bytes
                                             (* 8 1024 1024 1024)
                                             :swap-used-bytes
                                             (* 1 1024 1024 1024)
                                             :swap-total-bytes
                                             (* 2 1024 1024 1024)
                                             :cache-used-bytes
                                             (* 2 1024 1024 1024)
                                             :cache-total-bytes
                                             (* 8 1024 1024 1024))
                                            :network-sample
                                            (%wp12b-screen-network-sample 3d0
                                                                          2400
                                                                          5200)
                                            :disk-sample
                                            (ntop.disk.domain:make-disk-sample
                                             :sampled-at
                                             3d0
                                             :mounts
                                             (list
                                              (ntop.disk.domain:make-disk-mount
                                               :device-name
                                               "disk0s1"
                                               :mount-path
                                               "/"
                                               :filesystem-type
                                               "apfs"
                                               :used-bytes
                                               (* 90 1024 1024 1024)
                                               :free-bytes
                                               (* 10 1024 1024 1024)
                                               :total-bytes
                                               (* 100 1024 1024 1024)
                                               :io-key
                                               "disk0"))
                                             :devices
                                             (list
                                              (ntop.disk.domain:make-disk-device
                                               :device-name
                                               "disk0"
                                               :read-bytes
                                               12000
                                               :write-bytes
                                               6000)))
                                            :temperature-sample
                                            (ntop.temperature.domain:make-temperature-sample
                                             :sampled-at
                                             3d0
                                             :sensors
                                             (list
                                              (ntop.temperature.domain:make-temperature-sensor
                                               :name
                                               "CPU"
                                               :temperature-celsius
                                               52.5d0)
                                              (ntop.temperature.domain:make-temperature-sensor
                                               :name
                                               "GPU"
                                               :temperature-celsius
                                               44.0d0)))
                                            :process-sample
                                            (ntop.process.domain:make-process-sample
                                             :sampled-at
                                             3d0
                                             :processes
                                             (list
                                              (ntop.process.domain:make-process-record
                                               :pid
                                               100
                                               :ppid
                                               1
                                               :name
                                               "alpha"
                                               :command
                                               "alpha --serve"
                                               :cpu-time-ns
                                               200000000
                                               :resident-memory-bytes
                                               (* 512 1024 1024)
                                               :virtual-memory-bytes
                                               (* 1024 1024 1024)
                                               :read-bytes
                                               8000
                                               :write-bytes
                                               2000
                                               :user-name
                                               "user"
                                               :state
                                               "R"
                                               :ps-fallback-cpu-percent
                                               8.5d0)
                                              (ntop.process.domain:make-process-record
                                               :pid
                                               200
                                               :ppid
                                               1
                                               :name
                                               "beta"
                                               :command
                                               "beta --worker"
                                               :resident-memory-bytes
                                               (* 256 1024 1024)
                                               :virtual-memory-bytes
                                               (* 512 1024 1024)
                                               :read-bytes
                                               3000
                                               :write-bytes
                                               1000
                                               :user-name
                                               "user"
                                               :state
                                               "S"
                                               :ps-fallback-cpu-percent
                                               2.0d0)))))
(defun %wp12b-screen-fixture-series ()
  (list
   (cons :cpu         (list :usage
               (list
                (ntop.ui:make-chart-series :name
                                           "AVG"
                                           :samples
                                           (%wp12b-screen-history '(20 32 48)))
                (ntop.ui:make-chart-series :name
                                           "CPU0"
                                           :samples
                                           (%wp12b-screen-history '(12 25 35)))
                (ntop.ui:make-chart-series :name
                                           "CPU1"
                                           :samples
                                           (%wp12b-screen-history '(16 40 62))))))
   (cons :memory
         (list :ram-samples
               (%wp12b-screen-history '(3 4 5))
               :swap-samples
               (%wp12b-screen-history '(0 1 1))
               :cache-samples
               (%wp12b-screen-history '(1 2 2))))
   (cons :network
         (list (%wp12b-screen-network-sample 1d0 1000 2000)
               (%wp12b-screen-network-sample 2d0 1800 4000)
               (%wp12b-screen-network-sample 3d0 2400 5200)))
   (cons :process
         (list :previous-sample (%wp12b-screen-previous-process-sample)))))
(defun %wp12b-screen-config ()
  (ntop.shared.application:make-application-config :theme
                                                   :default
                                                   :process-default-grouped-p
                                                   nil
                                                   :process-columns
                                                   '(:pid-or-count
                                                     :name-or-command
                                                     :cpu
                                                     :memory)))
(defun %wp12b-screen-default-config ()
  (let ((config (%wp12b-screen-config)))
    (setf (ntop.shared.application:application-config-process-default-grouped-p
           config)
          t)
    config))
(defun %wp12b-screen-render (columns rows
                             &key
                             state
                             config
                             series                             unavailability-reasons)
  (let* ((screen (cl-tty-kit:make-screen columns rows))
         (state (or state (ntop.app.domain:make-app-state)))
         (config (or config (%wp12b-screen-default-config)))
         (snapshot (%wp12b-screen-fixture-snapshot)))
    (values
     (ntop.app.presentation:draw-application-screen screen
                                                    state
                                                    config
                                                    snapshot
                                                    columns
                                                    rows
                                                    unavailability-reasons
                                                    (or series
                                                        (%wp12b-screen-fixture-series)))
     screen
     (cl-tty-kit:screen-to-string screen))))
(defun %wp12b-screen-bytes (string)
  (coerce (sb-ext:string-to-octets string :external-format :utf-8) 'list))
(cl-weave:describe "application composition terminal output"
  (cl-weave:it "emits CRLF row separators on the first composition render"
    (let* ((config (%wp12b-screen-config))
           (composition             (ntop.app.application:make-application-composition config))
           (state (ntop.app.domain:make-app-state))
           (output             (with-output-to-string (stream)
               (let ((*standard-output* stream))
                 (funcall
                  (ntop.app.application:application-composition-render composition)
                  state config (%wp12b-screen-fixture-snapshot) 80 24 nil)))))
      (cl-weave:expect (plusp (count #\Newline output)) :to-be t)
      (cl-weave:expect
       (loop for index from 0 below (length output)
             always
               (or (not (char= (char output index) #\Newline))
                   (and (plusp index)
                        (char= (char output (1- index)) #\Return)
                        (or (< index 2)
                            (not (char= (char output (- index 2)) #\Return))))))
       :to-be t)))
  (cl-weave:it "preserves the CPU and legend frame corners"
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24)
      (declare (ignore areas))
      (cl-weave:expect
       (loop for (x y) in '((2 0) (0 0) (0 5) (45 0) (46 0) (47 0)
                            (45 5) (46 5))
             collect (cl-tty-kit:cell-char
                      (cl-tty-kit:screen-cell screen x y)))
       :to-equal '(#\C #\┌ #\└ #\┐ #\┌ #\─ #\┘ #\└))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 2 0))
       :to-equal '((:fg 7)))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
       :to-equal '((:fg 7)))
      (cl-weave:expect
       (list (ntop.ui:presentation-theme-style :default :widget-title)
             (ntop.ui:presentation-theme-style :default :border))
       :to-equal (list '((:fg 7)) '((:fg 7)))))
    (let ((config (%wp12b-screen-config)))
      (setf (ntop.shared.application:application-config-cpu-left-legend-p config) t)
      (multiple-value-bind (areas screen)
          (%wp12b-screen-render 80 24 :config config)
        (declare (ignore areas))
        (cl-weave:expect
         (and
          (equal
           (loop for (x y) in (quote ((6 0) (7 0) (6 5) (7 5)))
                 collect (cl-tty-kit:cell-char
                          (cl-tty-kit:screen-cell screen x y)))
           (quote (#\┐ #\┌ #\┘ #\└)))
          (not (null
                (search " CPU "
                        (cl-tty-kit:screen-row-string screen 0)))))
         :to-be
         t))))
)
(defun %wp12b-screen-golden-bytes (name)
  (with-open-file
      (stream (asdf:system-relative-pathname "ntop/test"
                                            (format nil "t/app/goldens/~A" name))
        :direction :input :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream) (coerce bytes 'list))))
(defun %wp12b-screen-memory-legend-summary (columns rows screen)  (case columns
    (80 (loop for y below rows
              always (let ((line (cl-tty-kit:screen-row-string screen y)))
                       (not (or (search "RAM:" line) (search "SWP:" line))))))
    (160 (mapcar (lambda (y) (subseq
                              (cl-tty-kit:screen-row-string screen y) 133 159))
                 '(2 3)))))
