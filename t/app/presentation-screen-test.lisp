(in-package #:ntop.test)

(defvar *wp12b-screen-widget-calls* nil)
(defvar *wp12b-screen-recording-p* nil)

(defmethod ntop.ui:draw-widget :around (widget-key target rectangle state config series)
  (when *wp12b-screen-recording-p*
    (push (list widget-key rectangle series state)
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
  (ntop.process.domain:make-process-sample :sampled-at
                                           2d0
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
   (cons :cpu
         (list :usage
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

(defun %wp12b-screen-render (columns rows
                                     &key
                                     state
                                     config
                                     series
                                     unavailability-reasons)
  (let* ((screen (cl-tty-kit:make-screen columns rows))
         (state (or state (ntop.app.domain:make-app-state)))
         (config (or config (%wp12b-screen-config)))
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

(defun %wp12b-screen-golden-bytes (name)
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      (format nil "t/app/goldens/~A" name))
       :direction
       :input
       :element-type
       '(unsigned-byte 8))
    (let ((bytes
           (make-array (file-length stream) :element-type '(unsigned-byte 8))))
      (read-sequence bytes stream)
      (coerce bytes 'list))))

(cl-weave:describe "application screen composition"
                   (cl-weave:it
                    "matches the default btm layout at each required terminal size"
                    (dolist
                        (case '((80 24 "application-80x24.txt")
                                (160 48 "application-160x48.txt")
                                (40 12 "application-40x12.txt")))
                      (destructuring-bind (columns rows golden) case
                        (multiple-value-bind (areas screen output)
                            (%wp12b-screen-render columns rows)
                          (declare (ignore areas))
                          (cl-weave:expect (cl-tty-kit:screen-width screen)
                                           :to-equal
                                           columns)
                          (cl-weave:expect (cl-tty-kit:screen-height screen)
                                           :to-equal
                                           rows)
                          (cl-weave:expect (%wp12b-screen-bytes output)
                                           :to-equal
                                           (%wp12b-screen-golden-bytes golden))))))
                   (cl-weave:it
                    "renders only startup widgets and draws the CPU parent once"
                    (let ((state (ntop.app.domain:make-app-state))
                          (*wp12b-screen-widget-calls* nil) (*wp12b-screen-recording-p* t))
                      (%wp12b-screen-render 80 24 :state state)
                      (setf *wp12b-screen-widget-calls* (nreverse
                                                         *wp12b-screen-widget-calls*))
                      (cl-weave:expect
                       (mapcar #'first *wp12b-screen-widget-calls*)
                       :to-equal
                       '(:cpu :memory :network :disk :temperature :process-table))
                      (cl-weave:expect
                       (loop for widget in '(:disk :temperature)
                             for key in '(:disks :temperature)
                             for call = (find widget *wp12b-screen-widget-calls*
                                              :key #'first)
                             always (and (typep (fourth call) 'ntop.ui:table-state)
                                         (not (nth-value
                                               1
                                               (gethash
                                                key
                                                (ntop.app.domain:app-state-widget-v-states
                                                 state))))))
                       :to-be t)
                      (cl-weave:expect
                       (count :cpu *wp12b-screen-widget-calls* :key #'first)
                       :to-equal
                       1)
                      (let* ((placements (ntop.ui:layout-widgets 80 24))
                             (cpu (assoc :cpu placements :test #'eq))
                             (legend (assoc :cpu-legend placements :test #'eq))
                             (cpu-call
                              (find :cpu *wp12b-screen-widget-calls* :key #'first))
                             (cpu-rectangle (second cpu-call)))
                        (cl-weave:expect
                         (ntop.ui:rectangle-width cpu-rectangle)
                         :to-equal
                         (- (+ (ntop.ui:rectangle-width (cdr cpu))
                               (ntop.ui:rectangle-width (cdr legend)))
                            2)))))
                   (cl-weave:it
                    "renders the basic selector and screen table roles"
                    (let ((state (ntop.app.domain:make-app-state :basic-mode-p t))
                          (*wp12b-screen-widget-calls* nil) (*wp12b-screen-recording-p* t))
                      (multiple-value-bind (areas screen output)
                          (%wp12b-screen-render 80 24 :state state)
                        (declare (ignore areas))
                        (cl-weave:expect
                         (not
                          (null
                           (member :basic-table-arrows
                                   *wp12b-screen-widget-calls*
                                   :key #'first
                                   :test #'eq)))
                         :to-be t)
                        (cl-weave:expect
                         (not (null (search "◄ Process ►" output))) :to-be t)
                        (let ((ntop.app.presentation::*screen-table-theme* :gruvbox))
                          (cl-weave:expect
                           (loop for (role theme-role) in '((:normal :text) (:header :table-header)
                                                            (:selected :selected-text)
                                                            (:title :widget-title))
                                 do (ntop.ui:draw-table-text screen 0 1 "x" role)
                                 always
                                   (equal (cl-tty-kit:cell-style
                                           (cl-tty-kit:screen-cell screen 0 1))
                                          (ntop.ui:presentation-theme-style :gruvbox theme-role)))
                           :to-be t)))))
                   (cl-weave:it
                    "computes process CPU and I/O rates from the previous sample"
                    (let ((*wp12b-screen-widget-calls* nil) (*wp12b-screen-recording-p* t))
                      (%wp12b-screen-render 80 24)
                      (let* ((process-call
                              (find :process-table
                                    *wp12b-screen-widget-calls*
                                    :key
                                    #'first
                                    :test
                                    #'eq))
                             (rows (third process-call))
                             (alpha
                              (find 100
                                    rows
                                    :key
                                    #'ntop.process.domain:process-row-pid
                                    :test
                                    #'eql)))
                        (cl-weave:expect (not (null alpha)) :to-be t)
                        (cl-weave:expect
                         (list
                          (ntop.process.domain:process-row-cpu-percent alpha)
                          (ntop.process.domain:process-row-read-bytes-per-second
                           alpha)
                          (ntop.process.domain:process-row-write-bytes-per-second
                           alpha))
                         :to-equal
                         '(5.0d0 2000.0d0 500.0d0)))))
                   (cl-weave:it
                    "highlights a selected frame only after interaction"
                    (let* ((state (ntop.app.domain:make-app-state))
                           (process-rectangle
                            (cdr
                             (assoc :process
                                    (ntop.app.presentation::%screen-layout-placements
                                     80 24 state :default)
                                    :test (function eq))))
                           (x (ntop.ui:rectangle-x process-rectangle))
                           (y (ntop.ui:rectangle-y process-rectangle)))
                      (dolist (case (list (list nil :border)
                                          (list t :highlighted-border)))
                        (let ((ntop.app.presentation::*screen-selection-active-p*
                                (first case)))
                          (multiple-value-bind (areas screen)
                              (%wp12b-screen-render 80 24 :state state)
                            (declare (ignore areas))
                            (cl-weave:expect
                             (cl-tty-kit:cell-style
                              (cl-tty-kit:screen-cell screen x y))
                             :to-equal
                             (ntop.ui:presentation-theme-style
                              :default (second case))))))))
                   (cl-weave:it
                    "selects alternate GPU, temperature, battery and disk graph views"
                    (dolist
                        (case '((:memory :gpu :gpu :memory)
                                (:temperature :temperature-graph
                                              :temperature-graph
                                              :temperature)
                                (:temperature :battery :battery :temperature)
                                (:disks :disk-io-graph :disk-io-graph :disks)))
                      (destructuring-bind (base view expected default-widget)
                          case
                        (let* ((state (ntop.app.domain:make-app-state :selected-widget
                                                                base))
                               (*wp12b-screen-widget-calls* nil)
                               (*wp12b-screen-recording-p* t)
                               (unavailability-reasons (and (eq view :gpu)
                                     (ntop.app.application::%make-application-unavailability-reasons
                                      :gpu
                                      :not-present))))
                          (setf (gethash base
                                         (ntop.app.domain:app-state-widget-v-states
                                          state)) view)
                          (%wp12b-screen-render 80
                                                24
                                                :state
                                                state
                                                :unavailability-reasons
                                                unavailability-reasons)
                          (cl-weave:expect
                           (not
                           (null
                            (member expected
                                    *wp12b-screen-widget-calls*
                                    :key #'first
                                    :test #'eq)))
                           :to-be
                           t)
                          (cl-weave:expect
                           (member default-widget
                                   *wp12b-screen-widget-calls*
                                   :key
                                   #'first
                                   :test
                                   #'eq)
                           :to-be
                           nil)
                          (when (eq view :gpu)
                            (let ((gpu-call
                                   (find :gpu
                                         *wp12b-screen-widget-calls*
                                         :key
                                         #'first
                                         :test
                                         #'eq)))
                              (cl-weave:expect
                               (getf (third gpu-call) :unavailability-reason)
                               :to-equal
                               :not-present))))))))
