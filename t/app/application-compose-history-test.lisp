(in-package #:ntop.test)

(cl-weave:describe
  "application composition history coverage"
  (cl-weave:it "records optional history sources and trims expired network samples"
      (let* ((composition (%make-composition-test-instance))
             (empty-composition
              (ntop.app.application::%make-application-composition))
             (record (ntop.shared.domain:make-sample-record :sampled-at 10d0))
             (cpu-sample
              (ntop.cpu.domain:make-cpu-sample :sampled-at
                                               10d0
                                               :average-usage-percent
                                               40d0
                                               :core-values
                                               (list
                                                (ntop.cpu.domain:make-cpu-core-value
                                                 :core-index
                                                 3
                                                 :usage-percent
                                                 60d0)
                                                (ntop.cpu.domain:make-cpu-core-value
                                                 :usage-percent
                                                 80d0))))
             (memory-sample
              (ntop.memory.domain:make-memory-sample :sampled-at
                                                     10d0
                                                     :ram-used-bytes
                                                     50
                                                     :ram-total-bytes
                                                     100
                                                     :swap-used-bytes
                                                     20
                                                     :swap-total-bytes
                                                     40
                                                     :cache-used-bytes
                                                     10
                                                     :cache-total-bytes
                                                     60))
             (gpu-sample
              (ntop.gpu.domain:make-gpu-sample :sampled-at
                                               10d0
                                               :devices
                                               (list
                                                (%composition-test-gpu-device
                                                 "GPU A"
                                                 50
                                                 200)
                                                (%composition-test-gpu-device
                                                 nil
                                                 25
                                                 100))))
             (first-network-sample
              (ntop.network.domain:make-network-sample :sampled-at
                                                       10d0
                                                       :interfaces
                                                       nil))
             (replacement-network-sample
              (ntop.network.domain:make-network-sample :sampled-at
                                                       10d0
                                                       :interfaces
                                                       (list
                                                        (ntop.network.domain:make-network-interface
                                                         :name
                                                         "en1"))))
             (first-disk-sample
              (ntop.disk.domain:make-disk-sample :sampled-at
                                                 10d0
                                                 :devices
                                                 (list
                                                  (ntop.disk.domain:make-disk-device
                                                   :device-name
                                                   "disk0"
                                                   :read-bytes
                                                   100
                                                   :write-bytes
                                                   200)
                                                  (ntop.disk.domain:make-disk-device
                                                   :read-bytes
                                                   10
                                                   :write-bytes
                                                   20)
                                                  (ntop.disk.domain:make-disk-device
                                                   :device-name
                                                   "missing-counter"))))
             (second-disk-sample
              (ntop.disk.domain:make-disk-sample :sampled-at
                                                 20d0
                                                 :devices
                                                 (list
                                                  (ntop.disk.domain:make-disk-device
                                                   :device-name
                                                   "disk0"
                                                   :read-bytes
                                                   300
                                                   :write-bytes
                                                   600)
                                                  (ntop.disk.domain:make-disk-device
                                                   :read-bytes
                                                   30
                                                   :write-bytes
                                                   60)
                                                  (ntop.disk.domain:make-disk-device
                                                   :device-name
                                                   "missing-counter"))))
             (temperature-sample
              (ntop.temperature.domain:make-temperature-sample
                :sampled-at
                10d0
                :sensors
                (list
                 (ntop.temperature.domain:make-temperature-sensor :name
                                                                  "CPU"
                                                                  :temperature-celsius
                                                                  70d0)
                 (ntop.temperature.domain:make-temperature-sensor
                  :temperature-celsius
                  32d0))))
             (first-process-sample
              (%composition-test-process-sample 10d0 100 1000))
             (second-process-sample
              (%composition-test-process-sample 20d0 200 2000))
             (first-snapshot
              (ntop.app.domain:make-collection-snapshot :sampled-at
                                                        10d0
                                                        :cpu-sample
                                                        cpu-sample
                                                        :memory-sample
                                                        memory-sample
                                                        :gpu-sample
                                                        gpu-sample
                                                        :network-sample
                                                        first-network-sample
                                                        :disk-sample
                                                        first-disk-sample
                                                        :temperature-sample
                                                        temperature-sample
                                                        :process-sample
                                                        first-process-sample))
             (second-snapshot
              (ntop.app.domain:make-collection-snapshot :sampled-at
                                                        20d0
                                                        :network-sample
                                                        replacement-network-sample
                                                        :disk-sample
                                                        second-disk-sample
                                                        :process-sample
                                                        second-process-sample)))
        (cl-weave:expect
         (length
          (ntop.app.application:application-composition-history-stores
           empty-composition))
         :to-be
         0)
        (cl-weave:expect
         (hash-table-count
          (ntop.app.application::application-composition-history-index
           empty-composition))
         :to-be
         0)
        (cl-weave:expect
         (length
          (ntop.app.application::application-composition-network-samples
           empty-composition))
         :to-be
         0)
        (cl-weave:expect
         (list
          (hash-table-count
           (ntop.app.application::application-composition-gpu-memory-device-totals
            empty-composition))
          (hash-table-count
           (ntop.app.application::application-composition-history-names
            empty-composition))
          (length
           (ntop.app.application::application-composition-history-keys
            empty-composition)))
         :to-equal
         (list 0 0 0))
        (flet ((values-for (key)
                 (loop for sample across (ntop.app.application::%history-samples-for
                                          composition
                                          key)
                       collect (ntop.shared.domain:history-sample-value sample))))
          (cl-weave:expect
           (ntop.app.application::%record-snapshot-history composition
                                                           first-snapshot)
           :to-be
           first-snapshot)
          (cl-weave:expect
           (ntop.app.application::%record-snapshot-history composition
                                                           second-snapshot)
           :to-be
           second-snapshot)
          (cl-weave:expect (values-for (list :cpu :usage (list :core 3)))
                           :to-equal
                           (list 60d0))
          (cl-weave:expect (values-for (list :cpu :usage (list :core 1)))
                           :to-equal
                           (list 80d0))
          (cl-weave:expect
           (list
            (values-for (list :memory :ram))
            (values-for (list :memory :swap)))
           :to-equal
           (list (list 50d0) (list 20d0)))
          (cl-weave:expect (values-for (list :memory :cache))
                           :to-equal
                           (list 10d0))
          (cl-weave:expect (values-for (list :gpu-memory :used (list nil 1)))
                           :to-equal
                           (list 25d0))
          (cl-weave:expect
           (gethash (list :gpu-memory :used (list nil 1))
                    (ntop.app.application::application-composition-history-names
                     composition))
           :to-equal
           "GPU1")
          (cl-weave:expect
           (gethash (list nil 1)
                    (ntop.app.application::application-composition-gpu-memory-device-totals
                     composition))
           :to-be
           100)
          (cl-weave:expect
           (eq
            (aref
             (ntop.app.application::application-composition-network-samples
              composition)
             0)
            replacement-network-sample)
           :to-be
           t)
          (cl-weave:expect (values-for (list :disk-io :read (list "disk0" 0)))
                           :to-equal
                           (list 0d0 20d0))
          (cl-weave:expect (values-for (list :disk-io :write (list nil 1)))
                           :to-equal
                           (list 0d0 4d0))
          (cl-weave:expect
           (nth-value 1
                      (gethash (list :disk-io :read (list "missing-counter" 2))
                               (ntop.app.application::application-composition-history-index
                                composition)))
           :to-be
           nil)
          (cl-weave:expect
           (eq
            (ntop.app.application::application-composition-previous-disk-sample
             composition)
            second-disk-sample)
           :to-be
           t)
          (cl-weave:expect (values-for (list :temperature :sensor (list nil 1)))
                           :to-equal
                           (list 32d0))
          (cl-weave:expect
           (gethash (list :temperature :sensor (list nil 1))
                    (ntop.app.application::application-composition-history-names
                     composition))
           :to-equal
           "Sensor1")
          (cl-weave:expect
           (eq
            (ntop.app.application::application-composition-previous-process-sample
             composition)
            first-process-sample)
           :to-be
           t)
          (cl-weave:expect
           (eq
            (ntop.app.application::application-composition-latest-process-sample
             composition)
            second-process-sample)
           :to-be
           t)
          (cl-weave:expect
           (ntop.app.application::application-composition-latest-snapshot
            composition)
           :to-be
           second-snapshot)
          (cl-weave:expect
           (length
            (ntop.app.application::%history-samples-for composition
                                                        (list :unrecorded)))
           :to-be
           0)
          (let ((raised-p
                 (handler-case (progn
                                 (ntop.app.application::%record-network-history
                                  composition
                                  (ntop.shared.domain:make-sample-record
                                   :sampled-at
                                   30d0)
                                  (ntop.network.domain:make-network-sample
                                   :sampled-at
                                   9d0
                                   :interfaces
                                   nil))
                                 nil)
                   (error (condition)
                     (search "chronological order" (format nil "~A" condition))))))
            (cl-weave:expect (not (null raised-p)) :to-be t))
          (let* ((retention
                  (ntop.shared.application:application-config-retention-seconds
                   (ntop.app.application::application-composition-config
                    composition)))
                 (now (+ retention 2d0)))
            (setf (ntop.app.application::application-composition-network-samples
                   composition) (vector
                                 (ntop.shared.domain:make-sample-record
                                  :sampled-at
                                  1d0)
                                 (ntop.shared.domain:make-sample-record
                                  :sampled-at
                                  2d0)
                                 (ntop.shared.domain:make-sample-record
                                  :sampled-at
                                  now)))
            (ntop.app.application::%trim-network-samples composition now)
            (cl-weave:expect
             (loop for sample across (ntop.app.application::application-composition-network-samples
                                      composition)
                   collect (ntop.shared.domain:sample-record-sampled-at sample))
             :to-equal
             (list 2d0 now))))
        (let* ((paused-composition (%make-composition-test-instance))
               (key (list :test :paused)))
          (setf (ntop.app.application::application-composition-paused-p
                 paused-composition) t)
          (cl-weave:expect
           (ntop.app.application::%append-composition-history paused-composition
                                                              key
                                                              "Paused"
                                                              record
                                                              nil)
           :to-be
           nil)
          (cl-weave:expect
           (ntop.shared.domain:history-paused-p
            (ntop.app.application::%append-composition-history
             paused-composition
             key
             "Paused"
             record
             1d0))
           :to-be
           t)
          (progn
            (let* ((help-composition (%make-composition-test-instance))
                   (state (ntop.app.domain:make-app-state
                           :selected-widget :cpu
                           :active-dialog :help-search))
                   (help-state
                     (ntop.app.presentation:make-help-state
                      :selected-widget :cpu
                      :query "needle"
                      :search-active-p t))
                   (on-action
                     (ntop.app.application:application-composition-on-action
                      help-composition)))
              (setf (gethash :help (ntop.app.domain:app-state-widget-v-states state))
                    help-state)
              (funcall on-action
                       state
                       (ntop.app.domain:make-app-action
                        :kind :dialog
                        :value (list :close :help-search)))
              (cl-weave:expect
               (ntop.app.domain:app-state-active-dialog state)
               :to-be
               nil)
              (cl-weave:expect
               (ntop.app.presentation:help-state-search-active-p
                (gethash
                 :help
                 (ntop.app.domain:app-state-widget-v-states state)))
               :to-be
               nil)
              (funcall on-action
                       state
                       (ntop.app.domain:make-app-action :kind :search :value :help))
              (funcall on-action
                       state
                       (ntop.app.domain:make-app-action :kind :dialog :value :unknown))
              (cl-weave:expect
               (ntop.app.domain:app-state-active-dialog state)
               :to-be
               :help-search)
              (cl-weave:expect
               (ntop.app.presentation:help-state-search-active-p
                (gethash
                 :help
                 (ntop.app.domain:app-state-widget-v-states state)))
               :to-be
               t)
              (funcall on-action
                       state
                       (ntop.app.domain:make-app-action :kind :dialog :value :help))
              (cl-weave:expect
               (ntop.app.domain:app-state-active-dialog state)
               :to-be
               :help)
              (cl-weave:expect
               (ntop.app.presentation:help-state-search-active-p
                (gethash
                 :help
                 (ntop.app.domain:app-state-widget-v-states state)))
               :to-be
               nil)
              (cl-weave:expect
               (ntop.app.presentation:help-state-query
                (gethash
                 :help
                 (ntop.app.domain:app-state-widget-v-states state)))
               :to-equal
               "needle"))
            (let* ((config
                     (ntop.shared.application:make-application-config
                      :process-default-grouped-p nil
                      :process-columns (list :pid-or-count :name-or-command :time)))
                   (composition
                     (ntop.app.application:make-application-composition config))
                   (state
                     (ntop.app.domain:make-app-state
                      :selected-widget :process
                      :expanded-p t))
                   (snapshot
                     (ntop.app.domain:make-collection-snapshot
                      :sampled-at 3d0
                      :process-sample
                      (ntop.process.domain:make-process-sample
                       :sampled-at 3d0
                       :processes
                       (list
                        (ntop.process.domain:make-process-record
                         :pid 100
                         :ppid 1
                         :name "alpha"
                         :command "alpha --serve"
                         :start-time-unix-seconds 100))))))
              (let ((output
                      (with-output-to-string (stream)
                        (let ((*standard-output* stream))
                          (funcall
                           (ntop.app.application:application-composition-render
                            composition)
                           state config snapshot 80 24 nil 175)))))
                (cl-weave:expect
                 (not (null (search "1m 15.00s" output)))
                 :to-be
                 t)))
            (let* ((called-p nil)
                   (composition
                     (%make-composition-test-instance
                      :on-render
                      (lambda (series-by-widget)
                        (declare (ignore series-by-widget))
                        (setf called-p t)
                        nil)))
                   (state (ntop.app.domain:make-app-state :selected-widget :cpu))
                   (config (ntop.shared.application:make-application-config)))
              (with-output-to-string (stream)
                (let ((*standard-output* stream))
                  (funcall
                   (ntop.app.application:application-composition-render composition)
                   state config nil 80 24 nil 175)))
              (cl-weave:expect called-p :to-be t))
            (let* ((render-diff-symbol (find-symbol "RENDER-DIFF" "CL-TTY-KIT"))
                   (original-render-diff (symbol-function render-diff-symbol))
                   (composition (%make-composition-test-instance))
                   (state (ntop.app.domain:make-app-state :selected-widget :cpu))
                   (config (ntop.shared.application:make-application-config))
                   (raw-diff (format nil "head~%middle~C~%tail~%" #\Return))
                   (expected
                     (format nil "head~C~%middle~C~%tail~C~%"
                             #\Return #\Return #\Return)))
              (let ((actual
                      (unwind-protect
                           (progn
                             (setf (symbol-function render-diff-symbol)
                                   (lambda (screen previous output)
                                     (declare (ignore screen previous))
                                     (write-string raw-diff output)))
                             (with-output-to-string (stream)
                               (let ((*standard-output* stream))
                                 (funcall
                                  (ntop.app.application:application-composition-render
                                   composition)
                                  state config nil 80 24 nil))))
                         (setf (symbol-function render-diff-symbol)
                               original-render-diff))))
                (cl-weave:expect actual :to-equal expected))))))))
