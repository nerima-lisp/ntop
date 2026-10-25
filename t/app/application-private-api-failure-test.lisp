(in-package #:ntop.test)

(defclass %private-api-failure-clock ()
  ((time :initform 0d0 :accessor %private-api-failure-clock-time)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock %private-api-failure-clock) on-time on-unavailable)
  (declare (ignore on-unavailable))
  (funcall on-time (incf (%private-api-failure-clock-time clock) 1d0)))

(defclass %private-api-failure-cpu-source ()
  ((read-count :initform 0 :accessor %private-api-failure-cpu-source-read-count)
   (ioreport-failure-count :initform 0
                           :accessor %private-api-failure-ioreport-failure-count)))

(defun %private-api-failure-read-cpu-source
    (source on-value on-unavailable)
  (declare (ignore on-unavailable))
  (let* ((read-count
           (incf (%private-api-failure-cpu-source-read-count source)))
         (ticks (* 100 (1- read-count)))
         (core
           (ntop.cpu.domain:make-cpu-core-value
            :core-index 0
            :user-ticks (/ ticks 4)
            :system-ticks (* ticks 3/20)
            :nice-ticks 0
            :idle-ticks (* ticks 3/5)
            :iowait-ticks 0
            :irq-ticks 0
            :softirq-ticks 0
            :steal-ticks 0)))
    (funcall
     on-value
     (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
      (list core)
      '(0.25d0 0.5d0 0.75d0)
      (lambda (on-data on-api-unavailable)
        (declare (ignore on-data))
        (incf (%private-api-failure-ioreport-failure-count source))
        (funcall on-api-unavailable :private-api-unavailable))))))

(defclass %private-api-failure-fixture-collector ()
  ((process-sample-count :initform 0
                         :accessor %private-api-failure-process-sample-count)))

(defun %private-api-failure-memory-sample ()
  (ntop.memory.domain:make-memory-sample
   :sampled-at 1d0
   :ram-used-bytes (* 5 1024 1024 1024)
   :ram-total-bytes (* 8 1024 1024 1024)
   :swap-used-bytes (* 1 1024 1024 1024)
   :swap-total-bytes (* 2 1024 1024 1024)
   :cache-used-bytes (* 2 1024 1024 1024)
   :cache-total-bytes (* 8 1024 1024 1024)))

(defun %private-api-failure-process-sample (sampled-at ordinal)
  (ntop.process.domain:make-process-sample
   :sampled-at sampled-at
   :processes
   (list
    (ntop.process.domain:make-process-record
     :pid 4242
     :ppid 1
     :name "fixture-worker"
     :command "fixture-worker --serve"
     :cpu-time-ns (* ordinal 100000000)
     :resident-memory-bytes (* 512 1024 1024)
     :virtual-memory-bytes (* 1024 1024 1024)
     :read-bytes (* ordinal 1024)
     :write-bytes (* ordinal 256)))))

(defmethod ntop.shared.application:collect-memory-sample
    ((collector %private-api-failure-fixture-collector) on-sample on-unavailable)
  (declare (ignore collector on-unavailable))
  (funcall on-sample (%private-api-failure-memory-sample)))

(defmethod ntop.shared.application:collect-process-sample
    ((collector %private-api-failure-fixture-collector) on-sample on-unavailable)
  (declare (ignore on-unavailable))
  (let ((ordinal
          (incf (%private-api-failure-process-sample-count collector))))
    (funcall on-sample
             (%private-api-failure-process-sample (float ordinal 1d0) ordinal))))

(defclass %private-api-failure-gpu-collector
    (ntop.gpu.infrastructure:gpu-collector)
  ((failure-count :initform 0 :accessor %private-api-failure-gpu-failure-count)))

(defmethod ntop.gpu.infrastructure:read-gpu-device-data
    ((collector %private-api-failure-gpu-collector) on-value on-unavailable)
  (declare (ignore on-value))
  (incf (%private-api-failure-gpu-failure-count collector))
  (funcall on-unavailable :private-api-unavailable))

(defun %private-api-failure-result (collectors clock)
  (let ((result nil)
        (top-level-unavailable :not-called))
    (ntop.app.application:collect-application-result
     collectors clock
     (lambda (value) (setf result value))
     (lambda (reason) (setf top-level-unavailable reason)))
    (cl-weave:expect top-level-unavailable :to-be :not-called)
    result))

(defun %private-api-failure-render-screen
    (config state collection-results current-result)
  (let* ((composition (ntop.app.application:make-application-composition config))
         (on-snapshot
           (ntop.app.application:application-composition-on-snapshot composition))
         (snapshot
           (ntop.app.application:application-collection-result-snapshot
            current-result))
         (reasons
           (ntop.app.application:application-collection-result-unavailability-reasons
            current-result))
         (terminal-output
           (with-output-to-string (stream)
             (let ((*standard-output* stream))
               (dolist (result collection-results)
                 (funcall
                  on-snapshot
                  (ntop.app.application:application-collection-result-snapshot
                   result)))
               (funcall
                (ntop.app.application:application-composition-render composition)
                state config snapshot 160 48 reasons))))
         (screen-text
           (cl-tty-kit:screen-to-string
            (ntop.app.application::application-composition-screen composition))))
    (values terminal-output screen-text)))

(cl-weave:describe "application private API failure composition"
  (cl-weave:it
   "renders private API failures without interrupting other sections"
   (let* ((clock (make-instance '%private-api-failure-clock))
          (cpu-source (make-instance '%private-api-failure-cpu-source))
          (cpu-collector
            (ntop.cpu.infrastructure:make-cpu-collector
             :clock clock
             :source-reader
             (lambda (on-value on-unavailable)
               (%private-api-failure-read-cpu-source
                cpu-source on-value on-unavailable))))
          (fixture-collector (make-instance '%private-api-failure-fixture-collector))
          (config
            (ntop.shared.application:make-application-config
             :disable-gpu-p nil
             :hide-average-cpu-p nil
             :process-default-grouped-p nil))
          (gpu-collector
            (make-instance '%private-api-failure-gpu-collector
                           :clock clock
                           :config config))
          (iohid-failure-count 0)
          (temperature-collector
            (ntop.temperature.infrastructure:make-darwin-temperature-collector
             clock
             :api-loader
             (lambda ()
               (incf iohid-failure-count)
               (error "Injected IOHID API loader failure."))))
          (collectors
            (list :cpu cpu-collector
                  :memory fixture-collector
                  :gpu gpu-collector
                  :temperature temperature-collector
                  :process fixture-collector))
          (initial-result (%private-api-failure-result collectors clock))
          (result (%private-api-failure-result collectors clock))
          (snapshot
            (ntop.app.application:application-collection-result-snapshot result))
          (reasons
            (ntop.app.application:application-collection-result-unavailability-reasons
             result))
          (cpu-sample
            (ntop.app.domain:collection-snapshot-cpu-sample snapshot))
          (cpu-core (first (ntop.cpu.domain:cpu-sample-core-values cpu-sample)))
          (memory-sample
            (ntop.app.domain:collection-snapshot-memory-sample snapshot))
          (temperature-sample
            (ntop.app.domain:collection-snapshot-temperature-sample snapshot))
          (process-sample
            (ntop.app.domain:collection-snapshot-process-sample snapshot)))
     (cl-weave:expect
      (%private-api-failure-ioreport-failure-count cpu-source)
      :to-be 2)
     (cl-weave:expect iohid-failure-count :to-be 2)
     (cl-weave:expect
      (%private-api-failure-gpu-failure-count gpu-collector)
      :to-be 2)
     (cl-weave:expect
      (ntop.app.application:application-unavailability-reason reasons :gpu)
      :to-be :private-api-unavailable)
     (cl-weave:expect
      (ntop.app.application:application-unavailability-reason reasons :temperature)
      :to-be :private-api-unavailable)
     (cl-weave:expect
      (list (ntop.cpu.domain:cpu-sample-average-usage-percent cpu-sample)
            (ntop.cpu.domain:cpu-core-value-usage-percent cpu-core)
            (ntop.cpu.domain:cpu-core-value-frequency-mhz cpu-core))
      :to-equal '(40 40 nil))
     (cl-weave:expect
      (list (ntop.memory.domain:memory-sample-ram-used-bytes memory-sample)
            (ntop.memory.domain:memory-sample-ram-total-bytes memory-sample))
      :to-equal (list (* 5 1024 1024 1024) (* 8 1024 1024 1024)))
     (cl-weave:expect temperature-sample :to-be nil)
     (cl-weave:expect
      (ntop.process.domain:process-record-name
       (first (ntop.process.domain:process-sample-processes process-sample)))
      :to-equal "fixture-worker")
     (multiple-value-bind (terminal-output screen-text)
         (%private-api-failure-render-screen
          config
          (ntop.app.domain:make-app-state :selected-widget :cpu)
          (list initial-result result)
          result)
       (cl-weave:expect (plusp (length terminal-output)) :to-be t)
       (cl-weave:expect (not (null (search "CPU" screen-text))) :to-be t)
       (cl-weave:expect (not (null (search "40%" screen-text))) :to-be t)
       (cl-weave:expect (not (null (search "5.0GiB/8.0GiB" screen-text))) :to-be t)
       (cl-weave:expect
        (not (null (search "Temperatures" screen-text)))
        :to-be t)
       (cl-weave:expect
        (let* ((lines
                 (with-input-from-string (input screen-text)
                   (loop for line = (read-line input nil nil)
                         while line
                         collect line)))
               (title-index
                 (position-if
                  (lambda (line) (search "Temperatures" line))
                  lines))
               (title-line (and title-index (nth title-index lines)))
               (title-position
                 (and title-line (search "Temperatures" title-line)))
               (widget-left
                 (and title-position
                      (position #\┌ title-line
                                :end title-position
                                :from-end t)))
               (widget-right
                 (and title-position
                      (position #\┐ title-line
                                :start (+ title-position
                                          (length "Temperatures")))))
               (header-line (and title-index (nth (1+ title-index) lines)))
               (temperature-row
                 (and title-index (nth (+ title-index 2) lines)))
               (temperature-column-position
                 (and widget-left
                      widget-right
                      header-line
                      (search "Temp(t)" header-line
                              :start2 (1+ widget-left)
                              :end2 widget-right)))
               (temperature-value-position
                 (and temperature-column-position
                      temperature-row
                      (search "N/A" temperature-row
                              :start2 temperature-column-position
                              :end2 widget-right))))
          (and temperature-column-position
               temperature-value-position
               (= temperature-column-position
                  temperature-value-position)))
        :to-be t)
       (cl-weave:expect
        (not (null (search "fixture-worker" screen-text)))
        :to-be t))
     (multiple-value-bind (terminal-output expanded-screen-text)
         (%private-api-failure-render-screen
          config
          (ntop.app.domain:make-app-state
           :selected-widget :cpu
           :expanded-p t)
          (list initial-result result)
          result)
       (cl-weave:expect (plusp (length terminal-output)) :to-be t)
       (cl-weave:expect
        (not (null (search "MHz" expanded-screen-text)))
        :to-be t)
       (cl-weave:expect
        (let* ((lines
                 (with-input-from-string (input expanded-screen-text)
                   (loop for line = (read-line input nil nil)
                         while line
                         collect line)))
               (header-index
                 (position-if
                  (lambda (line)
                    (and (search "Use" line)
                         (search "MHz" line)))
                  lines))
               (header-line (and header-index (nth header-index lines)))
               (use-position (and header-line (search "Use" header-line)))
               (cpu-header-position
                 (and use-position
                      (search "CPU" header-line
                              :from-end t
                              :end2 use-position)))
               (frequency-header-position
                 (and header-line (search "MHz" header-line)))
               (cpu-row (and header-index (nth (+ header-index 3) lines)))
               (label-cell
                 (and cpu-header-position
                      use-position
                      cpu-row
                      (subseq cpu-row cpu-header-position use-position)))
               (usage-position
                 (and use-position
                      cpu-row
                      (search "40%" cpu-row :start2 use-position)))
               (frequency-position
                 (and usage-position
                      frequency-header-position
                      cpu-row
                      (search "N/A"
                              cpu-row
                              :start2 (+ usage-position (length "40%"))
                              :end2 (+ frequency-header-position
                                       (length "MHz")))))
               (core-label (and label-cell (string-trim " " label-cell))))
          (and core-label
               (member core-label '("0" "CPU0") :test #'string=)
               use-position
               usage-position
               (= use-position usage-position)
               frequency-header-position
               frequency-position
               (= frequency-header-position frequency-position)))
        :to-be t)))))
