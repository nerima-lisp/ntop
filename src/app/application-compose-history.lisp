(in-package #:ntop.app.application)

(defstruct (application-composition (:constructor %make-application-composition))
  config
  draw-screen
  external-on-action
  process-signal-sender
  (history-stores (make-array 0 :adjustable t :fill-pointer 0))
  (history-index (make-hash-table :test (function equal)))
  (history-names (make-hash-table :test (function equal)))
  (history-keys (make-array 0 :adjustable t :fill-pointer 0))
  (network-samples (make-array 0 :adjustable t :fill-pointer 0))
  (gpu-memory-device-totals (make-hash-table :test (function equal)))
  latest-snapshot
  frozen-snapshot
  previous-disk-sample
  previous-process-sample
  latest-process-sample
  disk-rows
  paused-p
  selection-active-p
  screen
  previous-screen
  screen-columns
  screen-rows
  clickable-areas
  render
  on-snapshot
  on-action)
  (defparameter *memory-history-metrics*
    '((:ram ntop.memory.domain:memory-sample-ram-used-bytes
            ntop.memory.domain:memory-sample-ram-total-bytes)
      (:swap ntop.memory.domain:memory-sample-swap-used-bytes
             ntop.memory.domain:memory-sample-swap-total-bytes)
      (:cache ntop.memory.domain:memory-sample-cache-used-bytes
              ntop.memory.domain:memory-sample-cache-total-bytes)))
  (defun %ensure-composition-history (composition key name)
    (multiple-value-bind (store found-p)
        (gethash key (application-composition-history-index composition))
      (if found-p
          store
          (let* ((config (application-composition-config composition))
                 (interval
                   (/ (ntop.shared.application:application-config-rate-ms config)
                      1000d0))
                 (new-store
                   (ntop.shared.domain:make-history-store
                    1
                    :retention-seconds
                    (ntop.shared.application:application-config-retention-seconds
                     config)
                    :sample-interval-seconds interval
                    :initial-window-seconds
                    (ntop.shared.application:application-config-time-window-seconds
                     config)
                    :zoom-step-seconds
                    (ntop.shared.application:application-config-zoom-window-seconds
                     config))))
            (setf (gethash key (application-composition-history-index composition))
                  new-store
                  (gethash key (application-composition-history-names composition))
                  name)
            (vector-push-extend key (application-composition-history-keys composition))
            (vector-push-extend new-store
                                (application-composition-history-stores composition))
            new-store))))
  (defun %append-composition-history (composition key name record value)
    (multiple-value-bind (store found-p)
        (gethash key (application-composition-history-index composition))
      (when (or store value)
        (let ((store (or store (%ensure-composition-history composition key name))))
          (ntop.shared.domain:history-append store 0 record value)
          (when (and (application-composition-paused-p composition) (not found-p))
            (ntop.shared.domain:history-pause store))
          store))))
  (defun %history-samples-for (composition key)
    (let ((store (gethash key (application-composition-history-index composition))))
      (if store
          (ntop.shared.domain:history-slice store 0)
          #())))
  (defun %composition-current-snapshot (composition)
    (if (application-composition-paused-p composition)
        (application-composition-frozen-snapshot composition)
        (application-composition-latest-snapshot composition)))
  (defun %history-series-for (composition group kind)
    (loop for key across (application-composition-history-keys composition)
          when (and (eq (first key) group) (eq (second key) kind))
            collect (ntop.ui:make-chart-series :name
                                               (gethash key
                                                        (application-composition-history-names
                                                         composition))
                                               :samples
                                               (%history-samples-for composition key))))
  (defun %gpu-memory-series-data (composition)
    (let* ((theme
            (string-downcase
             (string
              (ntop.shared.application:application-config-theme
               (application-composition-config composition)))))
           (colors (ntop.ui:presentation-theme-style theme :gpu-colors))
           (device-totals
            (application-composition-gpu-memory-device-totals composition))
           (series nil)
           (total-bytes 0)
           (series-index 0))
      (loop for key across (application-composition-history-keys composition)
            when (and (eq (first key) :gpu-memory) (eq (second key) :used))
              do (let* ((samples (%history-samples-for composition key))
                        (device-total (gethash (third key) device-totals)))
                   (when
                       (plusp (length samples))
                     (push
                      (ntop.ui:make-chart-series :name
                                                 (gethash key
                                                          (application-composition-history-names
                                                           composition))
                                                 :style
                                                 (and colors
                                                      (nth
                                                       (mod series-index
                                                            (length colors))
                                                       colors))
                                                 :samples
                                                 samples)
                      series)
                     (incf total-bytes device-total)
                     (incf series-index))))
      (when series
        (list :gpu-memory-series (nreverse series) :gpu-memory-total-bytes total-bytes))))
  (defun %record-cpu-history (composition record sample)
    (when sample
      (%append-composition-history composition
                                   '(:cpu :usage :average)
                                   "AVG"
                                   record
                                   (ntop.cpu.domain:cpu-sample-average-usage-percent
                                    sample))
      (loop for core in (ntop.cpu.domain:cpu-sample-core-values sample)
            for position from 0
            for index = (or (ntop.cpu.domain:cpu-core-value-core-index core) position)
            for name = (format nil "CPU~D" index)
            do (%append-composition-history composition
                                            (list :cpu :usage (list :core index))
                                            name
                                            record
                                            (ntop.cpu.domain:cpu-core-value-usage-percent
                                             core)))))
  (defun %record-memory-history (composition record sample)
    (when sample
      (dolist (metric *memory-history-metrics*)
        (destructuring-bind (name used-accessor total-accessor) metric
          (declare (ignore total-accessor))
          (%append-composition-history composition
                                       (list :memory name)
                                       (string-upcase (symbol-name name))
                                       record
                                       (funcall (symbol-function used-accessor) sample))))))
  (defun %gpu-history-id (device position)
    (list (ntop.gpu.domain:gpu-device-name device) position))
  (defun %record-gpu-history (composition record sample)
    (when sample
      (loop for device in (ntop.gpu.domain:gpu-sample-devices sample)
            for position from 0
            for id = (%gpu-history-id device position)
            for name = (or (first id) (format nil "GPU~D" position))
            for used = (ntop.gpu.domain:gpu-device-memory-used-bytes device)
            for total = (ntop.gpu.domain:gpu-device-memory-total-bytes device)
            for memory-percent = (ntop.gpu.domain:gpu-memory-percent used total)
            do (progn
                 (%append-composition-history
                  composition
                  (list :gpu :usage id)
                  name
                  record
                  (ntop.gpu.domain:gpu-device-usage-percent device))
                 (%append-composition-history
                  composition
                  (list :gpu :memory id)
                  name
                  record
                  memory-percent)
                 (%append-composition-history
                  composition
                  (list :gpu-memory :used id)
                  name
                  record
                  (and memory-percent used))
                 (when memory-percent
                   (setf (gethash
                          id
                          (application-composition-gpu-memory-device-totals
                           composition))
                         total))))))
  (defun %record-network-history (composition record sample)
    (%append-composition-history composition '(:network :window) "Network" record 1)
    (when sample
      (let* ((samples (application-composition-network-samples composition))
             (sampled-at (ntop.shared.domain:sample-record-sampled-at sample))
             (last-sample
              (when (plusp (length samples))
                (aref samples (1- (length samples)))))
             (last-time
              (and last-sample
                   (ntop.shared.domain:sample-record-sampled-at last-sample))))
        (when (and last-time (< sampled-at last-time))
          (error "Network samples must arrive in chronological order."))
        (if (and last-time (= sampled-at last-time))
            (setf (aref samples (1- (length samples))) sample)
            (vector-push-extend sample samples)))))
  (defun %disk-device-rate (previous current device counter-accessor)
    (let ((current-counter (funcall counter-accessor device)))
      (when current-counter
        (let* ((name (ntop.disk.domain:disk-device-device-name device))
               (previous-device
                 (and previous
                      (find name
                            (ntop.disk.domain:disk-sample-devices previous)
                            :key
                            (function ntop.disk.domain:disk-device-device-name)
                            :test
                            (function equal))))
               (previous-counter
                 (and previous-device
                      (funcall counter-accessor previous-device))))
          (if previous-counter
              (ntop.shared.domain:counter-rate-per-second
               previous-counter
               current-counter
               (- (ntop.shared.domain:sample-record-sampled-at current)
                  (ntop.shared.domain:sample-record-sampled-at previous)))
              0)))))
  (defun %record-disk-history (composition record sample)
    (when sample
      (let ((previous (application-composition-previous-disk-sample composition)))
        (loop for device in (ntop.disk.domain:disk-sample-devices sample)
              for position from 0
              for id = (list (ntop.disk.domain:disk-device-device-name device)
                             position)
              for name = (or (first id) (format nil "Disk~D" position))
              do (progn
                   (%append-composition-history
                    composition
                    (list :disk-io :read id)
                    name
                    record
                    (%disk-device-rate
                     previous sample device
                     (function ntop.disk.domain:disk-device-read-bytes)))
                   (%append-composition-history
                    composition
                    (list :disk-io :write id)
                    name
                    record
                    (%disk-device-rate
                     previous sample device
                     (function ntop.disk.domain:disk-device-write-bytes)))))
        (setf (application-composition-disk-rows composition)
              (ntop.disk.domain:disk-sample-rows previous sample)
              (application-composition-previous-disk-sample composition)
              sample))))
  (defun %record-temperature-history (composition record sample)
    (when sample
      (loop for sensor in (ntop.temperature.domain:temperature-sample-sensors sample)
            for position from 0
            for id = (list (ntop.temperature.domain:temperature-sensor-name sensor)
                           position)
            for name = (or (first id) (format nil "Sensor~D" position))
            for value =
              (ntop.temperature.domain:temperature-sensor-temperature-celsius sensor)
            do (%append-composition-history
                composition
                (list :temperature :sensor id)
                name
                record
                value))))
  (defun %record-snapshot-history (composition snapshot)
  (check-type snapshot ntop.app.domain:collection-snapshot)
  (setf (application-composition-latest-snapshot composition) snapshot)
  (let* ((sampled-at (ntop.app.domain:collection-snapshot-sampled-at snapshot))
         (record (ntop.shared.domain:make-sample-record :sampled-at sampled-at))
         (process-sample
           (ntop.app.domain:collection-snapshot-process-sample snapshot)))
    (%record-cpu-history
     composition record
     (ntop.app.domain:collection-snapshot-cpu-sample snapshot))
    (%record-memory-history
     composition record
     (ntop.app.domain:collection-snapshot-memory-sample snapshot))
    (%record-gpu-history
     composition record
     (ntop.app.domain:collection-snapshot-gpu-sample snapshot))
    (%record-network-history
     composition record
     (ntop.app.domain:collection-snapshot-network-sample snapshot))
    (%record-disk-history
     composition record
     (ntop.app.domain:collection-snapshot-disk-sample snapshot))
    (%record-temperature-history
     composition record
     (ntop.app.domain:collection-snapshot-temperature-sample snapshot))
    (when process-sample
      (setf (application-composition-previous-process-sample composition)
            (application-composition-latest-process-sample composition)
            (application-composition-latest-process-sample composition)
            process-sample))
    (%trim-network-samples composition sampled-at)
    snapshot))
  (defun %trim-network-samples (composition sampled-at)
    (let* ((cutoff
            (- sampled-at
               (ntop.shared.application:application-config-retention-seconds
                (application-composition-config composition))))
           (retained
            (loop for sample across (application-composition-network-samples
                                     composition)
                  when (>= (ntop.shared.domain:sample-record-sampled-at sample) cutoff)
                    collect sample)))
      (setf (application-composition-network-samples composition) (make-array
                                                                   (length retained)
                                                                   :adjustable
                                                                   t
                                                                   :fill-pointer
                                                                   t
                                                                   :initial-contents
                                                                   retained))))
