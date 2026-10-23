(in-package #:ntop.cpu.infrastructure)

(defun %capture-cpu-reader (reader)
  (let ((value nil)
        (reason nil))
    (funcall reader
             (lambda (result)
               (setf value result))
             (lambda (unavailable-reason)
               (setf reason unavailable-reason)))
    (values value reason)))

(defun %linux-cpu-core-index (name)
  (when (and (stringp name) (> (length name) 3) (string= "cpu" name :end2 3))
    (let ((suffix (subseq name 3)))
      (when (every #'ntop.platform.linux::ascii-linux-digit-p suffix)
        (parse-integer suffix)))))

(defun %linux-cpu-core-from-row (row)
  (let ((index (%linux-cpu-core-index (car row)))
        (ticks (cdr row)))
    (when (and index (>= (length ticks) 4))
      (ntop.cpu.domain:make-cpu-core-value :core-index
                                           index
                                           :user-ticks
                                           (nth 0 ticks)
                                           :nice-ticks
                                           (nth 1 ticks)
                                           :system-ticks
                                           (nth 2 ticks)
                                           :idle-ticks
                                           (nth 3 ticks)
                                           :iowait-ticks
                                           (nth 4 ticks)
                                           :irq-ticks
                                           (nth 5 ticks)
                                           :softirq-ticks
                                           (nth 6 ticks)
                                           :steal-ticks
                                           (nth 7 ticks)
                                           :guest-ticks
                                           (nth 8 ticks)
                                           :guest-nice-ticks
                                           (nth 9 ticks)))))

(defun %linux-cpu-frequency-mhz (core-index)
  (let ((frequency-mhz nil))
    (ntop.platform.linux::read-linux-file
     (format nil
             "sys/devices/system/cpu/cpu~D/cpufreq/scaling_cur_freq"
             core-index)
     (lambda (text)
       (let ((kilohertz
              (ntop.platform.linux::parse-linux-integer
               (string-trim '(#\Space #\Tab #\Newline #\Return) text))))
         (when (and kilohertz (plusp kilohertz))
           (setf frequency-mhz (/ kilohertz 1000)))))
     (lambda (reason)
       (declare (ignore reason))))
    frequency-mhz))

(defun %linux-cpu-core-with-frequency (core)
  (ntop.cpu.domain:make-cpu-core-value :core-index
                                       (ntop.cpu.domain:cpu-core-value-core-index
                                        core)
                                       :user-ticks
                                       (ntop.cpu.domain:cpu-core-value-user-ticks
                                        core)
                                       :nice-ticks
                                       (ntop.cpu.domain:cpu-core-value-nice-ticks
                                        core)
                                       :system-ticks
                                       (ntop.cpu.domain:cpu-core-value-system-ticks
                                        core)
                                       :idle-ticks
                                       (ntop.cpu.domain:cpu-core-value-idle-ticks
                                        core)
                                       :iowait-ticks
                                       (ntop.cpu.domain:cpu-core-value-iowait-ticks
                                        core)
                                       :irq-ticks
                                       (ntop.cpu.domain:cpu-core-value-irq-ticks
                                        core)
                                       :softirq-ticks
                                       (ntop.cpu.domain:cpu-core-value-softirq-ticks
                                        core)
                                       :steal-ticks
                                       (ntop.cpu.domain:cpu-core-value-steal-ticks
                                        core)
                                       :guest-ticks
                                       (ntop.cpu.domain:cpu-core-value-guest-ticks
                                        core)
                                       :guest-nice-ticks
                                       (ntop.cpu.domain:cpu-core-value-guest-nice-ticks
                                        core)
                                       :frequency-mhz
                                       (%linux-cpu-frequency-mhz
                                        (ntop.cpu.domain:cpu-core-value-core-index
                                         core))))

(defun %linux-cpu-stat-value (cgroup-data key)
  (cdr (assoc key (getf cgroup-data :cpu-stat) :test #'string=)))

(defun %linux-cgroup-source-data (cgroup-data)
  (when cgroup-data
    (let* ((version (getf cgroup-data :version))
           (quota (getf cgroup-data :cpu-quota))
           (period (getf cgroup-data :cpu-period))
           (usage (getf cgroup-data :cpu-usage))
           (usage-ns
            (and (integerp usage)
                 (not (minusp usage))
                 (if (eql version 2)
                     (* usage 1000)
                     usage)))
           (throttled
            (if (eql version 2)
                (* (or (%linux-cpu-stat-value cgroup-data "throttled_usec") 0)
                   1000)
                (%linux-cpu-stat-value cgroup-data "throttled_time")))
           (throttled-periods
            (%linux-cpu-stat-value cgroup-data "nr_throttled"))
           (valid-quota (and (integerp quota) (plusp quota) quota))
           (valid-period (and (integerp period) (plusp period) period)))
      (when (or usage-ns valid-quota valid-period throttled throttled-periods)
        (list :cpu-usage-ns
              usage-ns
              :cpu-quota-micros
              valid-quota
              :cpu-period-micros
              valid-period
              :cpu-throttled-ns
              throttled
              :cpu-throttled-periods
              throttled-periods
              :cpu-limit-cores
              (and valid-quota valid-period (/ valid-quota valid-period)))))))

(defun %linux-cpu-core-values (stat-rows)
  (let* ((core-rows
          (remove-if-not
           (lambda (row)
             (%linux-cpu-core-index (car row)))
           stat-rows))
         (cores (mapcar #'%linux-cpu-core-from-row core-rows)))
    (when (and cores (every #'identity cores))
      (mapcar #'%linux-cpu-core-with-frequency cores))))

(defun %build-linux-cpu-source-sample ()
  (multiple-value-bind (stat-rows stat-reason)
      (%capture-cpu-reader #'ntop.platform.linux:read-linux-proc-stat)
    (when stat-reason
      (return-from %build-linux-cpu-source-sample
        (values nil stat-reason)))
    (let ((core-values (%linux-cpu-core-values stat-rows)))
      (unless core-values
        (return-from %build-linux-cpu-source-sample
          (values nil :invalid-data)))
      (multiple-value-bind (load-average load-reason)
          (%capture-cpu-reader #'ntop.platform.linux:read-linux-proc-loadavg)
        (when load-reason
          (return-from %build-linux-cpu-source-sample
            (values nil load-reason)))
        (multiple-value-bind (cgroup-data cgroup-reason)
            (%capture-cpu-reader #'ntop.platform.linux:read-linux-cgroup-data)
          (declare (ignore cgroup-reason))
          (values
           (make-cpu-source-sample :core-values
                                   core-values
                                   :load-average-1
                                   (getf load-average :one)
                                   :load-average-5
                                   (getf load-average :five)
                                   :load-average-15
                                   (getf load-average :fifteen)
                                   :cgroup-data
                                   (%linux-cgroup-source-data cgroup-data))
           nil))))))

(defun read-linux-cpu-source-sample (on-value on-unavailable)
  (multiple-value-bind (source reason)
      (%build-linux-cpu-source-sample)
    (if reason
        (funcall on-unavailable :read-failed)
        (funcall on-value source))))

#+linux
(defun read-platform-cpu-source-sample (on-value on-unavailable)
  (read-linux-cpu-source-sample on-value on-unavailable))
