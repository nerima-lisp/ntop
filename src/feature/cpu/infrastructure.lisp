(in-package #:ntop.cpu.infrastructure)

(defconstant +cpu-accounting-mode+
  #+linux :linux
  #+darwin :darwin)

(defstruct
    (cpu-source-sample
     (:constructor %make-cpu-source-sample
                   (core-values load-average-1
                                load-average-5
                                load-average-15
                                cgroup-data))
     (:copier nil))
  (core-values nil :read-only t)
  (load-average-1 nil :read-only t)
  (load-average-5 nil :read-only t)
  (load-average-15 nil :read-only t)
  (cgroup-data nil :read-only t))

(defun make-cpu-source-sample (&key core-values
                                    load-average-1
                                    load-average-5
                                    load-average-15
                                    cgroup-data)
  (%make-cpu-source-sample (copy-list core-values)
                           load-average-1
                           load-average-5
                           load-average-15
                           (copy-tree cgroup-data)))

(defstruct
    (cpu-baseline (:constructor make-cpu-baseline (source sampled-at))
                  (:copier nil))
  (source nil :read-only t)
  (sampled-at nil :read-only t))

(defstruct
    (cpu-collector (:constructor %make-cpu-collector (source-reader clock)))
  source-reader
  clock
  baseline)

(defun make-cpu-collector (&key
                           (source-reader #'read-platform-cpu-source-sample)
                           clock)
  (unless (functionp source-reader)
    (error "SOURCE-READER must be a function, got ~S." source-reader))
  (unless clock
    (error "CLOCK is required for CPU sample timestamps."))
  (%make-cpu-collector source-reader clock))

(defun %cpu-source-core (source-core usage-percent)
  (ntop.cpu.domain:make-cpu-core-value :core-index
                                       (ntop.cpu.domain:cpu-core-value-core-index
                                        source-core)
                                       :user-ticks
                                       (ntop.cpu.domain:cpu-core-value-user-ticks
                                        source-core)
                                       :nice-ticks
                                       (ntop.cpu.domain:cpu-core-value-nice-ticks
                                        source-core)
                                       :system-ticks
                                       (ntop.cpu.domain:cpu-core-value-system-ticks
                                        source-core)
                                       :idle-ticks
                                       (ntop.cpu.domain:cpu-core-value-idle-ticks
                                        source-core)
                                       :iowait-ticks
                                       (ntop.cpu.domain:cpu-core-value-iowait-ticks
                                        source-core)
                                       :irq-ticks
                                       (ntop.cpu.domain:cpu-core-value-irq-ticks
                                        source-core)
                                       :softirq-ticks
                                       (ntop.cpu.domain:cpu-core-value-softirq-ticks
                                        source-core)
                                       :steal-ticks
                                       (ntop.cpu.domain:cpu-core-value-steal-ticks
                                        source-core)
                                       :guest-ticks
                                       (ntop.cpu.domain:cpu-core-value-guest-ticks
                                        source-core)
                                       :guest-nice-ticks
                                       (ntop.cpu.domain:cpu-core-value-guest-nice-ticks
                                        source-core)
                                       :usage-percent
                                       usage-percent
                                       :frequency-mhz
                                       (ntop.cpu.domain:cpu-core-value-frequency-mhz
                                        source-core)))

(defun %cpu-previous-core (previous-core-values core-index)
  (and previous-core-values
       (integerp core-index)
       (find core-index
             previous-core-values
             :key
             #'ntop.cpu.domain:cpu-core-value-core-index
             :test
             #'eql)))

(defun %cpu-sample-core-values (previous-source current-source)
  (mapcar
   (lambda (current-core)
     (let* ((core-index
             (ntop.cpu.domain:cpu-core-value-core-index current-core))
            (previous-core
             (%cpu-previous-core
              (and previous-source
                   (cpu-source-sample-core-values previous-source))
              core-index))
            (usage-percent
             (and previous-core
                  (ntop.cpu.domain:cpu-core-usage-percent previous-core
                                                          current-core
                                                          :mode
                                                          +cpu-accounting-mode+))))
       (%cpu-source-core current-core usage-percent)))
   (cpu-source-sample-core-values current-source)))

(defun %cpu-cgroup-average (previous-source current-source elapsed-seconds)
  (let* ((previous-data
          (and previous-source (cpu-source-sample-cgroup-data previous-source)))
         (current-data (cpu-source-sample-cgroup-data current-source)))
    (when current-data
      (ntop.cpu.domain:cpu-cgroup-average-percent
       (getf previous-data :cpu-usage-ns)
       (getf current-data :cpu-usage-ns)
       elapsed-seconds
       (getf current-data :cpu-quota-micros)
       (getf current-data :cpu-period-micros)))))

(defun %cpu-sample-from-source (previous-source current-source
                                                previous-time
                                                sampled-at)
  (let* ((elapsed-seconds (and previous-time (- sampled-at previous-time)))
         (source-cores (cpu-source-sample-core-values current-source))
         (core-values (%cpu-sample-core-values previous-source current-source))
         (host-average
          (and previous-source
               (ntop.cpu.domain:cpu-average-usage-percent
                (cpu-source-sample-core-values previous-source)
                source-cores
                :mode
                +cpu-accounting-mode+)))
         (cgroup-average
          (%cpu-cgroup-average previous-source current-source elapsed-seconds))
         (cgroup-data (cpu-source-sample-cgroup-data current-source)))
    (ntop.cpu.domain:make-cpu-sample :sampled-at
                                     sampled-at
                                     :core-values
                                     core-values
                                     :average-usage-percent
                                     (or cgroup-average host-average)
                                     :load-average-1
                                     (cpu-source-sample-load-average-1
                                      current-source)
                                     :load-average-5
                                     (cpu-source-sample-load-average-5
                                      current-source)
                                     :load-average-15
                                     (cpu-source-sample-load-average-15
                                      current-source)
                                     :cgroup-average-cpu-percent
                                     cgroup-average
                                     :cgroup-cpu-usage-ns
                                     (getf cgroup-data :cpu-usage-ns)
                                     :cgroup-cpu-quota-micros
                                     (getf cgroup-data :cpu-quota-micros)
                                     :cgroup-cpu-period-micros
                                     (getf cgroup-data :cpu-period-micros)
                                     :cgroup-cpu-throttled-ns
                                     (getf cgroup-data :cpu-throttled-ns)
                                     :cgroup-cpu-throttled-periods
                                     (getf cgroup-data :cpu-throttled-periods)
                                     :cgroup-cpu-limit-cores
                                     (getf cgroup-data :cpu-limit-cores))))

(defun %collect-cpu-source (collector source on-sample on-unavailable)
  (if (not (cpu-source-sample-p source))
      (funcall on-unavailable :read-failed)
      (ntop.shared.domain:read-monotonic-time
       (cpu-collector-clock collector)
       (lambda (sampled-at)
         (if (not (realp sampled-at))
             (funcall on-unavailable :read-failed)
             (let* ((baseline (cpu-collector-baseline collector))
                    (previous-source
                      (and baseline (cpu-baseline-source baseline)))
                    (previous-time
                      (and baseline (cpu-baseline-sampled-at baseline)))
                    (sample
                      (%cpu-sample-from-source
                       previous-source source previous-time sampled-at)))
               (setf (cpu-collector-baseline collector)
                     (make-cpu-baseline source sampled-at))
               (funcall on-sample sample))))
       (lambda (reason)
         (declare (ignore reason))
         (funcall on-unavailable :read-failed)))))

(defmethod ntop.shared.application:collect-cpu-sample
    ((collector cpu-collector) on-sample on-unavailable)
  (funcall
   (cpu-collector-source-reader collector)
   (lambda (source)
     (%collect-cpu-source collector source on-sample on-unavailable))
   (lambda (reason)
     (funcall on-unavailable
              (if (typep reason
                         'ntop.shared.application:collector-unavailability-reason)
                  reason
                  :read-failed)))))
