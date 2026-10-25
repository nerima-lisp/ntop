(in-package #:ntop.process.domain)

(defun %process-in-sample (sample pid)
  (when (and sample pid)
    (or (and (process-sample-pid-index sample)
             (gethash pid (process-sample-pid-index sample)))
        (find pid (process-sample-processes sample)
              :key (function process-record-pid)
              :test (function eql)))))

(defun %sample-elapsed-seconds (previous-sample current-sample)
  (when (and previous-sample current-sample)
    (- (ntop.shared.domain:sample-record-sampled-at current-sample)
       (ntop.shared.domain:sample-record-sampled-at previous-sample))))

(defun %process-counter-rate (previous-sample current-sample pid accessor)
  (let* ((previous (%process-in-sample previous-sample pid))
         (current (%process-in-sample current-sample pid))
         (elapsed (%sample-elapsed-seconds previous-sample current-sample)))
    (when (and previous current elapsed (plusp elapsed))
      (ntop.shared.domain:counter-rate-per-second
       (funcall accessor previous)
       (funcall accessor current)
       elapsed))))

(defun process-read-bytes-per-second (previous-sample current-sample pid)
  (%process-counter-rate previous-sample current-sample pid
                         #'process-record-read-bytes))

(defun process-write-bytes-per-second (previous-sample current-sample pid)
  (%process-counter-rate previous-sample current-sample pid
                         #'process-record-write-bytes))

(defun %normalize-process-cpu-percent
    (cpu-percent cpu-capacity unnormalized-p current-usage-p
     active-system-cpu-fraction)
  (let ((normalized
          (when cpu-percent
            (if unnormalized-p
                cpu-percent
                (when (and cpu-capacity (plusp cpu-capacity))
                  (/ cpu-percent cpu-capacity))))))
    (when normalized
      (if current-usage-p
          (cond ((null active-system-cpu-fraction) nil)
                ((zerop active-system-cpu-fraction) 0)
                (t (/ normalized active-system-cpu-fraction)))
          normalized))))

(defun process-cpu-percent
    (previous-sample current-sample pid cpu-capacity
     &key (cpu-mode :unix) unnormalized-p current-usage-p
       active-system-cpu-fraction linux-active-cpu-delta)
  (let* ((previous (%process-in-sample previous-sample pid))
         (current (%process-in-sample current-sample pid))
         (elapsed (%sample-elapsed-seconds previous-sample current-sample)))
    (when current
      (let* ((previous-time (and previous
                                 (process-record-cpu-time-ns previous)))
             (current-time (process-record-cpu-time-ns current))
             (counter-rate
               (when (and previous-time current-time elapsed (plusp elapsed))
                 (ntop.shared.domain:counter-rate-per-second
                  previous-time current-time elapsed))))
        (cond
          ((null counter-rate)
           (%normalize-process-cpu-percent
            (process-record-ps-fallback-cpu-percent current)
            cpu-capacity unnormalized-p nil nil))
          ((eq cpu-mode :unix)
           (%normalize-process-cpu-percent
            (* counter-rate 100/1000000000)
            cpu-capacity unnormalized-p current-usage-p
            active-system-cpu-fraction))
          ((eq cpu-mode :linux)
           (let ((active-rate
                   (and linux-active-cpu-delta
                        (ntop.shared.domain:counter-rate-per-second
                         0 linux-active-cpu-delta elapsed))))
             (when (and active-rate (plusp active-rate))
               (let* ((capacity-adjusted-rate
                        (if unnormalized-p
                            (and cpu-capacity
                                 (plusp cpu-capacity)
                                 (/ active-rate cpu-capacity))
                            active-rate))
                      (usage-percent
                        (and capacity-adjusted-rate
                             (* 100 (/ counter-rate capacity-adjusted-rate)))))
                 (when usage-percent
                   (if current-usage-p
                       usage-percent
                       (and active-system-cpu-fraction
                            (* usage-percent active-system-cpu-fraction))))))))
          (t (error "Unsupported process CPU accounting mode: ~S" cpu-mode)))))))

(defun %memory-percent (memory-bytes total-memory-bytes)
  (when (and memory-bytes total-memory-bytes
             (plusp total-memory-bytes))
    (* 100 (/ memory-bytes total-memory-bytes))))

(defun %runtime-seconds (record current-time-unix-seconds)
  (let ((start-time (process-record-start-time-unix-seconds record)))
    (when (and start-time current-time-unix-seconds)
      (max 0 (- current-time-unix-seconds start-time)))))

(defun process-row-from-samples
    (previous-sample current-sample pid
     &key cpu-capacity active-system-cpu-fraction memory-total-bytes
       gpu-memory-total-bytes
       current-time-unix-seconds unnormalized-p current-usage-p (cpu-mode :unix)
       linux-active-cpu-delta)
  (let ((record (%process-in-sample current-sample pid)))
    (when record
      (make-process-row
       :count 1
       :pid (process-record-pid record)
       :ppid (process-record-ppid record)
       :name (process-record-name record)
       :command (process-record-command record)
       :cpu-percent
       (process-cpu-percent
        previous-sample current-sample pid cpu-capacity
        :cpu-mode cpu-mode
        :unnormalized-p unnormalized-p
        :current-usage-p current-usage-p
        :active-system-cpu-fraction active-system-cpu-fraction
        :linux-active-cpu-delta linux-active-cpu-delta)
       :memory-bytes (process-record-resident-memory-bytes record)
       :memory-percent
       (%memory-percent (process-record-resident-memory-bytes record)
                        memory-total-bytes)
       :virtual-memory-bytes
       (process-record-virtual-memory-bytes record)
       :read-bytes-per-second
       (process-read-bytes-per-second previous-sample current-sample pid)
       :write-bytes-per-second
       (process-write-bytes-per-second previous-sample current-sample pid)
       :total-read-bytes (process-record-read-bytes record)
       :total-write-bytes (process-record-write-bytes record)
       :user-name (process-record-user-name record)
       :state (process-record-state record)
       :elapsed-seconds (%runtime-seconds record current-time-unix-seconds)
       :priority (process-record-priority record)
       :nice (process-record-nice record)
       :gpu-usage-percent (process-record-gpu-usage-percent record)
       :gpu-memory-bytes (process-record-gpu-memory-bytes record)
       :gpu-memory-percent
       (%memory-percent (process-record-gpu-memory-bytes record)
                        gpu-memory-total-bytes))))
)
