(in-package #:ntop.platform.linux)

(defun parse-linux-proc-stat (text on-value on-unavailable)
  (let ((rows nil)
        (invalid-p nil))
    (dolist (line (split-linux-lines text))
      (let ((fields (split-linux-fields line)))
        (when fields
          (let ((values (mapcar #'parse-linux-integer (rest fields))))
            (if (every #'identity values)
                (push (cons (first fields) values) rows)
                (setf invalid-p t))))))
    (if (or invalid-p (null rows))
        (funcall on-unavailable :invalid-data)
        (funcall on-value (nreverse rows)))))

(define-linux-file-reader read-linux-proc-stat
                          "proc/stat"
                          parse-linux-proc-stat)

(defun split-linux-token (text character)
  (let ((position (position character text)))
    (when position
      (values (subseq text 0 position) (subseq text (1+ position)) t))))

(defun parse-linux-proc-loadavg (text on-value on-unavailable)
  (let ((fields (split-linux-fields text)))
    (multiple-value-bind (running-text total-text count-valid-p)
        (and (fourth fields) (split-linux-token (fourth fields) #\/))
      (let ((one (and (first fields) (parse-linux-decimal (first fields))))
            (five (and (second fields) (parse-linux-decimal (second fields))))
            (fifteen (and (third fields) (parse-linux-decimal (third fields))))
            (running (and running-text (parse-linux-integer running-text)))
            (total (and total-text (parse-linux-integer total-text)))
            (last-pid (and (fifth fields) (parse-linux-integer (fifth fields)))))
        (if (and one five fifteen count-valid-p running total last-pid)
            (funcall on-value
                     (list :one
                           one
                           :five
                           five
                           :fifteen
                           fifteen
                           :running
                           running
                           :total
                           total
                           :last-pid
                           last-pid))
            (funcall on-unavailable :invalid-data))))))

(define-linux-file-reader read-linux-proc-loadavg
                          "proc/loadavg"
                          parse-linux-proc-loadavg)

(defun parse-linux-proc-meminfo (text on-value on-unavailable)
  (let ((fields nil)
        (invalid-p nil))
    (dolist (line (split-linux-lines text))
      (let ((colon (position #\: line)))
        (when (and colon (plusp colon))
          (let* ((key
                  (string-trim '(#\Space #\Tab #\Return) (subseq line 0 colon)))
                 (values (split-linux-fields (subseq line (1+ colon))))
                 (amount
                  (and (first values) (parse-linux-integer (first values)))))
            (if (and amount (<= (length values) 2))
                (push (list key amount (second values)) fields)
                (setf invalid-p t))))))
    (if (or invalid-p (null fields))
        (funcall on-unavailable :invalid-data)
        (funcall on-value (nreverse fields)))))

(define-linux-file-reader read-linux-proc-meminfo
                          "proc/meminfo"
                          parse-linux-proc-meminfo)

(defun parse-linux-key-value-table (text)
  (let ((rows nil)
        (invalid-p nil))
    (dolist (line (split-linux-lines text))
      (let ((fields (split-linux-fields line)))
        (when fields
          (let ((value
                 (and (second fields) (parse-linux-integer (second fields)))))
            (if (and value (= (length fields) 2))
                (push (cons (first fields) value) rows)
                (setf invalid-p t))))))
    (if invalid-p
        (values nil nil)
        (values (nreverse rows) t))))

(defparameter +linux-cgroup-file-paths+
  '("sys/fs/cgroup/cpu.max" "sys/fs/cgroup/cpu.stat"
                            "sys/fs/cgroup/memory.max"
                            "sys/fs/cgroup/memory.current"
                            "sys/fs/cgroup/memory.stat"
                            "sys/fs/cgroup/cpu/cpu.cfs_quota_us"
                            "sys/fs/cgroup/cpu/cpu.cfs_period_us"
                            "sys/fs/cgroup/cpu/cpu.stat"
                            "sys/fs/cgroup/cpu/cpuacct.usage"
                            "sys/fs/cgroup/cpuacct/cpuacct.usage"
                            "sys/fs/cgroup/cpu,cpuacct/cpu.cfs_quota_us"
                            "sys/fs/cgroup/cpu,cpuacct/cpu.cfs_period_us"
                            "sys/fs/cgroup/cpu,cpuacct/cpu.stat"
                            "sys/fs/cgroup/cpu,cpuacct/cpuacct.usage"
                            "sys/fs/cgroup/cpuacct,cpu/cpu.cfs_quota_us"
                            "sys/fs/cgroup/cpuacct,cpu/cpu.cfs_period_us"
                            "sys/fs/cgroup/cpuacct,cpu/cpu.stat"
                            "sys/fs/cgroup/cpuacct,cpu/cpuacct.usage"
                            "sys/fs/cgroup/memory/memory.limit_in_bytes"
                            "sys/fs/cgroup/memory/memory.usage_in_bytes"
                            "sys/fs/cgroup/memory/memory.stat"))

(defun read-linux-cgroup-files (paths on-value)
  (let ((files nil))
    (dolist (path paths)
      (read-linux-file path
                       (lambda (text)
                         (push (cons path text) files))
                       (lambda (reason)
                         (declare (ignore reason)))))
    (funcall on-value (nreverse files))))

(defun linux-first-cgroup-entry (files paths)
  (loop for path in paths
        for entry = (assoc path files :test #'string=)
        when entry
          return entry))

(defun parse-linux-cgroup-number (entry)
  (when entry
    (let ((fields (split-linux-fields (cdr entry))))
      (when (= (length fields) 1)
        (parse-linux-integer (first fields))))))

(defun parse-linux-cgroup-limit (entry)
  (when entry
    (let ((fields (split-linux-fields (cdr entry))))
      (when (= (length fields) 1)
        (if (string= (first fields) "max")
            (values :unlimited t)
            (let ((value (parse-linux-integer (first fields))))
              (values value (not (null value)))))))))

(defun parse-linux-cgroup-cpu-max (entry)
  (when entry
    (let ((fields (split-linux-fields (cdr entry))))
      (when (= (length fields) 2)
        (multiple-value-bind (quota quota-valid-p)
            (if (string= (first fields) "max")
                (values :unlimited t)
                (let ((value (parse-linux-integer (first fields))))
                  (values value (not (null value)))))
          (let ((period (parse-linux-integer (second fields))))
            (when (and quota-valid-p period (plusp period))
              (values quota period t))))))))

(defun parse-linux-cgroup-v2 (files on-value on-unavailable)
  (let* ((cpu-max (assoc "sys/fs/cgroup/cpu.max" files :test #'string=))
         (cpu-stat-entry (assoc "sys/fs/cgroup/cpu.stat" files :test #'string=))
         (memory-max (assoc "sys/fs/cgroup/memory.max" files :test #'string=))
         (memory-current
          (assoc "sys/fs/cgroup/memory.current" files :test #'string=))
         (memory-stat-entry
          (assoc "sys/fs/cgroup/memory.stat" files :test #'string=)))
    (multiple-value-bind (quota period cpu-valid-p)
        (parse-linux-cgroup-cpu-max cpu-max)
      (multiple-value-bind (memory-limit memory-limit-valid-p)
          (parse-linux-cgroup-limit memory-max)
        (let ((cpu-stat nil)
              (memory-stat nil)
              (cpu-stat-valid-p t)
              (memory-stat-valid-p t))
          (when cpu-stat-entry
            (multiple-value-setq (cpu-stat cpu-stat-valid-p) (parse-linux-key-value-table
                                                              (cdr
                                                               cpu-stat-entry))))
          (when memory-stat-entry
            (multiple-value-setq (memory-stat memory-stat-valid-p) (parse-linux-key-value-table
                                                                    (cdr
                                                                     memory-stat-entry))))
          (let ((memory-usage (parse-linux-cgroup-number memory-current)))
            (if (and (or (null cpu-max) cpu-valid-p)
                     (or (null memory-max) memory-limit-valid-p)
                     cpu-stat-valid-p
                     memory-stat-valid-p
                     (or (null memory-current) memory-usage))
                (funcall on-value
                         (list :version
                               2
                               :cpu-quota
                               quota
                               :cpu-period
                               period
                               :cpu-stat
                               cpu-stat
                               :cpu-usage
                               (cdr
                                (assoc "usage_usec" cpu-stat :test #'string=))
                               :memory-limit
                               memory-limit
                               :memory-usage
                               memory-usage
                               :memory-stat
                               memory-stat))
                (funcall on-unavailable :invalid-data))))))))

(defun linux-v1-cgroup-paths (controller filename)
  (loop for directory in (if (string= controller "cpu")
                             '("cpu" "cpu,cpuacct" "cpuacct,cpu")
                             '("memory"))
        collect (format nil "sys/fs/cgroup/~A/~A" directory filename)))

(defun parse-linux-cgroup-v1 (files on-value on-unavailable)
  (let* ((cpu-quota-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "cpu"
                                                           "cpu.cfs_quota_us")))
         (cpu-period-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "cpu"
                                                           "cpu.cfs_period_us")))
         (cpu-stat-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "cpu" "cpu.stat")))
         (cpu-usage-entry
          (linux-first-cgroup-entry files
                                    '("sys/fs/cgroup/cpu/cpuacct.usage"
                                      "sys/fs/cgroup/cpuacct/cpuacct.usage"
                                      "sys/fs/cgroup/cpu,cpuacct/cpuacct.usage"
                                      "sys/fs/cgroup/cpuacct,cpu/cpuacct.usage")))
         (memory-limit-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "memory"
                                                           "memory.limit_in_bytes")))
         (memory-usage-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "memory"
                                                           "memory.usage_in_bytes")))
         (memory-stat-entry
          (linux-first-cgroup-entry files
                                    (linux-v1-cgroup-paths "memory"
                                                           "memory.stat"))))
    (if (not
         (some #'identity
               (list cpu-quota-entry
                     cpu-period-entry
                     cpu-stat-entry
                     cpu-usage-entry
                     memory-limit-entry
                     memory-usage-entry
                     memory-stat-entry)))
        (funcall on-unavailable :unavailable)
        (let ((cpu-quota (parse-linux-cgroup-number cpu-quota-entry))
              (cpu-period (parse-linux-cgroup-number cpu-period-entry))
              (cpu-usage (parse-linux-cgroup-number cpu-usage-entry))
              (memory-limit (parse-linux-cgroup-number memory-limit-entry))
              (memory-usage (parse-linux-cgroup-number memory-usage-entry))
              (cpu-stat nil)
              (memory-stat nil)
              (cpu-stat-valid-p t)
              (memory-stat-valid-p t))
          (when cpu-stat-entry
            (multiple-value-setq (cpu-stat cpu-stat-valid-p) (parse-linux-key-value-table
                                                              (cdr
                                                               cpu-stat-entry))))
          (when memory-stat-entry
            (multiple-value-setq (memory-stat memory-stat-valid-p) (parse-linux-key-value-table
                                                                    (cdr
                                                                     memory-stat-entry))))
          (if (and (or (null cpu-quota-entry) cpu-quota)
                   (or (null cpu-period-entry) cpu-period)
                   (or (null cpu-usage-entry) cpu-usage)
                   (or (null memory-limit-entry) memory-limit)
                   (or (null memory-usage-entry) memory-usage)
                   cpu-stat-valid-p
                   memory-stat-valid-p)
              (funcall on-value
                       (list :version
                             1
                             :cpu-quota
                             cpu-quota
                             :cpu-period
                             cpu-period
                             :cpu-stat
                             cpu-stat
                             :cpu-usage
                             cpu-usage
                             :memory-limit
                             memory-limit
                             :memory-usage
                             memory-usage
                             :memory-stat
                             memory-stat))
              (funcall on-unavailable :invalid-data))))))

(defun parse-linux-cgroup-files (files on-value on-unavailable)
  (cond
    ((or (assoc "sys/fs/cgroup/cpu.max" files :test #'string=)
         (assoc "sys/fs/cgroup/memory.max" files :test #'string=))
     (parse-linux-cgroup-v2 files on-value on-unavailable))
    (t (parse-linux-cgroup-v1 files on-value on-unavailable))))

(defun read-linux-cgroup-data (on-value on-unavailable)
  (read-linux-cgroup-files +linux-cgroup-file-paths+
                           (lambda (files)
                             (parse-linux-cgroup-files files
                                                       on-value
                                                       on-unavailable))))
