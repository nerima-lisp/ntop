(in-package #:ntop.cpu.domain)

(defun %cpu-counter-delta (previous current)
  (when
      (and (integerp previous)
           (not (minusp previous))
           (integerp current)
           (not (minusp current)))
    (max 0 (- current previous))))

(defun %cpu-required-deltas (previous current accessors)
  (let ((deltas
         (mapcar
          (lambda (accessor)
            (%cpu-counter-delta (funcall accessor previous)
                                (funcall accessor current)))
          accessors)))
    (when (every #'identity deltas)
      deltas)))

(defun %cpu-optional-delta (previous current accessor)
  (let ((previous-value (funcall accessor previous))
        (current-value (funcall accessor current)))
    (cond
      ((and (null previous-value) (null current-value)) 0)
      (t (%cpu-counter-delta previous-value current-value)))))

(defun %cpu-linux-deltas (previous current)
  (let ((required
         (%cpu-required-deltas previous
                               current
                               (list #'cpu-core-value-user-ticks
                                     #'cpu-core-value-nice-ticks
                                     #'cpu-core-value-system-ticks
                                     #'cpu-core-value-idle-ticks)))
        (iowait
         (%cpu-optional-delta previous current #'cpu-core-value-iowait-ticks))
        (irq (%cpu-optional-delta previous current #'cpu-core-value-irq-ticks))
        (softirq
         (%cpu-optional-delta previous current #'cpu-core-value-softirq-ticks))
        (steal
         (%cpu-optional-delta previous current #'cpu-core-value-steal-ticks)))
    (when (and required iowait irq softirq steal)
      (let ((user (first required))
            (nice (second required))
            (system (third required))
            (idle (fourth required)))
        (values (+ user nice system irq softirq steal)
                (+ user nice system idle iowait irq softirq steal)
                t)))))

(defun %cpu-darwin-deltas (previous current)
  (let ((deltas
         (%cpu-required-deltas previous
                               current
                               (list #'cpu-core-value-user-ticks
                                     #'cpu-core-value-system-ticks
                                     #'cpu-core-value-nice-ticks
                                     #'cpu-core-value-idle-ticks))))
    (when deltas
      (let ((active (+ (first deltas) (second deltas) (third deltas))))
        (values active (+ active (fourth deltas)) t)))))

(defun %cpu-tick-deltas (previous current mode)
  (ecase mode
    (:linux (%cpu-linux-deltas previous current))
    (:darwin (%cpu-darwin-deltas previous current))))

(defun %cpu-percent-from-deltas (active total)
  (when (and active total)
    (if (zerop total)
        0
        (* 100 (/ active total)))))

(defun cpu-core-usage-percent (previous current &key (mode :linux))
  (multiple-value-bind (active total valid-p)
      (%cpu-tick-deltas previous current mode)
    (when valid-p
      (%cpu-percent-from-deltas active total))))

(defun cpu-average-usage-percent (previous-core-values current-core-values
                                                       &key
                                                       (mode :linux))
  (let ((active-total 0)
        (tick-total 0)
        (matched-p nil))
    (dolist (current current-core-values)
      (let* ((index (cpu-core-value-core-index current))
             (previous
              (and (integerp index)
                   (find index
                         previous-core-values
                         :key
                         #'cpu-core-value-core-index
                         :test
                         #'eql))))
        (when previous
          (multiple-value-bind (active total valid-p)
              (%cpu-tick-deltas previous current mode)
            (when valid-p
              (incf active-total active)
              (incf tick-total total)
              (setf matched-p t))))))
    (when matched-p
      (%cpu-percent-from-deltas active-total tick-total))))

(defun cpu-cgroup-average-percent (previous-usage-ns current-usage-ns
                                                     elapsed-seconds
                                                     quota-micros
                                                     period-micros)
  (when
      (and (integerp previous-usage-ns)
           (not (minusp previous-usage-ns))
           (integerp current-usage-ns)
           (not (minusp current-usage-ns))
           (realp elapsed-seconds)
           (plusp elapsed-seconds)
           (integerp quota-micros)
           (plusp quota-micros)
           (integerp period-micros)
           (plusp period-micros))
    (let* ((used-ns (max 0 (- current-usage-ns previous-usage-ns)))
           (capacity-ns
            (* elapsed-seconds 1000000000 (/ quota-micros period-micros))))
      (min 100 (* 100 (/ used-ns capacity-ns))))))

(defun %cpu-frequency-state-name (state-name)
  (cond
    ((stringp state-name) (string-upcase state-name))
    ((symbolp state-name) (string-upcase (symbol-name state-name)))
    (t nil)))

(defun %cpu-excluded-frequency-state-p (state-name)
  (let ((name (%cpu-frequency-state-name state-name)))
    (and name (or (search "DOWN" name) (search "IDLE" name)))))

(defun %valid-cpu-frequency-table-p (dvfs-khz-table)
  (and (listp dvfs-khz-table)
       dvfs-khz-table
       (every
        (lambda (frequency)
          (and (realp frequency) (plusp frequency)))
        dvfs-khz-table)))

(defun cpu-weighted-frequency-mhz (states dvfs-khz-table)
  (when (and (listp states) (%valid-cpu-frequency-table-p dvfs-khz-table))
    (let ((active-index 0)
          (weighted-khz 0)
          (residency-total 0)
          (valid-p t))
      (dolist (state states)
        (if (and (consp state)
                 (%cpu-frequency-state-name (car state))
                 (realp (cdr state))
                 (not (minusp (cdr state))))
            (unless (%cpu-excluded-frequency-state-p (car state))
              (let ((frequency (nth active-index dvfs-khz-table)))
                (if frequency
                    (progn
                      (incf weighted-khz (* frequency (cdr state)))
                      (incf residency-total (cdr state))
                      (incf active-index))
                    (setf valid-p nil))))
            (setf valid-p nil)))
      (when (and valid-p (plusp residency-total))
        (/ weighted-khz residency-total 1000)))))
