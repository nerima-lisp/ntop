(in-package #:ntop.cpu.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (cpu-core-value (:constructor make-cpu-core-value-raw) (:copier nil))
    (core-index nil :read-only t)
    (user-ticks nil :read-only t)
    (nice-ticks nil :read-only t)
    (system-ticks nil :read-only t)
    (idle-ticks nil :read-only t)
    (iowait-ticks nil :read-only t)
    (irq-ticks nil :read-only t)
    (softirq-ticks nil :read-only t)
    (steal-ticks nil :read-only t)
    (guest-ticks nil :read-only t)
    (guest-nice-ticks nil :read-only t)
    (usage-percent nil :read-only t)
    (frequency-mhz nil :read-only t))
  (defun make-cpu-core-value (&rest initargs)
    (let ((instance (apply #'make-cpu-core-value-raw initargs)))
      (validate-typed-slots instance
                            '((cpu-core-value-core-index (or null integer))
                              (cpu-core-value-user-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-nice-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-system-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-idle-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-iowait-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-irq-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-softirq-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-steal-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-guest-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-guest-nice-ticks
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-core-value-usage-percent
                               ntop.shared.domain:measurement-value)
                              (cpu-core-value-frequency-mhz
                               ntop.shared.domain:measurement-value)))
      instance)))

(progn
  (defstruct
      (cpu-sample (:include ntop.shared.domain:sample-record)
                  (:constructor make-cpu-sample-raw)
                  (:copier nil))
    (core-values nil :read-only t)
    (average-usage-percent nil :read-only t)
    (load-average-1 nil :read-only t)
    (load-average-5 nil :read-only t)
    (load-average-15 nil :read-only t)
    (cgroup-average-cpu-percent nil :read-only t)
    (cgroup-cpu-usage-ns nil :read-only t)
    (cgroup-cpu-quota-micros nil :read-only t)
    (cgroup-cpu-period-micros nil :read-only t)
    (cgroup-cpu-throttled-ns nil :read-only t)
    (cgroup-cpu-throttled-periods nil :read-only t)
    (cgroup-cpu-limit-cores nil :read-only t))
  (defun make-cpu-sample (&rest initargs)
    (let ((instance (apply #'make-cpu-sample-raw initargs)))
      (validate-typed-slots instance
                            '((cpu-sample-core-values list)
                              (cpu-sample-average-usage-percent
                               ntop.shared.domain:measurement-value)
                              (cpu-sample-load-average-1
                               ntop.shared.domain:measurement-value)
                              (cpu-sample-load-average-5
                               ntop.shared.domain:measurement-value)
                              (cpu-sample-load-average-15
                               ntop.shared.domain:measurement-value)
                              (cpu-sample-cgroup-average-cpu-percent
                               ntop.shared.domain:measurement-value)
                              (cpu-sample-cgroup-cpu-usage-ns
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-sample-cgroup-cpu-quota-micros
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-sample-cgroup-cpu-period-micros
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-sample-cgroup-cpu-throttled-ns
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-sample-cgroup-cpu-throttled-periods
                               (or null ntop.shared.domain:measurement-counter))
                              (cpu-sample-cgroup-cpu-limit-cores
                               ntop.shared.domain:measurement-value)))
      instance)))
