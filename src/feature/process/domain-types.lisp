(in-package #:ntop.process.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (process-thread (:constructor make-process-thread-raw) (:copier nil))
    (thread-id nil :read-only t)
    (name nil :read-only t)
    (cpu-time-ns nil :read-only t)
    (state nil :read-only t)
    (priority nil :read-only t)
    (nice nil :read-only t))
  (defun make-process-thread (&rest initargs)
    (let ((instance (apply #'make-process-thread-raw initargs)))
      (validate-typed-slots instance
                            '((process-thread-thread-id (or null integer))
                              (process-thread-name (or null string))
                              (process-thread-cpu-time-ns
                               (or null ntop.shared.domain:measurement-counter))
                              (process-thread-state (or null string))
                              (process-thread-priority (or null integer))
                              (process-thread-nice (or null integer))))
      instance)))

(progn
  (defstruct
      (process-record (:constructor make-process-record-raw) (:copier nil))
    (pid nil :read-only t)
    (ppid nil :read-only t)
    (name nil :read-only t)
    (command nil :read-only t)
    (cpu-time-ns nil :read-only t)
    (resident-memory-bytes nil :read-only t)
    (virtual-memory-bytes nil :read-only t)
    (read-bytes nil :read-only t)
    (write-bytes nil :read-only t)
    (uid nil :read-only t)
    (user-name nil :read-only t)
    (state nil :read-only t)
    (start-time-unix-seconds nil :read-only t)
    (priority nil :read-only t)
    (nice nil :read-only t)
    (kernel-thread-p :unknown :read-only t)
    (threads nil :read-only t)
    (gpu-usage-percent nil :read-only t)
    (gpu-memory-bytes nil :read-only t)
    (ps-fallback-cpu-percent nil :read-only t))
  (defun make-process-record (&rest initargs)
    (let ((instance (apply #'make-process-record-raw initargs)))
      (validate-typed-slots instance
                            '((process-record-pid (or null integer))
                              (process-record-ppid (or null integer))
                              (process-record-name (or null string))
                              (process-record-command (or null string))
                              (process-record-cpu-time-ns
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-resident-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-virtual-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-read-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-write-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-uid (or null integer))
                              (process-record-user-name (or null string))
                              (process-record-state (or null string))
                              (process-record-start-time-unix-seconds
                               ntop.shared.domain:measurement-value)
                              (process-record-priority (or null integer))
                              (process-record-nice (or null integer))
                              (process-record-kernel-thread-p
                               (member t nil :unknown))
                              (process-record-threads list)
                              (process-record-gpu-usage-percent
                               ntop.shared.domain:measurement-value)
                              (process-record-gpu-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-record-ps-fallback-cpu-percent
                               ntop.shared.domain:measurement-value)))
      instance)))

(progn
  (defstruct
      (process-signal (:constructor make-process-signal-raw) (:copier nil))
    (operating-system nil :read-only t)
    (name nil :read-only t)
    (number nil :read-only t))
  (defun make-process-signal (&rest initargs)
    (let ((instance (apply #'make-process-signal-raw initargs)))
      (validate-typed-slots instance
                            '((process-signal-operating-system (or null string))
                              (process-signal-name (or null string))
                              (process-signal-number (or null integer))))
      instance)))

(progn
  (defstruct
      (process-sample (:include ntop.shared.domain:sample-record)
                      (:constructor make-process-sample-raw)
                      (:copier nil))
    (processes nil :read-only t)
    (signals nil :read-only t))
  (defun make-process-sample (&rest initargs)
    (let ((instance (apply #'make-process-sample-raw initargs)))
      (validate-typed-slots instance
                            '((process-sample-processes list)
                              (process-sample-signals list)))
      instance)))

(progn
  (defstruct (process-row (:constructor make-process-row-raw) (:copier nil))
    (count nil :read-only t)
    (pid nil :read-only t)
    (ppid nil :read-only t)
    (name nil :read-only t)
    (command nil :read-only t)
    (cpu-percent nil :read-only t)
    (memory-percent nil :read-only t)
    (memory-bytes nil :read-only t)
    (virtual-memory-bytes nil :read-only t)
    (read-bytes-per-second nil :read-only t)
    (write-bytes-per-second nil :read-only t)
    (total-read-bytes nil :read-only t)
    (total-write-bytes nil :read-only t)
    (user-name nil :read-only t)
    (state nil :read-only t)
    (elapsed-seconds nil :read-only t)
    (priority nil :read-only t)
    (nice nil :read-only t)
    (gpu-usage-percent nil :read-only t)
    (gpu-memory-bytes nil :read-only t)
    (gpu-memory-percent nil :read-only t))
  (defun make-process-row (&rest initargs)
    (let ((instance (apply #'make-process-row-raw initargs)))
      (validate-typed-slots instance
                            '((process-row-count (or null integer))
                              (process-row-pid (or null integer))
                              (process-row-ppid (or null integer))
                              (process-row-name (or null string))
                              (process-row-command (or null string))
                              (process-row-cpu-percent
                               ntop.shared.domain:measurement-value)
                              (process-row-memory-percent
                               ntop.shared.domain:measurement-value)
                              (process-row-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-row-virtual-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-row-read-bytes-per-second
                               ntop.shared.domain:measurement-value)
                              (process-row-write-bytes-per-second
                               ntop.shared.domain:measurement-value)
                              (process-row-total-read-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-row-total-write-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-row-user-name (or null string))
                              (process-row-state (or null string))
                              (process-row-elapsed-seconds
                               ntop.shared.domain:measurement-value)
                              (process-row-priority (or null integer))
                              (process-row-nice (or null integer))
                              (process-row-gpu-usage-percent
                               ntop.shared.domain:measurement-value)
                              (process-row-gpu-memory-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (process-row-gpu-memory-percent
                               ntop.shared.domain:measurement-value)))
      instance)))
