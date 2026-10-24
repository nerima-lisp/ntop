(in-package #:ntop.gpu.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct (gpu-device (:constructor make-gpu-device-raw) (:copier nil))
    (name nil :read-only t)
    (usage-percent nil :read-only t)
    (memory-used-bytes nil :read-only t)
    (memory-total-bytes nil :read-only t)
    (temperature-celsius nil :read-only t))
  (defun make-gpu-device (&rest initargs)
    (let ((instance (apply #'make-gpu-device-raw initargs)))
      (validate-typed-slots instance
                            '((gpu-device-name (or null string))
                              (gpu-device-usage-percent
                               ntop.shared.domain:measurement-value)
                              (gpu-device-memory-used-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (gpu-device-memory-total-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (gpu-device-temperature-celsius
                               ntop.shared.domain:measurement-value)))
      instance)))

(progn
  (defstruct
      (gpu-sample (:include ntop.shared.domain:sample-record)
                  (:constructor make-gpu-sample-raw)
                  (:copier nil))
    (devices nil :read-only t))
  (defun make-gpu-sample (&rest initargs)
    (let ((instance (apply #'make-gpu-sample-raw initargs)))
      (validate-typed-slots instance '((gpu-sample-devices list)))
      instance)))
