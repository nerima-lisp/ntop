(in-package #:ntop.memory.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (memory-sample (:include ntop.shared.domain:sample-record)
                     (:constructor make-memory-sample-raw)
                     (:copier nil))
    (ram-used-bytes nil :read-only t)
    (ram-total-bytes nil :read-only t)
    (swap-used-bytes nil :read-only t)
    (swap-total-bytes nil :read-only t)
    (cache-used-bytes nil :read-only t)
    (cache-total-bytes nil :read-only t)
    (arc-used-bytes nil :read-only t)
    (arc-total-bytes nil :read-only t))
  (defun make-memory-sample (&rest initargs)
    (let ((instance (apply #'make-memory-sample-raw initargs)))
      (validate-typed-slots instance
                            '((memory-sample-ram-used-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-ram-total-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-swap-used-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-swap-total-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-cache-used-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-cache-total-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (memory-sample-arc-used-bytes null)
                              (memory-sample-arc-total-bytes null)))
      instance)))
