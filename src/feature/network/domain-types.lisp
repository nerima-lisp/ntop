(in-package #:ntop.network.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (network-interface (:constructor make-network-interface-raw)
                         (:copier nil))
    (name nil :read-only t)
    (rx-bytes nil :read-only t)
    (tx-bytes nil :read-only t)
    (rx-packets nil :read-only t)
    (tx-packets nil :read-only t))
  (defun make-network-interface (&rest initargs)
    (let ((instance (apply #'make-network-interface-raw initargs)))
      (validate-typed-slots instance
                            '((network-interface-name (or null string))
                              (network-interface-rx-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (network-interface-tx-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (network-interface-rx-packets
                               (or null ntop.shared.domain:measurement-counter))
                              (network-interface-tx-packets
                               (or null ntop.shared.domain:measurement-counter))))
      instance)))

(progn
  (defstruct
      (network-sample (:include ntop.shared.domain:sample-record)
                      (:constructor make-network-sample-raw)
                      (:copier nil))
    (interfaces nil :read-only t))
  (defun make-network-sample (&rest initargs)
    (let ((instance (apply #'make-network-sample-raw initargs)))
      (validate-typed-slots instance '((network-sample-interfaces list)))
      instance)))
