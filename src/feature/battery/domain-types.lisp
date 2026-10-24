(in-package #:ntop.battery.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (battery-device (:constructor make-battery-device-raw) (:copier nil))
    (name nil :read-only t)
    (charge-percent nil :read-only t)
    (power-watts nil :read-only t)
    (state :unknown :read-only t)
    (seconds-to-full nil :read-only t)
    (seconds-to-empty nil :read-only t)
    (health-percent nil :read-only t))
  (defun make-battery-device (&rest initargs)
    (let ((instance (apply #'make-battery-device-raw initargs)))
      (validate-typed-slots instance
                            '((battery-device-name (or null string))
                              (battery-device-charge-percent
                               ntop.shared.domain:measurement-value)
                              (battery-device-power-watts
                               ntop.shared.domain:measurement-value)
                              (battery-device-state
                               (member :charging
                                       :discharging
                                       :full
                                       :empty
                                       :unknown))
                              (battery-device-seconds-to-full
                               ntop.shared.domain:measurement-value)
                              (battery-device-seconds-to-empty
                               ntop.shared.domain:measurement-value)
                              (battery-device-health-percent
                               ntop.shared.domain:measurement-value)))
      instance)))

(progn
  (defstruct
      (battery-sample (:include ntop.shared.domain:sample-record)
                      (:constructor make-battery-sample-raw)
                      (:copier nil))
    (devices nil :read-only t))
  (defun make-battery-sample (&rest initargs)
    (let ((instance (apply #'make-battery-sample-raw initargs)))
      (validate-typed-slots instance '((battery-sample-devices list)))
      instance)))
