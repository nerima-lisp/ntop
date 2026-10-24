(in-package #:ntop.temperature.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct
      (temperature-sensor (:constructor make-temperature-sensor-raw)
                          (:copier nil))
    (name nil :read-only t)
    (temperature-celsius nil :read-only t))
  (defun make-temperature-sensor (&rest initargs)
    (let ((instance (apply #'make-temperature-sensor-raw initargs)))
      (validate-typed-slots instance
                            '((temperature-sensor-name (or null string))
                              (temperature-sensor-temperature-celsius
                               ntop.shared.domain:measurement-value)))
      instance)))

(progn
  (defstruct
      (temperature-sample (:include ntop.shared.domain:sample-record)
                          (:constructor make-temperature-sample-raw)
                          (:copier nil))
    (sensors nil :read-only t))
  (defun make-temperature-sample (&rest initargs)
    (let ((instance (apply #'make-temperature-sample-raw initargs)))
      (validate-typed-slots instance '((temperature-sample-sensors list)))
      instance)))
