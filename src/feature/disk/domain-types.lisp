(in-package #:ntop.disk.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

(progn
  (defstruct (disk-mount (:constructor make-disk-mount-raw) (:copier nil))
    (device-name nil :read-only t)
    (mount-path nil :read-only t)
    (filesystem-type nil :read-only t)
    (used-bytes nil :read-only t)
    (free-bytes nil :read-only t)
    (total-bytes nil :read-only t)
    (io-key nil :read-only t))
  (defun make-disk-mount (&rest initargs)
    (let ((instance (apply #'make-disk-mount-raw initargs)))
      (validate-typed-slots instance
                            '((disk-mount-device-name (or null string))
                              (disk-mount-mount-path (or null string))
                              (disk-mount-filesystem-type (or null string))
                              (disk-mount-used-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (disk-mount-free-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (disk-mount-total-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (disk-mount-io-key (or null string))))
      instance)))

(progn
  (defstruct (disk-device (:constructor make-disk-device-raw) (:copier nil))
    (device-name nil :read-only t)
    (read-bytes nil :read-only t)
    (write-bytes nil :read-only t))
  (defun make-disk-device (&rest initargs)
    (let ((instance (apply #'make-disk-device-raw initargs)))
      (validate-typed-slots instance
                            '((disk-device-device-name (or null string))
                              (disk-device-read-bytes
                               (or null ntop.shared.domain:measurement-counter))
                              (disk-device-write-bytes
                               (or null ntop.shared.domain:measurement-counter))))
      instance)))

(progn
  (defstruct
      (disk-sample (:include ntop.shared.domain:sample-record)
                   (:constructor make-disk-sample-raw)
                   (:copier nil))
    (mounts nil :read-only t)
    (devices nil :read-only t))
  (defun make-disk-sample (&rest initargs)
    (let ((instance (apply #'make-disk-sample-raw initargs)))
      (validate-typed-slots instance
                            '((disk-sample-mounts list)
                              (disk-sample-devices list)))
      instance)))
