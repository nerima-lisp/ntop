(in-package #:ntop.test)

(defun %network-sysfs-fixture-root (fixture)
  "Returns the sysfs network fixture directory named by FIXTURE."
  (asdf:system-relative-pathname
   "ntop"
   (format nil "t/platform/linux/fixtures/sysfs-network/~A/" fixture)))

(defun %read-linux-network-feature-fixture (fixture)
  "Returns tagged reader output while FIXTURE is installed as the Linux root."
  (let ((result nil))
    (ntop.platform.linux:with-linux-root (%network-sysfs-fixture-root fixture)
      (ntop.network.infrastructure::read-linux-network-interface-data
       (lambda (value)
         (setf result (list :value value)))
       (lambda (reason)
         (setf result (list :unavailable reason)))))
    result))

(defclass network-fixture-clock ()
  ((unavailable-reason :initarg :unavailable-reason
                       :initform nil
                       :reader network-fixture-clock-unavailable-reason))
  (:documentation "Supplies time 21.0 or the configured unavailable reason."))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock network-fixture-clock) on-time on-unavailable)
  (let ((reason (network-fixture-clock-unavailable-reason clock)))
    (if reason
        (funcall on-unavailable reason)
        (funcall on-time 21.0))))

(defclass linux-fixture-network-collector
    (ntop.network.infrastructure:network-collector)
  ()
  (:documentation "Reads interfaces through the Linux fixture reader."))

(defmethod ntop.network.infrastructure:read-network-interface-data
    ((collector linux-fixture-network-collector) on-value on-unavailable)
  (declare (ignore collector))
  (ntop.network.infrastructure::read-linux-network-interface-data
   on-value on-unavailable))

(defun %collect-linux-network-fixture (fixture &optional unavailable-reason)
  "Returns tagged collector output for FIXTURE with a fixed clock and optional failure."
  (let ((result nil)
        (collector
          (make-instance (quote linux-fixture-network-collector)
                         :clock
                         (make-instance (quote network-fixture-clock)
                                        :unavailable-reason unavailable-reason))))
    (ntop.platform.linux:with-linux-root (%network-sysfs-fixture-root fixture)
      (ntop.shared.application:collect-network-sample
       collector
       (lambda (sample)
         (setf result (list :value sample)))
       (lambda (reason)
         (setf result (list :unavailable reason)))))
    result))
