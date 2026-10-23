(in-package #:ntop.network.infrastructure)

(defun read-linux-network-interface-data (on-value on-unavailable)
  (ntop.platform.linux:read-linux-network-statistics
   (lambda (rows)
     (if rows
         (funcall on-value rows)
         (funcall on-unavailable :not-present)))
   (lambda (reason)
     (funcall on-unavailable
              (cond
                ((eq reason :unavailable) :not-present)
                ((typep reason 'ntop.shared.application:collector-unavailability-reason) reason)
                (t :read-failed))))))

(progn
  #+linux
  (defmethod read-network-interface-data
      ((collector network-collector) on-value on-unavailable)
    (declare (ignore collector))
    (read-linux-network-interface-data on-value on-unavailable))
  #-(or linux darwin)
  (defmethod read-network-interface-data
      ((collector network-collector) on-value on-unavailable)
    (declare (ignore collector on-value))
    (funcall on-unavailable :unsupported-platform)))
