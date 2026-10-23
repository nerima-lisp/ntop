(in-package #:ntop.network.infrastructure)

(defclass network-collector ()
  ((clock :initarg :clock :reader %network-collector-clock)))

(defun make-network-collector (clock)
  (make-instance 'network-collector :clock clock))

(defgeneric read-network-interface-data (collector on-value on-unavailable))

(defun %network-unavailable-callback (on-unavailable)
  (lambda (reason)
    (funcall on-unavailable
             (if (typep reason 'ntop.shared.application:collector-unavailability-reason)
                 reason
                 :read-failed))))

(defun %network-interfaces-from-rows (rows)
  (mapcar
   (lambda (row)
     (apply #'ntop.network.domain:make-network-interface row))
   rows))

(defun %collect-network-sample-from-rows (collector rows on-sample on-unavailable)
  (let ((interfaces (%network-interfaces-from-rows rows)))
    (ntop.shared.domain:read-monotonic-time (%network-collector-clock collector)
                                            (lambda (sampled-at)
                                              (funcall on-sample
                                                       (ntop.network.domain:make-network-sample
                                                        :sampled-at
                                                        sampled-at
                                                        :interfaces
                                                        interfaces)))
                                            on-unavailable)))

(defmethod ntop.shared.application:collect-network-sample ((collector network-collector) on-sample
                                                           on-unavailable)
  (let ((network-on-unavailable (%network-unavailable-callback on-unavailable)))
    (read-network-interface-data collector
                                 (lambda (rows)
                                   (%collect-network-sample-from-rows collector
                                                                      rows
                                                                      on-sample
                                                                      network-on-unavailable))
                                 network-on-unavailable)))
