(in-package #:ntop.network.domain)

(defparameter +network-byte-counter-accessors+
  '((:rx . network-interface-rx-bytes) (:tx . network-interface-tx-bytes)))

(defparameter +network-packet-counter-accessors+
  '((:rx . network-interface-rx-packets) (:tx . network-interface-tx-packets)))

(defun %network-counter-accessor (direction accessors)
  (or (cdr (assoc direction accessors))
      (error "Unsupported network direction: ~S" direction)))

(defun %network-sample-counter-total (sample accessor-name)
  (when sample
    (loop with total = 0
          for interface in (network-sample-interfaces sample)
          for counter = (funcall (symbol-function accessor-name) interface)
          do (unless counter
               (return-from %network-sample-counter-total
                 nil)) (incf total counter)
          finally (return total))))

(defun network-total-bytes (sample direction
                                   &key
                                   baseline-sample
                                   start-zeroed-p)
  (let* ((accessor-name
          (%network-counter-accessor direction +network-byte-counter-accessors+))
         (total (%network-sample-counter-total sample accessor-name)))
    (when total
      (if (not start-zeroed-p)
          total
          (if baseline-sample
              (let ((baseline
                     (%network-sample-counter-total baseline-sample
                                                    accessor-name)))
                (when baseline
                  (max 0 (- total baseline))))
              0)))))

(defun network-total-packets (sample direction)
  (%network-sample-counter-total sample
                                 (%network-counter-accessor direction
                                                            +network-packet-counter-accessors+)))

(defun %network-counter-rate-per-second (previous-sample current-sample
                                                         accessor-name)
  (when (and previous-sample current-sample)
    (ntop.shared.domain:counter-rate-per-second
     (%network-sample-counter-total previous-sample accessor-name)
     (%network-sample-counter-total current-sample accessor-name)
     (- (ntop.shared.domain:sample-record-sampled-at current-sample)
        (ntop.shared.domain:sample-record-sampled-at previous-sample)))))

(defun network-rate (previous-sample current-sample direction &key (unit :bit))
  (unless (member unit '(:bit :byte))
    (error "Network rate unit must be :BIT or :BYTE, not ~S." unit))
  (let* ((accessor-name
          (%network-counter-accessor direction +network-byte-counter-accessors+))
         (bytes-per-second
          (%network-counter-rate-per-second previous-sample
                                            current-sample
                                            accessor-name)))
    (when bytes-per-second
      (ntop.shared.domain:convert-quantity
       (ntop.shared.domain:make-quantity (* bytes-per-second 8) :bit)
       unit))))

(defun network-packet-rate-per-second (previous-sample current-sample direction)
  (%network-counter-rate-per-second previous-sample
                                    current-sample
                                    (%network-counter-accessor direction
                                                               +network-packet-counter-accessors+)))

(defun network-average-packet-size (previous-sample current-sample direction)
  (let* ((byte-accessor
          (%network-counter-accessor direction
                                     +network-byte-counter-accessors+))
         (bytes-per-second
          (%network-counter-rate-per-second previous-sample
                                            current-sample
                                            byte-accessor))
         (packets-per-second
          (network-packet-rate-per-second previous-sample
                                          current-sample
                                          direction)))
    (when (and bytes-per-second packets-per-second)
      (ntop.shared.domain:make-quantity
       (if (zerop packets-per-second)
           0
           (/ bytes-per-second packets-per-second))
       :byte))))
