(in-package #:ntop.app.application)

(progn
  (defparameter +collector-ports+
    #((:cpu ntop.shared.application:collect-cpu-sample :cpu-sample :cpu)
      (:memory ntop.shared.application:collect-memory-sample :memory-sample :memory)
      (:gpu ntop.shared.application:collect-gpu-sample :gpu-sample :gpu)
      (:network ntop.shared.application:collect-network-sample :network-sample :network)
      (:disk ntop.shared.application:collect-disk-sample :disk-sample :disk)
      (:temperature ntop.shared.application:collect-temperature-sample :temperature-sample :temperature)
      (:battery ntop.shared.application:collect-battery-sample :battery-sample :battery)
      (:process ntop.shared.application:collect-process-sample :process-sample :process)))
  (defstruct (application-unavailability-reasons
              (:constructor %make-application-unavailability-reasons
                  (&key cpu memory gpu network disk temperature battery process))
              (:copier nil))
    (cpu nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (memory nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (gpu nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (network nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (disk nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (temperature nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (battery nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t)
    (process nil :type (or null ntop.shared.application:collector-unavailability-reason) :read-only t))
  (defstruct (application-collection-result
              (:constructor %make-application-collection-result
                  (snapshot unavailability-reasons))
              (:copier nil))
    (snapshot nil :type ntop.app.domain:collection-snapshot :read-only t)
    (unavailability-reasons nil :type application-unavailability-reasons :read-only t))
  (defun application-unavailability-reason (reasons feature)
    (when reasons
      (ecase feature
        (:cpu (application-unavailability-reasons-cpu reasons))
        (:memory (application-unavailability-reasons-memory reasons))
        (:gpu (application-unavailability-reasons-gpu reasons))
        (:network (application-unavailability-reasons-network reasons))
        (:disk (application-unavailability-reasons-disk reasons))
        (:temperature (application-unavailability-reasons-temperature reasons))
        (:battery (application-unavailability-reasons-battery reasons))
        (:process (application-unavailability-reasons-process reasons))))))

(defun %snapshot-initargs (samples sampled-at)
  (append (list :sampled-at sampled-at)
          (loop for descriptor across +collector-ports+
                for index from 0
                append (list (third descriptor) (aref samples index)))))

(progn
  (defun collect-application-result (collectors clock on-result on-unavailable)
  (let ((samples (make-array (length +collector-ports+) :initial-element nil))
        (reasons (make-array (length +collector-ports+) :initial-element nil)))
    (labels ((collect-next (index sampled-at)
               (if (= index (length +collector-ports+))
                   (let* ((snapshot
                            (apply (function ntop.app.domain:make-collection-snapshot)
                                   (%snapshot-initargs samples sampled-at)))
                          (unavailability-reasons
                            (apply (function %make-application-unavailability-reasons)
                                   (loop for descriptor across +collector-ports+
                                         for feature from 0
                                         append (list (fourth descriptor)
                                                      (aref reasons feature)))))
                          (result
                            (%make-application-collection-result
                             snapshot unavailability-reasons)))
                     (funcall on-result result))
                   (destructuring-bind (key port slot reason-slot)
                       (aref +collector-ports+ index)
                     (declare (ignore slot reason-slot))
                     (let ((collector (getf collectors key)))
                       (if collector
                           (funcall (symbol-function port)
                                    collector
                                    (lambda (sample)
                                      (setf (aref samples index) sample)
                                      (collect-next (1+ index) sampled-at))
                                    (lambda (reason)
                                      (setf (aref reasons index) reason)
                                      (collect-next (1+ index) sampled-at)))
                           (collect-next (1+ index) sampled-at)))))))
      (ntop.shared.domain:read-monotonic-time
       clock
       (lambda (sampled-at)
         (collect-next 0 sampled-at))
       on-unavailable))))
  (defun collect-application-snapshot (collectors clock on-snapshot on-unavailable)
    (collect-application-result
     collectors clock
     (lambda (result)
       (funcall on-snapshot
                (application-collection-result-snapshot result)))
     on-unavailable)))

(defun run-fixed-rate-sampler (rate-ms now wait collect publish stop-p)
  (unless (and (realp rate-ms) (plusp rate-ms))
    (error "RATE-MS must be a positive real number, got ~S." rate-ms))
  (funcall collect)
  (let ((origin (funcall now))
        (period (/ rate-ms 1000d0))
        (index 0))
    (loop until (funcall stop-p)
          for deadline = (+ origin (* index period))
          do (loop for remaining = (- deadline (funcall now))
                   while (and (plusp remaining) (not (funcall stop-p)))
                   do (funcall wait remaining))
          (unless (funcall stop-p)
            (funcall publish (funcall collect))
            (incf index)))))
