(in-package #:ntop.process.infrastructure)
(defclass process-collector ()
    ((clock :initarg :clock :reader %process-collector-clock)
     (config :initarg :config :reader %process-collector-config)))

  (defun make-process-collector (clock config)
    (make-instance 'process-collector :clock clock :config config))

  (defgeneric read-process-data (collector on-value on-unavailable))

  (defmethod read-process-data ((collector t) on-value on-unavailable)
    (declare (ignore collector on-value))
    (funcall on-unavailable :unsupported-platform))

  (defmethod ntop.shared.application:collect-process-sample
    ((collector process-collector) on-sample on-unavailable)
  (read-process-data
   collector
   (lambda (processes signals)
     (ntop.shared.domain:read-monotonic-time
      (%process-collector-clock collector)
      (lambda (sampled-at)
        (let ((pid-index (make-hash-table :test (function eql)
                                          :size (length processes))))
          (dolist (record processes)
            (let ((pid (ntop.process.domain:process-record-pid record)))
              (when (and pid (not (gethash pid pid-index)))
                (setf (gethash pid pid-index) record))))
          (funcall on-sample
                   (ntop.process.domain:make-process-sample
                    :sampled-at sampled-at
                    :processes processes
                    :pid-index pid-index
                    :signals signals))))
      (lambda (reason)
        (funcall on-unavailable
                 (ntop.shared.application:normalize-collector-unavailability-reason
                  reason)))))
   (lambda (reason)
     (funcall on-unavailable
              (ntop.shared.application:normalize-collector-unavailability-reason
               reason)))))

  (defun send-process-signal (target-pids signal-number)
    (unless (and (listp target-pids)
                 (every (lambda (pid) (and (integerp pid) (plusp pid)))
                        target-pids))
      (error "Every process signal target must be a positive PID integer."))
    (check-type signal-number integer)
    (dolist (pid target-pids)
      (sb-posix:kill pid signal-number))
    t)