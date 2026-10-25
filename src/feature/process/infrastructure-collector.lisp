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
    (let ((sent-count 0)
          (target-count (length target-pids)))
      (dolist (pid target-pids)
        (handler-case
            (progn
              (sb-posix:kill pid signal-number)
              (incf sent-count))
          (sb-posix:syscall-error (condition)
            (let* ((errno (sb-posix:syscall-errno condition))
                   (message
                    (cond
                      ((= errno sb-posix:esrch)
                       "the target process did not exist.")
                      ((= errno sb-posix:eperm)
                       (concatenate (quote string)
                                    "the calling process does not have the permissions to "
                                    "terminate the target process(es)."))
                      ((= errno sb-posix:einval)
                       "an invalid signal was specified.")
                      (t "Unknown error occurred."))))
              (if (> target-count 1)
                  (error
                   "PID ~D failed after successfully signaling ~D of ~D targets. Error code ~D - ~A"
                   pid sent-count target-count errno message)
                (error "Error code ~D - ~A" errno message))))))
      t))
