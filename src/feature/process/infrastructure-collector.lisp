(in-package #:ntop.process.infrastructure)
(defclass process-collector ()
  ((clock :initarg :clock :reader %process-collector-clock)
   (config :initarg :config :reader %process-collector-config)
   (sample-trace-path
    :initarg :sample-trace-path
    :initform nil
    :reader %process-collector-sample-trace-path)
   (sample-trace-pid
    :initarg :sample-trace-pid
    :initform nil
    :reader %process-collector-sample-trace-pid)))

  (defun make-process-collector (clock config)
  (let ((trace-path (sb-ext:posix-getenv "NTOP_D1_SAMPLE_TRACE_PATH"))
        (trace-pid-text (sb-ext:posix-getenv "NTOP_D1_SAMPLE_TRACE_PID")))
    (when (not (eql (null trace-path) (null trace-pid-text)))
      (error "D1 sample tracing requires both a trace path and target PID."))
    (make-instance (quote process-collector)
                   :clock clock
                   :config config
                   :sample-trace-path trace-path
                   :sample-trace-pid
                   (when trace-pid-text
                     (let ((pid (parse-integer trace-pid-text :junk-allowed nil)))
                       (unless (plusp pid)
                         (error "D1 sample trace PID must be positive."))
                       pid)))))

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
          (when (%process-collector-sample-trace-path collector)
            (with-open-file
                (stream (%process-collector-sample-trace-path collector)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
              (%write-process-sample-trace-row
               stream sampled-at
               (%process-collector-sample-trace-pid collector)
               processes)))
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
  (defun %write-process-sample-trace-row (stream sampled-at pid processes)
  (let ((record (find pid processes
                      :key (function ntop.process.domain:process-record-pid)
                      :test (function eql))))
    (format stream "~D~C~D~C~A~C~A~%"
            (round (* sampled-at 1000000000))
            (code-char 9)
            pid
            (code-char 9)
            (or (and record
                     (ntop.process.domain:process-record-read-bytes record))
                "N/A")
            (code-char 9)
            (or (and record
                     (ntop.process.domain:process-record-write-bytes record))
                "N/A"))
    (finish-output stream)))

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
