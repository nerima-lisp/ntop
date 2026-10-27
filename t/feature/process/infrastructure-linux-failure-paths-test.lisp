(in-package #:ntop.test)

(cl-weave:describe
  "Process collector and Linux failure paths"
  (cl-weave:it
    "dispatches both collector callbacks through the collection port"
    (let* ((config (ntop.shared.application:make-application-config))
           (collector
            (make-instance '%f8a-process-test-collector
                           :clock
                           (make-instance '%f8a-process-test-clock
                                          :outcome
                                          (list :time 42.5d0))
                           :config
                           config
                           :outcome
                           (list :value nil nil)))
           (events nil)
           (sample :unset)
           (reason :unset))
      (ntop.shared.application:collect-process-sample
       collector
       (lambda (value)
         (push :sample events)
         (setf sample value))
       (lambda (value)
         (push :unavailable events)
         (setf reason value)))
      (cl-weave:expect sample :to-be-truthy)
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       42.5d0)
      (cl-weave:expect reason :to-be :unset)
      (cl-weave:expect (nreverse events) :to-equal (list :sample))
      (let ((clock-failure
             (make-instance '%f8a-process-test-collector
                            :clock
                            (make-instance '%f8a-process-test-clock
                                           :outcome
                                           (list :unavailable :permission-denied))
                            :config
                            config
                            :outcome
                            (list :value nil nil))))
        (setf events nil
              sample :unset
              reason :unset)
        (ntop.shared.application:collect-process-sample
         clock-failure
         (lambda (value)
           (push :sample events)
           (setf sample value))
         (lambda (value)
           (push :unavailable events)
           (setf reason value)))
        (cl-weave:expect sample :to-be :unset)
        (cl-weave:expect reason :to-be :permission-denied)
        (cl-weave:expect (nreverse events) :to-equal (list :unavailable)))))
  (cl-weave:it
    "normalizes unavailable reasons and uses the unsupported-platform method"
    (let* ((config (ntop.shared.application:make-application-config))
           (reason :unset)
           (collector
            (make-instance '%f8a-process-test-collector
                           :clock
                           (make-instance '%f8a-process-test-clock
                                          :outcome
                                          '(:time 1d0))
                           :config
                           config
                           :outcome
                           '(:unavailable :not-present))))
      (ntop.shared.application:collect-process-sample collector
                                                      (lambda (value)
                                                        (declare (ignore value)))
                                                      (lambda (value)
                                                        (setf reason value)))
      (cl-weave:expect reason :to-be :not-present)
      (setf reason :unset)
      (let ((unknown-reason
             (make-instance '%f8a-process-test-collector
                            :clock
                            (make-instance '%f8a-process-test-clock
                                           :outcome
                                           '(:time 1d0))
                            :config
                            config
                            :outcome
                            '(:unavailable :unknown))))
        (ntop.shared.application:collect-process-sample unknown-reason
                                                        (lambda (value)
                                                          (declare (ignore
                                                                    value)))
                                                        (lambda (value)
                                                          (setf reason value)))
        (cl-weave:expect reason :to-be :read-failed))
      (cl-weave:expect
       (mapcar #'ntop.shared.application:normalize-collector-unavailability-reason
               '(:not-present :disabled
                              :permission-denied
                              :unsupported-platform
                              :private-api-unavailable
                              :read-failed))
       :to-equal
       '(:not-present :disabled
                      :permission-denied
                      :unsupported-platform
                      :private-api-unavailable
                      :read-failed))
      (let ((unsupported :unset))
        (ntop.process.infrastructure::read-process-data :not-a-process-collector
                                                        (lambda (value)
                                                          (declare (ignore
                                                                    value)))
                                                        (lambda (value)
                                                          (setf unsupported value)))
        (cl-weave:expect unsupported :to-be :unsupported-platform))))
  (cl-weave:it
    "rejects non-list and non-integer signal targets before sending"
    (cl-weave:expect
     (lambda ()
       (ntop.process.infrastructure::send-process-signal 4242 0))
     :to-throw
     'error)
    (cl-weave:expect
     (lambda ()
       (ntop.process.infrastructure::send-process-signal '("invalid") 0))
     :to-throw
     'error))
  (cl-weave:it
   "stops at the first failed target and reports prior grouped signals"
   (let* ((missing-pid 2147483647)
          (message nil))
     (handler-case
         (ntop.process.infrastructure::send-process-signal
          (list (sb-posix:getpid) missing-pid (sb-posix:getpid)) 0)
       (error (condition)
              (setf message (princ-to-string condition))))
     (cl-weave:expect
      message
      :to-equal
      (format nil
              (concatenate (quote string)
                           "PID ~D failed after successfully signaling 1 of 3 targets. "
                           "Error code ~D - the target process did not exist.")
              missing-pid sb-posix:esrch))))
  (cl-weave:it
    "maps signal errno codes to actionable messages"
    (cl-weave:expect
     (ntop.process.infrastructure::%process-signal-error-message sb-posix:esrch)
     :to-equal
     "the target process did not exist.")
    (cl-weave:expect
     (ntop.process.infrastructure::%process-signal-error-message sb-posix:eperm)
     :to-equal
     "the calling process does not have the permissions to terminate the target process(es).")
    (cl-weave:expect
     (ntop.process.infrastructure::%process-signal-error-message sb-posix:einval)
     :to-equal
     "an invalid signal was specified.")
    (cl-weave:expect
     (ntop.process.infrastructure::%process-signal-error-message 12345)
     :to-equal
     "Unknown error occurred."))
  (cl-weave:it
    "covers numeric and optional-value fallback inputs"
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-status-uid '(("Uid" . 1000)))
     :to-be
     1000)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-status-uid nil)
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-status-uid
      '(("Uid" . ("invalid"))))
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-cpu-time-ns
      '(:utime -1 :stime 2)
      100)
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-cpu-time-ns
      '(:utime 2 :stime -1)
      100)
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-start-time '(:starttime -1)
                                                             1700000000
                                                             100)
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-kernel-thread-p
      '(:flags :unknown))
     :to-be
     :unknown)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-status-bytes
      '(("VmRSS" :invalid "kB"))
      "VmRSS")
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-passwd-entry "not-a-passwd-line")
     :to-be-null)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-record 4242
                                                         '(:pid 4343
                                                                :starttime
                                                                1
                                                                :utime
                                                                1
                                                                :stime
                                                                1)
                                                         '(("Uid" . 1000))
                                                         nil
                                                         nil
                                                         nil
                                                         nil
                                                         1700000000
                                                         100)
     :to-be-null))
  (cl-weave:it
    "returns an empty passwd map when the file port denies access"
    (let ((user-names :unset))
      (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.linux::read-linux-file)
         (lambda (path on-value on-unavailable)
           (declare (ignore path on-value))
           (funcall on-unavailable :permission-denied))))
       (ntop.process.infrastructure::%read-linux-passwd-user-names
        (lambda (value)
          (setf user-names value))))
      (cl-weave:expect user-names :to-be-null)))
  (cl-weave:it
    "reports invalid clock data and proc directory permission failures"
    (let ((root (%f8a-linux-fixture-root "complete"))
          (data :unset)
          (reason :unset))
      (multiple-value-setq (data reason) (%f8a-linux-capture-process-data root
                                                                          :clock-ticks
                                                                          0))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :read-failed)
      (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.linux:read-linux-proc-stat)
         (lambda (on-value on-unavailable)
           (declare (ignore on-unavailable))
           (funcall on-value '(("btime" 0))))))
       (multiple-value-setq (data reason) (%f8a-linux-capture-process-data root
                                                                           :clock-ticks
                                                                           100)))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :read-failed)
      (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.linux:read-linux-proc-stat)
         (lambda (on-value on-unavailable)
           (declare (ignore on-unavailable))
           (funcall on-value '(("btime" 1700000000)))))
        ((symbol-function 'ntop.platform.linux::read-linux-directory)
         (lambda (path on-value on-unavailable)
           (declare (ignore path on-value))
           (funcall on-unavailable :permission-denied))))
       (multiple-value-setq (data reason) (%f8a-linux-capture-process-data root
                                                                           :clock-ticks
                                                                           100)))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :permission-denied)))
  (cl-weave:it
    "rejects non-numeric Linux clock metadata"
    (let ((root (%f8a-linux-fixture-root "complete"))
          (data :unset)
          (reason :unset))
      (multiple-value-setq (data reason) (%f8a-linux-capture-process-data root
                                                                          :clock-ticks
                                                                          nil))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :read-failed)
      (cl-weave:with-mocked-functions
       (((symbol-function 'ntop.platform.linux:read-linux-proc-stat)
         (lambda (on-value on-unavailable)
           (declare (ignore on-unavailable))
           (funcall on-value '(("btime" "invalid"))))))
       (multiple-value-setq (data reason) (%f8a-linux-capture-process-data root
                                                                           :clock-ticks
                                                                           100))
       (cl-weave:expect data :to-be :unset)
       (cl-weave:expect reason :to-be :read-failed))
      (cl-weave:expect
       (ntop.process.infrastructure::%linux-process-start-time
        '(:starttime :invalid)
        1700000000
        100)
       :to-be-null)))
  (cl-weave:it
 "validates paired sample trace environment settings"
 (let* ((config (ntop.shared.application:make-application-config))
        (path-name "NTOP_D1_SAMPLE_TRACE_PATH")
        (pid-name "NTOP_D1_SAMPLE_TRACE_PID")
        (old-path (sb-ext:posix-getenv path-name))
        (old-pid (sb-ext:posix-getenv pid-name)))
   (unwind-protect
       (progn
         (sb-posix:setenv path-name "/tmp/process-samples.tsv" 1)
         (sb-posix:setenv pid-name "4242" 1)
         (let ((collector
                 (ntop.process.infrastructure::make-process-collector nil config)))
           (cl-weave:expect
            (ntop.process.infrastructure::%process-collector-sample-trace-path
             collector)
            :to-equal
            "/tmp/process-samples.tsv")
           (cl-weave:expect
            (ntop.process.infrastructure::%process-collector-sample-trace-pid
             collector)
            :to-be
            4242))
         (sb-posix:unsetenv pid-name)
         (cl-weave:expect
          (lambda ()
            (ntop.process.infrastructure::make-process-collector nil config))
          :to-throw
          'simple-error)
         (sb-posix:setenv pid-name "0" 1)
         (cl-weave:expect
          (lambda ()
            (ntop.process.infrastructure::make-process-collector nil config))
          :to-throw
          'simple-error))
     (if old-path
         (sb-posix:setenv path-name old-path 1)
         (sb-posix:unsetenv path-name))
     (if old-pid
         (sb-posix:setenv pid-name old-pid 1)
         (sb-posix:unsetenv pid-name)))))
  (cl-weave:it
 "indexes non-nil PIDs first and writes missing trace counters through the collector"
 (host-kit:call-with-temporary-directory
  (lambda (directory)
   (let* ((trace-path (merge-pathnames "process-samples.tsv" directory))
          (nil-pid-record
           (ntop.process.domain:make-process-record :pid nil))
          (first-record (ntop.process.domain:make-process-record :pid 4242))
          (duplicate-record (ntop.process.domain:make-process-record :pid 4242))
          (processes (list nil-pid-record first-record duplicate-record))
          (collector
           (make-instance
            'ntop.process.infrastructure::process-collector
            :clock
            (make-instance '%f8a-process-test-clock :outcome '(:time 42d0))
            :config (ntop.shared.application:make-application-config)
            :sample-trace-path trace-path
            :sample-trace-pid 4242))
          (sample :unset)
          (reason :unset))
     (cl-weave:with-mocked-functions
      (((symbol-function 'ntop.process.infrastructure::read-process-data)
        (lambda (ignored on-value on-unavailable)
          (declare (ignore ignored on-unavailable))
          (funcall on-value processes nil))))
      (ntop.shared.application:collect-process-sample
       collector (lambda (value) (setf sample value))
       (lambda (value) (setf reason value))))
     (let ((pid-index
             (ntop.process.domain::process-sample-pid-index sample)))
       (cl-weave:expect sample :to-be-truthy)
       (cl-weave:expect reason :to-be :unset)
       (multiple-value-bind (value present-p) (gethash nil pid-index)
         (declare (ignore value))
         (cl-weave:expect present-p :to-be-null))
       (cl-weave:expect (eq (gethash 4242 pid-index) first-record)
                        :to-be t))
     (cl-weave:expect
      (with-open-file (stream trace-path :direction :input)
        (read-line stream nil))
      :to-equal
      (format nil "~D~C~D~C~A~C~A"
              42000000000 (code-char 9) 4242 (code-char 9)
              "N/A" (code-char 9) "N/A")))))))
