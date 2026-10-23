(in-package #:ntop.test)

(cl-weave:describe
  "Process collector and Linux failure paths"
  (cl-weave:it
    "builds samples and forwards clock failures through the collection port"
    (let* ((config (ntop.shared.application:make-application-config))
           (collector
            (make-instance '%f8a-process-test-collector
                           :clock
                           (make-instance '%f8a-process-test-clock
                                          :outcome
                                          '(:time 42.5d0))
                           :config
                           config
                           :outcome
                           '(:value nil nil)))
           (sample :unset)
           (reason :unset))
      (ntop.shared.application:collect-process-sample collector
                                                      (lambda (value)
                                                        (setf sample value))
                                                      (lambda (value)
                                                        (setf reason value)))
      (cl-weave:expect sample :to-be-truthy)
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       42.5d0)
      (cl-weave:expect reason :to-be :unset)
      (let ((clock-failure
             (make-instance '%f8a-process-test-collector
                            :clock
                            (make-instance '%f8a-process-test-clock
                                           :outcome
                                           '(:unavailable :permission-denied))
                            :config
                            config
                            :outcome
                            '(:value nil nil))))
        (setf sample :unset
              reason :unset)
        (ntop.shared.application:collect-process-sample clock-failure
                                                        (lambda (value)
                                                          (setf sample value))
                                                        (lambda (value)
                                                          (setf reason value)))
        (cl-weave:expect sample :to-be :unset)
        (cl-weave:expect reason :to-be :permission-denied))))
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
       (mapcar #'ntop.process.infrastructure::%process-unavailability-reason
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
       :to-be-null))))
