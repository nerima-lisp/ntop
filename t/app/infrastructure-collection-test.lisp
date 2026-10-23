(in-package #:ntop.test)

(progn
  (defclass application-collection-test-clock ()
    ((value :initarg :value :reader application-collection-test-clock-value)
     (first-read-gate :initarg
                      :first-read-gate
                      :initform
                      nil
                      :reader
                      application-collection-test-clock-first-read-gate)
     (first-read-p :initform t :accessor application-collection-test-clock-first-read-p)
     (observed-thread :initform
                      nil
                      :accessor
                      application-collection-test-clock-observed-thread)
     (unavailable-reason :initarg
                         :unavailable-reason
                         :initform
                         nil
                         :reader
                         application-collection-test-clock-unavailable-reason)))
  (defmethod
    ntop.shared.domain:read-monotonic-time
    ((clock application-collection-test-clock) on-time on-unavailable)
    (let ((reason (application-collection-test-clock-unavailable-reason clock)))
      (if reason
          (funcall on-unavailable reason)
          (progn
            (setf
              (application-collection-test-clock-observed-thread clock)
              sb-thread:*current-thread*)
            (when
                (and (application-collection-test-clock-first-read-gate clock)
                     (application-collection-test-clock-first-read-p clock))
              (setf
                (application-collection-test-clock-first-read-p clock)
                nil)
              (cl-concurrent-kit:wait-on-semaphore
               (application-collection-test-clock-first-read-gate clock)))
            (funcall on-time (application-collection-test-clock-value clock))))))
  (defclass application-collection-test-sample-collector ()
    ((sample :initarg :sample :reader application-collection-test-sample-collector-sample)))
  (defmethod
    ntop.shared.application:collect-cpu-sample
    ((collector application-collection-test-sample-collector) on-sample on-unavailable)
    (declare (ignore on-unavailable))
    (funcall on-sample (application-collection-test-sample-collector-sample collector))
    (values))
  (defclass application-collection-test-unavailable-collector ()
    ())
  (defmethod
    ntop.shared.application:collect-cpu-sample
    ((collector application-collection-test-unavailable-collector) on-sample
     on-unavailable)
    (declare (ignore collector on-sample))
    (funcall on-unavailable :read-failed)
    (values))
  (defmethod
    ntop.shared.application:collect-gpu-sample
    ((collector application-collection-test-unavailable-collector) on-sample
     on-unavailable)
    (declare (ignore collector on-sample))
    (funcall on-unavailable :not-present)
    (values))
  (defclass application-collection-test-failing-collector ()
    ((calls :initform 0 :accessor application-collection-test-failing-collector-calls)
     (failure-started :initarg
                      :failure-started
                      :reader
                      application-collection-test-failing-collector-failure-started)))
  (defmethod
    ntop.shared.application:collect-cpu-sample
    ((collector application-collection-test-failing-collector) on-sample on-unavailable)
    (declare (ignore on-unavailable))
    (if (= (incf (application-collection-test-failing-collector-calls collector)) 1)
        (funcall on-sample (ntop.cpu.domain:make-cpu-sample :sampled-at 17d0))
        (progn
          (cl-concurrent-kit:signal-semaphore
           (application-collection-test-failing-collector-failure-started collector))
          (error "synthetic CPU collector failure")))
    (values))
  (cl-weave:describe
    "application collection infrastructure"
    (cl-weave:it
      "assembles a snapshot from the clock and CPU success callback"
      (let* ((sample (ntop.cpu.domain:make-cpu-sample :sampled-at 7d0))
             (clock (make-instance 'application-collection-test-clock :value 42d0))
             (snapshot nil)
             (clock-reason nil))
        (ntop.app.application:collect-application-snapshot
         (list :cpu
               (make-instance 'application-collection-test-sample-collector
                              :sample
                              sample))
         clock
         (lambda (value)
           (setf
             snapshot
             value))
         (lambda (reason)
           (setf
             clock-reason
             reason)))
        (cl-weave:expect
          (ntop.app.domain:collection-snapshot-p snapshot)
          :to-be
          t)
        (cl-weave:expect
          (ntop.app.domain:collection-snapshot-sampled-at snapshot)
          :to-be
          42d0)
        (cl-weave:expect
          (ntop.app.domain:collection-snapshot-cpu-sample snapshot)
          :to-be
          sample)
        (cl-weave:expect
          clock-reason
          :to-be
          nil)))
    (cl-weave:it
      "retains unavailable reasons alongside nil samples"
      (let* ((clock (make-instance (quote application-collection-test-clock) :value 99d0))
             (collector
              (make-instance (quote application-collection-test-unavailable-collector)))
             (collection-result nil)
             (clock-reason nil))
        (ntop.app.application:collect-application-result
         (list :cpu collector :gpu collector)
         clock
         (lambda (value)
           (setf
             collection-result
             value))
         (lambda (reason)
           (setf
             clock-reason
             reason)))
        (let* ((snapshot
                (ntop.app.application:application-collection-result-snapshot
                 collection-result))
               (reasons
                (ntop.app.application:application-collection-result-unavailability-reasons
                 collection-result)))
          (cl-weave:expect
            (ntop.app.application:application-collection-result-p collection-result)
            :to-be
            t)
          (cl-weave:expect
            (ntop.app.domain:collection-snapshot-p snapshot)
            :to-be
            t)
          (cl-weave:expect
            (ntop.app.domain:collection-snapshot-sampled-at snapshot)
            :to-be
            99d0)
          (cl-weave:expect
            (ntop.app.domain:collection-snapshot-cpu-sample snapshot)
            :to-be
            nil)
          (cl-weave:expect
            (ntop.app.domain:collection-snapshot-gpu-sample snapshot)
            :to-be
            nil)
          (cl-weave:expect
            (ntop.app.application:application-unavailability-reason reasons :cpu)
            :to-equal
            :read-failed)
          (cl-weave:expect
            (ntop.app.application:application-unavailability-reason reasons :gpu)
            :to-equal
            :not-present)
          (cl-weave:expect
            (ntop.app.application:application-unavailability-reason reasons :memory)
            :to-be
            nil)
          (cl-weave:expect
            clock-reason
            :to-be
            nil))))
    (cl-weave:it
      "forwards a clock failure to the unavailable continuation"
      (let ((snapshot :not-called)
            (reason nil))
        (ntop.app.application:collect-application-snapshot
         (list :cpu
               (make-instance 'application-collection-test-sample-collector
                              :sample
                              (ntop.cpu.domain:make-cpu-sample :sampled-at 1d0)))
         (make-instance 'application-collection-test-clock
                        :value
                        nil
                        :unavailable-reason
                        :clock-unavailable)
         (lambda (value)
           (setf
             snapshot
             value))
         (lambda (unavailable-reason)
           (setf
             reason
             unavailable-reason)))
        (cl-weave:expect
          snapshot
          :to-be
          :not-called)
        (cl-weave:expect
          reason
          :to-be
          :clock-unavailable)))
    (cl-weave:it
      "publishes a snapshot from a worker thread and stops"
      (let* ((gate (cl-concurrent-kit:make-semaphore))
             (clock
              (make-instance (quote application-collection-test-clock)
                             :value
                             42d0
                             :first-read-gate
                             gate))
             (worker (ntop.app.infrastructure:start-collection-worker nil clock 60000))
             (channel (ntop.app.infrastructure:collection-worker-channel worker))
             (stopped-p nil))
        (unwind-protect
            (progn
              (cl-concurrent-kit:signal-semaphore gate)
              (let* ((collection-result
                      (cl-concurrent-kit:recv channel
                                              :timeout
                                              (cl-date-kit:duration-of-seconds 2)))
                     (snapshot
                      (ntop.app.application:application-collection-result-snapshot
                       collection-result)))
                (cl-weave:expect
                  (ntop.app.application:application-collection-result-p collection-result)
                  :to-be
                  t)
                (cl-weave:expect
                  (ntop.app.domain:collection-snapshot-p snapshot)
                  :to-be
                  t)
                (cl-weave:expect
                  (ntop.app.domain:collection-snapshot-sampled-at snapshot)
                  :to-be
                  42d0)
                (cl-weave:expect
                  (ntop.app.application:application-unavailability-reason
                   (ntop.app.application:application-collection-result-unavailability-reasons
                    collection-result)
                   :gpu)
                  :to-be
                  nil))
              (cl-weave:expect
                (eq (application-collection-test-clock-observed-thread clock)
                    sb-thread:*current-thread*)
                :to-be
                nil)
              (ntop.app.infrastructure:stop-collection-worker worker)
              (setf
                stopped-p
                t)
              (multiple-value-bind (value received-p closed-p)
                  (cl-concurrent-kit:try-recv channel)
                (cl-weave:expect
                  (list value received-p closed-p)
                  :to-equal
                  (list nil nil t))))
          (unless stopped-p
            (cl-concurrent-kit:signal-semaphore gate)
            (ntop.app.infrastructure:stop-collection-worker worker)))))
    (cl-weave:it
      "replaces stale channel data with collector failure"
      (let* ((gate (cl-concurrent-kit:make-semaphore))
             (failure-started (cl-concurrent-kit:make-semaphore))
             (collector
              (make-instance 'application-collection-test-failing-collector
                             :failure-started
                             failure-started))
             (clock
              (make-instance 'application-collection-test-clock
                             :value
                             17d0
                             :first-read-gate
                             gate))
             (worker
              (ntop.app.infrastructure:start-collection-worker (list :cpu collector)
                                                               clock
                                                               60000))
             (channel (ntop.app.infrastructure:collection-worker-channel worker))
             (stopped-p nil))
        (unwind-protect
            (progn
              (cl-weave:expect
                (cl-concurrent-kit:try-send channel :stale)
                :to-be
                t)
              (cl-weave:expect
                (cl-concurrent-kit:try-send channel :overflow)
                :to-be
                nil)
              (cl-concurrent-kit:signal-semaphore gate)
              (cl-weave:expect
                (cl-concurrent-kit:wait-on-semaphore failure-started :timeout 2d0)
                :to-be
                0)
              (ntop.app.infrastructure:stop-collection-worker worker)
              (setf
                stopped-p
                t)
              (multiple-value-bind (failure received-p closed-p)
                  (cl-concurrent-kit:try-recv channel)
                (cl-weave:expect
                  (consp failure)
                  :to-be
                  t)
                (cl-weave:expect
                  (car failure)
                  :to-be
                  :failure)
                (cl-weave:expect
                  (typep (cdr failure) 'error)
                  :to-be
                  t)
                (cl-weave:expect
                  received-p
                  :to-be
                  t)
                (cl-weave:expect
                  closed-p
                  :to-be
                  nil))
              (multiple-value-bind (value received-p closed-p)
                  (cl-concurrent-kit:try-recv channel)
                (cl-weave:expect
                  (list value received-p closed-p)
                  :to-equal
                  '(nil nil t))))
          (unless stopped-p
            (cl-concurrent-kit:signal-semaphore gate)
            (ntop.app.infrastructure:stop-collection-worker worker)))))
    (cl-weave:it
      "creates a system clock that reports monotonic readings"
      (let ((clock (ntop.app.infrastructure:make-system-clock))
            (first-time nil)
            (second-time nil)
            (unavailable-reason nil))
        (ntop.shared.domain:read-monotonic-time clock
                                                (lambda (value)
                                                  (setf
                                                    first-time
                                                    value))
                                                (lambda (reason)
                                                  (setf
                                                    unavailable-reason
                                                    reason)))
        (ntop.shared.domain:read-monotonic-time clock
                                                (lambda (value)
                                                  (setf
                                                    second-time
                                                    value))
                                                (lambda (reason)
                                                  (setf
                                                    unavailable-reason
                                                    reason)))
        (cl-weave:expect
          (typep clock 'ntop.app.infrastructure:system-clock)
          :to-be
          t)
        (cl-weave:expect
          (realp first-time)
          :to-be
          t)
        (cl-weave:expect
          (realp second-time)
          :to-be
          t)
        (cl-weave:expect
          (<= first-time second-time)
          :to-be
          t)
        (cl-weave:expect
          unavailable-reason
          :to-be
          nil)))
    (cl-weave:it
      "reports monotonic clock failures without catching callbacks"
      (let ((time nil)
            (reason nil)
            (unavailable-called-p nil))
        (ntop.app.infrastructure::%report-monotonic-time
         (lambda ()
           (error "synthetic clock failure"))
         (lambda (value)
           (setf
             time
             value))
         (lambda (condition)
           (setf
             reason
             condition)))
        (cl-weave:expect
          time
          :to-be
          nil)
        (cl-weave:expect
          (typep reason (quote error))
          :to-be
          t)
        (cl-weave:signals simple-error
                          (ntop.app.infrastructure::%report-monotonic-time
                           (lambda ()
                             (values 17 0))
                           (lambda (value)
                             (declare (ignore value))
                             (error "synthetic callback failure"))
                           (lambda (condition)
                             (declare (ignore condition))
                             (setf
                               unavailable-called-p
                               t))))
        (cl-weave:expect
          unavailable-called-p
          :to-be
          nil)))
    (cl-weave:it
      "raises on unavailable clock readings"
      (let ((clock
             (make-instance (quote application-collection-test-clock)
                            :value
                            nil
                            :unavailable-reason
                            :clock-unavailable)))
        (cl-weave:signals simple-error
                          (ntop.app.infrastructure::%read-monotonic-time clock))
        (cl-weave:signals simple-error
                          (ntop.app.infrastructure::%collect-snapshot nil clock))))
    (cl-weave:it
      "ignores publication to a closed snapshot channel"
      (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1)))
        (cl-concurrent-kit:close-channel channel)
        (cl-weave:expect
          (ntop.app.infrastructure::%publish-latest channel :snapshot)
          :to-be
          nil)))))
