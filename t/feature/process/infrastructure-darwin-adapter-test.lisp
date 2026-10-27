(in-package #:ntop.test)
(cl-weave:it
 "passes Darwin process rows through the command and signal adapters"
 (let* ((uid (sb-posix:getuid))
        (config
          (ntop.shared.application:make-application-config
           :process-show-command-p t
           :process-get-threads-p t))
        (collector
          (ntop.process.infrastructure::make-process-collector nil config))
        (rows
          (list
           (list :pid 41 :ppid 2 :uid uid :ruid uid :name "metadata"
                 :cpu-time-ns 25
                 :threads
                 (list
                  (list :thread-id 7 :name "worker" :cpu-time-ns 15
                        :state "Running" :priority 8 :nice 0)))))
        (ps-result
          (host-kit::make-process-result
           :exit-code 0
           :stdout (format nil "41 3.5 8 /usr/bin/worker --flag~%")))
        (result :unset)
        (unavailable :unset))
   (cl-weave:with-mocked-functions
    (((symbol-function 'ntop.platform.darwin::read-darwin-process-rows)
      (lambda (on-value on-unavailable &key include-threads-p)
        (declare (ignore on-unavailable))
        (cl-weave:expect include-threads-p :to-be t)
        (funcall on-value rows)))
     ((symbol-function 'host-kit:run-program)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        ps-result)))
    (ntop.process.infrastructure::read-darwin-process-data
     collector
     (lambda (processes signals)
       (setf result (list processes signals)))
     (lambda (reason) (setf unavailable reason))))
   (let* ((records (first result))
          (signals (second result))
          (record (first records))
          (thread
            (first (ntop.process.domain:process-record-threads record))))
     (cl-weave:expect unavailable :to-be :unset)
     (cl-weave:expect (length records) :to-be 1)
     (cl-weave:expect
      (ntop.process.domain:process-record-command record)
      :to-equal "/usr/bin/worker --flag")
     (cl-weave:expect
      (ntop.process.domain:process-record-ps-fallback-cpu-percent record)
      :to-be 3.5d0)
     (cl-weave:expect (ntop.process.domain:process-record-priority record)
                      :to-be 8)
     (cl-weave:expect (ntop.process.domain:process-thread-thread-id thread)
                      :to-be 7)
     (cl-weave:expect (length signals) :to-be 31)
     (cl-weave:expect
      (ntop.process.domain:process-signal-name (first signals))
      :to-equal "SIGHUP"))))
