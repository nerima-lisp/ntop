(in-package #:ntop.test)

(cl-weave:describe "process domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type process-record
                (pid nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type process-sample
                (processes nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a process sample with threads and a timestamp"
    (let* ((thread (ntop.process.domain:make-process-thread
                    :thread-id 12
                    :cpu-time-ns 240))
           (process (ntop.process.domain:make-process-record
                     :pid 100
                     :resident-memory-bytes 4096
                     :threads (list thread)))
           (sample (ntop.process.domain:make-process-sample
                    :sampled-at 36.0
                    :processes (list process))))
      (cl-weave:expect
       (ntop.shared.domain:sample-record-sampled-at sample)
       :to-be 36.0)
      (cl-weave:expect
       (ntop.process.domain:process-record-resident-memory-bytes
        (first (ntop.process.domain:process-sample-processes sample)))
       :to-be 4096)
      (cl-weave:expect (ntop.process.domain:process-thread-cpu-time-ns thread)
                       :to-be 240)))
  (cl-weave:it "constructs signals with operating system numbers"
    (let* ((signal (ntop.process.domain:make-process-signal
                    :operating-system "Darwin"
                    :name "SIGTERM"
                    :number 15))
           (sample (ntop.process.domain:make-process-sample
                    :sampled-at 0.0
                    :signals (list signal))))
      (cl-weave:expect
       (ntop.process.domain:process-signal-number
        (first (ntop.process.domain:process-sample-signals sample)))
       :to-be 15)
      (cl-weave:expect
       (ntop.process.domain:process-signal-operating-system signal)
       :to-equal "Darwin")))
  (cl-weave:it-each
    ((nil nil) (0 0))
    "preserves ~S as an optional process CPU counter"
    (value expected)
    (let ((process (ntop.process.domain:make-process-record
                    :cpu-time-ns value)))
      (cl-weave:expect
       (ntop.process.domain:process-record-cpu-time-ns process)
       :to-be expected))))
