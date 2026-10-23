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
       :to-be expected)))
  (cl-weave:it "constructs immutable computed process rows"
    (let* ((row
             (ntop.process.domain:make-process-row
              :count 2
              :pid 42
              :name "worker"
              :command "worker --child"
              :cpu-percent 0.0
              :memory-percent nil
              :memory-bytes 0
              :virtual-memory-bytes 8192
              :read-bytes-per-second 1.5
              :write-bytes-per-second nil
              :total-read-bytes 100
              :total-write-bytes 200
              :user-name "user"
              :state "Running"
              :elapsed-seconds 3.0
              :priority 20
              :nice 0
              :gpu-usage-percent 25.0
              :gpu-memory-bytes 4096
              :gpu-memory-percent 10.0))
           (accessors
             '(ntop.process.domain:process-row-count
               ntop.process.domain:process-row-pid
               ntop.process.domain:process-row-name
               ntop.process.domain:process-row-command
               ntop.process.domain:process-row-cpu-percent
               ntop.process.domain:process-row-memory-percent
               ntop.process.domain:process-row-memory-bytes
               ntop.process.domain:process-row-virtual-memory-bytes
               ntop.process.domain:process-row-read-bytes-per-second
               ntop.process.domain:process-row-write-bytes-per-second
               ntop.process.domain:process-row-total-read-bytes
               ntop.process.domain:process-row-total-write-bytes
               ntop.process.domain:process-row-user-name
               ntop.process.domain:process-row-state
               ntop.process.domain:process-row-elapsed-seconds
               ntop.process.domain:process-row-priority
               ntop.process.domain:process-row-nice
               ntop.process.domain:process-row-gpu-usage-percent
               ntop.process.domain:process-row-gpu-memory-bytes
               ntop.process.domain:process-row-gpu-memory-percent)))
      (cl-weave:expect (ntop.process.domain:process-row-p row) :to-be t)
      (cl-weave:expect
       (mapcar (lambda (accessor) (funcall accessor row)) accessors)
       :to-equal
       '(2 42 "worker" "worker --child" 0.0 nil 0 8192 1.5 nil
         100 200 "user" "Running" 3.0 20 0 25.0 4096 10.0))
      (cl-weave:expect
       (every (lambda (accessor)
                (not (fboundp (list 'setf accessor))))
              accessors)
       :to-be t)))
  (cl-weave:it "constructs process rows with a read-only parent PID"
    (let ((row
            (ntop.process.domain:make-process-row :pid 42 :ppid 7)))
      (cl-weave:expect (ntop.process.domain:process-row-ppid row) :to-be 7)
      (cl-weave:expect
       (fboundp (quote (setf ntop.process.domain:process-row-ppid)))
       :to-be nil))))
