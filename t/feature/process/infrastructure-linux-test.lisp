(in-package #:ntop.test)
  (defun %f8a-linux-fixture-root (name)
    (asdf:system-relative-pathname
     "ntop"
     (format nil "t/feature/process/fixtures/linux/~A/" name)))
  (defun %f8a-linux-capture-process-data
    (root &key threads
          (clock-ticks
            (ntop.process.infrastructure::%linux-process-clock-ticks-per-second)))
  (let* ((config
           (ntop.shared.application:make-application-config
            :process-get-threads-p threads))
         (collector
           (ntop.process.infrastructure::make-process-collector nil config))
         (data :unset)
         (reason :unset))
    (ntop.platform.linux:with-linux-root root
      (ntop.process.infrastructure::%read-linux-process-data
       collector
       (lambda (processes signals)
         (setf data (list processes signals)))
       (lambda (unavailable-reason)
         (setf reason unavailable-reason))
       clock-ticks))
    (values data reason)))
  (defun %f8a-linux-process-record (processes pid)
    (find pid processes
          :key #'ntop.process.domain:process-record-pid
          :test #'eql))
  (cl-weave:describe
   "Linux process collector"
   (cl-weave:it
    "collects complete values and retains records with missing optional files"
    (multiple-value-bind (data reason)
        (%f8a-linux-capture-process-data (%f8a-linux-fixture-root "complete"))
      (let* ((processes (first data))
             (complete (%f8a-linux-process-record processes 4242))
             (partial (%f8a-linux-process-record processes 4343))
             (signals (second data)))
        (cl-weave:expect reason :to-be :unset)
        (cl-weave:expect (mapcar #'ntop.process.domain:process-record-pid processes)
                         :to-equal
                         '(4242 4343))
        (cl-weave:expect (ntop.process.domain:process-record-name complete)
                         :to-equal
                         "worker (io) thread")
        (cl-weave:expect (ntop.process.domain:process-record-ppid complete)
                         :to-be
                         4000)
        (cl-weave:expect
         (ntop.process.domain:process-record-cpu-time-ns complete)
         :to-be
         (floor (* 154 1000000000)
                (ntop.process.infrastructure::%linux-process-clock-ticks-per-second)))
        (cl-weave:expect
         (ntop.process.domain:process-record-resident-memory-bytes complete)
         :to-be
         65536)
        (cl-weave:expect
         (ntop.process.domain:process-record-virtual-memory-bytes complete)
         :to-be
         5242880)
        (cl-weave:expect (ntop.process.domain:process-record-read-bytes complete)
                         :to-be
                         4096)
        (cl-weave:expect (ntop.process.domain:process-record-write-bytes complete)
                         :to-be
                         8192)
        (cl-weave:expect (ntop.process.domain:process-record-uid complete)
                         :to-be
                         1000)
        (cl-weave:expect (ntop.process.domain:process-record-user-name complete)
                         :to-equal
                         "fixture-user")
        (cl-weave:expect (ntop.process.domain:process-record-command complete)
                         :to-equal
                         "/usr/bin/worker --label two words")
        (cl-weave:expect (ntop.process.domain:process-record-state complete)
                         :to-equal
                         "R")
        (cl-weave:expect
         (< (abs (- (ntop.process.domain:process-record-start-time-unix-seconds
                     complete)
                    1700009876.54d0))
            0.0001d0)
         :to-be
         t)
        (cl-weave:expect
         (ntop.process.domain:process-record-kernel-thread-p complete)
         :to-be
         nil)
        (cl-weave:expect (ntop.process.domain:process-record-threads complete)
                         :to-be-null)
        (cl-weave:expect
         (ntop.process.domain:process-record-cpu-time-ns partial)
         :to-be
         (floor (* 9 1000000000)
                (ntop.process.infrastructure::%linux-process-clock-ticks-per-second)))
        (cl-weave:expect (ntop.process.domain:process-record-command partial)
                         :to-be-null)
        (cl-weave:expect (ntop.process.domain:process-record-read-bytes partial)
                         :to-be-null)
        (cl-weave:expect
         (ntop.process.domain:process-record-resident-memory-bytes partial)
         :to-be-null)
        (cl-weave:expect
         (ntop.process.domain:process-record-kernel-thread-p partial)
         :to-be
         t)
        (cl-weave:expect (%f8a-linux-process-record processes 4444)
                         :to-be-null)
        (cl-weave:expect (length signals) :to-be 31)
        (cl-weave:expect
         (mapcar #'ntop.process.domain:process-signal-number signals)
         :to-equal
         (loop for number from 1 to 31 collect number))
        (cl-weave:expect
         (ntop.process.domain:process-signal-operating-system (nth 15 signals))
         :to-equal
         "Linux")
        (cl-weave:expect
         (ntop.process.domain:process-signal-name (nth 15 signals))
         :to-equal
         "SIGSTKFLT")))) (cl-weave:it
    "reads task stats only when configured"
    (multiple-value-bind (data reason)
        (%f8a-linux-capture-process-data
         (%f8a-linux-fixture-root "complete")
         :threads t)
      (let* ((processes (first data))
             (complete (%f8a-linux-process-record processes 4242))
             (threads (ntop.process.domain:process-record-threads complete)))
        (cl-weave:expect reason :to-be :unset)
        (cl-weave:expect
         (mapcar #'ntop.process.domain:process-thread-thread-id threads)
         :to-equal
         '(4242 4243))
        (cl-weave:expect
         (ntop.process.domain:process-thread-name (second threads))
         :to-equal
         "worker thread")
        (cl-weave:expect
         (ntop.process.domain:process-thread-cpu-time-ns (second threads))
         :to-be
         (floor (* 60 1000000000)
                (ntop.process.infrastructure::%linux-process-clock-ticks-per-second)))
        (cl-weave:expect
         (ntop.process.domain:process-record-threads
          (%f8a-linux-process-record processes 4343))
         :to-be-null)))) (cl-weave:it
    "maps missing and malformed proc roots to direct failure reasons"
    (multiple-value-bind (data reason)
        (%f8a-linux-capture-process-data (%f8a-linux-fixture-root "absent"))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :not-present))
    (multiple-value-bind (data reason)
        (%f8a-linux-capture-process-data
         (%f8a-linux-fixture-root "malformed"))
      (cl-weave:expect data :to-be :unset)
      (cl-weave:expect reason :to-be :read-failed))
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :not-present)
     :to-be
     :not-present)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :permission-denied)
     :to-be
     :permission-denied)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :read-failed)
     :to-be
     :read-failed)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :disabled)
     :to-be
     :disabled)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :unsupported-platform)
     :to-be
     :unsupported-platform)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :private-api-unavailable)
     :to-be
     :private-api-unavailable)
    (cl-weave:expect
     (ntop.process.infrastructure::%linux-process-unavailability-reason
      :invalid-data)
     :to-be
     :read-failed)) (cl-weave:it
    "computes the write rate across a 0.7 second fixture interval"
    (multiple-value-bind (before before-reason)
        (%f8a-linux-capture-process-data
         (%f8a-linux-fixture-root "io-before"))
      (multiple-value-bind (after after-reason)
          (%f8a-linux-capture-process-data
           (%f8a-linux-fixture-root "io-after"))
        (let* ((previous
                 (ntop.process.domain:make-process-sample
                  :sampled-at 10.0d0
                  :processes (first before)
                  :signals (second before)))
               (current
                 (ntop.process.domain:make-process-sample
                  :sampled-at 10.7d0
                  :processes (first after)
                  :signals (second after)))
               (rate
                 (ntop.process.domain:process-write-bytes-per-second
                  previous current 4242)))
          (cl-weave:expect before-reason :to-be :unset)
          (cl-weave:expect after-reason :to-be :unset)
          (cl-weave:expect
           (ntop.process.domain:process-record-write-bytes
            (%f8a-linux-process-record (first before) 4242))
           :to-be
           2048)
          (cl-weave:expect
           (ntop.process.domain:process-record-write-bytes
            (%f8a-linux-process-record (first after) 4242))
           :to-be
           9216)
          (cl-weave:expect (< (abs (- rate 10240.0d0)) 0.001d0)
                           :to-be
                           t))))) (cl-weave:it
    "validates every target before signaling its child"
    (let* ((child (sb-ext:run-program "sleep"
                                     '("30")
                                     :search t
                                     :wait nil
                                     :input nil
                                     :output nil
                                     :error nil))
           (pid (sb-ext:process-pid child)))
      (unwind-protect
           (progn
             (cl-weave:expect
              (lambda ()
                (ntop.process.infrastructure::send-process-signal
                 (list pid 0)
                 sb-posix:sigterm))
              :to-throw 'error)
             (cl-weave:expect (sb-ext:process-alive-p child) :to-be t)
             (cl-weave:expect
              (ntop.process.infrastructure::send-process-signal
               (list pid)
               sb-posix:sigterm)
              :to-be
              t)
             (loop repeat 100
                   until (not (sb-ext:process-alive-p child))
                   do (sleep 0.01))
             (cl-weave:expect (sb-ext:process-alive-p child) :to-be nil))
        (when (sb-ext:process-alive-p child)
          (handler-case
              (sb-posix:kill pid sb-posix:sigkill)
            (sb-posix:syscall-error () nil)))
        (sb-ext:process-wait child)))))


(cl-weave:describe
 "F8a process signal permission handling"
 (cl-weave:it
  "preserves a live owned child when the send port reports permission denied"
  (let* ((child (sb-ext:run-program "sleep"
                                    '("30")
                                    :search t
                                    :wait nil
                                    :input nil
                                    :output nil
                                    :error nil))
         (pid (sb-ext:process-pid child))
         (signal
           (ntop.process.domain:make-process-signal
            :name "SIGTERM"
            :number sb-posix:sigterm
            :operating-system "Linux"))
         (sample
           (ntop.process.domain:make-process-sample
            :sampled-at 0.0d0
            :signals (list signal)))
         (state
           (ntop.process.application::make-process-termination-state
            :target-pids (list pid)))
         (selected-state nil)
         (selection-result nil)
         (config (ntop.shared.application:make-application-config)))
    (unwind-protect
         (progn
           (multiple-value-setq (selected-state selection-result)
             (ntop.process.application::select-process-signal
              state sample "SIGTERM"))
           (cl-weave:expect selection-result :to-be :selected)
           (multiple-value-bind (returned-state result)
               (ntop.process.application::send-process-termination
                selected-state
                config
                (lambda (pids number)
                  (cl-weave:expect pids :to-equal (list pid))
                  (cl-weave:expect number :to-be sb-posix:sigterm)
                  (error "permission denied: EPERM")))
             (cl-weave:expect returned-state :to-be selected-state)
             (cl-weave:expect (typep result 'error) :to-be-truthy)
             (cl-weave:expect
              (search "permission denied" (princ-to-string result))
              :to-be-truthy)
             (cl-weave:expect
              (ntop.process.application::process-termination-state-send-status
               selected-state)
              :to-be
              :pending)
             (cl-weave:expect (sb-ext:process-alive-p child) :to-be t)))
      (when (sb-ext:process-alive-p child)
        (handler-case
            (sb-posix:kill pid sb-posix:sigkill)
          (sb-posix:syscall-error () nil)))
      (sb-ext:process-wait child)))))

(defclass %f8a-process-test-clock ()
  ((outcome :initarg :outcome :reader %f8a-process-test-clock-outcome)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock %f8a-process-test-clock) on-time on-unavailable)
  (let ((outcome (%f8a-process-test-clock-outcome clock)))
    (ecase (first outcome)
      (:time (funcall on-time (second outcome)))
      (:unavailable (funcall on-unavailable (second outcome))))))

(defclass %f8a-process-test-collector
    (ntop.process.infrastructure::process-collector)
  ((outcome :initarg :outcome :reader %f8a-process-test-collector-outcome)))

(defmethod ntop.process.infrastructure::read-process-data
    ((collector %f8a-process-test-collector) on-value on-unavailable)
  (let ((outcome (%f8a-process-test-collector-outcome collector)))
    (ecase (first outcome)
      (:value (funcall on-value (second outcome) (third outcome)))
      (:unavailable (funcall on-unavailable (second outcome))))))

