(in-package #:ntop.process.infrastructure)
  (defparameter +linux-process-signal-catalog+
  '(("SIGHUP" 1) ("SIGINT" 2) ("SIGQUIT" 3) ("SIGILL" 4)
    ("SIGTRAP" 5) ("SIGABRT" 6) ("SIGBUS" 7) ("SIGFPE" 8)
    ("SIGKILL" 9) ("SIGUSR1" 10) ("SIGSEGV" 11) ("SIGUSR2" 12)
    ("SIGPIPE" 13) ("SIGALRM" 14) ("SIGTERM" 15) ("SIGSTKFLT" 16)
    ("SIGCHLD" 17) ("SIGCONT" 18) ("SIGSTOP" 19) ("SIGTSTP" 20)
    ("SIGTTIN" 21) ("SIGTTOU" 22) ("SIGURG" 23) ("SIGXCPU" 24)
    ("SIGXFSZ" 25) ("SIGVTALRM" 26) ("SIGPROF" 27) ("SIGWINCH" 28)
    ("SIGIO" 29) ("SIGPWR" 30) ("SIGSYS" 31)))
  (defconstant +linux-sc-clk-tck+ 2)
  (defconstant +linux-pf-kthread+ #x00200000)
  #+linux
  (sb-alien:define-alien-routine ("sysconf" %linux-process-sysconf)
      sb-alien:long
    (name sb-alien:int))
  (defun %linux-process-clock-ticks-per-second ()
    #+linux
    (let ((ticks (%linux-process-sysconf +linux-sc-clk-tck+)))
      (and (integerp ticks) (plusp ticks) ticks))
    #-linux
    100)
  (defun %linux-process-status-value (status key)
    (cdr (assoc key status :test #'string=)))
  (defun %linux-process-io-value (io key)
    (cdr (assoc key io :test #'string=)))
  (defun %linux-process-status-uid (status)
  (let ((uid (%linux-process-status-value status "Uid")))
    (cond ((integerp uid) uid)
          ((and (consp uid) (integerp (first uid))) (first uid)))))
  (defun %linux-process-status-bytes (status key)
  (let ((value (%linux-process-status-value status key)))
    (when (and (consp value)
               (integerp (first value))
               (equal (second value) "kB"))
      (* 1024 (first value)))))
  (defun %linux-process-cpu-time-ns (stat clock-ticks)
    (let ((user-ticks (getf stat :utime))
          (system-ticks (getf stat :stime)))
      (when (and (integerp user-ticks) (not (minusp user-ticks))
                 (integerp system-ticks) (not (minusp system-ticks)))
        (floor (* (+ user-ticks system-ticks) 1000000000)
               clock-ticks))))
  (defun %linux-process-start-time (stat boot-time clock-ticks)
    (let ((start-ticks (getf stat :starttime)))
      (when (and (integerp start-ticks) (not (minusp start-ticks)))
        (+ boot-time (/ (coerce start-ticks 'double-float)
                        (coerce clock-ticks 'double-float))))))
  (defun %linux-process-kernel-thread-p (stat)
  (let ((flags (getf stat :flags)))
    (if (integerp flags)
        (plusp (logand flags +linux-pf-kthread+))
        :unknown)))
  (defun %linux-process-command (arguments)
    (when arguments
      (format nil "~{~A~^ ~}" arguments)))
  (defun %linux-process-signal-list ()
    (loop for (name number) in +linux-process-signal-catalog+
          collect
          (ntop.process.domain:make-process-signal
           :operating-system "Linux"
           :name name
           :number number)))
  (defun %linux-process-thread (stat clock-ticks)
    (ntop.process.domain:make-process-thread
     :thread-id (getf stat :pid)
     :name (getf stat :comm)
     :cpu-time-ns (%linux-process-cpu-time-ns stat clock-ticks)
     :state (getf stat :state)
     :priority (getf stat :priority)
     :nice (getf stat :nice)))
  (defun %read-linux-process-threads (pid thread-ids clock-ticks on-value)
    (let ((threads nil))
      (dolist (thread-id thread-ids)
        (ntop.platform.linux::read-linux-file
         (format nil "proc/~D/task/~D/stat" pid thread-id)
         (lambda (text)
           (ntop.platform.linux::parse-linux-process-stat
            text
            (lambda (stat)
              (push (%linux-process-thread stat clock-ticks) threads))
            (lambda (reason)
              (declare (ignore reason)))))
         (lambda (reason)
           (declare (ignore reason)))))
      (funcall on-value (nreverse threads))))
  (defun %linux-process-record
    (pid stat status io arguments user-names threads boot-time clock-ticks)
  (let* ((uid (%linux-process-status-uid status))
         (start-time (%linux-process-start-time stat boot-time clock-ticks)))
    (when (and start-time (= pid (getf stat :pid)))
      (ntop.process.domain:make-process-record
       :pid pid
       :ppid (getf stat :ppid)
       :name (getf stat :comm)
       :command (%linux-process-command arguments)
       :cpu-time-ns (%linux-process-cpu-time-ns stat clock-ticks)
       :resident-memory-bytes (%linux-process-status-bytes status "VmRSS")
       :virtual-memory-bytes (%linux-process-status-bytes status "VmSize")
       :read-bytes (%linux-process-io-value io "read_bytes")
       :write-bytes (%linux-process-io-value io "write_bytes")
       :uid uid
       :user-name (cdr (assoc uid user-names :test #'eql))
       :state (getf stat :state)
       :start-time-unix-seconds start-time
       :priority (getf stat :priority)
       :nice (getf stat :nice)
       :kernel-thread-p (%linux-process-kernel-thread-p stat)
       :threads threads))))
  (defun %read-linux-process-record
      (pid boot-time clock-ticks user-names include-threads on-value)
    (ntop.platform.linux:read-linux-process-stat
     pid
     (lambda (stat)
       (labels
           ((read-optional (reader continuation)
              (funcall reader
                       pid
                       continuation
                       (lambda (reason)
                         (declare (ignore reason))
                         (funcall continuation nil))))
            (finish (status io arguments threads)
              (funcall on-value
                       (%linux-process-record
                        pid stat status io arguments user-names threads
                        boot-time clock-ticks)))
            (read-threads (status io arguments)
              (if include-threads
                  (read-optional
                   #'ntop.platform.linux:read-linux-process-tasks
                   (lambda (thread-ids)
                     (%read-linux-process-threads
                      pid thread-ids clock-ticks
                      (lambda (threads)
                        (finish status io arguments threads)))))
                  (finish status io arguments nil))))
         (read-optional
          #'ntop.platform.linux:read-linux-process-status
          (lambda (status)
            (read-optional
             #'ntop.platform.linux:read-linux-process-io
             (lambda (io)
               (read-optional
                #'ntop.platform.linux:read-linux-process-cmdline
                (lambda (arguments)
                  (read-threads status io arguments)))))))))
     (lambda (reason)
       (declare (ignore reason))
       (funcall on-value nil))))
  (defun %linux-process-pids (entries)
    (loop for entry in entries
          for pid = (ntop.platform.linux::parse-linux-integer entry)
          when (and pid (plusp pid))
            collect pid))
  (defun %linux-passwd-entry (line)
    (let* ((first-colon (position #\: line))
           (second-colon
             (and first-colon (position #\: line :start (1+ first-colon))))
           (third-colon
             (and second-colon (position #\: line :start (1+ second-colon))))
           (uid (and third-colon
                     (ntop.platform.linux::parse-linux-integer
                      (subseq line (1+ second-colon) third-colon)))))
      (when (and uid first-colon)
        (cons uid (subseq line 0 first-colon)))))
  (defun %linux-passwd-user-names (text)
    (loop for line in (ntop.platform.linux::split-linux-lines text)
          for entry = (%linux-passwd-entry line)
          when entry
            collect entry))
  (defun %read-linux-passwd-user-names (on-value)
    (ntop.platform.linux::read-linux-file
     "etc/passwd"
     (lambda (text)
       (funcall on-value (%linux-passwd-user-names text)))
     (lambda (reason)
       (declare (ignore reason))
       (funcall on-value nil))))
  (defun %linux-process-unavailability-reason (reason)
    (case reason
      ((:not-present :permission-denied :disabled :unsupported-platform
        :private-api-unavailable :read-failed)
       reason)
      (otherwise :read-failed)))
  (defun %read-linux-process-data (collector on-value on-unavailable)
  (let ((clock-ticks (%linux-process-clock-ticks-per-second))
        (include-threads
          (ntop.shared.application:application-config-process-get-threads-p
           (%process-collector-config collector))))
    (if (not (and (integerp clock-ticks) (plusp clock-ticks)))
        (funcall on-unavailable :read-failed)
        (ntop.platform.linux:read-linux-proc-stat
         (lambda (system-stat)
           (let ((boot-time
                   (second (assoc "btime" system-stat :test (function string=)))))
             (if (not (and (integerp boot-time) (plusp boot-time)))
                 (funcall on-unavailable :read-failed)
                 (ntop.platform.linux::read-linux-directory
                  "proc"
                  (lambda (entries)
                    (%read-linux-passwd-user-names
                     (lambda (user-names)
                       (let ((records nil))
                         (dolist (pid (%linux-process-pids entries))
                           (%read-linux-process-record
                            pid boot-time clock-ticks user-names include-threads
                            (lambda (record)
                              (when record (push record records)))))
                         (funcall on-value
                                  (nreverse records)
                                  (%linux-process-signal-list))))))
                  (lambda (reason)
                    (funcall on-unavailable
                             (%linux-process-unavailability-reason reason)))))))
         (lambda (reason)
           (funcall on-unavailable
                    (%linux-process-unavailability-reason reason)))))))
  #+linux
(defmethod read-process-data
    ((collector process-collector) on-value on-unavailable)
  (%read-linux-process-data collector on-value on-unavailable))
