(in-package #:ntop.process.infrastructure)
(defparameter *darwin-process-signals*
    '((1 "SIGHUP") (2 "SIGINT") (3 "SIGQUIT") (4 "SIGILL")
      (5 "SIGTRAP") (6 "SIGABRT") (7 "SIGEMT") (8 "SIGFPE")
      (9 "SIGKILL") (10 "SIGBUS") (11 "SIGSEGV") (12 "SIGSYS")
      (13 "SIGPIPE") (14 "SIGALRM") (15 "SIGTERM") (16 "SIGURG")
      (17 "SIGSTOP") (18 "SIGTSTP") (19 "SIGCONT") (20 "SIGCHLD")
      (21 "SIGTTIN") (22 "SIGTTOU") (23 "SIGIO") (24 "SIGXCPU")
      (25 "SIGXFSZ") (26 "SIGVTALRM") (27 "SIGPROF") (28 "SIGWINCH")
      (29 "SIGINFO") (30 "SIGUSR1") (31 "SIGUSR2")))

  (defun %darwin-process-signals ()
    (loop for (number name) in *darwin-process-signals*
          collect (ntop.process.domain:make-process-signal
                   :operating-system "Darwin"
                   :name name
                   :number number)))

  (defun %darwin-space-fields (line)
    (let ((fields nil)
          (start nil))
      (loop for index from 0 to (length line)
            for separator = (or (= index (length line))
                                (find (char line index)
                                      '(#\Space #\Tab #\Return)))
            do (cond
                 ((and separator start)
                  (push (subseq line start index) fields)
                  (setf start nil))
                 ((and (not separator) (null start))
                  (setf start index))))
      (nreverse fields)))

  (defun %darwin-ps-number (text)
    (let ((*read-eval* nil))
      (multiple-value-bind (value end)
          (read-from-string text nil nil)
        (and (= end (length text))
             (realp value)
             (float value 1d0)))))

  (defun %darwin-ps-cpu-values (pids)
  (if (null pids)
      (values (make-hash-table :test #'eql) nil)
      (handler-case
          (let* ((pid-list (format nil "~{~D~^,~}" pids))
                 (result (host-kit:run-program
                          "/bin/ps"
                          (list "-o" "pid=,pcpu=" "-p" pid-list)
                          :timeout 5
                          :environment '("LC_ALL=C")
                          :max-output-characters 1048576))
                 (values (make-hash-table :test #'eql)))
            (unless (and (zerop (host-kit:process-result-exit-code result))
                         (not (host-kit:process-result-timed-out-p result))
                         (not (host-kit:process-result-stdout-truncated-p result))
                         (not (host-kit:process-result-stderr-truncated-p result)))
              (error "ps CPU fallback failed or exceeded its output limit."))
            (with-input-from-string
                (input (host-kit:process-result-stdout result))
              (loop for line = (read-line input nil nil)
                    while line
                    for fields = (%darwin-space-fields line)
                    when (= (length fields) 2)
                      do (let ((pid (ignore-errors
                                      (parse-integer (first fields))))
                               (cpu (%darwin-ps-number (second fields))))
                           (when (and pid cpu)
                             (setf (gethash pid values) cpu))))
              (values values nil)))
        (error () (values nil :read-failed)))))

  (defun %darwin-user-name (uid)
    (when (integerp uid)
      (or (handler-case
              (let ((entry (sb-posix:getpwuid uid)))
                (and entry (sb-posix:passwd-name entry)))
            (error () nil))
          (write-to-string uid))))

  (defun %darwin-process-record (row cpu-values)
  (let ((ruid (getf row :ruid)))
    (ntop.process.domain:make-process-record
     :pid (getf row :pid)
     :ppid (getf row :ppid)
     :name (getf row :name)
     :command (getf row :command)
     :cpu-time-ns (getf row :cpu-time-ns)
     :resident-memory-bytes (getf row :resident-memory-bytes)
     :virtual-memory-bytes (getf row :virtual-memory-bytes)
     :read-bytes (getf row :read-bytes)
     :write-bytes (getf row :write-bytes)
     :uid ruid
     :user-name (%darwin-user-name ruid)
     :state (getf row :state)
     :start-time-unix-seconds (getf row :start-time-unix-seconds)
     :priority (getf row :priority)
     :nice (getf row :nice)
     :kernel-thread-p (getf row :kernel-thread-p)
     :threads
     (mapcar
      (lambda (thread-row)
        (ntop.process.domain:make-process-thread
         :thread-id (getf thread-row :thread-id)
         :name (getf thread-row :name)
         :cpu-time-ns (getf thread-row :cpu-time-ns)
         :state (getf thread-row :state)
         :priority (getf thread-row :priority)
         :nice (getf thread-row :nice)))
      (getf row :threads))
     :ps-fallback-cpu-percent
     (gethash (getf row :pid) cpu-values))))

  (defun read-darwin-process-data (collector on-value on-unavailable)
  (ntop.platform.darwin::read-darwin-process-rows
   (lambda (rows)
     (multiple-value-bind (cpu-values cpu-failure)
         (%darwin-ps-cpu-values (mapcar (lambda (row) (getf row :pid)) rows))
       (declare (ignore cpu-failure))
       (funcall on-value
                (loop for row in rows
                      collect (%darwin-process-record row cpu-values))
                (%darwin-process-signals))))
   on-unavailable
   :include-threads-p
   (ntop.shared.application:application-config-process-get-threads-p
    (ntop.process.infrastructure::%process-collector-config collector))))

  #+darwin
  (defmethod read-process-data
      ((collector process-collector) on-value on-unavailable)
    (read-darwin-process-data collector on-value on-unavailable))