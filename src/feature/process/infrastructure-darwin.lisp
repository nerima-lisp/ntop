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
        (start nil)
        (command-start nil))
    (loop for index from 0 to (length line)
          for separator = (or (= index (length line))
                              (find (char line index)
                                    '(#\Space #\Tab #\Return)))
          do (cond
               ((and separator start)
                (when (< (length fields) 3)
                  (push (subseq line start index) fields))
                (setf start nil))
               ((and (not separator) (null start))
                (cond
                  ((< (length fields) 3)
                   (setf start index))
                  (t
                   (setf command-start index)
                   (return))))))
    (values (nreverse fields) command-start)))

  (defun %darwin-ps-number (text)
    (let ((*read-eval* nil))
      (multiple-value-bind (value end)
          (read-from-string text nil nil)
        (and (= end (length text))
             (realp value)
             (float value 1d0)))))

(defun %darwin-ps-cpu-values (rows &optional (include-command-p t))
  (if (null rows)
      (values (make-hash-table :test (function eql)) nil)
      (handler-case
          (let* ((pids (mapcar (lambda (row) (getf row :pid)) rows))
                 (command-pids
                   (when include-command-p
                     (let ((table (make-hash-table :test (function eql)))
                           (user-id (sb-posix:getuid)))
                       (dolist (row rows)
                         (when (eql (getf row :ruid) user-id)
                           (setf (gethash (getf row :pid) table) t)))
                       table)))
                 (pid-list (format nil "~{~D~^,~}" pids))
                 (result
                   (host-kit:run-program
                    "/bin/ps"
                    (list "-ww" "-o"
                          (if include-command-p
                              "pid=,pcpu=,pri=,command="
                              "pid=,pcpu=,pri=")
                          "-p" pid-list)
                    :timeout 5
                    :environment (quote ("LC_ALL=C"))
                    :max-output-characters (* 4 1024 1024)))
                 (ps-values (make-hash-table :test (function eql))))
            (unless (and (zerop (host-kit:process-result-exit-code result))
                         (not (host-kit:process-result-timed-out-p result))
                         (not (host-kit:process-result-stdout-truncated-p result))
                         (not (host-kit:process-result-stderr-truncated-p result)))
              (error "ps CPU fallback failed or exceeded its output limit."))
            (with-input-from-string
                (input (host-kit:process-result-stdout result))
              (loop for line = (read-line input nil nil)
                    while line
                    do (multiple-value-bind (fields command-start)
                           (%darwin-space-fields line)
                         (when (<= 2 (length fields) 3)
                           (let* ((pid (ignore-errors
                                         (parse-integer (first fields))))
                                  (cpu (%darwin-ps-number (second fields)))
                                  (priority
                                    (and (= (length fields) 3)
                                         (ignore-errors
                                           (parse-integer (third fields)))))
                                  (command
                                    (and include-command-p
                                         pid
                                         (gethash pid command-pids)
                                         priority
                                         command-start
                                         (subseq line command-start))))
                             (when (and pid cpu)
                               (setf (gethash pid ps-values)
                                     (list :cpu cpu
                                           :priority priority
                                           :command command))))))))
            (values ps-values nil))
        (error () (values nil :read-failed)))))

  (defun %darwin-user-name (uid)
    (when (integerp uid)
      (or (handler-case
              (let ((entry (sb-posix:getpwuid uid)))
                (and entry (sb-posix:passwd-name entry)))
            (error () nil))
          (write-to-string uid))))

(defun %darwin-process-record (row ps-values)
  (let* ((uid (getf row :uid))
         (pid (getf row :pid))
         (process-ps-values (and (hash-table-p ps-values)
                                 (gethash pid ps-values))))
    (ntop.process.domain:make-process-record
     :pid pid
     :ppid (getf row :ppid)
     :name (getf row :name)
     :command (or (getf process-ps-values :command) (getf row :command))
     :cpu-time-ns (getf row :cpu-time-ns)
     :resident-memory-bytes (getf row :resident-memory-bytes)
     :virtual-memory-bytes (getf row :virtual-memory-bytes)
     :read-bytes (getf row :read-bytes)
     :write-bytes (getf row :write-bytes)
     :uid uid
     :user-name (%darwin-user-name uid)
     :state (getf row :state)
     :start-time-unix-seconds (getf row :start-time-unix-seconds)
     :priority (or (getf row :priority) (getf process-ps-values :priority))
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
     :ps-fallback-cpu-percent (getf process-ps-values :cpu))))

  (defun read-darwin-process-data (collector on-value on-unavailable)
  (ntop.platform.darwin::read-darwin-process-rows
   (lambda (rows)
     (multiple-value-bind (cpu-values cpu-failure)
         (%darwin-ps-cpu-values
          rows
          (ntop.shared.application:application-config-process-show-command-p
           (ntop.process.infrastructure::%process-collector-config collector)))
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