(in-package #:ntop.platform.darwin)
(progn
  (defparameter +darwin-process-layout+
  '(:proc-listpids-type 1
    :proc-pidinfo-bsdinfo 3 :proc-pidinfo-taskallinfo 2
    :proc-pidinfo-listthreads 6 :proc-pidinfo-threadinfo 5
    :proc-rusage-v2 2 :sysctl-kern 1 :sysctl-proc 14
    :sysctl-proc-pid 1 :sysctl-procargs2 49
    :initial-buffer-size 4096 :maximum-buffer-size 1048576
    :pid-width 4
    :bsdinfo-size 136 :bsdinfo-flags 0 :bsdinfo-pid 12 :bsdinfo-ppid 16
    :bsdinfo-uid 20 :bsdinfo-ruid 28 :bsdinfo-name 48
    :bsdinfo-name-size 16 :bsdinfo-display-name 64
    :bsdinfo-display-name-size 32 :bsdinfo-status 4 :bsdinfo-nice 116
    :bsdinfo-start 120 :bsdinfo-start-usec 128
    :kinfo-size 648 :kinfo-flags 32 :kinfo-state 36
    :kinfo-pid 40 :kinfo-priority 240
    :kinfo-nice 242 :kinfo-name 243 :kinfo-name-size 17
    :kinfo-ruid 392 :kinfo-uid 420 :kinfo-ppid 560
    :kinfo-start 0 :kinfo-start-usec 8
    :taskallinfo-size 232 :taskallinfo-vsize 136 :taskallinfo-rss 144
    :taskallinfo-user 152 :taskallinfo-system 160 :taskallinfo-threads 220
    :threadinfo-size 112 :threadinfo-user 0 :threadinfo-system 8
    :threadinfo-state 24 :threadinfo-state-width 4
    :threadinfo-priority 40 :threadinfo-priority-width 4
    :threadinfo-name 48 :threadinfo-name-size 64
    :rusage-size 160 :rusage-read 144 :rusage-write 152))
  (progn
  (defconstant +darwin-proc-flag-system+ 1)
  (defconstant +darwin-p-system+ #x200)))
(progn
  (defparameter *darwin-process-statuses*
    '((1 . "Creating") (2 . "Running") (3 . "Sleeping")
      (4 . "Stopped") (5 . "Zombie")))
  (defparameter *darwin-thread-statuses*
    '((1 . "Running") (2 . "Stopped") (3 . "Waiting")
      (4 . "Uninterruptible") (5 . "Halted"))))
(defun %darwin-process-integer (octets offset width &optional signed-p)
    (funcall (if signed-p #'decode-little-endian-signed
                 #'decode-little-endian-unsigned)
             octets :start offset :end (+ offset width)))
  (defun %darwin-process-string (octets offset width)
    (decode-c-string octets :start offset :end (+ offset width)))
  (defun decode-darwin-proc-bsdinfo (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let ((layout +darwin-process-layout+))
    (unless (= (length octets) (getf layout :bsdinfo-size))
      (error "Expected a 136-byte proc_bsdinfo buffer."))
    (let* ((flags (%darwin-process-integer
                   octets (getf layout :bsdinfo-flags) 4))
           (short-name
             (%darwin-process-string octets
                                     (getf layout :bsdinfo-name)
                                     (getf layout :bsdinfo-name-size)))
           (display-name
             (%darwin-process-string octets
                                     (getf layout :bsdinfo-display-name)
                                     (getf layout :bsdinfo-display-name-size))))
      (list :flags flags
            :kernel-thread-p
            (not (zerop (logand flags +darwin-proc-flag-system+)))
            :pid (%darwin-process-integer octets (getf layout :bsdinfo-pid) 4)
            :ppid (%darwin-process-integer octets (getf layout :bsdinfo-ppid) 4)
            :uid (%darwin-process-integer octets (getf layout :bsdinfo-uid) 4)
            :ruid (%darwin-process-integer octets (getf layout :bsdinfo-ruid) 4)
            :name (if (plusp (length display-name)) display-name short-name)
            :state (cdr (assoc
                         (%darwin-process-integer
                          octets (getf layout :bsdinfo-status) 4)
                         *darwin-process-statuses*))
            :nice (%darwin-process-integer
                   octets (getf layout :bsdinfo-nice) 4 t)
            :start-time-unix-seconds
            (+ (%darwin-process-integer octets (getf layout :bsdinfo-start) 8)
               (/ (%darwin-process-integer
                   octets (getf layout :bsdinfo-start-usec) 4)
                  1000000d0))))))
  (defun decode-darwin-kinfo-proc (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let ((layout +darwin-process-layout+))
    (unless (= (length octets) (getf layout :kinfo-size))
      (error "Expected a 648-byte kinfo_proc buffer."))
    (let ((flags (%darwin-process-integer
                  octets (getf layout :kinfo-flags) 4)))
      (list :flags flags
            :kernel-thread-p (not (zerop (logand flags +darwin-p-system+)))
            :pid (%darwin-process-integer octets (getf layout :kinfo-pid) 4)
            :ppid (%darwin-process-integer octets (getf layout :kinfo-ppid) 4)
            :uid (%darwin-process-integer octets (getf layout :kinfo-uid) 4)
            :ruid (%darwin-process-integer octets (getf layout :kinfo-ruid) 4)
            :name (%darwin-process-string octets
                                          (getf layout :kinfo-name)
                                          (getf layout :kinfo-name-size))
            :state (cdr (assoc
                         (%darwin-process-integer
                          octets (getf layout :kinfo-state) 1)
                         *darwin-process-statuses*))
            :priority (%darwin-process-integer
                       octets (getf layout :kinfo-priority) 1)
            :nice (%darwin-process-integer octets (getf layout :kinfo-nice) 1 t)
            :start-time-unix-seconds
            (+ (%darwin-process-integer octets (getf layout :kinfo-start) 8)
               (/ (%darwin-process-integer
                   octets (getf layout :kinfo-start-usec) 8)
                  1000000d0))))))
  (progn
  (defun decode-darwin-taskallinfo (octets)
    (check-type octets (vector (unsigned-byte 8)))
    (let ((layout +darwin-process-layout+))
      (unless (= (length octets) (getf layout :taskallinfo-size))
        (error "Expected a 232-byte proc_taskallinfo buffer."))
      (list :virtual-memory-bytes
            (%darwin-process-integer octets (getf layout :taskallinfo-vsize) 8)
            :resident-memory-bytes
            (%darwin-process-integer octets (getf layout :taskallinfo-rss) 8)
            :user-time-ns
            (%darwin-process-integer octets (getf layout :taskallinfo-user) 8)
            :system-time-ns
            (%darwin-process-integer octets (getf layout :taskallinfo-system) 8)
            :thread-count
            (%darwin-process-integer octets (getf layout :taskallinfo-threads) 4))))
  (defun decode-darwin-proc-threadinfo (octets thread-id)
  (check-type octets (vector (unsigned-byte 8)))
  (check-type thread-id (integer 1 *))
  (let ((layout +darwin-process-layout+))
    (unless (= (length octets) (getf layout :threadinfo-size))
      (error "Expected a 112-byte proc_threadinfo buffer."))
    (let ((name (%darwin-process-string
                 octets
                 (getf layout :threadinfo-name)
                 (getf layout :threadinfo-name-size))))
      (list :thread-id thread-id
            :name (and (plusp (length name)) name)
            :cpu-time-ns
            (+ (%darwin-process-integer
                octets (getf layout :threadinfo-user) 8)
               (%darwin-process-integer
                octets (getf layout :threadinfo-system) 8))
            :state
            (cdr (assoc
                  (%darwin-process-integer
                   octets
                   (getf layout :threadinfo-state)
                   (getf layout :threadinfo-state-width))
                  *darwin-thread-statuses*))
            :priority
            (%darwin-process-integer
             octets
             (getf layout :threadinfo-priority)
             (getf layout :threadinfo-priority-width)))))))
  (defun decode-darwin-rusage-info (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let ((layout +darwin-process-layout+))
    (unless (= (length octets) (getf layout :rusage-size))
      (error "Expected a 160-byte rusage_info_v2 buffer."))
    (list :read-bytes (%darwin-process-integer octets
                                               (getf layout :rusage-read) 8)
          :write-bytes (%darwin-process-integer octets
                                                (getf layout :rusage-write) 8))))
  (defun decode-darwin-pid-buffer (octets &optional (width (getf +darwin-process-layout+ :pid-width)))
  (check-type octets (vector (unsigned-byte 8)))
  (check-type width (integer 1 *))
  (unless (zerop (mod (length octets) width))
    (error "A process or thread ID buffer must contain whole values."))
  (loop for offset from 0 below (length octets) by width
        for pid = (%darwin-process-integer octets offset width)
        when (plusp pid) collect pid))
  (defun decode-darwin-procargs2 (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (when (< (length octets) 5)
    (error "A KERN_PROCARGS2 buffer must include argc and a path."))
  (let* ((argc (%darwin-process-integer octets 0 4 t))
         (path-end (position 0 octets :start 4)))
    (when (minusp argc)
      (error "KERN_PROCARGS2 argc cannot be negative."))
    (unless path-end
      (error "A KERN_PROCARGS2 path must be NUL-terminated."))
    (let ((path (%darwin-process-string octets 4 (- path-end 4)))
          (start (1+ path-end))
          (arguments nil))
      (loop while (and (< start (length octets)) (zerop (aref octets start)))
            do (incf start))
      (loop repeat argc
            for end = (or (position 0 octets :start start)
                          (error "A KERN_PROCARGS2 argument is truncated."))
            do (push (%darwin-process-string octets start (- end start))
                     arguments)
               (setf start (1+ end)))
      (if arguments (format nil "~{~A~^ ~}" (nreverse arguments)) path))))
  #+darwin
  (eval-when (:load-toplevel :execute)
    (sb-alien:load-shared-object "/usr/lib/libproc.dylib"))
  #+darwin
  (define-darwin-ffi
    (:routine %proc-listpids "proc_listpids" sb-alien:int
      (type sb-alien:unsigned-int) (type-info sb-alien:unsigned-int)
      (buffer sb-alien:system-area-pointer) (buffer-size sb-alien:int))
    (:routine %proc-pidinfo "proc_pidinfo" sb-alien:int
      (pid sb-alien:int) (flavor sb-alien:int)
      (argument sb-alien:unsigned-long-long)
      (buffer sb-alien:system-area-pointer) (buffer-size sb-alien:int))
    (:routine %proc-pid-rusage "proc_pid_rusage" sb-alien:int
      (pid sb-alien:int) (flavor sb-alien:int)
      (buffer sb-alien:system-area-pointer)))
  #+darwin
(defun %darwin-list-pids ()
  (let ((layout +darwin-process-layout+))
    (loop with capacity = (getf layout :initial-buffer-size)
          do (let* ((buffer (make-array capacity
                                        :element-type '(unsigned-byte 8)))
                    (count (sb-sys:with-pinned-objects (buffer)
                             (%proc-listpids
                              (getf layout :proc-listpids-type)
                              0 (sb-sys:vector-sap buffer) capacity))))
               (cond
                 ((minusp count)
                  (return (values nil :private-api-unavailable)))
                 ((< count capacity)
                  (return
                    (values (decode-darwin-pid-buffer
                             (subseq buffer 0 count))
                            nil)))
                 ((>= capacity (getf layout :maximum-buffer-size))
                  (return (values nil :read-failed)))
                 (t (setf capacity (* 2 capacity))))))))
  #+darwin
(defun %darwin-info (pid flavor size &optional (exact-p t) (argument 0))
  (let ((buffer (make-array size :element-type '(unsigned-byte 8))))
    (let ((count (sb-sys:with-pinned-objects (buffer)
                   (%proc-pidinfo pid flavor argument
                                  (sb-sys:vector-sap buffer) size))))
      (when (and (<= 0 count size)
                 (or (not exact-p) (= count size)))
        (if (= count size) buffer (subseq buffer 0 count))))))
  #+darwin
(defun %darwin-rusage (pid)
  (let* ((layout +darwin-process-layout+)
         (buffer (make-array (getf layout :rusage-size)
                             :element-type '(unsigned-byte 8)))
         (status
           (sb-sys:with-pinned-objects (buffer)
             (%proc-pid-rusage pid
                               (getf layout :proc-rusage-v2)
                               (sb-sys:vector-sap buffer)))))
    (and (zerop status) buffer)))
  #+darwin
  (defun %darwin-sysctl (mib)
    (let ((result nil))
      (sysctl-read-mib mib
                       (lambda (octets) (setf result octets))
                       (lambda (reason) (declare (ignore reason))))
      result))
  #+darwin
  (defun %darwin-try (thunk)
    (handler-case (funcall thunk)
      (error () nil)))
  #+darwin
(defun %darwin-info-decoded (pid flavor size decoder &optional (argument 0))
  (let ((octets (%darwin-try
                 (lambda ()
                   (%darwin-info pid flavor size t argument)))))
    (and octets
         (%darwin-try (lambda () (funcall decoder octets))))))
  #+darwin
(defun %darwin-metadata (pid)
  (let* ((layout +darwin-process-layout+)
         (bsd
           (%darwin-info-decoded
            pid (getf layout :proc-pidinfo-bsdinfo)
            (getf layout :bsdinfo-size) #'decode-darwin-proc-bsdinfo))
         (kernel-octets
           (%darwin-try
            (lambda ()
              (%darwin-sysctl
               (list (getf layout :sysctl-kern)
                     (getf layout :sysctl-proc)
                     (getf layout :sysctl-proc-pid)
                     pid)))))
         (kernel
           (and kernel-octets
                (= (length kernel-octets) (getf layout :kinfo-size))
                (%darwin-try
                 (lambda () (decode-darwin-kinfo-proc kernel-octets))))))
    (if bsd
        (progn
          (setf (getf bsd :priority) (getf kernel :priority))
          bsd)
        kernel)))
  #+darwin
(defun %darwin-command-line (pid)
  (let* ((layout +darwin-process-layout+)
         (octets
           (%darwin-try
            (lambda ()
              (%darwin-sysctl
               (list (getf layout :sysctl-kern)
                     (getf layout :sysctl-procargs2)
                     pid))))))
    (and octets
         (%darwin-try (lambda () (decode-darwin-procargs2 octets))))))
  #+darwin
(progn
  (defun %darwin-thread-ids (pid)
    (let ((layout +darwin-process-layout+))
      (loop with capacity = (getf layout :initial-buffer-size)
            for octets = (%darwin-info
                          pid (getf layout :proc-pidinfo-listthreads)
                          capacity nil)
            when (null octets) return nil
            when (< (length octets) capacity)
              return (decode-darwin-pid-buffer octets 8)
            when (>= capacity (getf layout :maximum-buffer-size))
              return nil
            do (setf capacity (* 2 capacity)))))
  (defun %darwin-thread-rows (pid)
    (let ((layout +darwin-process-layout+))
      (loop for thread-id in (%darwin-thread-ids pid)
            for row = (%darwin-info-decoded
                       pid
                       (getf layout :proc-pidinfo-threadinfo)
                       (getf layout :threadinfo-size)
                       (lambda (octets)
                         (decode-darwin-proc-threadinfo octets thread-id))
                       thread-id)
            when row collect row)))
  (defun %darwin-process-row (pid real-uid include-threads-p)
    (let* ((metadata (%darwin-metadata pid))
           (ruid (getf metadata :ruid)))
      (when metadata
        (let* ((own-p (and (integerp ruid) (= ruid real-uid)))
               (task (and own-p
                          (%darwin-info-decoded
                           pid
                           (getf +darwin-process-layout+
                                 :proc-pidinfo-taskallinfo)
                           (getf +darwin-process-layout+ :taskallinfo-size)
                           #'decode-darwin-taskallinfo)))
               (usage (and own-p
                           (%darwin-try
                            (lambda ()
                              (let ((bytes (%darwin-rusage pid)))
                                (and bytes
                                     (decode-darwin-rusage-info bytes)))))))
               (command (and own-p (%darwin-command-line pid)))
               (threads (and own-p include-threads-p
                             (%darwin-thread-rows pid))))
          (list :pid (getf metadata :pid)
                :ppid (getf metadata :ppid)
                :uid (getf metadata :uid)
                :ruid ruid
                :name (getf metadata :name)
                :state (getf metadata :state)
                :kernel-thread-p (getf metadata :kernel-thread-p)
                :start-time-unix-seconds
                (getf metadata :start-time-unix-seconds)
                :priority (getf metadata :priority)
                :nice (getf metadata :nice)
                :cpu-time-ns
                (and task
                     (+ (getf task :user-time-ns)
                        (getf task :system-time-ns)))
                :resident-memory-bytes (getf task :resident-memory-bytes)
                :virtual-memory-bytes (getf task :virtual-memory-bytes)
                :read-bytes (getf usage :read-bytes)
                :write-bytes (getf usage :write-bytes)
                :command command
                :threads threads))))))
  #+darwin
(defun read-darwin-process-rows (on-value on-unavailable
                                 &key include-threads-p)
  (multiple-value-bind (pids failure)
      (handler-case (%darwin-list-pids)
        (error () (values nil :private-api-unavailable)))
    (if failure
        (funcall on-unavailable failure)
        (let ((real-uid (sb-posix:getuid)))
          (funcall on-value
                   (loop for pid in pids
                         for row = (%darwin-try
                                    (lambda ()
                                      (%darwin-process-row
                                       pid real-uid include-threads-p)))
                         when row collect row))))))
  #-darwin
  (defun read-darwin-process-rows (on-value on-unavailable
                                   &key include-threads-p)
    (declare (ignore on-value include-threads-p))
    (funcall on-unavailable :unsupported-platform))
