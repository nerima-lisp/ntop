(in-package #:ntop.test)
(defun %f8a-darwin-test-octets (size)
    (make-array size :element-type '(unsigned-byte 8) :initial-element 0))

  (defun %f8a-darwin-test-write-integer (octets offset width value)
    (loop for byte-index below width
          do (setf (aref octets (+ offset byte-index))
                   (ldb (byte 8 (* byte-index 8)) value)))
    octets)

  (defun %f8a-darwin-test-write-string (octets offset width value)
    (loop for character across value
          for index from offset
          below (+ offset width)
          do (setf (aref octets index) (char-code character)))
    octets)

  (progn
(cl-weave:describe
   "Darwin libproc decoders"
   (cl-weave:it
    "decodes proc_bsdinfo identity, flags, and display fields"
    (let ((octets (%f8a-darwin-test-octets 136)))
      (%f8a-darwin-test-write-integer octets 0 4 1)
      (%f8a-darwin-test-write-integer octets 4 4 3)
      (%f8a-darwin-test-write-integer octets 12 4 4242)
      (%f8a-darwin-test-write-integer octets 16 4 41)
      (%f8a-darwin-test-write-integer octets 20 4 501)
      (%f8a-darwin-test-write-integer octets 28 4 502)
      (%f8a-darwin-test-write-string octets 48 16 "worker")
      (%f8a-darwin-test-write-string octets 64 32 "worker --mode test")
      (%f8a-darwin-test-write-integer octets 116 4 -5)
      (%f8a-darwin-test-write-integer octets 120 8 1700000000)
      (%f8a-darwin-test-write-integer octets 128 4 500000)
      (let ((row (ntop.platform.darwin::decode-darwin-proc-bsdinfo octets)))
        (cl-weave:expect (getf row :flags) :to-be 1)
        (cl-weave:expect (getf row :pid) :to-be 4242)
        (cl-weave:expect (getf row :ppid) :to-be 41)
        (cl-weave:expect (getf row :uid) :to-be 501)
        (cl-weave:expect (getf row :ruid) :to-be 502)
        (cl-weave:expect (getf row :name) :to-equal "worker --mode test")
        (cl-weave:expect (getf row :state) :to-equal "Sleeping")
        (cl-weave:expect (getf row :nice) :to-be -5)
        (cl-weave:expect (getf row :start-time-unix-seconds)
                         :to-be 1700000000.5d0))))

   (cl-weave:it
    "decodes kinfo_proc fallback identity, flags, and state"
    (let ((octets (%f8a-darwin-test-octets 648)))
      (%f8a-darwin-test-write-integer octets 0 8 1700000001)
      (%f8a-darwin-test-write-integer octets 8 8 500000)
      (%f8a-darwin-test-write-integer octets 32 4 #x200)
      (%f8a-darwin-test-write-integer octets 36 1 2)
      (%f8a-darwin-test-write-integer octets 40 4 5252)
      (%f8a-darwin-test-write-integer octets 240 1 7)
      (%f8a-darwin-test-write-integer octets 242 1 -3)
      (%f8a-darwin-test-write-string octets 243 17 "fallback")
      (%f8a-darwin-test-write-integer octets 392 4 1000)
      (%f8a-darwin-test-write-integer octets 420 4 1001)
      (%f8a-darwin-test-write-integer octets 560 4 50)
      (let ((row (ntop.platform.darwin::decode-darwin-kinfo-proc octets)))
        (cl-weave:expect (getf row :pid) :to-be 5252)
        (cl-weave:expect (getf row :ppid) :to-be 50)
        (cl-weave:expect (getf row :uid) :to-be 1001)
        (cl-weave:expect (getf row :ruid) :to-be 1000)
        (cl-weave:expect (getf row :name) :to-equal "fallback")
        (cl-weave:expect (getf row :state) :to-equal "Running")
        (cl-weave:expect (getf row :priority) :to-be 7)
        (cl-weave:expect (getf row :nice) :to-be -3)
        (cl-weave:expect (getf row :kernel-thread-p) :to-be t)
        (cl-weave:expect (getf row :start-time-unix-seconds)
                         :to-be 1700000001.5d0))))

   (cl-weave:it
    "decodes task, rusage, and PID-list buffers"
    (let ((task (%f8a-darwin-test-octets 232))
          (usage (%f8a-darwin-test-octets 160))
          (pids (%f8a-darwin-test-octets 12)))
      (%f8a-darwin-test-write-integer task 136 8 8192)
      (%f8a-darwin-test-write-integer task 144 8 4096)
      (%f8a-darwin-test-write-integer task 152 8 77)
      (%f8a-darwin-test-write-integer task 160 8 23)
      (%f8a-darwin-test-write-integer task 220 4 4)
      (%f8a-darwin-test-write-integer usage 144 8 1234)
      (%f8a-darwin-test-write-integer usage 152 8 5678)
      (%f8a-darwin-test-write-integer pids 0 4 11)
      (%f8a-darwin-test-write-integer pids 4 4 0)
      (%f8a-darwin-test-write-integer pids 8 4 22)
      (let ((task-row
              (ntop.platform.darwin::decode-darwin-taskallinfo task 125 3))
            (without-timebase
              (ntop.platform.darwin::decode-darwin-taskallinfo task)))
        (cl-weave:expect (getf task-row :virtual-memory-bytes) :to-be 8192)
        (cl-weave:expect (getf task-row :resident-memory-bytes) :to-be 4096)
        (cl-weave:expect (getf task-row :cpu-time-ns) :to-be 4166)
        (cl-weave:expect (getf task-row :user-time-ns) :to-be 3208)
        (cl-weave:expect (getf task-row :system-time-ns) :to-be 958)
        (cl-weave:expect (getf task-row :thread-count) :to-be 4)
        (cl-weave:expect (getf without-timebase :cpu-time-ns) :to-be-null)
        (cl-weave:expect
         (getf without-timebase :resident-memory-bytes) :to-be 4096))
      (cl-weave:expect
       (ntop.platform.darwin::decode-darwin-rusage-info usage)
       :to-equal '(:read-bytes 1234 :write-bytes 5678))
      (cl-weave:expect
       (ntop.platform.darwin::decode-darwin-pid-buffer pids)
       :to-equal '(11 22))))

   (cl-weave:it
    "decodes proc_threadinfo with the requested thread id"
    (let ((octets (%f8a-darwin-test-octets 112)))
      (%f8a-darwin-test-write-integer octets 0 8 100)
      (%f8a-darwin-test-write-integer octets 8 8 50)
      (%f8a-darwin-test-write-integer octets 24 4 3)
      (%f8a-darwin-test-write-integer octets 40 4 12)
      (%f8a-darwin-test-write-string octets 48 64 "worker-thread")
      (let ((row
              (ntop.platform.darwin::decode-darwin-proc-threadinfo
               octets 77 125 3))
            (without-timebase
              (ntop.platform.darwin::decode-darwin-proc-threadinfo
               octets 77)))
        (cl-weave:expect (getf row :thread-id) :to-be 77)
        (cl-weave:expect (getf row :name) :to-equal "worker-thread")
        (cl-weave:expect (getf row :cpu-time-ns) :to-be 6250)
        (cl-weave:expect (getf without-timebase :cpu-time-ns) :to-be-null)
        (cl-weave:expect (getf row :state) :to-equal "Waiting")
        (cl-weave:expect (getf row :priority) :to-be 12))))

   (cl-weave:it
    "rejects malformed or truncated kernel buffers"
    (let ((short-bsd (%f8a-darwin-test-octets 135))
          (short-kinfo (%f8a-darwin-test-octets 647))
          (short-thread (%f8a-darwin-test-octets 111))
          (misaligned-pids (%f8a-darwin-test-octets 5)))
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-proc-bsdinfo short-bsd))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-kinfo-proc short-kinfo))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-proc-threadinfo short-thread 1))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-pid-buffer misaligned-pids))
       :to-throw 'error))))

(cl-weave:describe
 "Darwin libproc failure paths"
 (cl-weave:it
  "covers empty names and exact decoder sizes"
  (let ((bsd (%f8a-darwin-test-octets 136)))
    (%f8a-darwin-test-write-string bsd 48 16 "short-name")
    (cl-weave:expect
     (getf (ntop.platform.darwin::decode-darwin-proc-bsdinfo bsd) :name)
     :to-equal "short-name"))
  (dolist (octets (list (%f8a-darwin-test-octets 231)
                        (%f8a-darwin-test-octets 159)))
    (cl-weave:expect
     (lambda ()
       (if (= (length octets) 231)
           (ntop.platform.darwin::decode-darwin-taskallinfo octets)
           (ntop.platform.darwin::decode-darwin-rusage-info octets)))
     :to-throw 'error)))
 #+darwin
 (cl-weave:it
  "bounds native process buffers and catches decoder failures"
  (let ((count 4)
        (rusage-status 0))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%proc-pidinfo)
       (lambda (pid flavor argument sap size)
         (declare (ignore pid flavor argument sap size))
         count))
      ((symbol-function 'ntop.platform.darwin::%proc-pid-rusage)
       (lambda (pid flavor sap)
         (declare (ignore pid flavor sap))
         rusage-status)))
     (cl-weave:expect
      (length (ntop.platform.darwin::%darwin-info 1 2 4))
      :to-be 4)
     (setf count 0)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-info 1 2 4)
      :to-be-null)
     (setf count 2)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-info 1 2 4)
      :to-be-null)
     (cl-weave:expect
      (length (ntop.platform.darwin::%darwin-info 1 2 4 nil))
      :to-be 2)
     (setf count -1)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-info 1 2 4 nil)
      :to-be-null)
     (setf count 5)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-info 1 2 4 nil)
      :to-be-null)
     (setf rusage-status 1)
     (cl-weave:expect (ntop.platform.darwin::%darwin-rusage 1)
                      :to-be-null))
   (cl-weave:with-mocked-functions
    (((symbol-function 'ntop.platform.darwin::%darwin-info)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        (%f8a-darwin-test-octets 4))))
    (cl-weave:expect
     (ntop.platform.darwin::%darwin-info-decoded
      1 2 4
      (lambda (octets)
        (declare (ignore octets))
        (error "mock decoder failure")))
     :to-be-null))
   (cl-weave:with-mocked-functions
    (((symbol-function 'ntop.platform.darwin::%darwin-info)
      (lambda (&rest arguments)
        (declare (ignore arguments))
        nil)))
    (cl-weave:expect
     (ntop.platform.darwin::%darwin-info-decoded
      1 2 4 (lambda (octets) octets))
     :to-be-null))))
 #+darwin
 (cl-weave:it
  "handles PID and thread enumeration limits and unavailable callbacks"
  (let* ((layout (copy-list ntop.platform.darwin::+darwin-process-layout+))
         (ntop.platform.darwin::+darwin-process-layout+ layout)
         (mode :negative)
         (calls 0))
    (setf (getf layout :initial-buffer-size) 4
          (getf layout :maximum-buffer-size) 8)
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%proc-listpids)
       (lambda (type type-info sap size)
         (declare (ignore type type-info sap))
         (incf calls)
         (if (eq mode :negative) -1 size))))
     (multiple-value-bind (pids reason)
         (ntop.platform.darwin::%darwin-list-pids)
       (cl-weave:expect pids :to-be-null)
       (cl-weave:expect reason :to-be :private-api-unavailable))
     (setf mode :full calls 0)
     (multiple-value-bind (pids reason)
         (ntop.platform.darwin::%darwin-list-pids)
       (cl-weave:expect pids :to-be-null)
       (cl-weave:expect reason :to-be :read-failed)
       (cl-weave:expect calls :to-be 2))))
  (let* ((layout (copy-list ntop.platform.darwin::+darwin-process-layout+))
         (ntop.platform.darwin::+darwin-process-layout+ layout)
         (mode :empty))
    (setf (getf layout :initial-buffer-size) 8
          (getf layout :maximum-buffer-size) 16)
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-info)
       (lambda (pid flavor size exact-p)
        (declare (ignore pid flavor size exact-p))
        (when (eq mode :growth)
         (%f8a-darwin-test-write-integer (%f8a-darwin-test-octets 8) 0 8 77)))))
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-thread-ids 1)
      :to-be-null)
     (setf mode :growth)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-thread-ids 1)
      :to-equal '(77))
     (setf (getf layout :maximum-buffer-size) 8)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-thread-ids 1)
      :to-be-null)))
  (let ((reason :unset)
        (mode :raise))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-list-pids)
       (lambda ()
         (if (eq mode :raise)
             (error "mock proc_listpids failure")
             (values nil :read-failed)))))
     (ntop.platform.darwin:read-darwin-process-rows
      (lambda (rows) (declare (ignore rows)))
      (lambda (value) (setf reason value)))
     (cl-weave:expect reason :to-be :private-api-unavailable)
     (setf mode :failure reason :unset)
     (ntop.platform.darwin:read-darwin-process-rows
      (lambda (rows) (declare (ignore rows)))
      (lambda (value) (setf reason value)))
     (cl-weave:expect reason :to-be :read-failed)))
  (let ((metadata nil))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-metadata)
       (lambda (pid) (declare (ignore pid)) metadata)))
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-process-row 1 501 nil)
      :to-be-null)
     (setf metadata '(:pid 1 :ruid :unknown))
     (cl-weave:expect
      (getf (ntop.platform.darwin::%darwin-process-row 1 501 nil) :ruid)
      :to-be :unknown))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-info-decoded)
       (lambda (&rest arguments)
         (declare (ignore arguments))
         nil))
      ((symbol-function 'ntop.platform.darwin::%darwin-sysctl)
       (lambda (mib)
         (declare (ignore mib))
         (%f8a-darwin-test-octets 1))))
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-metadata 1)
      :to-be-null))))
)



(cl-weave:describe
 "Darwin executable names and process rows"
 (cl-weave:it
  "extracts executable basenames from KERN_PROCARGS2"
  (let* ((path "/Applications/Ntop/bin/worker")
         (octets (%f8a-darwin-test-octets (+ 5 (length path)))))
    (%f8a-darwin-test-write-integer octets 0 4 2)
    (%f8a-darwin-test-write-string octets 4 (length path) path)
    (cl-weave:expect
     (ntop.platform.darwin::%darwin-process-path-basename
      (ntop.platform.darwin::%darwin-procargs2-executable-path octets))
     :to-equal "worker")))
 (cl-weave:it
  "rejects malformed KERN_PROCARGS2 headers"
  (let ((octets (%f8a-darwin-test-octets 8)))
    (%f8a-darwin-test-write-integer octets 0 4 -1)
    (%f8a-darwin-test-write-string octets 4 4 "bad")
    (cl-weave:expect
     (ntop.platform.darwin::%darwin-procargs2-executable-path octets)
     :to-be-null)))
 #+darwin
 (cl-weave:it
  "falls back from KERN_PROCARGS2 to proc_pidpath and metadata"
  (let ((procargs-name nil)
        (pidpath-name "from-pidpath")
        (procargs-fails-p nil)
        (pidpath-fails-p nil))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-procargs2-name)
       (lambda (pid)
         (declare (ignore pid))
         (if procargs-fails-p (error "mock sysctl failure") procargs-name)))
      ((symbol-function 'ntop.platform.darwin::%darwin-pidpath-name)
       (lambda (pid)
         (declare (ignore pid))
         (if pidpath-fails-p (error "mock pidpath failure") pidpath-name))))
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-process-executable-name 1 "metadata")
      :to-equal "from-pidpath")
     (setf procargs-fails-p t)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-process-executable-name 1 "metadata")
      :to-equal "from-pidpath")
     (setf procargs-fails-p nil procargs-name "from-procargs")
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-process-executable-name 1 "metadata")
      :to-equal "from-procargs")
     (setf procargs-name nil pidpath-name nil pidpath-fails-p t)
     (cl-weave:expect
      (ntop.platform.darwin::%darwin-process-executable-name 1 "metadata")
      :to-equal "metadata"))))
 #+darwin
 (cl-weave:it
  "reads the Mach timebase once and keeps rows when it is unavailable"
  (let ((timebase-calls 0)
        (row-timebase nil)
        (rows :unset)
        (unavailable :unset)
        (timebase-mode :success))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%darwin-list-pids)
       (lambda () (values '(17) nil)))
      ((symbol-function 'ntop.platform.darwin::mach-timebase-info)
       (lambda (on-value on-error)
         (incf timebase-calls)
         (if (eq timebase-mode :success)
             (funcall on-value 125 3)
             (funcall on-error :mach-error))))
      ((symbol-function 'ntop.platform.darwin::%darwin-process-row)
       (lambda (pid uid include-threads-p numerator denominator)
         (declare (ignore uid include-threads-p))
         (setf row-timebase (list numerator denominator))
         (list :pid pid :cpu-time-ns (when numerator 5)
               :resident-memory-bytes 4096))))
     (ntop.platform.darwin:read-darwin-process-rows
      (lambda (values) (setf rows values))
      (lambda (reason) (setf unavailable reason)))
     (cl-weave:expect timebase-calls :to-be 1)
     (cl-weave:expect row-timebase :to-equal '(125 3))
     (cl-weave:expect (getf (first rows) :cpu-time-ns) :to-be 5)
     (cl-weave:expect unavailable :to-be :unset)
     (setf timebase-mode :failure rows :unset row-timebase :unset)
     (ntop.platform.darwin:read-darwin-process-rows
      (lambda (values) (setf rows values))
      (lambda (reason) (setf unavailable reason)))
     (cl-weave:expect timebase-calls :to-be 2)
     (cl-weave:expect row-timebase :to-equal '(nil nil))
     (cl-weave:expect (getf (first rows) :cpu-time-ns) :to-be-null)
     (cl-weave:expect
      (getf (first rows) :resident-memory-bytes) :to-be 4096)
     (cl-weave:expect unavailable :to-be :unset))))
 #+darwin
 (cl-weave:it
  "returns resident memory for the current user process"
  (let ((rows nil)
        (unavailable nil)
        (pid (sb-posix:getpid))
        (uid (sb-posix:getuid)))
    (ntop.platform.darwin:read-darwin-process-rows
     (lambda (values) (setf rows values))
     (lambda (reason) (setf unavailable reason)))
    (let ((row (find pid rows :key (lambda (value) (getf value :pid)))))
      (cl-weave:expect unavailable :to-be-null)
      (cl-weave:expect row :to-be-truthy)
      (cl-weave:expect (getf row :ruid) :to-be uid)
      (cl-weave:expect
       (integerp (getf row :resident-memory-bytes)) :to-be t)
      (cl-weave:expect (stringp (getf row :name)) :to-be t)
      (cl-weave:expect (plusp (length (getf row :name))) :to-be t)))))
)
