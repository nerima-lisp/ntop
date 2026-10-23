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
    "decodes task, rusage, PID-list, and procargs buffers"
    (let ((task (%f8a-darwin-test-octets 232))
          (usage (%f8a-darwin-test-octets 160))
          (pids (%f8a-darwin-test-octets 12))
          (args (%f8a-darwin-test-octets 64)))
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
      (%f8a-darwin-test-write-integer args 0 4 3)
      (%f8a-darwin-test-write-string args 4 15 "/usr/bin/worker")
      (%f8a-darwin-test-write-string args 20 15 "/usr/bin/worker")
      (%f8a-darwin-test-write-string args 36 7 "--label")
      (%f8a-darwin-test-write-string args 44 9 "two words")
      (let ((task-row (ntop.platform.darwin::decode-darwin-taskallinfo task)))
        (cl-weave:expect (getf task-row :virtual-memory-bytes) :to-be 8192)
        (cl-weave:expect (getf task-row :resident-memory-bytes) :to-be 4096)
        (cl-weave:expect (getf task-row :user-time-ns) :to-be 77)
        (cl-weave:expect (getf task-row :system-time-ns) :to-be 23)
        (cl-weave:expect (getf task-row :thread-count) :to-be 4))
      (cl-weave:expect
       (ntop.platform.darwin::decode-darwin-rusage-info usage)
       :to-equal '(:read-bytes 1234 :write-bytes 5678))
      (cl-weave:expect
       (ntop.platform.darwin::decode-darwin-pid-buffer pids)
       :to-equal '(11 22))
      (cl-weave:expect
       (ntop.platform.darwin::decode-darwin-procargs2 args)
       :to-equal "/usr/bin/worker --label two words")))

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
               octets 77)))
        (cl-weave:expect (getf row :thread-id) :to-be 77)
        (cl-weave:expect (getf row :name) :to-equal "worker-thread")
        (cl-weave:expect (getf row :cpu-time-ns) :to-be 150)
        (cl-weave:expect (getf row :state) :to-equal "Waiting")
        (cl-weave:expect (getf row :priority) :to-be 12))))

   (cl-weave:it
    "rejects malformed or truncated kernel buffers"
    (let ((short-bsd (%f8a-darwin-test-octets 135))
          (short-kinfo (%f8a-darwin-test-octets 647))
          (short-thread (%f8a-darwin-test-octets 111))
          (misaligned-pids (%f8a-darwin-test-octets 5))
          (negative-argc (%f8a-darwin-test-octets 8))
          (truncated-args (%f8a-darwin-test-octets 9)))
      (%f8a-darwin-test-write-integer negative-argc 0 4 -1)
      (%f8a-darwin-test-write-integer truncated-args 0 4 2)
      (%f8a-darwin-test-write-string truncated-args 4 1 "x")
      (%f8a-darwin-test-write-string truncated-args 6 1 "a")
      (%f8a-darwin-test-write-string truncated-args 8 1 "b")
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
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-procargs2 negative-argc))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::decode-darwin-procargs2 truncated-args))
       :to-throw 'error))))