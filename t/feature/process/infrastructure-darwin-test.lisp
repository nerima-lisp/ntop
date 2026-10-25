(in-package #:ntop.test)
(progn
(progn
  (defclass %f8a-darwin-clock () ())
  (defmethod ntop.shared.domain:read-monotonic-time
      ((clock %f8a-darwin-clock) on-time on-unavailable)
    (declare (ignore clock))
    (handler-case
        (multiple-value-bind (seconds nanoseconds)
            (sb-unix:clock-gettime sb-unix:clock-monotonic)
          (funcall on-time (+ seconds (/ nanoseconds 1000000000d0))))
      (error ()
        (funcall on-unavailable :read-failed))))
  (labels
    ((%f8a-darwin-ps-output (arguments)
       (let ((result
               (host-kit:run-program
                "ps" arguments
                :timeout 5
                :environment '("LC_ALL=C")
                :max-output-characters 1048576)))
         (unless (and (zerop (host-kit:process-result-exit-code result))
                      (not (host-kit:process-result-stdout-truncated-p result))
                      (not (host-kit:process-result-stderr-truncated-p result)))
           (error "The bounded ps reference command failed."))
         (host-kit:process-result-stdout result)))
     (%f8a-darwin-fields (text)
       (let ((fields nil)
             (start nil))
         (loop for index from 0 to (length text)
               for separator =
                 (or (= index (length text))
                     (find (char text index)
                           '(#\Space #\Tab #\Return #\Newline)))
               do (cond
                    ((and separator start)
                     (push (subseq text start index) fields)
                     (setf start nil))
                    ((and (not separator) (null start))
                     (setf start index))))
         (nreverse fields)))
     (%f8a-darwin-process-pids ()
       (with-input-from-string
           (stream (%f8a-darwin-ps-output '("-ax" "-o" "pid=")))
         (remove-duplicates
          (loop for line = (read-line stream nil nil)
                while line
                for pid = (ignore-errors
                            (parse-integer
                             (string-trim
                              '(#\Space #\Tab #\Return #\Newline)
                              line)))
                when (and pid (plusp pid))
                  collect pid)
          :test #'eql)))
     (%f8a-darwin-windowserver-pid ()
       (with-input-from-string
           (stream (%f8a-darwin-ps-output '("-ax" "-o" "pid=,comm=")))
         (loop for line = (read-line stream nil nil)
               while line
               for fields = (%f8a-darwin-fields line)
               when (and (>= (length fields) 2)
                         (search "WindowServer"
                                 (format nil "~{~A~^ ~}" (rest fields))))
                 return (parse-integer (first fields))
               finally (error "ps did not report WindowServer."))))
     (%f8a-darwin-ps-start-time (fields)
       (unless (= (length fields) 5)
         (error "Unexpected ps lstart fields: ~S." fields))
       (let* ((month
                (position (second fields)
                          '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                            "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                          :test #'string=))
              (day (parse-integer (third fields)))
              (time-fields
                (loop with start = 0
                      for end = (position #\: (fourth fields) :start start)
                      collect (parse-integer
                               (fourth fields) :start start :end end)
                      while end
                      do (setf start (1+ end))))
              (year (parse-integer (fifth fields))))
         (unless (and month (= (length time-fields) 3))
           (error "Unexpected ps lstart value: ~S." fields))
         (- (encode-universal-time (third time-fields)
                                   (second time-fields)
                                   (first time-fields)
                                   day (1+ month) year)
            2208988800)))
     (%f8a-darwin-ps-user-start (pid)
       (with-input-from-string
           (stream (%f8a-darwin-ps-output
                    (list "-o" "ruser=,lstart=" "-p"
                          (write-to-string pid))))
         (let* ((line (read-line stream nil nil))
                (fields (and line (%f8a-darwin-fields line))))
           (unless (and fields (= (length fields) 6)
                        (null (read-line stream nil nil)))
             (error "ps did not return one user/lstart row for PID ~D." pid))
           (list (first fields)
                 (%f8a-darwin-ps-start-time (rest fields))))))
     (%f8a-darwin-sample (&optional threads &key pause-pid)
       (let* ((config
                (ntop.shared.application:make-application-config
                 :process-get-threads-p threads))
              (collector
                (ntop.process.infrastructure::make-process-collector
                 (make-instance '%f8a-darwin-clock)
                 config))
              (sample :unset)
              (reason :unset))
         (flet ((writer-stopped-p ()
                  (let ((record
                          (and (eq reason :unset)
                               (%f8a-darwin-record sample pause-pid))))
                    (and record
                         (equal "Stopped"
                                (ntop.process.domain:process-record-state
                                 record))))))
           (when pause-pid
             (sb-posix:kill pause-pid sb-posix:sigstop))
           (loop repeat (if pause-pid 5 1)
                 do (setf sample :unset
                          reason :unset)
                    (ntop.shared.application:collect-process-sample
                     collector
                     (lambda (value) (setf sample value))
                     (lambda (value) (setf reason value)))
                    (when (or (null pause-pid)
                              (not (eq reason :unset))
                              (writer-stopped-p))
                      (return))
                    (sleep 0.01))
           (when (and pause-pid
                      (eq reason :unset)
                      (not (writer-stopped-p)))
             (error "The child writer did not stop for sampling."))
           (values sample reason))))
     (%f8a-darwin-record (sample pid)
       (find pid (ntop.process.domain:process-sample-processes sample)
             :key #'ntop.process.domain:process-record-pid
             :test #'eql))
     (%f8a-darwin-start-writer (pathname)
       (let ((form
               (format nil
                       (concatenate 'string
                                    "(let* ((stream (open ~S :direction :output :if-exists "
                                    ":append :if-does-not-exist :create :element-type "
                                    "'(unsigned-byte 8))) (chunk (make-array 65536 "
                                    ":element-type '(unsigned-byte 8)))) "
                                    "(dotimes (index (length chunk)) "
                                    "(setf (aref chunk index) (random 256))) "
                                    "(unwind-protect (loop do "
                                    "(write-sequence chunk stream) "
                                    "(finish-output stream) (sb-posix:fsync "
                                    "(sb-sys:fd-stream-fd stream)) (sleep (/ 1 16))) "
                                    "(close stream)))")
                       (namestring pathname))))
         (sb-ext:run-program
          (namestring sb-ext:*runtime-pathname*)
          (list "--noinform" "--non-interactive"
                "--eval" "(require :sb-posix)"
                "--eval" form)
          :search nil :wait nil :input nil :output nil :error nil)))
     (%f8a-darwin-stop-child (child)
       (unwind-protect
            (when (sb-ext:process-alive-p child)
              (sb-posix:kill (sb-ext:process-pid child) sb-posix:sigkill))
         (sb-ext:process-wait child))
       (when (sb-ext:process-alive-p child)
         (error "The child writer remained alive after process-wait."))
       t))
  (cl-weave:describe
   "Darwin process infrastructure"
   #+darwin
   (cl-weave:it
    "returns a sampled process list and Darwin signal catalog"
    (multiple-value-bind (sample reason)
        (%f8a-darwin-sample)
      (cl-weave:expect (not (eq sample :unset)) :to-be t)
      (cl-weave:expect reason :to-be :unset)
      (unless (eq sample :unset)
        (let ((processes (ntop.process.domain:process-sample-processes sample))
              (signals (ntop.process.domain:process-sample-signals sample)))
          (cl-weave:expect (plusp (length processes)) :to-be t)
          (cl-weave:expect
           (let* ((pid (sb-posix:getpid))
                  (record (%f8a-darwin-record sample pid)))
             (and record
                  (eq record
                      (gethash
                       pid
                       (ntop.process.domain::process-sample-pid-index sample)))))
           :to-be t)
          (cl-weave:expect (length signals) :to-be 31)
          (cl-weave:expect
           (ntop.process.domain:process-signal-operating-system (first signals))
           :to-equal "Darwin")
          (cl-weave:expect
           (ntop.process.domain:process-signal-name (first signals))
           :to-equal "SIGHUP")))))
   #+darwin
   (cl-weave:it
    "reads details for the current process threads when requested"
    (let* ((pid (sb-posix:getpid))
           (row (ntop.platform.darwin::%darwin-process-row
                 pid (sb-posix:getuid) t))
           (threads (getf row :threads))
           (record
             (ntop.process.infrastructure::%darwin-process-record
              row (make-hash-table :test #'eql)))
           (record-threads
             (ntop.process.domain:process-record-threads record)))
      (cl-weave:expect (not (null threads)) :to-be t)
      (cl-weave:expect
       (and record-threads
            (every
             (lambda (thread)
               (and (integerp
                     (ntop.process.domain:process-thread-thread-id thread))
                    (integerp
                     (ntop.process.domain:process-thread-cpu-time-ns thread))))
             record-threads))
       :to-be t)
      (cl-weave:expect
       (ntop.process.domain:process-record-kernel-thread-p record)
       :to-be nil)))
   #+darwin
   (cl-weave:it
    "overlaps ps process IDs and matches WindowServer real user and start time"
    (let* ((windowserver-pid (%f8a-darwin-windowserver-pid))
           (before-pids (%f8a-darwin-process-pids))
           (before-identity (%f8a-darwin-ps-user-start windowserver-pid)))
      (multiple-value-bind (sample reason)
          (%f8a-darwin-sample)
        (let* ((after-pids (%f8a-darwin-process-pids))
               (after-identity
                 (%f8a-darwin-ps-user-start windowserver-pid))
               (record (%f8a-darwin-record sample windowserver-pid))
               (stable-pids (intersection before-pids after-pids :test #'eql))
               (sample-pids
                 (mapcar #'ntop.process.domain:process-record-pid
                         (ntop.process.domain:process-sample-processes sample)))
               (unix-now (- (get-universal-time) 2208988800))
               (row
                 (and record
                      (ntop.process.domain:process-row-from-samples
                       nil sample windowserver-pid
                       :memory-total-bytes 1
                       :current-time-unix-seconds unix-now)))
               (expected-elapsed
                 (max 0 (- unix-now (second after-identity)))))
          (cl-weave:expect reason :to-be :unset)
          (cl-weave:expect (plusp (length stable-pids)) :to-be t)
          (cl-weave:expect
           (subsetp stable-pids sample-pids :test #'eql)
           :to-be t)
          (cl-weave:expect record :to-be-truthy)
          (cl-weave:expect
           (ntop.process.domain:process-record-user-name record)
           :to-equal (first before-identity))
          (cl-weave:expect
           (ntop.process.domain:process-record-user-name record)
           :to-equal (first after-identity))
          (cl-weave:expect
           (< (abs (- (ntop.process.domain:process-record-start-time-unix-seconds
                       record)
                      (second before-identity)))
              1.1d0)
           :to-be t)
          (cl-weave:expect
           (< (abs (- (ntop.process.domain:process-record-start-time-unix-seconds
                       record)
                      (second after-identity)))
              1.1d0)
           :to-be t)
          (cl-weave:expect row :to-be-truthy)
          (cl-weave:expect
           (< (abs (- (ntop.process.domain:process-row-elapsed-seconds row)
                      expected-elapsed))
              1.1d0)
           :to-be t)))))
   #+darwin
   (cl-weave:it
    "keeps root-owned memory unavailable and its Mem% absent"
    (multiple-value-bind (sample reason)
        (%f8a-darwin-sample)
      (let* ((records (ntop.process.domain:process-sample-processes sample))
             (record
               (find 0 records
                     :key #'ntop.process.domain:process-record-uid
                     :test #'eql)))
        (cl-weave:expect (/= (sb-posix:getuid) 0) :to-be t)
        (cl-weave:expect reason :to-be :unset)
        (cl-weave:expect record :to-be-truthy)
        (cl-weave:expect
         (ntop.process.domain:process-record-resident-memory-bytes record)
         :to-be-null)
        (let ((row
                (ntop.process.domain:process-row-from-samples
                 nil sample (ntop.process.domain:process-record-pid record)
                 :memory-total-bytes 1000000000000
                 :current-time-unix-seconds (- (get-universal-time) 2208988800))))
          (cl-weave:expect
           (ntop.process.domain:process-row-memory-percent row)
           :to-be-null)))))
   #+darwin
   (cl-weave:it
    "matches sampled write deltas to file growth and elapsed time"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let* ((pathname (merge-pathnames "process-writer.bin" directory))
              (child (%f8a-darwin-start-writer pathname))
              (pid (sb-ext:process-pid child)))
         (unwind-protect
             (progn
               (sleep 0.5)
               (multiple-value-bind (before before-reason)
                   (%f8a-darwin-sample nil :pause-pid pid)
                 (let* ((before-record (%f8a-darwin-record before pid))
                        (before-file-bytes
                          (with-open-file (stream pathname
                                                  :direction :input
                                                  :element-type '(unsigned-byte 8))
                            (file-length stream)))
                        (before-write-bytes
                          (and before-record
                               (ntop.process.domain:process-record-write-bytes
                                before-record))))
                   (cl-weave:expect before-reason :to-be :unset)
                   (cl-weave:expect before-record :to-be-truthy)
                   (cl-weave:expect
                    (equal "Stopped"
                           (ntop.process.domain:process-record-state before-record))
                    :to-be t)
                   (cl-weave:expect before-write-bytes :to-be-truthy)
                   (sb-posix:kill pid sb-posix:sigcont)
                   (sleep 4)
                   (multiple-value-bind (after after-reason)
                       (%f8a-darwin-sample nil :pause-pid pid)
                     (let* ((after-record (%f8a-darwin-record after pid))
                            (after-file-bytes
                              (with-open-file (stream pathname
                                                      :direction :input
                                                      :element-type '(unsigned-byte 8))
                                (file-length stream)))
                            (after-write-bytes
                              (and after-record
                                   (ntop.process.domain:process-record-write-bytes
                                    after-record)))
                            (file-byte-delta
                              (- after-file-bytes before-file-bytes))
                            (write-byte-delta
                              (and before-write-bytes after-write-bytes
                                   (- after-write-bytes before-write-bytes)))
                            (sample-elapsed-seconds
                              (- (ntop.shared.domain:sample-record-sampled-at after)
                                 (ntop.shared.domain:sample-record-sampled-at before)))
                            (expected-write-rate
                              (and write-byte-delta
                                   (plusp sample-elapsed-seconds)
                                   (/ write-byte-delta sample-elapsed-seconds)))
                            (write-rate
                              (ntop.process.domain:process-write-bytes-per-second
                               before after pid))
                            ;; Allow one page per cumulative sample for page/block rounding.
                            ;; On 16 KiB pages, the two-page overcount allowance is 32 KiB.
                            (rounding-allowance (* 2 (sb-posix:getpagesize))))
                       (cl-weave:expect after-reason :to-be :unset)
                       (cl-weave:expect after-record :to-be-truthy)
                       (cl-weave:expect
                        (equal "Stopped"
                               (ntop.process.domain:process-record-state after-record))
                        :to-be t)
                       (cl-weave:expect (plusp file-byte-delta) :to-be t)
                       (cl-weave:expect
                        (and write-byte-delta
                             (<= file-byte-delta
                                 write-byte-delta
                                 (+ file-byte-delta rounding-allowance)))
                        :to-be t)
                       (cl-weave:expect
                        (and expected-write-rate write-rate
                             (= write-rate expected-write-rate))
                        :to-be t))))))
           (%f8a-darwin-stop-child child))))
     :prefix "ntop-f8a-process-"))

)))

(cl-weave:describe
 "Darwin process fallback error paths"
 (cl-weave:it
  "filters command capture to current-user rows and keeps ps fallbacks"
  (multiple-value-bind (values reason)
      (ntop.process.infrastructure::%darwin-ps-cpu-values nil)
    (cl-weave:expect (hash-table-p values) :to-be t)
    (cl-weave:expect (hash-table-count values) :to-be 0)
    (cl-weave:expect reason :to-be-null))
  (cl-weave:expect
   (nth-value 1
              (ntop.process.infrastructure::%darwin-space-fields
               "41 2.5 7 /usr/bin/worker --flag value"))
   :to-be 9)
  (let* ((own-uid (sb-posix:getuid))
         (other-uid (if (zerop own-uid) 1 0))
         (rows (list (list :pid 41 :ruid own-uid) (list :pid 42 :ruid other-uid)
                     (list :pid 43 :ruid other-uid) (list :pid 44 :ruid other-uid)))
         (stdout
           (format nil "~{~A~%~}" '("41 2.5 7 /usr/bin/worker --flag value"
                                    "42 nope 6 /bad" "43 4.0 invalid /bad"
                                    "44 8.5 10 /usr/bin/other --flag")))
         (success (host-kit::make-process-result :exit-code 0 :stdout stdout))
         (exit-failure (host-kit::make-process-result :exit-code 1))
         (timeout-failure (host-kit::make-process-result :exit-code 0 :timed-out-p t))
         (stdout-failure (host-kit::make-process-result :exit-code 0 :stdout-truncated-p t))
         (stderr-failure (host-kit::make-process-result :exit-code 0 :stderr-truncated-p t))
         (mode :success))
    (cl-weave:with-mocked-functions
     (((symbol-function 'host-kit:run-program)
       (lambda (&rest arguments)
         (declare (ignore arguments))
         (case mode
           (:raise (error "mock ps launch failure"))
           (:exit exit-failure)
           (:timeout timeout-failure)
           (:stdout-truncated stdout-failure)
           (:stderr-truncated stderr-failure)
           (otherwise success)))))
     (multiple-value-bind (values reason)
         (ntop.process.infrastructure::%darwin-ps-cpu-values rows)
       (cl-weave:expect reason :to-be-null)
       (cl-weave:expect
        (ntop.process.domain:process-record-command
         (ntop.process.infrastructure::%darwin-process-record '(:pid 41) values))
        :to-equal "/usr/bin/worker --flag value")
       (cl-weave:expect (gethash 42 values) :to-be-null)
       (cl-weave:expect (getf (gethash 43 values) :cpu) :to-be 4d0)
       (cl-weave:expect (getf (gethash 43 values) :priority) :to-be-null)
       (let ((record
               (ntop.process.infrastructure::%darwin-process-record
                '(:pid 44) values)))
         (cl-weave:expect
          (list (ntop.process.domain:process-record-command record)
                (ntop.process.domain:process-record-priority record)
                (ntop.process.domain:process-record-ps-fallback-cpu-percent record))
          :to-equal '(nil 10 8.5d0))))
     (dolist (bad-mode
              '(:exit :timeout :stdout-truncated :stderr-truncated :raise))
       (setf mode bad-mode)
       (multiple-value-bind (values reason)
           (ntop.process.infrastructure::%darwin-ps-cpu-values rows)
         (cl-weave:expect (and (null values) (eq reason :read-failed)) :to-be t))))))
 (cl-weave:it
  "uses a numeric user name when lookup misses or errors"
  (let ((fail-lookup nil))
    (cl-weave:with-mocked-functions
     (((symbol-function 'sb-posix:getpwuid)
       (lambda (uid)
         (declare (ignore uid))
         (when fail-lookup
           (error "mock passwd lookup failure"))
         nil)))
     (cl-weave:expect
      (ntop.process.infrastructure::%darwin-user-name 4242)
      :to-equal "4242")
     (setf fail-lookup t)
     (cl-weave:expect
      (ntop.process.infrastructure::%darwin-user-name 4242)
      :to-equal "4242")
     (cl-weave:expect
      (ntop.process.infrastructure::%darwin-user-name "4242")
      :to-be-null))))
 (cl-weave:it
 "omits command output from ps when command display is disabled"
 (let* ((rows (list (list :pid 41 :ruid (sb-posix:getuid))))
        (stdout (format nil "41 2.5 7~%"))
        (success (host-kit::make-process-result :exit-code 0 :stdout stdout)))
   (cl-weave:with-mocked-functions
    (((symbol-function (quote host-kit:run-program))
      (lambda (&rest arguments)
        (cl-weave:expect (third (second arguments))
         :to-equal "pid=,pcpu=,pri=")
        success)))
    (multiple-value-bind (values reason)
        (ntop.process.infrastructure::%darwin-ps-cpu-values rows nil)
      (cl-weave:expect reason :to-be-null)
      (cl-weave:expect (getf (gethash 41 values) :cpu) :to-be 2.5d0)
      (cl-weave:expect (getf (gethash 41 values) :priority) :to-be 7)
      (cl-weave:expect (getf (gethash 41 values) :command) :to-be-null)))))
)

)

