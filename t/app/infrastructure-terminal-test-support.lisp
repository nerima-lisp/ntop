(in-package #:ntop.test)

(defparameter +terminal-test-timeout-seconds+
  20d0)

(defparameter +terminal-test-output-limit+
  (* 1024 1024))

(defparameter +terminal-test-shell-script+
  (concatenate 'string
               "app_pid=
trap 'if [ -n \"$app_pid\" ]; "
               "then kill -TERM \"$app_pid\" "
               "2>/dev/null; sleep 0.1; kill -KILL "
               "\"$app_pid\" 2>/dev/null; fi; exit 1' "
               "INT TERM HUP
IFS= read -r start
printf "
               "'__NTOP_BEFORE_BEGIN__\\n'
stty -a
"
               "printf '__NTOP_BEFORE_END__\\n'
\"$@\" "
               "<&0 &
app_pid=$!
printf "
               "'__NTOP_APP_PID__%s__\\n' \"$app_pid\"
"
               "wait \"$app_pid\"
app_status=$?
app_pid=
"
               "printf '__NTOP_APP_STATUS__%s__\\n' "
               "\"$app_status\"
IFS= read -r release
"
               "printf '__NTOP_AFTER_BEGIN__\\n'
"
               "stty -a
printf "
               "'__NTOP_AFTER_END__\\n'"))

(defun %terminal-test-deadline ()
  (+ (get-internal-real-time)
     (ceiling
      (* +terminal-test-timeout-seconds+ internal-time-units-per-second))))

(defun %terminal-test-remaining-seconds (deadline)
  (/ (float (- deadline (get-internal-real-time)) 1d0)
     internal-time-units-per-second))

(progn
  (defun %terminal-test-coverage-fasl-directory ()
    (uiop:ensure-directory-pathname
     (format nil "/tmp/ntop-wp12a/parent-~D/" (sb-posix:getpid))))
  (defun %terminal-test-prepare-source-fasls ()
    (require :sb-cover)
    (proclaim (list (quote optimize)
                    (list (find-symbol "STORE-COVERAGE-DATA" "SB-COVER") 3)))
    (let* ((directory (%terminal-test-coverage-fasl-directory))
           (sources
            (list "src/app/application-sampler.lisp"
                  "src/app/application-loop.lisp"
                  "src/app/infrastructure-clock.lisp"
                  "src/app/infrastructure-collection.lisp"
                  "src/app/infrastructure-terminal.lisp"))
           (outputs
            (mapcar
             (lambda (source)
               (merge-pathnames
                (format nil "~A.fasl" (pathname-name (pathname source)))
                directory))
             sources)))
      (unless (every (function probe-file) outputs)
        (dolist (source sources)
          (let ((output
                 (merge-pathnames
                  (format nil "~A.fasl" (pathname-name (pathname source)))
                  directory)))
            (ensure-directories-exist output)
            (multiple-value-bind (fasl warnings failure-p)
                (compile-file source
                              :output-file
                              output
                              :verbose
                              nil
                              :print
                              nil)
              (declare (ignore fasl warnings))
              (when failure-p
                (error "Unable to compile terminal test source ~A." source))))))
      directory))
  (defun %terminal-test-environment (coverage-directory)
    (let* ((prefixes (list "TERM=" "NTOP_TERMINAL_TEST_FASL_DIRECTORY="))
           (environment
            (remove-if
             (lambda (entry)
               (some
                (lambda (prefix)
                  (and (>= (length entry) (length prefix))
                       (string= prefix entry :end2 (length prefix))))
                prefixes))
             (sb-ext:posix-environ))))
      (list* "TERM=xterm-256color"
             (format nil
                     "NTOP_TERMINAL_TEST_FASL_DIRECTORY=~A"
                     (namestring coverage-directory))
             environment))))

(defun %terminal-test-asd-path ()
  (namestring
   (truename
    (merge-pathnames "ntop.asd"
                     (asdf:component-pathname (asdf:find-system "ntop"))))))

(defun %terminal-test-callback-source (kind signal)
  (case kind
    (:normal
     "(lambda (poll-terminal-event)
          (declare (ignore poll-terminal-event))
          (write-line \"__NTOP_SESSION_READY__\")
          (finish-output))")
    (:error
     "(lambda (poll-terminal-event)
          (declare (ignore poll-terminal-event))
          (write-line \"__NTOP_SESSION_READY__\")
          (finish-output)
          (error \"deliberate terminal restoration test error\"))")
    (:signal
     (format nil
             "(lambda (poll-terminal-event)
                  (write-line \"__NTOP_SESSION_READY__\")
                  (finish-output)
                  (unless
                      (loop repeat 500
                            for event = (funcall poll-terminal-event)
                            when (and (consp event)
                                      (eq (first event) :terminate)
                                      (= (second event) ~D))
                              return t
                            do (sleep 0.01d0))
                    (error \"terminal signal was not observed\"))
                  (write-line \"__NTOP_SIGNAL_SEEN__\")
                  (finish-output)
                  (sb-cover:save-coverage-in-file
                   (merge-pathnames
                    (format nil \"pty-coverage-~~D.coverage\"
                            (sb-posix:getpid))
                    (uiop:ensure-directory-pathname
                     (uiop:getenv \"NTOP_TERMINAL_TEST_FASL_DIRECTORY\")))))"
             signal))
    (:resize
     "(lambda (poll-terminal-event)
          (write-line \"__NTOP_SESSION_READY__\")
          (finish-output)
          (unless
              (loop repeat 500
                    for event = (funcall poll-terminal-event)
                    when (eq event :resize)
                      return t
                    do (sleep 0.01d0))
            (error \"terminal resize was not observed\"))
          (multiple-value-bind (columns rows)
              (cl-tty-kit:terminal-size 0)
            (unless (and (eql columns 100) (eql rows 30))
              (error \"PTY size change was not visible to the child.\")))
          (write-line \"__NTOP_RESIZE_SEEN__\")
          (finish-output))")
    (otherwise (error "Unknown terminal test case: ~S" kind))))

(defun %terminal-test-session-form (kind signal)
  (format nil
          "(unwind-protect
                 (ntop.app.infrastructure:call-with-terminal-session ~A
                   :output-stream *standard-output*
                   :fd 0)
               (sb-cover:save-coverage-in-file
                (merge-pathnames
                 (format nil \"pty-coverage-~~D.coverage\"
                         (sb-posix:getpid))
                 (uiop:ensure-directory-pathname
                  (uiop:getenv \"NTOP_TERMINAL_TEST_FASL_DIRECTORY\")))))"
          (%terminal-test-callback-source kind signal)))

(defun %terminal-test-child-arguments (kind signal)
  (append
   (list "-c"
         +terminal-test-shell-script+
         "ntop-terminal-test"
         (or
          (and sb-ext:*runtime-pathname* (namestring sb-ext:*runtime-pathname*))
          "sbcl")
         "--noinform"
         "--no-userinit"
         "--no-sysinit"
         "--disable-debugger"
         "--non-interactive"
         "--eval"
         "(require :asdf)"
         "--eval"
         (concatenate 'string
                      "(progn (require :sb-cover) (eval "
                      "(read-from-string \"(proclaim (quote "
                      "(optimize (sb-cover:store-coverage-data "
                      "3))))\")))")
         "--eval"
         (format nil
                 (concatenate 'string
                              "(with-open-file (stream #P~S) (read "
                              "stream nil nil) (let* ((form (read "
                              "stream nil nil)) (pathname-index "
                              "(position :pathname form)) "
                              "(test-op-index (position :in-order-to "
                              "form))) (setf (nth (1+ pathname-index) "
                              "form) (uiop:pathname-directory-pathname "
                              "#P~S)) (when test-op-index (setf form "
                              "(append (subseq form 0 test-op-index) "
                              "(nthcdr (+ test-op-index 2) form)))) "
                              "(eval form)))")
                 (%terminal-test-asd-path)
                 (%terminal-test-asd-path))
         "--eval"
         (concatenate 'string
                      "(let* ((system (asdf:find-system "
                      "\"ntop\")) (dependencies "
                      "(asdf:system-depends-on system)) "
                      "(components (asdf:module-components "
                      "system)) (coverage-directory "
                      "(uiop:ensure-directory-pathname "
                      "(uiop:getenv "
                      "\"NTOP_TERMINAL_TEST_FASL_DIRECTORY\")))"
                      ") (dolist (dependency dependencies) "
                      "(asdf:load-system dependency)) (dolist "
                      "(component components) (unless (equal "
                      "(asdf:component-name component) "
                      "\"src/feature/process/domain-metrics\") "
                      "(load (asdf:component-pathname "
                      "component)))) (dolist (source (list "
                      "\"application-sampler.fasl\" "
                      "\"application-loop.fasl\" "
                      "\"infrastructure-clock.fasl\" "
                      "\"infrastructure-collection.fasl\" "
                      "\"infrastructure-terminal.fasl\")) (load "
                      "(merge-pathnames source "
                      "coverage-directory))))")
         "--eval"
         (%terminal-test-session-form kind signal))))

(defun %terminal-test-write (fd text deadline)
  (let ((octet (make-array 1 :element-type '(unsigned-byte 8))))
    (loop for
      character
      across
      text
      do
      (setf (aref octet 0) (char-code character))
      (loop for
        remaining
        =
        (%terminal-test-remaining-seconds deadline)
        do
        (unless (plusp remaining)
          (error "PTY terminal test timed out while writing."))
        (unless (cl-tty-kit:fd-wait fd :output remaining)
          (error "PTY terminal test timed out while writing."))
        (let ((count (cl-tty-kit:fd-write-octets fd octet)))
          (unless (and (integerp count) (<= 0 count 1))
            (error "PTY returned an invalid write count: ~S" count))
          (when (= count 1)
            (return)))))))

(defun %terminal-test-read-through-marker (fd output marker deadline)
  (let ((octets (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop
        (when (search marker output)
          (return output))
      (let ((remaining (%terminal-test-remaining-seconds deadline)))
        (unless (plusp remaining)
          (error
           "PTY terminal test timed out waiting for ~A; captured output: ~S."
           marker
           output))
        (unless (cl-tty-kit:fd-wait fd :input remaining)
          (error
           "PTY terminal test timed out waiting for ~A; captured output: ~S."
           marker
           output))
        (let ((count (cl-tty-kit:fd-read-octets fd octets)))
          (cond
            ((null count) (error "PTY closed before marker ~A." marker))
            ((zerop count)
             (error "PTY returned no data before marker ~A." marker))
            (t
             (dotimes (index count)
               (let ((character (code-char (aref octets index))))
                 (unless character
                   (error "PTY returned a non-character byte."))
                 (vector-push-extend character output)))))))
      (when (> (length output) +terminal-test-output-limit+)
        (error "PTY terminal test output exceeded its limit.")))))

(defun %terminal-test-marker-integer (output marker)
  (let ((marker-start (search marker output)))
    (unless marker-start
      (error "PTY output did not contain marker ~A." marker))
    (let* ((start (+ marker-start (length marker)))
           (end (search "__" output :start2 start)))
      (unless end
        (error "PTY output had an incomplete marker ~A." marker))
      (parse-integer output :start start :end end))))

(defun %terminal-test-between (output opening closing)
  (let ((opening-start (search opening output)))
    (unless opening-start
      (error "PTY output did not contain marker ~A." opening))
    (let* ((start (+ opening-start (length opening)))
           (end (search closing output :start2 start)))
      (unless end
        (error "PTY output did not contain marker ~A." closing))
      (subseq output start end))))

(defun %run-terminal-restoration-case (kind &optional signal)
  (let* ((coverage-directory (%terminal-test-prepare-source-fasls))
         (pty nil)
         (output
          (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
         (deadline (%terminal-test-deadline)))
    (unwind-protect
        (progn
          (setf pty (cl-tty-kit:make-pty :program
                                         "sh"
                                         :args
                                         (%terminal-test-child-arguments kind
                                                                         signal)
                                         :environment
                                         (%terminal-test-environment
                                          coverage-directory)))
          (cl-tty-kit:pty-resize pty 80 24)
          (%terminal-test-write (cl-tty-kit:pty-fd pty)
                                (format nil "start~%")
                                deadline)
          (setf output (%terminal-test-read-through-marker
                        (cl-tty-kit:pty-fd pty)
                        output
                        "__NTOP_APP_PID__"
                        deadline))
          (let ((app-pid
                 (%terminal-test-marker-integer output "__NTOP_APP_PID__")))
            (setf output (%terminal-test-read-through-marker
                          (cl-tty-kit:pty-fd pty)
                          output
                          "__NTOP_SESSION_READY__"
                          deadline))
            (ecase kind
              (:normal nil)
              (:error nil)
              (:signal
                (sb-posix:kill app-pid signal)
                (setf output (%terminal-test-read-through-marker
                              (cl-tty-kit:pty-fd pty)
                              output
                              "__NTOP_SIGNAL_SEEN__"
                              deadline)))
              (:resize
                (cl-tty-kit:pty-resize pty 100 30)
                (sb-posix:kill app-pid sb-posix:sigwinch)
                (setf output (%terminal-test-read-through-marker
                              (cl-tty-kit:pty-fd pty)
                              output
                              "__NTOP_RESIZE_SEEN__"
                              deadline))))
            (setf output (%terminal-test-read-through-marker
                          (cl-tty-kit:pty-fd pty)
                          output
                          "__NTOP_APP_STATUS__"
                          deadline))
            (when (eq kind :resize)
              (cl-tty-kit:pty-resize pty 80 24))
            (%terminal-test-write (cl-tty-kit:pty-fd pty)
                                  (format nil "release~%")
                                  deadline)
            (setf output (%terminal-test-read-through-marker
                          (cl-tty-kit:pty-fd pty)
                          output
                          "__NTOP_AFTER_END__"
                          deadline))
            (unless
                (probe-file
                 (merge-pathnames
                  (format nil "pty-coverage-~D.coverage" app-pid)
                  coverage-directory))
              (error "PTY child did not save coverage data."))
            (values (%terminal-test-marker-integer output "__NTOP_APP_STATUS__")
                    (%terminal-test-between output
                                            "__NTOP_BEFORE_BEGIN__"
                                            "__NTOP_BEFORE_END__")
                    (%terminal-test-between output
                                            "__NTOP_AFTER_BEGIN__"
                                            "__NTOP_AFTER_END__")
                    output)))
      (when pty
        (cl-tty-kit:close-pty pty)))))

(defun %run-terminal-application-with-loop (loop-function render)
  (let* ((config
          (ntop.shared.application:make-application-config :rate-ms
                                                           123
                                                           :default-widget-type
                                                           :cpu
                                                           :expanded-p
                                                           t
                                                           :basic-p
                                                           t))
         (collectors :collectors)
         (clock :clock)
         (input-stream (make-string-input-stream ""))
         (output-stream (make-string-output-stream))
         (channel :channel)
         (worker
          (ntop.app.infrastructure::make-collection-worker :channel
                                                           channel
                                                           :thread
                                                           (cl-concurrent-kit:make-thread
                                                            (lambda ()
                                                              nil)
                                                            :name
                                                            "application terminal test worker")
                                                           :stop-flag
                                                           (cl-concurrent-kit:make-atomic-counter)
                                                           :wakeup
                                                           (cl-concurrent-kit:make-semaphore)))
         (started nil)
         (stopped nil)
         (result nil))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.app.infrastructure:call-with-terminal-session)
       (lambda (function &key output-stream fd)
         (declare (ignore output-stream fd))
         (funcall function
                  (lambda ()
                    nil))))
      ((symbol-function 'ntop.app.infrastructure:start-collection-worker)
       (lambda (received-collectors received-clock rate-ms)
         (setf started (list received-collectors received-clock rate-ms))
         worker))
      ((symbol-function 'ntop.app.infrastructure:collection-worker-channel)
       (lambda (received-worker)
         (declare (ignore received-worker))
         channel))
      ((symbol-function 'ntop.app.infrastructure:stop-collection-worker)
       (lambda (received-worker)
         (setf stopped received-worker)
         nil))
      ((symbol-function 'cl-tty-kit:make-stream-input-poller)
       (lambda (stream)
         (declare (ignore stream))
         (lambda (state timeout)
           (declare (ignore state timeout))
           nil)))
      ((symbol-function 'cl-tty-kit:terminal-size)
       (lambda (fd)
         (declare (ignore fd))
         (values 110 36)))
      ((symbol-function 'ntop.app.application:run-application-loop)
       loop-function))
     (setf result (handler-case (list :return
                                      (ntop.app.infrastructure:run-application
                                       config
                                       collectors
                                       clock
                                       render
                                       :input-stream
                                       input-stream
                                       :output-stream
                                       output-stream))
                    (error (condition)
                      (list :error condition))))
     (values result started channel stopped config collectors clock))))
