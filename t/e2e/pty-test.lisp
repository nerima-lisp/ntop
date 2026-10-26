(in-package #:ntop.e2e.test)
(cl-weave:describe
 "PTY session resize signaling"
 (cl-weave:it
  "reports resized PTY dimensions to the child"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os, signal, time~%"
                                "def resized(signum, frame):~%"
                                "    size = os.get_terminal_size(0)~%"
                                "    os.write(1, f'RESIZE={size.columns}x{size.lines}'.encode())~%"
                                "signal.signal(signal.SIGWINCH, resized)~%"
                                "os.write(1, b'RESIZE_READY')~%"
                                "while True: time.sleep(1)~%")))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 5d0)))
    (unwind-protect
         (progn
           (ntop.e2e::pty-read-until-idle
            session :quiet-seconds 0.05d0 :timeout-seconds 2d0)
           (ntop.e2e::pty-resize-session
            session 36 120 :timeout-seconds 2d0)
           (multiple-value-bind (output)
               (ntop.e2e::%route-read-through-quiet-period
                session 0.3d0 2d0 0.75d0)
             (cl-weave:expect
              (not (null (search "RESIZE=120x36"
                                 (ntop.e2e::%octets-to-text output))))
              :to-be
              t)))
      (ntop.e2e::close-e2e-session session)))))
(defmacro define-frame-cases (&body cases)
  `(cl-weave:it-each ,cases
                     "frame readiness for ~A"
                     (case-name started-at
                                last-output-at
                                now
                                first-frame-not-before-at
                                quiet-seconds
                                expected)
                     (declare (ignore case-name))
                     (cl-weave:expect
                      (ntop.e2e::frame-ready-p started-at
                                               last-output-at
                                               now
                                               first-frame-not-before-at
                                               quiet-seconds)
                      :to-be
                      expected)))

(cl-weave:describe "E2E PTY frame timing"
                   (cl-weave:it
                    "expands the frame case table into cl-weave cases"
                    (cl-weave:expect
                     (macroexpand-1
                      '(define-frame-cases
                        (:ready-at-boundaries 10 11 12 12 1 t)))
                     :to-equal
                     '(cl-weave:it-each ((:ready-at-boundaries 10 11 12 12 1 t))
                                        "frame readiness for ~A"
                                        (case-name started-at
                                                   last-output-at
                                                   now
                                                   first-frame-not-before-at
                                                   quiet-seconds
                                                   expected)
                                        (declare (ignore case-name))
                                        (cl-weave:expect
                                         (ntop.e2e::frame-ready-p started-at
                                                                  last-output-at
                                                                  now
                                                                  first-frame-not-before-at
                                                                  quiet-seconds)
                                         :to-be
                                         expected))))
                   (define-frame-cases
                    (:before-first-frame 10 11 12 121/10 1 nil)
                    (:before-quiet-interval 10 11 1199/100 11 1 nil)
                    (:at-both-boundaries 10 11 12 12 1 t)
                    (:output-before-start 10 9 12 12 0 nil)
                    (:no-output 10 nil 12 12 0 nil)
                    (:later-output-rests-briefly 10 119/10 12 11 1 nil)))
(cl-weave:describe
 "E2E PTY input writes"
 (cl-weave:it
  "passes a multi-byte logical input to fd-write-octets in one call"
  (let* ((session
          (ntop.e2e::launch-e2e-process
           "sleep"
           (list "30")
           24
           80
           :timeout-seconds 5d0))
         (input
          (make-array 3
                      :element-type '(unsigned-byte 8)
                      :initial-contents '(27 79 82)))
         (writes nil)
         (original-write
          (symbol-function 'cl-tty-kit:fd-write-octets)))
    (unwind-protect
         (progn
           (unwind-protect
                (progn
                  (setf (symbol-function 'cl-tty-kit:fd-write-octets)
                        (lambda (fd octets)
                          (push (coerce octets 'list) writes)
                          (funcall original-write fd octets)))
                  (ntop.e2e::pty-send-input
                   session input :timeout-seconds 3d0)
                  (cl-weave:expect writes :to-equal '((27 79 82))))
             (setf (symbol-function 'cl-tty-kit:fd-write-octets)
                   original-write)))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "preserves the route result when fallback reaping follows a close error"
  (let* ((session
          (ntop.e2e::launch-e2e-process
           "sleep" (list "30") 24 80 :timeout-seconds 5d0))
         (original-close (symbol-function 'cl-tty-kit:close-pty))
         (warning-condition nil)
         (actual nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'cl-tty-kit:close-pty)
                 (lambda (pty)
                   (declare (ignore pty))
                   (error "forced close failure")))
           (setf actual
                 (handler-bind
                     ((warning
                        (lambda (condition)
                          (setf warning-condition condition)
                          (muffle-warning condition))))
                   (handler-case
                       (unwind-protect
                            :route-result
                         (ntop.e2e::close-e2e-session session))
                     (error (condition) condition))))
           (cl-weave:expect actual :to-equal :route-result)
           (cl-weave:expect (typep warning-condition 'warning) :to-be t)
           (cl-weave:expect
            (not (null (search "forced close failure"
                               (princ-to-string warning-condition))))
            :to-be t)
           (cl-weave:expect
            (sb-ext:process-alive-p
             (ntop.e2e::e2e-session-process session))
            :to-be nil)
           (cl-weave:expect (ntop.e2e::e2e-session-closed-p session) :to-be t))
      (setf (symbol-function 'cl-tty-kit:close-pty) original-close)
      (unless (ntop.e2e::e2e-session-closed-p session)
        (ntop.e2e::close-e2e-session session))))))

(cl-weave:describe "E2E PTY operation deadlines"
                   (cl-weave:it "expands the operation timeout macro"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e::with-e2e-operation-timeout
                                    ("probe" deadline 1d0)
                                    (values :completed)))
                                 :to-equal
                                 '(handler-case (sb-ext:with-timeout
                                                 (ntop.e2e::%ticks-to-seconds
                                                  (- deadline (ntop.e2e::%now)))
                                                 (values :completed))
                                    (sb-ext:timeout ()
                                      (ntop.e2e::%signal-timeout "probe" 1.0d0)))))
                   (cl-weave:it
                    "turns an operation deadline into an E2E timeout"
                    (let ((deadline
                           (+ (ntop.e2e::%now)
                              (ntop.e2e::%seconds-to-ticks 0.02d0)))
                          (caught nil))
                      (handler-case (ntop.e2e::with-e2e-operation-timeout
                                     ("probe" deadline 0.02d0)
                                     (sleep 0.08d0))
                        (ntop.e2e::e2e-timeout (condition)
                          (setf caught condition)))
                      (cl-weave:expect (typep caught 'ntop.e2e::e2e-timeout)
                                       :to-be
                                       t)
                      (cl-weave:expect (ntop.e2e::e2e-timeout-operation caught)
                                       :to-equal
                                       "probe"))))
(cl-weave:describe "E2E shared deadlines and process cleanup"
                   (cl-weave:it
                    "clamps calls to one measurement deadline"
                    (let* ((started 1000)
                           (deadline
                            (+ started
                               (ntop.e2e::%seconds-to-ticks 10d0)))
                           (ntop.e2e::*e2e-deadline* deadline)
                           (first
                            (ntop.e2e::%effective-timeout-seconds
                             30d0 "command" started))
                           (second
                            (ntop.e2e::%effective-timeout-seconds
                             30d0
                             "pty"
                             (+ started
                                (ntop.e2e::%seconds-to-ticks 7d0))))
                           (short
                            (ntop.e2e::%effective-timeout-seconds
                             2d0 "command" started)))
                      (cl-weave:expect first :to-equal 10)
                      (cl-weave:expect second :to-equal 3)
                      (cl-weave:expect short :to-equal 2)
                      (let ((expired nil)
                            (ntop.e2e::*e2e-deadline* started))
                        (handler-case
                            (ntop.e2e::%effective-timeout-seconds
                             1d0 "expired" started)
                          (ntop.e2e::e2e-timeout (condition)
                            (setf expired condition)))
                        (cl-weave:expect
                         (typep expired 'ntop.e2e::e2e-timeout)
                         :to-be
                         t))))
                   (cl-weave:it
 "starts session timing after launch while respecting the absolute deadline"
 (let* ((launch-started-at 1000)
        (session-started-at
         (+ launch-started-at
            (ntop.e2e::%seconds-to-ticks 7d0)))
        (ntop.e2e::*e2e-deadline*
         (+ launch-started-at
            (ntop.e2e::%seconds-to-ticks 30d0))))
   (multiple-value-bind (launch-deadline launch-timeout)
       (ntop.e2e::%deadline-from-start 12d0 "launch" launch-started-at)
     (multiple-value-bind (session-deadline session-timeout)
         (ntop.e2e::%deadline-from-start 12d0 "read" session-started-at)
       (cl-weave:expect launch-timeout :to-equal 12)
       (cl-weave:expect session-timeout :to-equal 12)
       (cl-weave:expect session-deadline
                        :to-equal
                        (+ session-started-at
                           (ntop.e2e::%seconds-to-ticks 12d0)))
       (cl-weave:expect (> session-deadline launch-deadline) :to-be t)))
   (let ((ntop.e2e::*e2e-deadline*
          (+ launch-started-at
             (ntop.e2e::%seconds-to-ticks 10d0))))
     (multiple-value-bind (session-deadline session-timeout)
         (ntop.e2e::%deadline-from-start 12d0 "read" session-started-at)
       (cl-weave:expect session-timeout :to-equal 3)
       (cl-weave:expect session-deadline
                        :to-equal
                        ntop.e2e::*e2e-deadline*)))))
                   (cl-weave:it
                    "passes the remaining deadline to host-kit"
                    (let* ((started (ntop.e2e::%now))
                           (deadline
                            (+ started
                               (ntop.e2e::%seconds-to-ticks 0.05d0)))
                           (ntop.e2e::*e2e-deadline* deadline)
                           (condition-type
                            (find-symbol "PROCESS-TIMEOUT" "HOST-KIT"))
                           (caught nil))
                      (handler-case
                          (ntop.e2e::%read-command-output
                           "sleep" (list "2") 10d0)
                        (error (condition)
                          (setf caught condition)))
                      (cl-weave:expect
                       (and condition-type
                            (typep caught condition-type))
                       :to-be
                       t)))
                   (cl-weave:it
                    "waits for the child process during session close"
                    (let* ((session
                            (ntop.e2e::launch-e2e-process
                             "sleep" (list "30") 24 80
                             :timeout-seconds 5d0))
                           (process
                            (ntop.e2e::e2e-session-process session)))
                      (ntop.e2e::close-e2e-session session)
                      (cl-weave:expect
                       (and
                        (ntop.e2e::e2e-session-closed-p session)
                        (not (sb-ext:process-alive-p process)))
                       :to-be
                       t)))
                   (cl-weave:it
                    "answers the terminal cursor position query"
                    (let* ((command
                            (concatenate 'string
                                         "import os,sys,tty,time; tty.setraw(0); "
                                         "query=bytes([27,91,54,110]); "
                                         "expected=bytes([27,91,49,59,49,82]); "
                                         "os.write(1,query); "
                                         "actual=bytes().join(os.read(0,1) for _ in range(6)); "
                                         "time.sleep(0.25); "
                                         "os.write(1,b'CURSOR_POSITION_RESPONSE_OK' "
                                         "if actual==expected else "
                                         "b'CURSOR_POSITION_RESPONSE_BAD'); "
                                         "time.sleep(0.25); sys.exit(actual!=expected)"))
                           (session
                            (ntop.e2e::launch-e2e-process
                             "python3" (list "-c" command) 24 80 :timeout-seconds 3d0))
                           (output nil))
                      (unwind-protect
                          (progn
                            (multiple-value-bind (frame)
                                (ntop.e2e::pty-read-until-idle
                                 session :quiet-seconds 0.1d0 :timeout-seconds 3d0)
                              (setf output frame))
                            (cl-weave:expect
                             (ntop.e2e::%route-wait-for-exit session 3d0)
                             :to-equal
                             0)
                            (cl-weave:expect
                             (not (null
       (search
        "CURSOR_POSITION_RESPONSE_OK"
        (ntop.e2e::%octets-to-text output))))
                             :to-be
                             t))
                        (ntop.e2e::close-e2e-session session)))))
(cl-weave:describe
 "E2E PTY child reaping"
 (cl-weave:it
  "reaps a SIGKILLed child before draining output and closing the PTY"
  (let* ((command
          (concatenate
           'string
           "import os,signal,time; "
           "signal.signal(signal.SIGHUP,signal.SIG_IGN); "
           "signal.signal(signal.SIGTERM,signal.SIG_IGN); "
           "os.write(1,b'REAP_READY'); time.sleep(30)"))
         (session
          (ntop.e2e::launch-e2e-process
           "python3" (list "-c" command) 24 80 :timeout-seconds 5d0))
         (process (ntop.e2e::e2e-session-process session))
         (original-close (symbol-function 'cl-tty-kit:close-pty))
         (close-state nil)
         (ready-output nil))
    (unwind-protect
         (progn
           (multiple-value-bind (frame)
               (ntop.e2e::pty-read-until-idle
                session :quiet-seconds 0.05d0 :timeout-seconds 2d0)
             (setf ready-output frame))
           (cl-weave:expect
            (not (null
                  (search "REAP_READY"
                          (ntop.e2e::%octets-to-text ready-output))))
            :to-be
            t)
           (cl-weave:expect (sb-ext:process-alive-p process) :to-be t)
           (unwind-protect
                (progn
                  (setf (symbol-function 'cl-tty-kit:close-pty)
                        (lambda (pty)
                          (setf close-state
                                (list
                                 (ntop.e2e::e2e-session-reaped-p session)
                                 (sb-ext:process-alive-p process)
                                 (ntop.e2e::%octets-to-text
                                  (ntop.e2e::e2e-session-output session))))
                          (funcall original-close pty)))
                  (ntop.e2e::close-e2e-session session))
             (setf (symbol-function 'cl-tty-kit:close-pty) original-close))
           (cl-weave:expect (first close-state) :to-be t)
           (cl-weave:expect (second close-state) :to-be nil)
           (cl-weave:expect
            (not (null (search "REAP_READY" (third close-state))))
            :to-be
            t)
           (cl-weave:expect
            (ntop.e2e::e2e-session-closed-p session)
            :to-be
            t))
      (setf (symbol-function 'cl-tty-kit:close-pty) original-close)
      (unless (ntop.e2e::e2e-session-closed-p session)
        (ntop.e2e::close-e2e-session session))))))
(cl-weave:describe
 "PTY initial screen readiness"
 (cl-weave:it
  "ignores terminal mode setup and cursor query bytes"
  (let ((octets
         (map '(vector (unsigned-byte 8))
              #'char-code
              (format nil "~c[?1049h~c[?2004h~c[?1000h~c[1;1R"
                      #\Escape #\Escape #\Escape #\Escape))))
    (cl-weave:expect
     (ntop.e2e::%pty-visible-screen-output-p octets)
     :to-be
     nil)))
 (cl-weave:it
  "recognizes printable screen content after ANSI controls"
  (let ((octets
         (map '(vector (unsigned-byte 8))
              #'char-code
              (format nil "~c[2JVisible screen" #\Escape))))
    (cl-weave:expect
     (ntop.e2e::%pty-visible-screen-output-p octets)
     :to-be
     t)))
 (cl-weave:it
 "waits for the visible frame after terminal setup output"
 (let* ((command
          (concatenate
           (quote string)
           "printf \"%b\" \"\\033[?1049h\\033[?2004h\"; "
           "sleep 0.8; "
           "printf \"%b\" \"\\033[2JVisible screen\"; "
           "sleep 0.2"))
        (session
          (ntop.e2e::launch-e2e-process
           "/bin/sh" (list "-c" command) 24 80 :timeout-seconds 3d0))
        (output nil)
        (last-output-at nil))
   (unwind-protect
        (progn
          (multiple-value-bind (frame last)
              (ntop.e2e::pty-read-until-idle
               session
               :quiet-seconds 0.1d0
               :first-frame-not-before 0.2d0
               :timeout-seconds 2d0)
            (setf output frame last-output-at last))
          (cl-weave:expect
           (ntop.e2e::%pty-visible-screen-output-p output)
           :to-be
           t)
          (cl-weave:expect (> last-output-at 0.6d0) :to-be t)
          (cl-weave:expect
           (not
            (null
             (search "Visible screen"
                     (ntop.e2e::%octets-to-text output))))
           :to-be
           t))
     (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
 "retains PTY bytes received before a later timeout"
 (let* ((session
          (ntop.e2e::launch-e2e-process
           "sleep" (list "30") 24 80 :timeout-seconds 3d0))
        (output
          (map (quote (vector (unsigned-byte 8)))
               (function char-code) "PARTIAL_ROUTE_OUTPUT"))
        (delivered-p nil)
        (original-wait (symbol-function (quote cl-tty-kit:fd-wait)))
        (original-read (symbol-function (quote cl-tty-kit:fd-read-octets)))
        (timed-out nil)
        (captured-text nil))
   (unwind-protect
        (progn
          (unwind-protect
               (progn
                 (setf (symbol-function (quote cl-tty-kit:fd-wait))
                       (lambda (fd direction wait-seconds)
                         (declare (ignore fd direction))
                         (if delivered-p
                             (progn (sleep wait-seconds) nil)
                           t)))
                 (setf (symbol-function (quote cl-tty-kit:fd-read-octets))
                       (lambda (fd buffer)
                         (declare (ignore fd))
                         (if delivered-p
                             nil
                           (progn
                             (setf delivered-p t)
                             (replace buffer output)
                             (length output)))))
                 (handler-case
                     (ntop.e2e::pty-read-until-idle
                      session :quiet-seconds 1d0 :timeout-seconds 0.5d0)
                   (ntop.e2e::e2e-timeout () (setf timed-out t)))
                 (setf captured-text
                       (ntop.e2e::%octets-to-text
                        (ntop.e2e::e2e-session-output session)))
                 (cl-weave:expect timed-out :to-be t)
                 (cl-weave:expect
                  (not (null (search "PARTIAL_ROUTE_OUTPUT" captured-text)))
                  :to-be
                  t)
                 (cl-weave:expect
                  (realp
                   (ntop.e2e::e2e-session-first-output-seconds session))
                  :to-be
                  t)
                 (cl-weave:expect
                  (realp
                   (ntop.e2e::e2e-session-first-visible-output-seconds session))
                  :to-be
                  t))
            (setf (symbol-function (quote cl-tty-kit:fd-wait)) original-wait
                  (symbol-function (quote cl-tty-kit:fd-read-octets)) original-read)))
     (ntop.e2e::close-e2e-session session)))))
