(in-package #:ntop.test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames
         "infrastructure-terminal-test-support.lisp"
         (uiop:pathname-directory-pathname
          (asdf:component-pathname
           (asdf:find-component "ntop/test"
                                "t/app/infrastructure-terminal-test"))))))
(cl-weave:describe "application terminal restoration"
  (cl-weave:it "restores terminal settings after normal return"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :normal)
      (cl-weave:expect status :to-equal 0)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "restores terminal settings after an unhandled Lisp error"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :error)
      (cl-weave:expect (not (zerop status)) :to-be t)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "restores terminal settings after SIGINT"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :signal sb-posix:sigint)
      (cl-weave:expect (not (zerop status)) :to-be t)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "restores terminal settings after SIGTERM"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :signal sb-posix:sigterm)
      (cl-weave:expect (not (zerop status)) :to-be t)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "restores terminal settings after SIGHUP"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :signal sb-posix:sighup)
      (cl-weave:expect (not (zerop status)) :to-be t)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "reports SIGWINCH as a resize event and restores terminal settings"
    (multiple-value-bind (status before after)
        (%run-terminal-restoration-case :resize)
      (cl-weave:expect status :to-equal 0)
      (cl-weave:expect before :to-equal after)))

  (cl-weave:it "falls back to 80 by 24 when terminal size is unavailable"
    (with-open-file (stream #P"/dev/null" :direction :input)
      (cl-weave:expect
       (multiple-value-list
        (ntop.app.infrastructure::%application-terminal-size
         (cl-tty-kit::%stream-fd stream)))
       :to-equal
       (list 80 24))))

  #+darwin
  (cl-weave:it "releases saved Darwin termios after capture fails"
    (with-open-file (stream #P"/dev/null" :direction :input)
      (cl-weave:expect
       (handler-case
           (progn
             (ntop.app.infrastructure::%capture-terminal-settings
              (cl-tty-kit::%stream-fd stream))
             nil)
         (error () t))
       :to-be t)))

  #+darwin
  (cl-weave:it "reports Darwin termios restoration failure"
    (with-open-file (stream #P"/dev/null" :direction :input)
      (let ((termios
              (sb-alien:make-alien
               ntop.app.infrastructure::%darwin-terminal-termios)))
        (unwind-protect
             (cl-weave:expect
              (handler-case
                  (progn
                    (ntop.app.infrastructure::%restore-terminal-settings
                     (cl-tty-kit::%stream-fd stream)
                     termios)
                    nil)
                (error () t))
              :to-be t)
          (sb-alien:free-alien termios)))))

  (cl-weave:it "uses available terminal dimensions"
    (cl-weave:with-mocked-functions
        (((symbol-function 'cl-tty-kit:terminal-size)
          (lambda (fd)
            (declare (ignore fd))
            (values 100 30))))
      (cl-weave:expect
       (multiple-value-list
        (ntop.app.infrastructure::%application-terminal-size 0))
       :to-equal
       (list 100 30))))

  (cl-weave:it "wires application services and passes render reasons"
    (let ((observed-loop nil)
          (observed-render nil)
          (render-result nil))
      (multiple-value-bind (result started channel stopped config)
          (%run-terminal-application-with-loop
           (lambda (state received-config collection-channel poll-input
                    poll-size render &key poll-signal histories on-snapshot
                                      on-action)
             (setf observed-loop
                   (list state
                         received-config
                         collection-channel
                         (functionp poll-input)
                         (funcall poll-signal)
                         (multiple-value-list (funcall poll-size state 0))
                         histories
                         on-snapshot
                         on-action))
             (setf render-result
                   (funcall render state received-config :snapshot 110 36
                            (quote (:unavailable))))
             :loop-result)
           (lambda (state received-config snapshot columns rows reasons
                     &optional current-time-unix-seconds)
             (setf observed-render
                   (list state received-config snapshot columns rows reasons
                         current-time-unix-seconds))
             :rendered))
        (cl-weave:expect
         (list result
               started
               channel
               (typep stopped (quote ntop.app.infrastructure::collection-worker))
               (list (ntop.app.domain:app-state-selected-widget (first observed-loop))
                     (ntop.app.domain:app-state-expanded-p (first observed-loop))
                     (ntop.app.domain:app-state-basic-mode-p (first observed-loop)))
               (nth 2 observed-loop)
               (nth 3 observed-loop)
               (nth 4 observed-loop)
               (nth 5 observed-loop)
               (nth 5 observed-render)
               (nth 6 observed-render)
               render-result
               (ntop.shared.application:application-config-rate-ms config))
         :to-equal
         (quote ((:return :loop-result)
                 (:collectors :clock 123)
                 :channel
                 t
                 (:cpu t t)
                 :channel
                 t
                 nil
                 (110 36)
                 (:unavailable)
                 1700000000
                 :rendered
                 123))))))

  (cl-weave:it "stops the collection worker when the application loop errors"
    (multiple-value-bind (result started channel stopped)
        (%run-terminal-application-with-loop
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error "deliberate application loop error"))
         (lambda (state config snapshot columns rows reasons
                  &optional current-time-unix-seconds)
           (declare (ignore state config snapshot columns rows reasons
                            current-time-unix-seconds))
           nil))
      (cl-weave:expect
       (list (first result)
             (typep (second result) (quote simple-error))
             started
             channel
             (typep stopped (quote ntop.app.infrastructure::collection-worker)))
       :to-equal
       (quote (:error t (:collectors :clock 123) :channel t)))))

  (cl-weave:it "rejects a non-function renderer before starting a terminal session"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.app.infrastructure:run-application
            (ntop.shared.application:make-application-config)
            nil nil nil)
           nil)
       (simple-error () t))
     :to-be t))
  (cl-weave:it "polls terminal counters in signal order"
  (let ((signals (ntop.app.infrastructure::%make-terminal-signals))
        (installed nil)
        (defaults nil)
        (observations nil)
        (expected-signals
          (list sb-posix:sigint sb-posix:sigterm sb-posix:sighup
                sb-posix:sigwinch)))
    (sb-ext:without-package-locks
      (cl-weave:with-mocked-functions
        (((symbol-function (find-symbol "ENABLE-INTERRUPT" "SB-SYS"))
          (lambda (signal handler)
            (if (eq handler :default)
                (push signal defaults)
                (push (list signal handler) installed)))))
        (ntop.app.infrastructure::%install-terminal-signal-handlers signals)
        (let* ((handlers (nreverse installed))
               (counters (ntop.app.infrastructure::%signal-counters signals))
               (resize-handler (second (assoc sb-posix:sigwinch handlers)))
               (terminate-handler (second (assoc sb-posix:sigterm handlers)))
               (interrupt-handler (second (assoc sb-posix:sigint handlers))))
          (setf observations
                (list
                 (list (mapcar #'first handlers) (mapcar #'first counters))
                 (list
                  (ntop.app.infrastructure::%pending-termination-signal signals)
                  (ntop.app.infrastructure::%poll-terminal-event signals))))
          (funcall resize-handler nil nil nil)
          (setf observations
                (append
                 observations
                 (list
                  (list
                   (ntop.app.infrastructure::%pending-termination-signal signals)
                   (ntop.app.infrastructure::%poll-terminal-event signals)
                   (cl-concurrent-kit:atomic-counter-value
                    (ntop.app.infrastructure::%terminal-signals-resize
                     signals))))))
          (funcall terminate-handler nil nil nil)
          (funcall interrupt-handler nil nil nil)
          (setf observations
                (append
                 observations
                 (list
                  (list
                   (ntop.app.infrastructure::%pending-termination-signal signals)
                   (ntop.app.infrastructure::%poll-terminal-event signals)))))
          (ntop.app.infrastructure::%restore-default-signal-handlers signals)
          (cl-weave:expect
           (list observations (nreverse defaults))
           :to-equal
           (list
            (list
             (list expected-signals expected-signals)
             '(nil nil)
             (list nil :resize 0)
             (list sb-posix:sigint (list :terminate sb-posix:sigint)))
            expected-signals)))))))
  (cl-weave:it "runs its callback in a PTY terminal session"
  (let ((pty (cl-tty-kit:make-pty :program "cat"))
        (installed-signals nil)
        (defaults-restored nil)
        (callback-event :unset)
        (result nil)
        (expected-signals
          (list sb-posix:sigint sb-posix:sigterm sb-posix:sighup
                sb-posix:sigwinch)))
    (unwind-protect
        (progn
          (setf result
                (cl-weave:with-mocked-functions
                    (((symbol-function
                       (find-symbol "%INSTALL-TERMINAL-SIGNAL-HANDLERS"
                                    "NTOP.APP.INFRASTRUCTURE"))
                      (lambda (signals)
                        (setf installed-signals
                              (mapcar
                               #'first
                               (ntop.app.infrastructure::%signal-counters
                                signals)))))
                     ((symbol-function
                       (find-symbol "%RESTORE-DEFAULT-SIGNAL-HANDLERS"
                                    "NTOP.APP.INFRASTRUCTURE"))
                      (lambda (signals)
                        (declare (ignore signals))
                        (setf defaults-restored t))))
                  (ntop.app.infrastructure:call-with-terminal-session
                   (lambda (poll-terminal-event)
                     (setf callback-event (funcall poll-terminal-event))
                     :session-result)
                   :output-stream
                   (make-string-output-stream)
                   :fd
                   (cl-tty-kit:pty-fd pty))))
          (cl-weave:expect (list result callback-event)
                           :to-equal
                           '(:session-result nil))
          (cl-weave:expect
           (list installed-signals defaults-restored)
           :to-equal
           (list expected-signals t)))
      (cl-tty-kit:close-pty pty))))
)
