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
                            '(:unavailable)))
             :loop-result)
           (lambda (state received-config snapshot columns rows reasons)
             (setf observed-render
                   (list state received-config snapshot columns rows reasons))
             :rendered))
        (cl-weave:expect result :to-equal '(:return :loop-result))
        (cl-weave:expect started :to-equal '(:collectors :clock 123))
        (cl-weave:expect channel :to-be :channel)
        (cl-weave:expect (typep stopped 'ntop.app.infrastructure::collection-worker) :to-be t)
        (cl-weave:expect
         (list (ntop.app.domain:app-state-selected-widget (first observed-loop))
               (ntop.app.domain:app-state-expanded-p (first observed-loop))
               (ntop.app.domain:app-state-basic-mode-p (first observed-loop)))
         :to-equal
         '(:cpu t t))
        (cl-weave:expect (nth 2 observed-loop) :to-be :channel)
        (cl-weave:expect (nth 3 observed-loop) :to-be t)
        (cl-weave:expect (nth 4 observed-loop) :to-be nil)
        (cl-weave:expect (nth 5 observed-loop) :to-equal '(110 36))
        (cl-weave:expect (nth 5 observed-render) :to-equal '(:unavailable))
        (cl-weave:expect render-result :to-be :rendered)
        (cl-weave:expect (ntop.shared.application:application-config-rate-ms
                          config)
                         :to-be 123))))

  (cl-weave:it "stops the collection worker when the application loop errors"
    (multiple-value-bind (result started channel stopped)
        (%run-terminal-application-with-loop
         (lambda (&rest arguments)
           (declare (ignore arguments))
           (error "deliberate application loop error"))
         (lambda (state config snapshot columns rows reasons)
           (declare (ignore state config snapshot columns rows reasons))
           nil))
      (cl-weave:expect (first result) :to-be :error)
      (cl-weave:expect (typep (second result) 'simple-error) :to-be t)
      (cl-weave:expect started :to-equal '(:collectors :clock 123))
      (cl-weave:expect channel :to-be :channel)
      (cl-weave:expect (typep stopped 'ntop.app.infrastructure::collection-worker) :to-be t)))

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
)
