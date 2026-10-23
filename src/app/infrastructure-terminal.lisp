(in-package #:ntop.app.infrastructure)

(defstruct (%terminal-signals (:constructor %make-terminal-signals))
  (resize (cl-concurrent-kit:make-atomic-counter))
  (interrupt (cl-concurrent-kit:make-atomic-counter))
  (terminate (cl-concurrent-kit:make-atomic-counter))
  (hangup (cl-concurrent-kit:make-atomic-counter)))

#+darwin
(progn
  (sb-alien:define-alien-type
      %darwin-terminal-termios
    (sb-alien:struct termios
      (iflag sb-alien:unsigned-long)
      (oflag sb-alien:unsigned-long)
      (cflag sb-alien:unsigned-long)
      (lflag sb-alien:unsigned-long)
      (cc (sb-alien:array sb-alien:unsigned-char 20))
      (ispeed sb-alien:unsigned-long)
      (ospeed sb-alien:unsigned-long)))
  (sb-alien:define-alien-routine
      ("tcgetattr" %darwin-terminal-tcgetattr)
      sb-alien:int
    (fd sb-alien:int)
    (termios (* %darwin-terminal-termios)))
  (sb-alien:define-alien-routine
      ("tcsetattr" %darwin-terminal-tcsetattr)
      sb-alien:int
    (fd sb-alien:int)
    (actions sb-alien:int)
    (termios (* %darwin-terminal-termios))))

(defun %signal-counters (signals)
  (list (cons sb-posix:sigint (%terminal-signals-interrupt signals))
        (cons sb-posix:sigterm (%terminal-signals-terminate signals))
        (cons sb-posix:sighup (%terminal-signals-hangup signals))
        (cons sb-posix:sigwinch (%terminal-signals-resize signals))))

(progn
  #+darwin
  (defun %capture-terminal-settings (fd)
    (let ((termios (sb-alien:make-alien %darwin-terminal-termios)))
      (unless (zerop (%darwin-terminal-tcgetattr fd termios))
        (sb-alien:free-alien termios)
        (error "Unable to capture terminal settings for file descriptor ~D."
               fd))
      termios))
  #-darwin
  (defun %capture-terminal-settings (fd)
    (sb-posix:tcgetattr fd))
  #+darwin
  (defun %restore-terminal-settings (fd termios)
    (unless (zerop (%darwin-terminal-tcsetattr fd sb-posix:tcsadrain termios))
      (error "Unable to restore terminal settings for file descriptor ~D."
             fd)))
  #-darwin
  (defun %restore-terminal-settings (fd termios)
    (sb-posix:tcsetattr fd sb-posix:tcsadrain termios))
  #+darwin
  (defun %release-terminal-settings (termios)
    (sb-alien:free-alien termios))
  #-darwin
  (defun %release-terminal-settings (termios)
    (declare (ignore termios))
    nil))

(defun %install-terminal-signal-handlers (signals)
  (dolist (entry (%signal-counters signals))
    (let ((counter (cdr entry)))
      (sb-sys:enable-interrupt (car entry)
                               (lambda (signal info context)
                                 (declare (ignore signal info context))
                                 (cl-concurrent-kit:atomic-counter-incf counter))))))

(defun %restore-default-signal-handlers (signals)
  (dolist (entry (%signal-counters signals))
    (sb-sys:enable-interrupt (car entry) :default)))

(defun %pending-termination-signal (signals)
  (loop for entry in (%signal-counters signals)
        for signal = (car entry)
        for counter = (cdr entry)
        unless (= signal sb-posix:sigwinch)
        when (plusp (cl-concurrent-kit:atomic-counter-value counter))
          do (return signal)))

(defun %poll-terminal-event (signals)
  (or
   (let ((signal (%pending-termination-signal signals)))
     (and signal (list :terminate signal)))
   (let ((counter (%terminal-signals-resize signals)))
     (when (plusp (cl-concurrent-kit:atomic-counter-value counter))
       (cl-concurrent-kit:atomic-counter-decf counter)
       :resize))))

(defun call-with-terminal-session (function &key
                                            (output-stream *standard-output*)
                                            (fd 0))
  (let ((signals (%make-terminal-signals))
        (original-termios (%capture-terminal-settings fd)))
    (unwind-protect
        (unwind-protect
            (progn
              (%install-terminal-signal-handlers signals)
              (multiple-value-prog1
                  (unwind-protect
                      (cl-tty-kit:with-terminal-session
                       (session-output :stream
                                       output-stream
                                       :fd
                                       fd
                                       :raw-mode
                                       t
                                       :alternate-screen
                                       t
                                       :hide-cursor
                                       t
                                       :mouse
                                       :normal)
                       (let ((*standard-output* session-output))
                         (funcall function
                                  (lambda ()
                                    (%poll-terminal-event signals)))))
                    (%restore-terminal-settings fd original-termios))
                (let ((signal (%pending-termination-signal signals)))
                  (when signal
                    (sb-sys:enable-interrupt signal :default)
                    (sb-posix:kill (sb-posix:getpid) signal)))))
          (%restore-default-signal-handlers signals))
      (%release-terminal-settings original-termios))))

(defun %application-terminal-size (fd)
  (multiple-value-bind (columns rows) (cl-tty-kit:terminal-size fd)
    (if (and columns rows (plusp columns) (plusp rows))
        (values columns rows)
        (values 80 24))))

(defun run-application (config collectors
                               clock
                               render
                               &key
                               (input-stream *standard-input*)
                               (output-stream *standard-output*)
                               (fd 0)
                               history-stores
                               on-snapshot
                               on-action)
  (unless (functionp render)
    (error "RENDER must be a function."))
  (call-with-terminal-session
   (lambda (poll-terminal-event)
     (let* ((state
             (let* ((default-widget
                     (ntop.shared.application:application-config-default-widget-type
                      config))
                    (expanded-p
                     (ntop.shared.application:application-config-expanded-p
                      config))
                    (basic-mode-p
                     (ntop.shared.application:application-config-basic-p config)))
               (ntop.app.domain:make-app-state :selected-widget
                                               default-widget
                                               :expanded-p
                                               expanded-p
                                               :basic-mode-p
                                               basic-mode-p)))
            (worker
             (start-collection-worker collectors
                                      clock
                                      (ntop.shared.application:application-config-rate-ms
                                       config)))
            (poll-input (cl-tty-kit:make-stream-input-poller input-stream))
            (poll-size
             (lambda (loop-state timeout)
               (declare (ignore loop-state timeout))
               (%application-terminal-size fd))))
       (unwind-protect
           (ntop.app.application:run-application-loop state
                                                      config
                                                      (collection-worker-channel
                                                       worker)
                                                      poll-input
                                                      poll-size
                                                      render
                                                      :poll-signal
                                                      poll-terminal-event
                                                      :histories
                                                      history-stores
                                                      :on-snapshot
                                                      on-snapshot
                                                      :on-action
                                                      on-action)
         (stop-collection-worker worker))))
   :output-stream
   output-stream
   :fd
   fd))
