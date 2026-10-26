(in-package #:ntop.e2e)

(defun %route-terminal-signature (fd)
  (let ((termios (sb-posix:tcgetattr fd)))
    (list (sb-posix:termios-iflag termios)
          (sb-posix:termios-oflag termios)
          (sb-posix:termios-cflag termios)
          (sb-posix:termios-lflag termios)
          (copy-seq (sb-posix:termios-cc termios)))))

(defun %route-terminal-baseline (rows columns timeout)
  (let ((session (launch-e2e-process "sleep" '("30") rows columns
                                     :environment (%route-environment)
                                     :timeout-seconds timeout)))
    (unwind-protect
         (%route-terminal-signature (e2e-session-fd session))
      (close-e2e-session session))))

(defun %route-run-terminal-side (program route subcase evidence-prefix rows
                                         columns timeout config-path evidence-directory)
  (let* ((baseline (%route-terminal-baseline rows columns timeout))
         (args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session (launch-e2e-process program args rows columns
                                      :environment (%route-environment)
                                      :timeout-seconds timeout))
         (process (e2e-session-process session))
         (terminal-error nil)
         (during nil)
         (after nil)
         (exit-code nil)
         (signal-sent-p nil)
         (exited-p nil)
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (ansi (make-array 0 :element-type '(unsigned-byte 8)
                           :adjustable t :fill-pointer 0)))
    (unwind-protect
        (progn
          (handler-case
              (progn
                (multiple-value-bind (initial)
                    (pty-read-until-idle
                     session :quiet-seconds *route-frame-quiet-seconds*
                     :first-frame-not-before 0.75d0
                     :timeout-seconds timeout)
                  (%append-octets ansi initial))
                (setf during (%route-terminal-signature
                              (e2e-session-fd session)))
                (unless (and (sb-ext:process-alive-p process)
                             (not (equalp baseline during)))
                  (error "Application did not enter raw mode while alive."))
                (if (string= (route-id route) "restore-normal")
                    (pty-send-input session (%route-octets "q")
                                    :timeout-seconds timeout)
                  (let ((signal
                         (cond
                          ((string= (route-id route) "restore-sigint")
                           sb-posix:sigint)
                          ((string= (route-id route) "restore-sigterm")
                           sb-posix:sigterm)
                          ((string= (route-id route) "restore-sighup")
                           sb-posix:sighup)
                          (t (error "Unknown terminal signal route ~a."
                                    (route-id route))))))
                    (unless (sb-ext:process-alive-p process)
                      (error "Application exited before signal delivery."))
                    (sb-ext:process-kill process signal :pid)
                    (setf signal-sent-p t)))
                (setf exit-code (%route-wait-for-exit session timeout)
                      exited-p t)
                (when (and (string= (route-id route) "restore-normal")
                           (not (eql exit-code 0)))
                  (error "Normal quit exited with status ~s." exit-code)))
            (error (condition) (setf terminal-error condition)))
          (handler-case
              (setf after (%route-terminal-signature
                           (e2e-session-fd session)))
            (error (condition)
                   (setf terminal-error (or terminal-error condition))))
          (%route-write-ansi path ansi)
          (list :passed
                (and (null terminal-error)
                     during
                     (not (equalp baseline during))
                     exited-p
                     (or (string= (route-id route) "restore-normal") signal-sent-p)
                     (equalp baseline after))
                :actual
                (format nil "raw-mode ~a; exited ~a with status ~s; restored ~a~a"
                        (if (and during (not (equalp baseline during)))
                            "observed" "not observed")
                        (if exited-p "yes" "no") exit-code
                        (if (equalp baseline after) "yes" "no")
                        (if terminal-error
                            (format nil "; error ~a" terminal-error)
                          ""))
                :exit-code exit-code :evidence path
                :capture-error (and terminal-error (not during) terminal-error)))
      (close-e2e-session session))))

(defun %route-exception-child-arguments ()
  (list "--noinform" "--non-interactive"
        "--eval" "(require :asdf)"
        "--eval" "(asdf:load-asd #p\"ntop.asd\")"
        "--eval" "(asdf:load-system \"ntop\")"
        "--eval"
        (format nil "~{~a~^ ~}"
                (list "(let* ((signature (lambda (termios)"
                      "(list (sb-posix:termios-iflag termios)"
                      "(sb-posix:termios-oflag termios)"
                      "(sb-posix:termios-cflag termios)"
                      "(sb-posix:termios-lflag termios)"
                      "(copy-seq (sb-posix:termios-cc termios)))))"
                      "(before (funcall signature (sb-posix:tcgetattr 0)))"
                      "(thunk-ran nil)"
                      "(during nil)"
                      "(caught nil))"
                      "(handler-case"
                      "(ntop.app.infrastructure:call-with-terminal-session"
                      "(lambda (poll-terminal-event) (declare (ignore poll-terminal-event))"
                      "(setf thunk-ran t"
                      "during (funcall signature (sb-posix:tcgetattr 0)))"
                      "(error \"route exception sentinel\")))"
                      "(error (condition)"
                      "(setf caught"
                      "(not (null (search \"route exception sentinel\""
                      "(princ-to-string condition)))))))"
                      "(format t \"ROUTE-THUNK-RAN=~a~%\" thunk-ran)"
                      "(format t \"ROUTE-RAW=~a~%\""
                      "(and thunk-ran during (not (equalp before during))))"
                      "(format t \"ROUTE-EXCEPTION-CAUGHT=~a~%\" caught)"
                      "(format t \"ROUTE-RESTORED=~a~%\""
                      "(equalp before (funcall signature"
                      "(sb-posix:tcgetattr 0)))))"))))

(defun %route-run-exception-side (evidence-prefix rows columns timeout)
  (let* ((session (launch-e2e-process "sbcl" (%route-exception-child-arguments)
                                      rows columns
                                      :environment (%route-environment)
                                      :timeout-seconds (* 4 timeout)))
         (output nil)
         (exit-code nil)
         (error-value nil))
    (unwind-protect
         (handler-case
             (progn
               (setf exit-code (%route-wait-for-exit session (* 4 timeout)))
               (setf output (e2e-session-output session)))
           (error (condition) (setf error-value condition)))
      (close-e2e-session session))
    (let* ((text (and output (%octets-to-text output)))
           (path (merge-pathnames (format nil "~a.txt" evidence-prefix))))
      (%write-text path (or text (princ-to-string error-value)))
      (list :passed (and (null error-value)
                         (eql exit-code 0)
                         (search "ROUTE-THUNK-RAN=T" text)
                         (search "ROUTE-RAW=T" text)
                         (search "ROUTE-EXCEPTION-CAUGHT=T" text)
                         (search "ROUTE-RESTORED=T" text))
            :actual (format nil "exit ~s; ~a"
                            exit-code
                            (or (and text
                                     (string-trim
                                      '(#\Space #\Newline #\Return) text))
                                (princ-to-string error-value)))
            :evidence path))))
