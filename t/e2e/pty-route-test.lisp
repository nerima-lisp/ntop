(in-package #:ntop.e2e.test)

(cl-weave:describe
 "PTY child exit output draining"
 (cl-weave:it
  "drains output while the child restores terminal state"
  (let* ((script
           (format nil
                   "import os, termios, tty
fd=0
saved=termios.tcgetattr(fd)
tty.setraw(fd)
payload=b'P'*65536
while payload:
    written=os.write(1,payload)
    payload=payload[written:]
termios.tcsetattr(fd,termios.TCSADRAIN,saved)
os.write(1,b'EXIT_DRAIN_COMPLETE')
"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 5d0))
         (exit-code nil)
         (captured-text nil))
    (unwind-protect
         (progn
           (setf exit-code
                 (ntop.e2e::%route-wait-for-exit session 5d0)
                 captured-text
                 (ntop.e2e::%octets-to-text
                  (ntop.e2e::e2e-session-output session)))
           (cl-weave:expect exit-code :to-equal 0)
           (cl-weave:expect
            (not (null (search "EXIT_DRAIN_COMPLETE" captured-text)))
            :to-be
            t)
           (cl-weave:expect (>= (length captured-text) 65536) :to-be t))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "drains output triggered by a shutdown signal before closing the PTY"
  (let* ((script
           (format nil
                   "import os, signal, sys, termios, time, tty
fd=0
saved=termios.tcgetattr(fd)
tty.setraw(fd)
def restore(signum, frame):
    payload=b'P'*65536
    while payload:
        written=os.write(1,payload)
        payload=payload[written:]
    termios.tcsetattr(fd,termios.TCSADRAIN,saved)
    os.write(1,b'CLOSE_DRAIN_COMPLETE')
    sys.exit(0)
signal.signal(signal.SIGHUP, restore)
os.write(1,b'SHUTDOWN_READY')
while True:
    time.sleep(1)
"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 5d0))
         (process (ntop.e2e::e2e-session-process session))
         (captured-text nil))
    (unwind-protect
         (progn
           (multiple-value-bind (initial)
               (ntop.e2e::pty-read-until-idle
                session :quiet-seconds 0.05d0 :timeout-seconds 2d0)
             (cl-weave:expect
              (not (null
                    (search "SHUTDOWN_READY"
                            (ntop.e2e::%octets-to-text initial))))
              :to-be
              t))
           (ntop.e2e::close-e2e-session session)
           (setf captured-text
                 (ntop.e2e::%octets-to-text
                  (ntop.e2e::e2e-session-output session)))
           (cl-weave:expect (ntop.e2e::e2e-session-closed-p session) :to-be t)
           (cl-weave:expect (sb-ext:process-alive-p process) :to-be nil)
           (cl-weave:expect
            (not (null (search "CLOSE_DRAIN_COMPLETE" captured-text)))
            :to-be
            t)
           (cl-weave:expect (>= (length captured-text) 65536) :to-be t))
      (ntop.e2e::close-e2e-session session)))))

(cl-weave:describe "process search state observation"
 (cl-weave:it
  "recognizes the BTM and ntop process-search prompt labels"
  (cl-weave:expect
   (ntop.e2e::%route-process-search-open-state-p
    (format nil "Case(F1) Whole(F2) Regex(F3)~%> ")
    "Processes list")
   :to-be
   t)
  (cl-weave:expect
   (ntop.e2e::%route-process-search-open-state-p
    (format nil "Case Whole Regex~%> ")
    "Processes list")
   :to-be
   t))
 (cl-weave:it
  "requires the prompt to appear only after the action"
  (cl-weave:expect
   (ntop.e2e::%route-process-search-open-state-p
    "Case Whole Regex > "
    "Case Whole Regex > ")
   :to-be
   nil)
  (cl-weave:expect
   (ntop.e2e::%route-process-search-open-state-p
    "Case Whole > "
    "Processes")
   :to-be
   nil)))

(cl-weave:describe
 "route input quiet reads and help search state"
 (cl-weave:it
  "returns after the no-output observation window"
  (let* ((session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" "import time; time.sleep(30)") 24 80
            :timeout-seconds 3d0))
         (started (ntop.e2e::%now))
         (output nil))
    (unwind-protect
         (progn
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session 0.05d0 2d0 0.08d0))
           (cl-weave:expect (length output) :to-equal 0)
           (cl-weave:expect
            (< (ntop.e2e::%ticks-to-seconds
                (- (ntop.e2e::%now) started))
               1d0)
            :to-be
            t)
           (cl-weave:expect
            (sb-ext:process-alive-p (ntop.e2e::e2e-session-process session))
            :to-be
            t))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "closes the PTY after the child-reap fallback"
  (let* ((session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" "import time; time.sleep(30)") 24 80
            :timeout-seconds 3d0))
         (process (ntop.e2e::e2e-session-process session))
         (original-stop
           (symbol-function 'ntop.e2e::%e2e-stop-session-process))
         (original-close-pty
           (symbol-function 'cl-tty-kit:close-pty))
         (close-count 0))
    (unwind-protect
         (progn
           (setf (symbol-function 'ntop.e2e::%e2e-stop-session-process)
                 (lambda (ignored-session)
                   (declare (ignore ignored-session))
                   (ntop.e2e::%signal-timeout "child reap" 0.6d0)))
           (setf (symbol-function 'cl-tty-kit:close-pty)
                 (lambda (pty)
                   (incf close-count)
                   (funcall original-close-pty pty)))
           (ntop.e2e::close-e2e-session session)
           (cl-weave:expect close-count :to-equal 1)
           (cl-weave:expect (sb-ext:process-alive-p process) :to-be nil)
           (cl-weave:expect
            (ntop.e2e::e2e-session-closed-p session)
            :to-be
            t))
      (setf (symbol-function 'ntop.e2e::%e2e-stop-session-process)
            original-stop)
      (setf (symbol-function 'cl-tty-kit:close-pty)
            original-close-pty)
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "recognizes the BTM and ntop help-search prompts"
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p
    "Search:   Type to search, Esc to close")
   :to-be
   t)
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p "Search: |")
   :to-be
   t)
  (cl-weave:expect
   (ntop.e2e::%route-help-search-open-state-p "Help shortcut: Search: /")
   :to-be
   nil)))
