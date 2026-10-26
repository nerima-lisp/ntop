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
  "waits for a complete resize frame after the initial quiet window"
  (let* ((script
           (format nil
                   (concatenate 'string
                                "import os,time~%"
                                "os.write(1,b'INITIAL_FRAME')~%"
                                "time.sleep(0.35)~%"
                                "os.write(1,b'RESIZE_FRAME_COMPLETE')~%"
                                "while True: time.sleep(1)~%")))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 3d0))
         (started (ntop.e2e::%now))
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait
             (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be
            t)
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session 0.05d0 2d0 0.08d0
                  (lambda (octets)
                    (search
                     "RESIZE_FRAME_COMPLETE"
                     (ntop.e2e::%octets-to-text octets)))))
           (let ((text (ntop.e2e::%octets-to-text output)))
             (cl-weave:expect
              (not (null (search "INITIAL_FRAME" text)))
              :to-be
              t)
             (cl-weave:expect
              (not (null (search "RESIZE_FRAME_COMPLETE" text)))
              :to-be
              t)
             (cl-weave:expect
              (>= (ntop.e2e::%ticks-to-seconds
                   (- (ntop.e2e::%now) started))
                  0.25d0)
              :to-be
              t)))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "keeps reading a CSI sequence split across the quiet period"
  (let* ((script
           (format nil
                   "import os, termios, time, tty
fd=0
saved=termios.tcgetattr(fd)
tty.setraw(fd)
os.write(1,bytes([27])+b'[31')
time.sleep(0.15)
os.write(1,b'mX')
while True:
    time.sleep(1)
"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 3d0))
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait
             (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session 0.05d0 2d0))
           (let* ((text (ntop.e2e::%octets-to-text output))
                  (result
                    (ntop.e2e:parse-ansi
                     text
                     (ntop.e2e:make-terminal-grid :rows 24 :cols 80)
                     (lambda (grid) (declare (ignore grid)) :accepted)
                     (lambda (condition)
                       (declare (ignore condition))
                       :rejected))))
             (cl-weave:expect
              text :to-equal (format nil "~c[31mX" (code-char 27)))
             (cl-weave:expect result :to-be :accepted)))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "keeps reading a UTF-8 code point split across the quiet period"
  (let* ((script
           (format nil
                   "import os, termios, time, tty
fd=0
tty.setraw(fd)
os.write(1,bytes([0xe3,0x81]))
time.sleep(0.15)
os.write(1,bytes([0x82]))
while True:
    time.sleep(1)
"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 3d0))
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait
             (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session 0.05d0 2d0))
           (cl-weave:expect
            (ntop.e2e::%octets-to-text output)
            :to-equal
            (format nil "~c" (code-char #x3042))))
      (ntop.e2e::close-e2e-session session))))
 (cl-weave:it
  "does not treat a UTF-8 continuation byte as standalone C1 CSI"
  (let* ((script
           (format nil
                   "import os, time, tty
fd=0
tty.setraw(fd)
time.sleep(0.05)
os.write(1,bytes([0xc3,0x9b]))
while True:
    time.sleep(1)
"))
         (session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" script) 24 80 :timeout-seconds 2d0))
         (output nil))
    (unwind-protect
         (progn
           (cl-weave:expect
            (cl-tty-kit:fd-wait
             (ntop.e2e::e2e-session-fd session) :input 2d0)
            :to-be t)
           (setf output
                 (ntop.e2e::%route-read-through-quiet-period
                  session 0.05d0 2d0))
           (cl-weave:expect
            (ntop.e2e::%octets-to-text output)
            :to-equal (format nil "~c" (code-char #x00db)))
           (cl-weave:expect
            (sb-ext:process-alive-p (ntop.e2e::e2e-session-process session))
            :to-be t))
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
  "reaps a SIGKILLed child after draining exhausts the shared deadline"
  (let* ((session
           (ntop.e2e::launch-e2e-process
            "python3" (list "-c" "import time; time.sleep(30)") 24 80
            :timeout-seconds 3d0))
         (process (ntop.e2e::e2e-session-process session))
         (original-drain
           (symbol-function 'ntop.e2e::%e2e-drain-session-until-exit))
         (original-now (symbol-function 'ntop.e2e::%now))
         (clock-value 0)
         (drain-called nil))
    (unwind-protect
         (progn
           (cl-weave:expect (sb-ext:process-alive-p process) :to-be t)
           (setf (symbol-function 'ntop.e2e::%now)
                 (lambda () clock-value))
           (setf (symbol-function 'ntop.e2e::%e2e-drain-session-until-exit)
                 (lambda (ignored-session ignored-timeout &optional ignored-operation)
                   (declare (ignore ignored-session ignored-timeout ignored-operation))
                   (setf drain-called t
                         clock-value (ntop.e2e::%seconds-to-ticks 1.1d0))
                   nil))
           (ntop.e2e::%reap-e2e-process session)
           (cl-weave:expect drain-called :to-be t)
           (cl-weave:expect (sb-ext:process-alive-p process) :to-be nil))
      (setf (symbol-function 'ntop.e2e::%e2e-drain-session-until-exit)
            original-drain)
      (setf (symbol-function 'ntop.e2e::%now) original-now)
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

(cl-weave:describe
 "route help section headings"
 (cl-weave:it
  "ignores the section-selection instruction"
  (let ((before (ntop.e2e:make-terminal-grid :rows 24 :cols 80))
        (after (ntop.e2e:make-terminal-grid :rows 24 :cols 80))
        (subcase (list :input "9")))
    (criterion-test-write
     before 8 0 "0" "│1, 2, 3, 4, 5, 6, 7, 8, 9 - Select help section")
    (criterion-test-write after 16 0 "0" "│6 - Temperature widget")
    (criterion-test-write after 18 0 "0" "│9 - Basic memory widget")
    (cl-weave:expect
     (ntop.e2e::%route-help-section-number before 9)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.e2e::%route-help-section-number after 9)
     :to-equal
     9)
    (cl-weave:expect
     (ntop.e2e::%route-help-section-transition-p subcase before after)
     :to-be
     t)))
 (cl-weave:it
  "uses the first visible heading when the target is also in view"
  (let ((before (ntop.e2e:make-terminal-grid :rows 24 :cols 80))
        (after (ntop.e2e:make-terminal-grid :rows 24 :cols 80))
        (subcase (list :input "9")))
    (criterion-test-write before 4 0 "0" "│1 - General")
    (criterion-test-write
     before 5 0 "0" "│1, 2, 3, 4, 5, 6, 7, 8, 9 - Select help section")
    (criterion-test-write before 9 0 "0" "│9 - Basic memory widget")
    (criterion-test-write after 16 0 "0" "│6 - Temperature widget")
    (criterion-test-write after 18 0 "0" "│9 - Basic memory widget")
    (cl-weave:expect
     (ntop.e2e::%route-help-section-number before)
     :to-equal
     1)
    (cl-weave:expect
     (ntop.e2e::%route-help-section-number before 9)
     :to-equal
     9)
    (cl-weave:expect
     (ntop.e2e::%route-help-section-number after 9)
     :to-equal
     9)
    (cl-weave:expect
     (ntop.e2e::%route-help-section-transition-p subcase before after)
     :to-be
     t))))
