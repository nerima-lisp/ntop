(in-package #:ntop.e2e.test)

(cl-weave:describe
 "owned child readiness"
 (cl-weave:it
  "captures a stable owned PID marker without extending the deadline"
  (let* ((pid 12345)
         (deadline
           (+ (ntop.e2e::%now)
              (ntop.e2e::%seconds-to-ticks 30d0)))
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0 deadline 30d0))
         (raw
           (ntop.e2e::%route-octets
            (format nil "~c[1;1H~d ntop-owned-~d" #\Escape pid pid)))
         (grid (ntop.e2e::%route-grid raw 12 80))
         (capture-symbol (find-symbol "%ROUTE-CAPTURE-FRAME" "NTOP.E2E"))
         (original-capture (symbol-function capture-symbol))
         (capture-count 0)
         (ntop.e2e::*route-frame-stability-seconds* 0.05d0)
         (ntop.e2e::*route-frame-capture-side* :candidate))
    (unwind-protect
         (progn
           (setf (symbol-function capture-symbol)
                 (lambda (capture-session prefix rows columns
                          &key side stage timeout-seconds stability-seconds
                            completion-predicate allow-unchanged-p)
                   (incf capture-count)
                   (cl-weave:expect capture-session :to-be session)
                   (cl-weave:expect prefix :to-be raw)
                   (cl-weave:expect rows :to-equal 12)
                   (cl-weave:expect columns :to-equal 80)
                   (cl-weave:expect side :to-equal :candidate)
                   (cl-weave:expect stage :to-equal :owned-pid-marker-row-readiness)
                   (cl-weave:expect
                    timeout-seconds
                    :to-equal ntop.e2e::*route-process-readiness-timeout-seconds*)
                   (cl-weave:expect stability-seconds :to-equal 0.05d0)
                   (cl-weave:expect allow-unchanged-p :to-be nil)
                   (cl-weave:expect (funcall completion-predicate grid) :to-be t)
                   (values prefix grid (list :stage stage))))
           (multiple-value-bind (result diagnostic updated)
               (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
             (cl-weave:expect result :to-equal :ready)
             (cl-weave:expect
              diagnostic
              :to-equal "Owned PID 12345 marker row is visible.")
             (cl-weave:expect (equalp updated raw) :to-be t)
             (cl-weave:expect capture-count :to-equal 1)
             (cl-weave:expect
              (ntop.e2e::e2e-session-deadline session)
              :to-equal deadline)))
      (setf (symbol-function capture-symbol) original-capture))))
 (cl-weave:it
  "reports marker-row and owned-dialog capture timeout metadata"
  (let* ((ntop.e2e::*route-process-readiness-timeout-seconds* 0d0)
         (ntop.e2e::*route-frame-stability-seconds* 0.05d0)
         (ntop.e2e::*route-frame-capture-side* :candidate)
         (pid 12345)
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0
                                        (ntop.e2e::%now) 0d0))
         (raw
           (make-array 0 :element-type '(unsigned-byte 8)
                       :adjustable t :fill-pointer 0))
         (capture-symbol (find-symbol "%ROUTE-CAPTURE-FRAME" "NTOP.E2E"))
         (original-capture (symbol-function capture-symbol))
         (capture-count 0))
    (unwind-protect
         (progn
           (setf (symbol-function capture-symbol)
                 (lambda (capture-session prefix rows columns
                          &key side stage timeout-seconds stability-seconds
                            completion-predicate allow-unchanged-p)
                   (declare (ignore capture-session prefix rows columns
                                    stability-seconds completion-predicate
                                    allow-unchanged-p))
                   (incf capture-count)
                   (error 'ntop.e2e::route-frame-capture-timeout
                          :operation "route frame capture"
                          :timeout-seconds timeout-seconds
                          :side side
                          :stage stage
                          :elapsed-seconds 0d0
                          :last-output-seconds nil
                          :stable-signature "test-signature")))
           (multiple-value-bind (result diagnostic)
               (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
             (cl-weave:expect result :to-equal :timeout)
             (cl-weave:expect
              (search "Owned PID 12345 marker row not ready before timeout."
                      diagnostic)
              :to-satisfy (function identity))
             (cl-weave:expect
              (search "stage: :owned-pid-marker-row-readiness"
                      diagnostic
                      :test (function char-equal))
              :to-satisfy (function plusp)))
           (multiple-value-bind (result diagnostic)
               (ntop.e2e::%route-wait-owned-dialog session raw 12 80 pid)
             (cl-weave:expect result :to-equal :timeout)
             (cl-weave:expect
              (search "Owned PID 12345 kill dialog not ready before timeout."
                      diagnostic)
              :to-satisfy (function identity))
             (cl-weave:expect
              (search "stage: :owned-pid-dialog-readiness"
                      diagnostic
                      :test (function char-equal))
              :to-satisfy (function plusp))
             (cl-weave:expect capture-count :to-equal 2)))
      (setf (symbol-function capture-symbol) original-capture))))
 (cl-weave:it
  "waits for an exact PID dialog with one capture call"
  (let* ((pid 12345)
         (marker (format nil "~d ntop-owned-~d" pid pid))
         (marker-grid (criterion-test-grid 12 80 marker))
         (session
           (ntop.e2e::%make-e2e-session
            nil nil nil nil 0
            (+ (ntop.e2e::%now) (ntop.e2e::%seconds-to-ticks 30d0))
            30d0))
         (raw
           (make-array 0 :element-type '(unsigned-byte 8)
                       :adjustable t :fill-pointer 0))
         (frames
           (list
            (ntop.e2e::%route-octets
             (format nil "~c[3;3HKill process sleep with PID 54321?"
                     #\Escape))
            (ntop.e2e::%route-octets
             (format nil "~c[3;3HKill process sleep with PID ~d?"
                     #\Escape pid))))
         (capture-symbol (find-symbol "%ROUTE-CAPTURE-FRAME" "NTOP.E2E"))
         (original-capture (symbol-function capture-symbol))
         (capture-count 0)
         (predicate-count 0)
         (ntop.e2e::*route-frame-stability-seconds* 0.05d0))
    (ntop.e2e.test::criterion-test-write
     marker-grid 10 2 "38;5;0;48;5;12" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p marker-grid pid)
     :to-be t)
    (ntop.e2e.test::criterion-test-write marker-grid 10 2 "38;5;7;49" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p marker-grid pid)
     :to-be nil)
    (unwind-protect
         (progn
           (setf (symbol-function capture-symbol)
                 (lambda (capture-session prefix rows columns
                          &key side stage timeout-seconds stability-seconds
                            completion-predicate allow-unchanged-p)
                   (declare (ignore capture-session side timeout-seconds
                                    stability-seconds allow-unchanged-p))
                   (incf capture-count)
                   (cl-weave:expect stage :to-equal :owned-pid-dialog-readiness)
                   (let ((updated prefix)
                         (grid nil))
                     (dolist (frame frames)
                       (setf updated (ntop.e2e::%append-octets updated frame)
                             grid (ntop.e2e::%route-grid updated rows columns))
                       (when (funcall completion-predicate grid)
                         (incf predicate-count)))
                     (values updated grid (list :stage stage)))))
           (multiple-value-bind (result diagnostic updated)
               (ntop.e2e::%route-wait-owned-dialog session raw 12 80 pid)
             (cl-weave:expect result :to-equal :ready)
             (cl-weave:expect
              diagnostic
              :to-equal "Owned PID 12345 kill dialog is visible.")
             (cl-weave:expect capture-count :to-equal 1)
             (cl-weave:expect predicate-count :to-equal 1)
             (cl-weave:expect
              (ntop.e2e::%route-owned-dialog-pid-matches-p
               (ntop.e2e::%route-grid updated 12 80) pid)
              :to-be t)))
      (setf (symbol-function capture-symbol) original-capture))))
 (cl-weave:it
  "reports observed child and dialog state after confirmation input"
  (progn
   (cl-weave:expect
    (ntop.e2e::%route-sleep-child-action-actual 62070 t t)
    :to-equal
    "owned PID 62070 after confirmation input; child alive yes; exact-PID dialog open")
   (cl-weave:expect
    (ntop.e2e::%route-sleep-child-action-actual 62070 nil nil)
    :to-equal
    "owned PID 62070 after confirmation input; child alive no; exact-PID dialog closed")))
 (cl-weave:it
  "retains PTY output and the capture error when owned-child capture fails"
  (let* ((session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0 0 1d0))
         (initial (ntop.e2e::%route-octets "partial terminal frame"))
         (drained (ntop.e2e::%route-octets " after close"))
         (expected
          (make-array 0
                      :element-type (quote (unsigned-byte 8))
                      :adjustable t
                      :fill-pointer 0))
         (saved-output nil)
         (route
           (ntop.e2e::make-route-case
            :id "key-36" :kind :key :action "Open process kill dialog"))
         (subcase (list :context "kill dialog"))
         (symbols
          (list (find-symbol "%ROUTE-BASE-ARGUMENTS" "NTOP.E2E")
                (find-symbol "LAUNCH-E2E-PROCESS" "NTOP.E2E")
                (find-symbol "%ROUTE-CHILD-CAPTURE-FRAME" "NTOP.E2E")
                (find-symbol "CLOSE-E2E-SESSION" "NTOP.E2E")
                (find-symbol "%ROUTE-WRITE-ANSI" "NTOP.E2E")))
         (original-functions (mapcar (function symbol-function) symbols)))
    (setf (ntop.e2e::e2e-session-output session)
          (ntop.e2e::%append-octets
           (ntop.e2e::e2e-session-output session) initial)
          expected (ntop.e2e::%append-octets expected initial)
          expected (ntop.e2e::%append-octets expected drained))
    (unwind-protect
         (progn
           (setf (symbol-function (first symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   nil)
                 (symbol-function (second symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   session)
                 (symbol-function (third symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (error "simulated frame capture failure"))
                 (symbol-function (fourth symbols))
                 (lambda (capture-session)
                   (setf (ntop.e2e::e2e-session-output capture-session)
                         (ntop.e2e::%append-octets
                          (ntop.e2e::e2e-session-output capture-session)
                          drained)))
                 (symbol-function (fifth symbols))
                 (lambda (path octets)
                   (setf saved-output (copy-seq octets))
                   (namestring path)))
           (let ((result
                   (ntop.e2e::%route-run-sleep-child-side
                    "btm" route subcase "capture-failure" 60 220 250 120 nil nil)))
             (cl-weave:expect (equalp saved-output expected) :to-be t)
             (cl-weave:expect (getf result :passed) :to-be nil)
             (cl-weave:expect (getf result :capture-error) :to-be-truthy)))
      (loop for symbol in symbols
            for original in original-functions
            do (setf (symbol-function symbol) original))))))

