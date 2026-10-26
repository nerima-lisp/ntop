(in-package #:ntop.e2e.test)

(cl-weave:describe
 "owned child readiness"
 (cl-weave:it
  "accepts an already visible owned PID marker without extending the deadline"
  (let* ((pid 12345)
         (deadline
           (+ (ntop.e2e::%now)
              (ntop.e2e::%seconds-to-ticks 30d0)))
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0 deadline 30d0))
         (raw
           (ntop.e2e::%route-octets
            (format nil "~c[1;1H~d ntop-owned-~d" #\Escape pid pid))))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
      (cl-weave:expect result :to-equal :ready)
      (cl-weave:expect (string= diagnostic "Owned PID 12345 marker row is visible.") :to-be t)
      (cl-weave:expect
       (ntop.e2e::e2e-session-deadline session)
       :to-equal deadline))))
 (cl-weave:it
  "returns marker-row and owned-dialog timeouts at the readiness deadline"
  (let* ((ntop.e2e::*route-process-readiness-timeout-seconds* 0d0)
         (pid 12345)
         (session
           (ntop.e2e::%make-e2e-session nil nil nil nil 0
                                        (ntop.e2e::%now) 0d0))
         (raw
           (make-array 0 :element-type '(unsigned-byte 8)
                       :adjustable t :fill-pointer 0)))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-marker-row session raw 12 80 pid)
      (cl-weave:expect result :to-equal :timeout)
      (cl-weave:expect
       (string= diagnostic
                "Owned PID 12345 marker row not ready before timeout.")
       :to-be t)
      (cl-weave:expect
       (search "marker row not ready before timeout" diagnostic)
       :to-satisfy (function plusp)))
    (multiple-value-bind (result diagnostic)
        (ntop.e2e::%route-wait-owned-dialog session raw 12 80 pid)
      (cl-weave:expect result :to-equal :timeout)
      (cl-weave:expect
       (string= diagnostic
                "Owned PID 12345 kill dialog not ready before timeout.")
       :to-be t))))
 (cl-weave:it
  "waits for an exact PID dialog after marker selection"
  (let* ((pid 12345)
         (marker (format nil "~d ntop-owned-~d" pid pid))
         (grid (criterion-test-grid 12 80 marker))
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
         (read-count 0)
         (read-symbol (find-symbol "%ROUTE-READ-INTERVAL" "NTOP.E2E"))
         (original-read-interval (symbol-function read-symbol)))
    (criterion-test-write grid 10 2 "38;5;0;48;5;12" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p grid pid)
     :to-be t)
    (criterion-test-write grid 10 2 "38;5;7;49" marker)
    (cl-weave:expect
     (ntop.e2e::%route-owned-marker-selected-p grid pid)
     :to-be nil)
    (unwind-protect
         (progn
           (setf (symbol-function read-symbol)
                 (lambda (ignored-session target ignored-seconds)
                   (declare (ignore ignored-session ignored-seconds))
                   (incf read-count)
                   (ntop.e2e::%append-octets target (pop frames))))
           (multiple-value-bind (result diagnostic)
               (ntop.e2e::%route-wait-owned-dialog
                session raw 12 80 pid)
             (cl-weave:expect result :to-equal :ready)
             (cl-weave:expect
              diagnostic
              :to-equal "Owned PID 12345 kill dialog is visible.")
             (cl-weave:expect read-count :to-equal 2)
             (cl-weave:expect
              (ntop.e2e::%route-owned-dialog-pid-matches-p
               (ntop.e2e::%route-grid raw 12 80) pid)
              :to-be t)))
      (setf (symbol-function read-symbol) original-read-interval))))
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
    "owned PID 62070 after confirmation input; child alive no; exact-PID dialog closed"))))

