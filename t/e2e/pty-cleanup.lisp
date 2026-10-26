(in-package #:ntop.e2e)

(progn
  (defun %reap-e2e-process (session)
  (let ((process (e2e-session-process session)))
    (when (sb-ext:process-alive-p process)
      (sb-ext:process-kill process 9 :pid)
      (%e2e-drain-session-until-exit session 1d0 "child reap"))
    (let ((reap-deadline (+ (%now) (%seconds-to-ticks 1d0))))
      (with-e2e-operation-timeout ("child reap" reap-deadline 1d0)
        (sb-ext:process-wait process)))
    (when (sb-ext:process-alive-p process)
      (%signal-timeout "child reap" 1d0))
    (sb-ext:process-close process)))
  (defun %e2e-stop-session-process (session)
    (let ((process (e2e-session-process session)))
      (dolist (signal (list 1 15 9))
        (when (sb-ext:process-alive-p process)
          (sb-ext:process-kill process signal :pid)
          (handler-case
              (%e2e-drain-session-until-exit session 0.2d0 "child reap")
            (e2e-timeout () nil))))
      (when (sb-ext:process-alive-p process)
        (%signal-timeout "child reap" 0.6d0))
      process))
  (defun close-e2e-session (session)
  (unless (e2e-session-closed-p session)
    (let ((deadline (+ (%now) (%seconds-to-ticks 1d0)))
          (close-attempted-p nil)
          (close-error nil))
      (handler-case
          (progn
            (%e2e-stop-session-process session)
            (%e2e-drain-session-until-exit session 1d0 "close")
            (setf close-attempted-p t)
            (with-e2e-operation-timeout ("close" deadline 1d0)
              (cl-tty-kit:close-pty (e2e-session-pty session)))
            (setf (e2e-session-closed-p session) t))
        (error (condition)
          (setf close-error condition)))
      (when close-error
        (%reap-e2e-process session)
        (unless close-attempted-p
          (with-e2e-operation-timeout
              ("close fallback" (+ (%now) (%seconds-to-ticks 1d0)) 1d0)
            (cl-tty-kit:close-pty (e2e-session-pty session))))
        (setf (e2e-session-closed-p session) t)
        (warn "PTY cleanup used the child-reap fallback: ~a" close-error))))
  session))

(defmacro with-e2e-session ((session program args rows cols &rest options) &body
                            body)
  `(let ((,session (launch-e2e-process ,program ,args ,rows ,cols ,@options)))
     (unwind-protect
         (progn
           ,@body)
       (close-e2e-session ,session))))
