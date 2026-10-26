(in-package #:ntop.e2e)

(progn
  (defun %e2e-wait-for-process-exit (session timeout-seconds operation)
    (let ((process (e2e-session-process session)))
      (unless (e2e-session-reaped-p session)
        (let* ((started-at (%now))
               (deadline (+ started-at
                            (%seconds-to-ticks timeout-seconds))))
          (when (>= started-at deadline)
            (%signal-timeout operation timeout-seconds))
          (with-e2e-operation-timeout (operation deadline timeout-seconds)
            (sb-ext:process-wait process))
          (when (sb-ext:process-alive-p process)
            (%signal-timeout operation timeout-seconds))
          (setf (e2e-session-reaped-p session) t)))
      process))
  (defun %e2e-drain-session-after-exit (session timeout-seconds operation)
    (let ((deadline (+ (%now) (%seconds-to-ticks timeout-seconds)))
          (buffer (make-array 4096 :element-type '(unsigned-byte 8))))
      (loop for now = (%now)
            do (when (>= now deadline)
                 (%signal-timeout operation timeout-seconds))
               (let ((wait-seconds
                      (min 0.05d0
                           (%ticks-to-seconds (- deadline now)))))
                 (unless (with-e2e-operation-timeout
                             (operation deadline timeout-seconds)
                           (cl-tty-kit:fd-wait
                            (e2e-session-fd session) :input wait-seconds))
                   (return (e2e-session-output session))))
               (let ((count
                      (with-e2e-operation-timeout
                          (operation deadline timeout-seconds)
                        (cl-tty-kit:fd-read-octets
                         (e2e-session-fd session) buffer))))
                 (cond
                   ((and (integerp count) (plusp count))
                    (%e2e-session-record-output
                     session (subseq buffer 0 count)))
                   ((or (null count) (and (integerp count) (zerop count)))
                    (return (e2e-session-output session)))
                   (t
                    (error "PTY read count ~S is invalid." count)))))))
  (defun %reap-e2e-process (session)
  (let ((process (e2e-session-process session)))
    (unless (e2e-session-reaped-p session)
      (when (sb-ext:process-alive-p process)
        (sb-ext:process-kill process 9 :pid))
      (handler-case
          (%e2e-drain-session-until-exit session 1d0 "child reap drain")
        (e2e-timeout () nil))
      (%e2e-wait-for-process-exit session 1d0 "child reap"))
    (%e2e-drain-session-after-exit session 1d0 "child output drain")
    process))
  (defun %e2e-stop-session-process (session)
  (let ((process (e2e-session-process session)))
    (dolist (signal '(1 15))
      (unless (e2e-session-reaped-p session)
        (when (sb-ext:process-alive-p process)
          (sb-ext:process-kill process signal :pid)
          (handler-case
              (progn
                (%e2e-drain-session-until-exit session 1d0 "child exit")
                (%e2e-wait-for-process-exit session 1d0 "child exit"))
            (e2e-timeout () nil)))))
    (unless (e2e-session-reaped-p session)
      (when (sb-ext:process-alive-p process)
        (sb-ext:process-kill process 9 :pid))
      (%e2e-wait-for-process-exit session 1d0 "child reap"))
    (%e2e-drain-session-after-exit session 1d0 "child output drain")
    process))
  (defun close-e2e-session (session)
    (unless (e2e-session-closed-p session)
      (let ((close-attempted-p nil)
            (close-error nil))
        (handler-case
            (progn
              (%e2e-stop-session-process session)
              (setf close-attempted-p t)
              (with-e2e-operation-timeout
                  ("close" (+ (%now) (%seconds-to-ticks 1d0)) 1d0)
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
