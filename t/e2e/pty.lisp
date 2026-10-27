(in-package #:ntop.e2e)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-posix))

(defmacro with-e2e-operation-timeout ((operation deadline timeout-seconds) &body
                                      body)
  `(handler-case (sb-ext:with-timeout (%ticks-to-seconds (- ,deadline (%now)))
                                      ,@body)
     (sb-ext:timeout ()
       (%signal-timeout ,operation ,timeout-seconds))))

(define-condition e2e-timeout
  (error)
  ((operation :initarg :operation :reader e2e-timeout-operation)
   (timeout-seconds :initarg :timeout-seconds :reader e2e-timeout-seconds))
  (:report
   (lambda (condition stream)
     (format stream
             "E2E ~A timed out after ~,3F seconds."
             (e2e-timeout-operation condition)
             (e2e-timeout-seconds condition)))))

(defstruct
    (e2e-session
     (:constructor %make-e2e-session
                   (pty process fd pid started-at deadline timeout-seconds)))
  pty
  process
  fd
  pid
  started-at
  deadline
  timeout-seconds
  (reaped-p nil)
  (closed-p nil)
  (output (make-array 0 :element-type (quote (unsigned-byte 8))
                      :adjustable t :fill-pointer 0))
  (first-output-seconds nil)
  (first-visible-output-seconds nil))

(defun frame-ready-p (started-at last-output-at
                                 now
                                 first-frame-not-before-at
                                 quiet-seconds)
  (and (realp started-at)
       (realp last-output-at)
       (realp now)
       (realp first-frame-not-before-at)
       (realp quiet-seconds)
       (>= last-output-at started-at)
       (>= now first-frame-not-before-at)
       (>= (- now last-output-at) quiet-seconds)))
(defun %pty-visible-screen-output-p (octets)
  (let ((state :text))
    (loop for byte across octets
          do (case state
               (:text
                (cond
                  ((= byte 27)
                   (setf state :escape))
                  ((and (<= 32 byte) (/= byte 127))
                   (return-from %pty-visible-screen-output-p t))))
               (:escape
                (setf state
                      (case byte
                        (91 :csi)
                        ((80 93 94 95) :string)
                        (otherwise :text))))
               (:csi
                (when (<= 64 byte 126)
                  (setf state :text)))
               (:string
                (cond
                  ((= byte 7)
                   (setf state :text))
                  ((= byte 27)
                   (setf state :string-escape))))
               (:string-escape
                (setf state
                      (if (= byte 92) :text :string)))))
    nil))
(defun %e2e-session-record-output (session octets)
  (when (plusp (length octets))
    (let ((output (e2e-session-output session))
          (elapsed (%elapsed-seconds session (%now))))
      (loop for index below (length octets)
            do (vector-push-extend (aref octets index) output))
      (unless (e2e-session-first-output-seconds session)
        (setf (e2e-session-first-output-seconds session) elapsed))
      (when (and (null (e2e-session-first-visible-output-seconds session))
                 (%pty-visible-screen-output-p output))
        (setf (e2e-session-first-visible-output-seconds session) elapsed)))))
(defun %e2e-drain-session-until-exit
    (session timeout-seconds &optional (operation "child exit"))
  (let ((process (e2e-session-process session))
        (deadline (+ (%now) (%seconds-to-ticks timeout-seconds)))
        (buffer (make-array 4096 :element-type (quote (unsigned-byte 8)))))
    (loop for now = (%now)
          do (when (>= now deadline)
               (%signal-timeout operation timeout-seconds))
             (let ((alive (sb-ext:process-alive-p process)))
               (if (cl-tty-kit:fd-wait
                    (e2e-session-fd session)
                    :input
                    (min 0.05d0 (%ticks-to-seconds (- deadline now))))
                   (let ((count (cl-tty-kit:fd-read-octets
                                 (e2e-session-fd session) buffer)))
                     (cond
                       ((and (integerp count) (plusp count))
                        (%e2e-session-record-output
                         session (subseq buffer 0 count)))
                       ((and (integerp count) (zerop count))
                        (unless alive (return (e2e-session-output session))))
                       ((and (null count) (not alive))
                        (return (e2e-session-output session)))))
                 (unless alive
                   (return (e2e-session-output session))))))))

(defun %validate-seconds (seconds name)
  (unless
      (and (realp seconds)
           (<= 0 seconds)
           (< seconds most-positive-double-float))
    (error "~A must be a finite non-negative real number." name))
  seconds)

(defun %seconds-to-ticks (seconds)
  (ceiling (* seconds internal-time-units-per-second)))

(defun %ticks-to-seconds (ticks)
  (/ ticks internal-time-units-per-second))

(defun %now ()
  (get-internal-real-time))

(defun %elapsed-seconds (session timestamp)
  (%ticks-to-seconds (- timestamp (e2e-session-started-at session))))

(defun %signal-timeout (operation timeout-seconds)
  (error 'e2e-timeout :operation operation :timeout-seconds timeout-seconds))

(progn
  (defvar *e2e-deadline*
    nil)
  (defun %effective-timeout-seconds (timeout-seconds operation
                                                     &optional
                                                     (now (%now)))
    (let* ((timeout-seconds
            (%validate-seconds timeout-seconds "TIMEOUT-SECONDS"))
           (requested-deadline (+ now (%seconds-to-ticks timeout-seconds)))
           (deadline
            (if *e2e-deadline*
                (min requested-deadline *e2e-deadline*)
                requested-deadline))
           (remaining (- deadline now)))
      (when (<= remaining 0)
        (%signal-timeout operation timeout-seconds))
      (%ticks-to-seconds remaining)))
  (defun %deadline-from-start (timeout-seconds operation started-at)
    (let ((effective-timeout
           (%effective-timeout-seconds timeout-seconds operation started-at)))
      (values (+ started-at (%seconds-to-ticks effective-timeout))
              effective-timeout))))

(defun %ensure-session-open (session)
  (when (e2e-session-closed-p session)
    (error "The E2E PTY session is closed."))
  session)

(declaim (ftype function close-e2e-session))
(declaim (ftype function %pty-answer-cursor-query))

(defun %deadline-for-call (session operation timeout-seconds)
  (%ensure-session-open session)
  (let* ((now (%now))
         (overall-deadline (e2e-session-deadline session))
         (local-timeout
          (unless (null timeout-seconds)
            (%validate-seconds timeout-seconds "TIMEOUT-SECONDS")))
         (local-deadline
          (when local-timeout
            (+ now (%seconds-to-ticks local-timeout))))
         (deadline
          (min overall-deadline
               (or local-deadline overall-deadline)
               (or *e2e-deadline* overall-deadline)))
         (deadline-seconds (%ticks-to-seconds (max 0 (- deadline now)))))
    (when (<= deadline now)
      (%signal-timeout operation deadline-seconds))
    (values deadline deadline-seconds)))

(defun %call-with-session-cleanup (session thunk)
  (let ((completed nil))
    (unwind-protect
        (multiple-value-prog1 (funcall thunk)
          (setf completed t))
      (unless completed
        (close-e2e-session session)))))

(defun launch-e2e-process (program args
                                   rows
                                   cols
                                   &key
                                   environment
                                   (timeout-seconds 30d0))
  (%validate-seconds timeout-seconds "TIMEOUT-SECONDS")
  (unless (stringp program)
    (error "PROGRAM must be a string."))
  (unless (and (integerp rows) (plusp rows))
    (error "ROWS must be a positive integer."))
  (unless (and (integerp cols) (plusp cols))
    (error "COLS must be a positive integer."))
  (let ((launch-started-at (%now))
        (pty nil)
        (session nil)
        (completed nil))
    (multiple-value-bind (launch-deadline launch-timeout)
        (%deadline-from-start timeout-seconds "launch" launch-started-at)
      (unwind-protect
          (progn
            (setf pty (with-e2e-operation-timeout
                       ("launch" launch-deadline launch-timeout)
                       (cl-tty-kit:make-pty :program
                                            program
                                            :args
                                            args
                                            :environment
                                            environment)))
            (let* ((started-at (%now))
                   (process (cl-tty-kit:pty-process pty))
                   (fd (cl-tty-kit:pty-fd pty))
                   (pid (cl-tty-kit:pty-pid pty)))
              (unless process
                (error "PTY launch returned no process."))
              (unless (and (integerp pid) (plusp pid))
                (error "PTY launch returned no process id."))
              (multiple-value-bind (deadline session-timeout)
                  (%deadline-from-start timeout-seconds "launch" started-at)
                (setf session (%make-e2e-session pty
                                                 process
                                                 fd
                                                 pid
                                                 started-at
                                                 deadline
                                                 session-timeout))
                (when (>= (%now) deadline)
                  (%signal-timeout "launch" session-timeout))
                (with-e2e-operation-timeout ("launch" deadline session-timeout)
                                            (cl-tty-kit:pty-resize pty
                                                                   cols
                                                                   rows))
                (setf completed t)
                session)))
        (unless completed
          (when pty
            (cl-tty-kit:close-pty pty)))))))

(defun pty-read-until-idle (session &key
                                    (quiet-seconds 0.3d0)
                                    ((:first-frame-not-before
                                      first-frame-not-before-seconds))
                                    timeout-seconds completion-predicate)
  (%validate-seconds quiet-seconds "QUIET-SECONDS")
  (when first-frame-not-before-seconds
    (%validate-seconds first-frame-not-before-seconds "FIRST-FRAME-NOT-BEFORE"))
  (%call-with-session-cleanup session
                              (lambda ()
                                (multiple-value-bind (deadline deadline-seconds)
                                    (%deadline-for-call session
                                                        "read"
                                                        timeout-seconds)
                                  (let* ((started-at
                                          (e2e-session-started-at session))
                                         (first-frame-at
                                          (+ started-at
                                             (%seconds-to-ticks
                                              (or first-frame-not-before-seconds
                                                  0))))
                                         (quiet-ticks
                                          (%seconds-to-ticks quiet-seconds))
                                         (quiet-ticks-seconds
                                          (%ticks-to-seconds quiet-ticks))
                                         (buffer
                                          (make-array 4096
                                                      :element-type
                                                      '(unsigned-byte 8)))
                                         (output
                                          (make-array 0
                                                      :element-type
                                                      '(unsigned-byte 8)
                                                      :adjustable
                                                      t
                                                      :fill-pointer
                                                      0))
                                         (last-output-at nil)
                                         (query-scan-index 0)
                                         (awaiting-post-query-output-p nil))
                                    (loop for
                                          now
                                          =
                                          (%now)
                                          do
                                          (when (>= now deadline)
                                            (%signal-timeout "read"
                                                             deadline-seconds))
                                          (when
                                              (and last-output-at (not awaiting-post-query-output-p)
                                                   (and
                                                    (or (null first-frame-not-before-seconds)
                                                        (%pty-visible-screen-output-p output))
                                                    (and
                                                     (frame-ready-p
                                                      (%ticks-to-seconds started-at)
                                                     (%ticks-to-seconds last-output-at)
                                                     (%ticks-to-seconds now)
                                                     (%ticks-to-seconds first-frame-at)
                                                      quiet-ticks-seconds)
                                                     (or (null completion-predicate)
                                                         (funcall completion-predicate output)))))
                                            (return
                                             (values
                                              (make-array (length output)
                                                          :element-type
                                                          '(unsigned-byte 8)
                                                          :initial-contents
                                                          output)
                                              (%elapsed-seconds session
                                                                last-output-at)
                                              (%elapsed-seconds session now))))
                                          (let* ((idle-deadline
                                                  (when (and last-output-at
                                                             (not awaiting-post-query-output-p)
                                                             (or
                                                              (null first-frame-not-before-seconds)
                                                              (%pty-visible-screen-output-p
                                                               output)))
                                                     (when (or (null completion-predicate)
                                                           (funcall completion-predicate
                                                            output))
                                                       (+ last-output-at quiet-ticks))))
                                                 (wake-at
                                                  (if idle-deadline
                                                      (min deadline
                                                           (max idle-deadline
                                                                first-frame-at))
                                                    deadline))
                                                 (wait-seconds
                                                  (%ticks-to-seconds
                                                   (- wake-at now))))
                                            (unless (plusp wait-seconds)
                                              (error
                                               "PTY frame timing did not advance."))
                                            (when
                                                (with-e2e-operation-timeout
                                                 ("read" deadline
                                                              deadline-seconds)
                                                 (cl-tty-kit:fd-wait
                                                  (e2e-session-fd session)
                                                  :input
                                                  wait-seconds))
                                              (let ((count
                                                     (with-e2e-operation-timeout
                                                      ("read" deadline
                                                              deadline-seconds)
                                                      (cl-tty-kit:fd-read-octets
                                                       (e2e-session-fd session)
                                                       buffer))))
                                                (cond
                                                 ((null count) nil)
                                                 ((zerop count)
                                                  (error
                                                   "PTY closed before a frame was complete."))
                                                 (t
                                                  (progn
  (loop for index below count
        do (vector-push-extend (aref buffer index) output))
  (%e2e-session-record-output session (subseq buffer 0 count)))
                                                  (setf awaiting-post-query-output-p nil)
                                                  (multiple-value-bind
                                                      (next-index answered-p output-after-query-p)
                                                      (%pty-answer-cursor-query
                                                       session output query-scan-index deadline)
                                                    (setf query-scan-index next-index)
                                                    (when answered-p
                                                      (setf awaiting-post-query-output-p
                                                            (not output-after-query-p))))
                                                  (setf last-output-at (%now)))))))))))))

(defun pty-send-input (session octets &key timeout-seconds)
  (%call-with-session-cleanup session
                              (lambda ()
                                (multiple-value-bind (deadline deadline-seconds)
                                    (%deadline-for-call session
                                                        "write"
                                                        timeout-seconds)
                                  (unless
                                      (typep octets '(vector (unsigned-byte 8)))
                                    (error
                                     "OCTETS must be a vector of unsigned bytes."))
                                  (let ((input
                                         (make-array (length octets)
                                                     :element-type
                                                     '(unsigned-byte 8)
                                                     :initial-contents
                                                     octets)))
                                    (when (plusp (length input))
                                      (let ((now (%now)))
                                        (when (>= now deadline)
                                          (%signal-timeout "write"
                                                           deadline-seconds))
                                        (unless
                                            (with-e2e-operation-timeout
                                             ("write wait" deadline
                                              deadline-seconds)
                                             (cl-tty-kit:fd-wait
                                              (e2e-session-fd session)
                                              :output
                                              (%ticks-to-seconds
                                               (- deadline now))))
                                          (%signal-timeout "write"
                                                           deadline-seconds))
                                        (let ((count
                                               (with-e2e-operation-timeout
                                                ("write" deadline
                                                 deadline-seconds)
                                                (cl-tty-kit:fd-write-octets
                                                 (e2e-session-fd session)
                                                 input))))
                                          (unless
                                              (and (integerp count)
                                                   (<= 0 count (length input)))
                                            (error "PTY write count ~S invalid for ~D octets."
                                             count
                                             (length input)))
                                          (unless (= count (length input))
                                            (error
                                             "PTY wrote only ~D of ~D input octets in one write."
                                             count
                                             (length input)))))
                                    (%elapsed-seconds session (%now))))))))

(defun %pty-answer-cursor-query
    (session output start-index deadline)
  (loop with query-start = start-index
        with answered-p = nil
        with last-query-end = nil
        for query-position = (search #(27 91 54 110) output
                                     :start2 query-start
                                     :test #'=)
        while query-position
        do (let* ((query-end (+ query-position 4))
                  (remaining (%ticks-to-seconds (- deadline (%now)))))
             (when (<= remaining 0)
               (%signal-timeout "PTY terminal response" 0d0))
             (pty-send-input
              session
              (make-array 6
                          :element-type '(unsigned-byte 8)
                          :initial-contents '(27 91 49 59 49 82))
              :timeout-seconds remaining)
             (setf answered-p t
                   last-query-end query-end
                   query-start query-end))
        finally
        (return
         (values (max query-start (- (length output) 3))
                 answered-p
                 (and last-query-end
                      (< last-query-end (length output)))))))

(defun pty-resize-session (session rows cols &key timeout-seconds)
  (%call-with-session-cleanup session
                              (lambda ()
                                (multiple-value-bind (deadline deadline-seconds)
                                    (%deadline-for-call session
                                                        "resize"
                                                        timeout-seconds)
                                  (unless (and (integerp rows) (plusp rows))
                                    (error "ROWS must be a positive integer."))
                                  (unless (and (integerp cols) (plusp cols))
                                    (error "COLS must be a positive integer."))
                                  (let ((resized-at (%now)))
                                    (when (>= resized-at deadline)
                                      (%signal-timeout "resize"
                                                       deadline-seconds))
                                    (with-e2e-operation-timeout
                                     ("resize" deadline deadline-seconds)
                                     (cl-tty-kit:pty-resize
                                      (e2e-session-pty session)
                                      cols
                                      rows))
                                    ;; The Darwin PTY resize probe received no SIGWINCH.
                                    #+darwin
                                    (when (sb-ext:process-alive-p
                                           (e2e-session-process session))
                                      (sb-ext:process-kill
                                       (e2e-session-process session)
                                       sb-posix:sigwinch :pid))
                                    (%elapsed-seconds session resized-at))))))

