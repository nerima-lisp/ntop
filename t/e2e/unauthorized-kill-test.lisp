(in-package #:ntop.e2e.test)

(defun %unauthorized-kill-whitespace-fields (text)
  (loop with start = nil
        with fields = nil
        for index from 0 to (length text)
        for separator = (or (= index (length text))
                            (find (char text index)
                                  '(#\Space #\Tab #\Return #\Newline)))
        do (cond
             ((and separator start)
              (push (subseq text start index) fields)
              (setf start nil))
             ((and (not separator) (null start))
              (setf start index)))
        finally (return (nreverse fields))))

(defun %unauthorized-kill-process-snapshot ()
  (let* ((result
           (host-kit:run-program
            "ps"
            '("-p" "1" "-o" "uid=" "-o" "pid=" "-o" "ppid="
              "-o" "lstart=" "-o" "comm=")
            :timeout 5d0
            :max-output-characters 4096))
         (output (host-kit:process-result-stdout result)))
    (when (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
      (error "PID 1 identity output was truncated; no signal was sent."))
    (unless (zerop (host-kit:process-result-exit-code result))
      (error "Unable to inspect PID 1; no signal was sent."))
    (let ((fields (%unauthorized-kill-whitespace-fields output)))
      (unless (>= (length fields) 9)
        (error "PID 1 identity output was incomplete; no signal was sent."))
      (let ((uid (first fields))
            (pid (second fields))
            (ppid (third fields))
            (start-time (format nil "~{~A~^ ~}" (subseq fields 3 8)))
            (command (format nil "~{~A~^ ~}" (nthcdr 8 fields))))
        (unless (and (string= uid "0")
                     (string= pid "1")
                     (string= ppid "0")
                     (plusp (length start-time))
                     (member command '("launchd" "/sbin/launchd")
                             :test #'string=))
          (error "Expected root-owned PID 1 launchd; no signal was sent."))
        (list uid pid ppid start-time command)))))

(defun %unauthorized-kill-require-nonroot (effective-uid real-uid)
  (unless (and (integerp effective-uid)
               (integerp real-uid)
               (plusp effective-uid)
               (= effective-uid real-uid)
               (= effective-uid (sb-posix:geteuid))
               (= real-uid (sb-posix:getuid)))
    (error "The E2E must run as the same non-root real/effective UID; no signal was sent.")))

(defun %unauthorized-kill-child-environment ()
  (cons "TERM=xterm-256color"
        (remove-if
         (lambda (entry)
           (or (and (>= (length entry) 5)
                    (string= entry "TERM=" :end1 5))
               (and (>= (length entry) 9)
                    (string= entry "NO_COLOR=" :end1 9))))
         (sb-ext:posix-environ))))

(progn
  (defun %unauthorized-kill-child-test-setup ()
    (format nil
            "(progn
               (defvar ntop.cli::*unauthorized-kill-e2e-sampled-at* 0d0)
               (defmethod ntop.shared.application:collect-process-sample
                   ((collector (eql :unauthorized-kill-e2e))
                    on-sample
                    on-unavailable)
                 (declare (ignore collector on-unavailable))
                 (incf ntop.cli::*unauthorized-kill-e2e-sampled-at* 1d0)
                 (funcall
                  on-sample
                  (ntop.process.domain:make-process-sample
                   :sampled-at ntop.cli::*unauthorized-kill-e2e-sampled-at*
                   :processes
                   (list
                    (ntop.process.domain:make-process-record
                     :pid 1
                     :ppid 0
                     :name ~S
                     :command ~S
                     :uid 0
                     :user-name ~S
                     :start-time-unix-seconds 0d0))
                   :signals
                   (list
                    (ntop.process.domain:make-process-signal
                     :operating-system ~S
                     :name ~S
                     :number 15)))))
               (setf ntop.cli::*application-runner*
                     (lambda (config collectors clock render &rest options)
                       (declare (ignore collectors))
                       (apply
                        (function ntop.app.infrastructure:run-application)
                        config
                        (list :process :unauthorized-kill-e2e)
                        clock
                        render
                        options))))"
            "launchd" "/sbin/launchd" "root" "Darwin" "SIGTERM"))
  (defun %unauthorized-kill-child-arguments ()
    (list "--noinform"
          "--disable-debugger"
          "--eval" "(require :asdf)"
          "--eval" "(asdf:load-system \"ntop\")"
          "--eval" (%unauthorized-kill-child-test-setup)
          "--eval" "(setf sb-ext:*posix-argv* (list \"ntop\"))"
          "--eval" "(funcall (find-symbol \"MAIN\" \"NTOP.CLI\"))"
          "--quit")))

(defun %unauthorized-kill-append-octets (target source)
  (loop for octet across source
        do (vector-push-extend octet target))
  target)

(defun %unauthorized-kill-grid (octets rows columns)
  (let ((grid (ntop.e2e:make-terminal-grid :rows rows :cols columns)))
    (ntop.e2e:parse-ansi
     (sb-ext:octets-to-string octets :external-format :utf-8)
     grid
     #'identity
     (lambda (condition) (error condition)))))

(defun %unauthorized-kill-grid-text (grid)
  (with-output-to-string (output)
    (dotimes (row (ntop.e2e::grid-height grid))
      (dotimes (column (ntop.e2e::grid-width grid))
        (write-char
         (ntop.e2e:cell-character
          (ntop.e2e:grid-cell-at grid row column))
         output))
      (terpri output))))

(defun %unauthorized-kill-row-text (grid row)
  (with-output-to-string (output)
    (dotimes (column (ntop.e2e::grid-width grid))
      (write-char
       (ntop.e2e:cell-character
        (ntop.e2e:grid-cell-at grid row column))
       output))))

(defun %unauthorized-kill-sigterm-selected-p (grid)
  (loop for row below (ntop.e2e::grid-height grid)
      for line = (%unauthorized-kill-row-text grid row)
      for column = (search "15: SIGTERM" line :test #'char-equal)
      when column
        return
        (not
         (equal
          (ntop.e2e:cell-background
           (ntop.e2e:grid-cell-at grid row column))
          '(:default)))
      finally (return nil)))

(defun %unauthorized-kill-safe-dialog-p (grid)
  (let ((text (%unauthorized-kill-grid-text grid)))
    (and (search "Select Signal" text)
         (search "Kill process 'launchd' with PID 1?" text)
         (search "15: SIGTERM" text :test #'char-equal)
         (%unauthorized-kill-sigterm-selected-p grid))))

(defun %unauthorized-kill-read-screen (session output rows columns
                                       &key (timeout-seconds 10d0)
                                         first-frame-not-before)
  (handler-case
      (multiple-value-bind (frame-octets)
          (ntop.e2e::pty-read-until-idle
           session
           :quiet-seconds 0.3d0
           :first-frame-not-before first-frame-not-before
           :timeout-seconds timeout-seconds)
        (%unauthorized-kill-append-octets output frame-octets))
    (error (condition)
      (let* ((child-process (ntop.e2e::e2e-session-process session))
             (exit-status (sb-ext:process-exit-code child-process))
             (error-stream (sb-ext:process-error child-process))
             (child-error-output
               (when (and (integerp exit-status) (streamp error-stream))
                 (with-output-to-string (stream)
                   (loop for line = (read-line error-stream nil nil)
                         while line
                         do (write-line line stream))))))
        (error "PTY read failed, child status ~S: ~A~%stderr: ~A~%screen: ~A"
               exit-status
               condition
               (or child-error-output "<not captured>")
               (if (plusp (length output))
                   (%unauthorized-kill-grid-text
                    (%unauthorized-kill-grid output rows columns))
                   "<no terminal output>")))))
  (%unauthorized-kill-grid output rows columns))

(cl-weave:describe
 "Darwin unauthorized process termination"
 (cl-weave:it
  "shows and dismisses the real kill dialog permission failure"
  (let* ((effective-uid (sb-posix:geteuid))
         (real-uid (sb-posix:getuid))
         (initial-snapshot (%unauthorized-kill-process-snapshot))
         (rows 48)
         (columns 160)
         (output (make-array 0
                             :element-type (quote (unsigned-byte 8))
                             :adjustable t
                             :fill-pointer 0)))
    (%unauthorized-kill-require-nonroot effective-uid real-uid)
    (ntop.e2e::with-e2e-session
     (session
      "sbcl"
      (%unauthorized-kill-child-arguments)
      rows
      columns
      :environment (%unauthorized-kill-child-environment)
      :timeout-seconds 35d0)
     (%unauthorized-kill-read-screen
      session output rows columns
      :timeout-seconds 25d0
      :first-frame-not-before 5d0)
     (ntop.e2e::pty-send-input
      session
      (sb-ext:string-to-octets "/"
                               :external-format :utf-8))
     (%unauthorized-kill-read-screen session output rows columns)
     (ntop.e2e::pty-send-input
      session
      (sb-ext:string-to-octets
       (concatenate 'string
                    "launchd"
                    (string (code-char 13))
                    "dd")
       :external-format :utf-8))
     (let ((dialog-grid
             (%unauthorized-kill-read-screen session output rows columns)))
       (unless (%unauthorized-kill-safe-dialog-p dialog-grid)
         (error "Expected PID 1 launchd with highlighted SIGTERM; no signal was sent:~%~A"
               (%unauthorized-kill-grid-text dialog-grid)))
       (%unauthorized-kill-require-nonroot effective-uid real-uid)
       (unless (equal initial-snapshot (%unauthorized-kill-process-snapshot))
         (error "PID 1 identity changed before confirmation; no signal was sent."))
       (ntop.e2e::pty-send-input
        session
        (sb-ext:string-to-octets (string #\Return) :external-format :utf-8))
       (let* ((error-grid
                (%unauthorized-kill-read-screen session output rows columns))
              (error-text (%unauthorized-kill-grid-text error-grid)))
         (dolist (line
                  (list "Error"
                        "Failed to kill process launchd (1):"
                        (concatenate
                         (quote string)
                         "Error code 1 - the calling process does not have the permissions "
                         "to terminate the target process(es).")
                        "Please press ENTER or ESC to close this dialog."))
           (cl-weave:expect
            (not (null (search line error-text :test (function char-equal))))
            :to-be
            t))
         (ntop.e2e::pty-send-input
          session
          (sb-ext:string-to-octets (string #\Return) :external-format :utf-8))
         (let* ((closed-grid
                  (%unauthorized-kill-read-screen session output rows columns))
                (closed-text (%unauthorized-kill-grid-text closed-grid))
                (final-snapshot (%unauthorized-kill-process-snapshot)))
           (cl-weave:expect
            (not (null
                  (search "Failed to kill process launchd (1):"
                          closed-text :test (function char-equal))))
            :to-be
            nil)
           (cl-weave:expect (equal final-snapshot initial-snapshot)
                            :to-be
                            t))))))))
