(in-package #:ntop.e2e)

(defun %route-wait-for-exit (session timeout-seconds)
  (let ((deadline (+ (%now) (%seconds-to-ticks timeout-seconds)))
        (process (e2e-session-process session)))
    (loop while (sb-ext:process-alive-p process)
          do (when (>= (%now) deadline)
               (error "Route child ~d did not exit within ~,1f seconds."
                      (e2e-session-pid session) timeout-seconds))
             (sleep 0.05d0))
    (sb-ext:process-exit-code process)))

(defun %route-drain-exited-session (session timeout-seconds)
  (let* ((deadline (+ (%now) (%seconds-to-ticks timeout-seconds)))
         (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
         (output (make-array 0 :element-type '(unsigned-byte 8)
                             :adjustable t :fill-pointer 0)))
    (loop
      for alive = (sb-ext:process-alive-p (e2e-session-process session))
      for ready = (cl-tty-kit:fd-wait (e2e-session-fd session) :input 0.05d0)
      do (when (>= (%now) deadline)
           (error "Route child ~d did not finish output within ~,1f seconds."
                  (e2e-session-pid session) timeout-seconds))
         (if ready
             (let ((count (cl-tty-kit:fd-read-octets
                           (e2e-session-fd session) buffer)))
               (cond
                 ((and (integerp count) (plusp count))
                  (loop for index below count
                        do (vector-push-extend (aref buffer index) output)))
                 ((and (integerp count) (zerop count))
                  (unless alive (return output)))
                 ((and (null count) (not alive)) (return output))))
             (unless alive (return output))))))

(defun %route-parse-output (program arguments timeout-seconds)
  (let* ((result (host-kit:run-program program arguments
                                       :timeout timeout-seconds
                                       :max-output-characters (* 1024 1024)))
         (stdout (host-kit:process-result-stdout result))
         (stderr (host-kit:process-result-stderr result)))
    (when (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
      (error "Route CLI output exceeded its capture limit."))
    (list :exit-code (host-kit:process-result-exit-code result)
          :stdout stdout
          :stderr stderr)))

(defun %route-read-through-quiet-period (session quiet-seconds timeout-seconds)
  (multiple-value-bind (deadline deadline-seconds)
      (%deadline-for-call session "read" timeout-seconds)
    (let ((quiet-deadline (+ (%now) (%seconds-to-ticks quiet-seconds)))
          (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
          (output (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
      (loop for now = (%now)
            do (when (>= now deadline)
                 (%signal-timeout "read" deadline-seconds))
               (when (>= now quiet-deadline)
                 (return (values output (%elapsed-seconds session now))))
               (let ((wait-until (min deadline quiet-deadline)))
                 (when (cl-tty-kit:fd-wait
                        (e2e-session-fd session) :input
                        (%ticks-to-seconds (- wait-until now)))
                   (let ((count (cl-tty-kit:fd-read-octets
                                 (e2e-session-fd session) buffer)))
                     (cond
                       ((null count) nil)
                       ((zerop count)
                        (error "PTY closed before the quiet period completed."))
                       (t
                        (loop for index below count
                              do (vector-push-extend (aref buffer index) output))
                        (setf quiet-deadline
                              (+ (%now) (%seconds-to-ticks quiet-seconds))))))))))))

(defun %route-read-initial-and-action (session rows columns rate timeout
                                               action-text context-text evidence-prefix
                                               route subcase)
  (let ((raw (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (initial-projection nil))
    (multiple-value-bind (initial)
        (pty-read-until-idle session
                             :quiet-seconds *route-frame-quiet-seconds*
                             :first-frame-not-before
                             (* *route-first-frame-factor* (/ rate 1000d0))
                             :timeout-seconds timeout)
      (%append-octets raw initial)
      (setf initial-projection
            (project-criterion-grid (%route-grid raw rows columns))))
    (when (plusp (length context-text))
      (pty-send-input session (%route-octets context-text)
                      :timeout-seconds timeout)
      (multiple-value-bind (context-output)
          (pty-read-until-idle session
                               :quiet-seconds *route-frame-quiet-seconds*
                               :timeout-seconds timeout)
        (%append-octets raw context-output)))
    (let* ((before-grid (progn
                          (%route-write-ansi
                           (merge-pathnames (format nil "~a.ansi" evidence-prefix)) raw)
                          (%route-grid raw rows columns)))
           (before-action (project-criterion-grid before-grid))
           (resolved-action
            (if (member action-text
                        (quote (:basic-mode-right-arrow :focus-cpu :focus-memory
                                                        :route-mouse-action))
                        :test (function eq))
                (%route-marker-input action-text before-grid route subcase rows columns)
              action-text))
           (actions (cond
                     ((stringp resolved-action)
                      (if (plusp (length resolved-action))
                          (list resolved-action)
                        nil))
                     ((listp resolved-action) resolved-action)
                     (t (error "Invalid route action input: ~s"
                               resolved-action))))
           (action-grids nil))
      (%route-write-ansi
       (merge-pathnames (format nil "~a.ansi" evidence-prefix)) raw)
      (dolist (action actions)
        (pty-send-input session (%route-octets action)
                        :timeout-seconds timeout)
        (multiple-value-bind (action-output)
            (if (and (eq (route-kind route) :mouse)
                     (string= (route-action route) "Ignore pointer motion"))
                (%route-read-through-quiet-period
                 session *route-frame-quiet-seconds* timeout)
              (pty-read-until-idle session
                                   :quiet-seconds *route-frame-quiet-seconds*
                                   :timeout-seconds timeout))
          (%append-octets raw action-output))
        (push (%route-grid raw rows columns) action-grids))
      (let* ((ansi-path (merge-pathnames
                         (format nil "~a.ansi" evidence-prefix)))
             (grid (%route-grid raw rows columns)))
        (values grid (project-criterion-grid grid)
                (%route-write-ansi ansi-path raw)
                before-action initial-projection before-grid
                (nreverse action-grids))))))

(defun %route-run-interactive-side (program route subcase evidence-prefix
                                    rows columns rate timeout config-path
                                    evidence-directory)
  (let* ((context (or (getf subcase :context) ""))
         (context-text (%route-context-input context))
         (action-text (%route-action-text route subcase rows columns))
         (args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session (launch-e2e-process program args rows columns
                                      :environment (%route-environment)
                                      :timeout-seconds timeout)))
    (unwind-protect
         (multiple-value-bind (grid projection evidence before initial
                               initial-grid action-grids)
             (%route-read-initial-and-action session rows columns rate timeout
                                             action-text context-text
                                             evidence-prefix route subcase)
           (list :grid grid :projection projection
                 :initial-projection (or before initial)
                 :initial-grid initial-grid
                 :action-grids action-grids
                 :evidence evidence
                 :actual (format nil "captured a ~d by ~d terminal frame"
                                 columns rows)))
      (close-e2e-session session))))
(defun %route-run-exit-side (program route subcase evidence-prefix rows columns
                             rate timeout config-path evidence-directory)
  (let* ((args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session (launch-e2e-process program args rows columns
                                      :environment (%route-environment)
                                      :timeout-seconds timeout))
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (exit-code nil)
         (error-value nil))
    (unwind-protect
         (handler-case
             (progn
               (multiple-value-bind (initial)
                   (pty-read-until-idle
                    session :quiet-seconds *route-frame-quiet-seconds*
                    :first-frame-not-before
                    (* *route-first-frame-factor* (/ rate 1000d0))
                    :timeout-seconds timeout)
                 (%route-write-ansi path initial))
               (pty-send-input session
                               (%route-octets
                                (%route-action-text route subcase rows columns))
                               :timeout-seconds timeout)
               (setf exit-code (%route-wait-for-exit session timeout)))
           (error (condition) (setf error-value condition)))
      (close-e2e-session session))
    (list :passed (and (null error-value) (eql exit-code 0))
          :exit-code exit-code
          :actual (if error-value
                      (princ-to-string error-value)
                      (format nil "child exit status ~a" exit-code))
          :evidence path)))
(defun %route-run-resize-side (program route subcase evidence-prefix rate timeout
                               config-path evidence-directory)
  (let* ((args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session (launch-e2e-process program args 24 80
                                      :environment (%route-environment)
                                      :timeout-seconds timeout))
         (raw (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix))))
    (unwind-protect
         (progn
           (multiple-value-bind (initial)
               (pty-read-until-idle
                session :quiet-seconds *route-frame-quiet-seconds*
                :first-frame-not-before (* *route-first-frame-factor*
                                           (/ rate 1000d0))
                :timeout-seconds timeout)
             (%append-octets raw initial))
           (let ((initial-grid (%route-grid raw 24 80)))
           (pty-resize-session session 36 120 :timeout-seconds timeout)
           (multiple-value-bind (resized)
               (pty-read-until-idle session
                                    :quiet-seconds *route-frame-quiet-seconds*
                                    :timeout-seconds timeout)
             (%append-octets raw resized))
           (let ((grid (%route-grid raw 36 120)))
             (%route-write-ansi path raw)
             (list :grid grid :projection (project-criterion-grid grid)
                   :initial-projection nil :initial-grid initial-grid
                   :action-grids (list grid) :evidence (namestring path)
                   :actual (format nil "terminal frame is ~d by ~d"
                                   (grid-width grid) (grid-height grid))))))
      (close-e2e-session session))))

(progn
  (defun %route-pid-line-p (text pid)
    (let ((token (format nil "~d" pid)))
      (loop with start = 0
            for pos = (search token text :start2 start)
            while pos
            for end = (+ pos (length token))
            when (and (or (zerop pos)
                          (not (digit-char-p (char text (1- pos)))))
                      (or (= end (length text))
                          (not (digit-char-p (char text end)))))
              return t
            do (setf start (1+ pos))
            finally (return nil))))
  (progn
  (defun %route-owned-row-index (grid pid)
    (let ((matches
            (loop for row below (grid-height grid)
                  for text = (criterion-row-text grid row)
                  when (and (search "ntop-owned" text)
                            (%route-pid-line-p text pid))
                    collect row)))
      (and (= 1 (length matches)) (first matches))))
  (defun %route-owned-row-p (grid pid)
    (not (null (%route-owned-row-index grid pid))))
  (defun %route-owned-marker-style (grid pid)
    (let* ((row (%route-owned-row-index grid pid))
           (text (and row (criterion-row-text grid row)))
           (marker (and text (search "ntop-owned" text))))
      (when marker
        (loop for column from marker below (+ marker (length "ntop-owned"))
              for cell = (grid-cell-at grid row column)
              collect (list (copy-tree (cell-foreground cell))
                            (copy-tree (cell-background cell))
                            (cell-bold-p cell))))))
  (defun %route-owned-row-click-input (grid pid)
    (let* ((row (%route-owned-row-index grid pid))
           (text (and row (criterion-row-text grid row)))
           (column (and text (search "ntop-owned" text))))
      (unless (and row column)
        (error "Cannot click a unique visible row for owned PID ~d." pid))
      (values (format nil "~c[<0;~d;~dM" #\Escape
                      (1+ column) (1+ row))
              (1+ column)
              (1+ row))))
)
  (defun %route-owned-dialog-p (grid pid)
    (= 1 (loop for row below (grid-height grid)
               for text = (criterion-row-text grid row)
               count (and (search "Kill process" text :test #'char-equal)
                          (search "ntop-owned" text)
                          (search "PID" text :test #'char-equal)
                          (%route-pid-line-p text pid)))))
  (progn
  (defun %route-safe-dialog-click-input (grid)
  (let* ((title-row
           (loop for row below (grid-height grid)
                 for text = (criterion-row-text grid row)
                 when (or (search "Confirm Kill Process" text)
                          (search "Select Signal" text))
                   return row))
         (choice-row (and title-row (+ title-row 2)))
         (text (and choice-row (criterion-row-text grid choice-row)))
         (cancel (and text (search "0: Cancel" text)))
         (no (and text (search "No" text)))
         (point (cond (cancel (list (+ cancel 4) choice-row))
                      (no (list (1+ no) choice-row)))))
    (unless point
      (error "No safe No or Cancel choice appears under the dialog title."))
    (format nil "~c[<0;~d;~dM" #\Escape (1+ (first point))
            (1+ (second point)))))
  )
)
