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

(defun %route-read-initial-and-action (session rows columns rate timeout
                                               action-text context-text evidence-prefix)
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
    (let* ((before-grid (%route-grid raw rows columns))
           (before-action (project-criterion-grid before-grid))
           (resolved-action
            (if (member action-text
                        '(:basic-mode-right-arrow :focus-cpu :focus-memory)
                        :test #'eq)
                (%route-marker-input action-text before-grid)
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
      (dolist (action actions)
        (pty-send-input session (%route-octets action)
                        :timeout-seconds timeout)
        (multiple-value-bind (action-output)
            (pty-read-until-idle session
                                 :quiet-seconds *route-frame-quiet-seconds*
                                 :timeout-seconds timeout)
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
                                             evidence-prefix)
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
  (defun %route-run-sleep-child-side
    (program route subcase evidence-prefix rows columns rate timeout
     config-path evidence-directory)
  (let* ((context (or (getf subcase :context) ""))
         (dialog-context (string-equal context "kill dialog"))
         (sleep-route (string= (route-id route) "sleep-child"))
         (action-name (route-action route))
         (select-route
           (and (eq (route-kind route) :mouse)
                (string= action-name "Select process row")))
         (child (sb-ext:run-program
                 "/bin/bash"
                 (if select-route
                     '("-c" "exec -a ntop-owned sleep 5")
                     '("-c" "exec -a ntop-owned sleep 300"))
                 :wait nil))
         (pid (sb-ext:process-pid child))
         (process-route (and (not dialog-context)
                             (member action-name
                                     '("Delete selected process"
                                       "Open process kill dialog"
                                       "read_only"
                                       "disable_advanced_kill")
                                     :test #'string=)))
         (screen-rows (if select-route (max rows 512) rows))
         (base-args (%route-base-arguments program config-path route subcase
                                           evidence-directory))
         (args (append (remove "--process-command" base-args
                               :test #'string=)
                       (list "--process-command")))
         (session nil)
         (raw (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0))
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (passed nil)
         (sent nil)
         (actual "owned process row and dialog target were not proven")
         (error-value nil))
    (unwind-protect
         (handler-case
             (progn
               (setf session (launch-e2e-process
                              program args screen-rows columns
                              :environment (%route-environment)
                              :timeout-seconds timeout))
               (multiple-value-bind (initial)
                   (pty-read-until-idle
                    session :quiet-seconds *route-frame-quiet-seconds*
                    :first-frame-not-before
                    (* *route-first-frame-factor* (/ rate 1000d0))
                    :timeout-seconds timeout)
                 (%append-octets raw initial))
               (if select-route
                   (let* ((before-grid (%route-grid raw screen-rows columns))
                          (before-style
                            (%route-owned-marker-style before-grid pid)))
                     (unless before-style
                       (error "Owned PID ~d is not uniquely visible." pid))
                     (unless (sb-ext:process-alive-p child)
                       (error "Owned PID ~d exited before the click." pid))
                     (multiple-value-bind (action column row)
                         (%route-owned-row-click-input before-grid pid)
                       (pty-send-input session (%route-octets action)
                                       :timeout-seconds timeout)
                       (setf sent t)
                       (multiple-value-bind (clicked)
                           (pty-read-until-idle
                            session :quiet-seconds *route-frame-quiet-seconds*
                            :timeout-seconds timeout)
                         (%append-octets raw clicked))
                       (let* ((after-grid (%route-grid raw screen-rows columns))
                              (after-style
                                (%route-owned-marker-style after-grid pid))
                              (alive (sb-ext:process-alive-p child))
                              (changed
                                (and after-style
                                     (not (equal before-style after-style)))))
                         (setf passed (and alive changed)
                               actual
                               (format nil
                                       "clicked PID ~d at ~d,~d; style ~a; child alive ~a"
                                       pid column row
                                       (if changed "changed" "unchanged")
                                       (if alive "yes" "no"))))))
                   (progn
                     (pty-send-input
                      session
                      (%route-octets (format nil "/ntop-owned~c" #\Return))
                      :timeout-seconds timeout)
                     (multiple-value-bind (searched)
                         (pty-read-until-idle
                          session :quiet-seconds *route-frame-quiet-seconds*
                          :timeout-seconds timeout)
                       (%append-octets raw searched))
                     (unless (%route-owned-row-p
                              (%route-grid raw screen-rows columns) pid)
                       (error "Owned PID ~d and marker are not on one row." pid))
                     (pty-send-input session (%route-octets (string #\Return))
                                     :timeout-seconds timeout)
                     (multiple-value-bind (selected)
                         (pty-read-until-idle
                          session :quiet-seconds *route-frame-quiet-seconds*
                          :timeout-seconds timeout)
                       (%append-octets raw selected))
                     (unless (%route-owned-row-p
                              (%route-grid raw screen-rows columns) pid)
                       (error "Owned PID ~d was not selected after search."
                              pid))
                     (when (or dialog-context sleep-route)
                       (pty-send-input
                        session (%route-octets (%route-key-input "Delete"))
                        :timeout-seconds timeout)
                       (multiple-value-bind (opened)
                           (pty-read-until-idle
                            session :quiet-seconds *route-frame-quiet-seconds*
                            :timeout-seconds timeout)
                         (%append-octets raw opened))
                       (unless (%route-owned-dialog-p
                                (%route-grid raw screen-rows columns) pid)
                         (error "Setup Delete did not open owned PID ~d dialog."
                                pid)))
                     (let* ((grid (%route-grid raw screen-rows columns))
                            (before (project-criterion-grid grid))
                            (action
                              (cond (sleep-route (%route-key-input "Enter"))
                                    (process-route
                                     (%route-action-text
                                      route subcase rows columns))
                                    (t (%route-action-text
                                        route subcase rows columns)))))
                       (when (and dialog-context
                                  (not (%route-owned-dialog-p grid pid)))
                         (error "Kill action refused without exact PID ~d proof."
                                pid))
                       (when (and (eq (route-kind route) :mouse)
                                  (string= action-name "Choose kill action"))
                         (setf action (%route-safe-dialog-click-input grid)))
                       (unless (stringp action)
                         (error "No explicit safe input for kill route ~a."
                                action-name))
                       (unless (or sleep-route process-route
                                   (member action-name
                                           '("Accept current choice"
                                             "Close current modal"
                                             "Choose kill action"
                                             "Page up" "Page down"
                                             "Move left within widget"
                                             "Move right within widget"
                                             "Scroll up" "Scroll down")
                                           :test #'string=))
                         (error "Unsupported kill-dialog action ~a."
                                action-name))
                       (pty-send-input session (%route-octets action)
                                       :timeout-seconds timeout)
                       (setf sent t)
                       (multiple-value-bind (result)
                           (pty-read-until-idle
                            session :quiet-seconds *route-frame-quiet-seconds*
                            :timeout-seconds timeout)
                         (%append-octets raw result))
                       (let* ((after (%route-grid raw screen-rows columns))
                              (dialog-after (%route-owned-dialog-p after pid))
                              (changed
                                (not
                                 (criterion-projections-equal-p
                                  before (project-criterion-grid after))))
                              (alive (sb-ext:process-alive-p child)))
                         (cond
                           ((or sleep-route
                                (string= action-name "Accept current choice"))
                            (setf passed (and (not alive) (not dialog-after))
                                  actual
                                  (format nil
                                          "confirmed only owned PID ~d; child ended"
                                          pid)))
                           (process-route
                            (let* ((read-only-p (string= action-name "read_only"))
                                   (advanced-p
                                     (string= action-name "disable_advanced_kill"))
                                   (text (and advanced-p (%route-grid-text after)))
                                   (simple-p
                                     (and advanced-p (search "Yes" text)
                                          (search "No" text)
                                          (not (or (search "Select Signal" text)
                                                   (search "SIGTERM" text))))))
                              (setf passed
                                    (and alive
                                         (if read-only-p
                                             (not dialog-after)
                                             (and dialog-after
                                                  (or (not advanced-p) simple-p))))
                                    actual
                                    (format nil
                                            "alive ~a; dialog ~a; simple choices ~a"
                                            alive dialog-after simple-p))))
                           ((or (string= action-name "Close current modal")
                                (string= action-name "Choose kill action"))
                            (setf passed (and alive (not dialog-after))
                                  actual
                                  (format nil "safe cancel closed PID ~d dialog"
                                          pid)))
                           ((member action-name
                                    '("Page up" "Page down"
                                      "Move left within widget"
                                      "Move right within widget"
                                      "Scroll up" "Scroll down")
                                    :test #'string=)
                            (setf passed (and alive dialog-after changed)
                                  actual
                                  (format nil
                                          "owned PID ~d; dialog ~a; frame ~a"
                                          pid
                                          (if dialog-after "retained" "lost")
                                          (if changed "changed" "unchanged"))))
                           (t (error "No safe assertion for kill action ~a."
                                     action-name))))))))
           (error (condition)
             (setf error-value condition
                   actual (princ-to-string condition))))
      (when session
        (close-e2e-session session))
      (when (and child (sb-ext:process-alive-p child))
        (if select-route
            (sb-ext:process-wait child)
            (progn
              (sb-ext:process-kill child 9 :pid)
              (sb-ext:process-wait child))))
      (%route-write-ansi path raw)
      (list :passed (and passed sent (null error-value))
            :actual actual :evidence path))))
)
)
