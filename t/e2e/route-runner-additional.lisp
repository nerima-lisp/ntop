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
        (%append-octets raw context-output)
        (when (and (string= (route-id route) "mouse-69")
                   (string-equal (or (getf subcase :context) "") "help"))
          (dolist (input
                   (%route-mouse-action-inputs
                    route (list :context "help" :gesture "wheel-down")
                    (%route-grid raw rows columns) rows columns))
            (pty-send-input session (%route-octets input)
                            :timeout-seconds timeout)
            (multiple-value-bind (preparation-output)
                (pty-read-until-idle session
                                     :quiet-seconds *route-frame-quiet-seconds*
                                     :timeout-seconds timeout)
              (%append-octets raw preparation-output))))))
    (let* ((before-grid
            (progn
              (when (and (string= (route-id route) "mouse-70")
                         (string-equal (or (getf subcase :context) "") "graphs"))
                ;; Start wheel-down at 45s; the CPU graph opens at its 60s limit.
                (dolist (input
                         (%route-mouse-action-inputs
                          route (list :context "graphs" :gesture "wheel-up")
                          (%route-grid raw rows columns) rows columns))
                  (pty-send-input session (%route-octets input)
                                  :timeout-seconds timeout)
                  (multiple-value-bind (preparation-output)
                      (pty-read-until-idle session
                                           :quiet-seconds *route-frame-quiet-seconds*
                                           :timeout-seconds timeout)
                    (%append-octets raw preparation-output))))
              (when (and (eq (route-kind route) :key)
                         (member (route-action route)
                                 (list "Zoom graph out" "Reset graph zoom")
                                 :test (function string=))
                         (string-equal (or (getf subcase :context) "") "graphs"))
                ;; Move off the 60s limit before zoom-out or reset.
                (pty-send-input session (%route-octets "+")
                                :timeout-seconds timeout)
                (multiple-value-bind (preparation-output)
                    (pty-read-until-idle session
                                         :quiet-seconds *route-frame-quiet-seconds*
                                         :timeout-seconds timeout)
                  (%append-octets raw preparation-output)))
              (progn
                (when (and (eq (route-kind route) :key)
                           (string= (route-action route) "Select help section")
                           (string= (getf subcase :input) "1"))
                  ;; Start on section 2 so selecting section 1 has a visible transition.
                  (pty-send-input session (%route-octets "2")
                                  :timeout-seconds timeout)
                  (multiple-value-bind (preparation-output)
                      (pty-read-until-idle session
                                           :quiet-seconds *route-frame-quiet-seconds*
                                           :timeout-seconds timeout)
                    (%append-octets raw preparation-output)))
                (when (and (eq (route-kind route) :key)
                           (string= (route-action route) "Select help section")
                           (member (getf subcase :input) '("7" "8" "9") :test #'string=))
                  ;; Start at section 1 so 7-9 produce a measurable jump to the bottom.
                  (pty-send-input session (%route-octets "1")
                                  :timeout-seconds timeout)
                  (multiple-value-bind (preparation-output)
                      (pty-read-until-idle session
                                           :quiet-seconds *route-frame-quiet-seconds*
                                           :timeout-seconds timeout)
                    (%append-octets raw preparation-output))))
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
            (cond
             ((and (eq (route-kind route) :mouse)
                   (string= (route-action route) "Ignore pointer motion"))
              (%route-read-through-quiet-period
               session *route-frame-quiet-seconds* timeout))
             ((and (eq (route-kind route) :flag)
                   (string= (ntop.app.domain:parity-flag-snake-case
                             (route-entry route))
                            "disable_keys")
                   (not (getf subcase :invalid-value)))
              (%route-read-through-quiet-period
               session *route-frame-quiet-seconds* timeout))
             (t
              (pty-read-until-idle session
                                   :quiet-seconds *route-frame-quiet-seconds*
                                   :timeout-seconds timeout)))
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
         (context-text (%route-context-input context route subcase))
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
  (defun %route-help-visible-sections (grid)
    (loop for row below (grid-height grid)
          for line = (criterion-row-text grid row)
          for separator = (search " - " line)
          for number =
            (and separator
                 (plusp separator)
                 (digit-char-p (char line (1- separator))))
          when number collect (cons number row)))
  (defun %route-help-navigation-state-p (route subcase before-grid after-grid)
    (when (and (string-equal (or (getf subcase :context) "") "help")
               before-grid after-grid)
      (let* ((action (route-action route))
             (before (%route-help-visible-sections before-grid))
             (after (%route-help-visible-sections after-grid))
             (before-first (first before))
             (before-last (car (last before)))
             (after-last (car (last after))))
        (cond
          ((string= action "Go to first row")
           (let ((before-two (assoc 2 before))
                 (after-one (assoc 1 after))
                 (after-two (assoc 2 after)))
             (and before-two after-one after-two
                  (> (cdr after-two) (cdr before-two)))))
          ((string= action "Go to last row")
           (let ((before-two (assoc 2 before))
                 (before-nine (assoc 9 before))
                 (after-nine (assoc 9 after)))
             (and before-two after-nine
                  (or (null before-nine)
                      (/= (cdr before-nine) (cdr after-nine))))))
          ((string= action "Move to previous row")
           (let ((old (assoc 2 before)) (new (assoc 2 after)))
             (and old new (= (cdr new) (1+ (cdr old))))))
          ((string= action "Move to next row")
           (let* ((old (assoc 1 before))
                  (row (and old (cdr old)))
                  (old-line (and row (criterion-row-text before-grid row)))
                  (new-line (and row (criterion-row-text after-grid row))))
             (and old-line new-line
                  (search "1 - General" old-line)
                  (search "q, Q, Ctrl-c" new-line)
                  (not (string= old-line new-line)))))
          ((member action '("Page up" "Half-page up") :test #'string=)
           (and before-last after-last
                (= (car before-last) 9)
                (< (car after-last) (car before-last))))
          ((member action '("Page down" "Half-page down") :test #'string=)
           (and before-first before-last after-last
                (= (car before-first) 1)
                (> (car after-last) (car before-last))))
          (t nil)))))
  (defun %route-modal-action-state-p (route subcase before-grid after-grid)
    (let* ((id (route-id route))
           (action (route-action route))
           (context (or (getf subcase :context) ""))
           (before (%route-grid-text before-grid))
           (after (%route-grid-text after-grid)))
      (cond
        ((and (string= id "key-35")
              (string= action "Accept current choice"))
         (cond
           ((string-equal context "help search")
            (and (%route-help-search-query-visible-p before-grid "process")
                 (search "Help" after :test #'char-equal)
                 (not (search "Search: process" after :test #'char-equal))))
           ((member context '("process search" "process sort" "kill dialog")
                    :test #'string-equal)
            :manual)
           (t nil)))
        ((and (string= id "key-36")
              (string= action "Close current modal"))
         (cond
           ((string-equal context "process search")
            (and (search "Case(F1)" before :test #'char-equal)
                 (search "> " before)
                 (not (search "Case(F1)" after :test #'char-equal))))
           ((string-equal context "help search")
            (and (%route-help-search-query-visible-p before-grid "process")
                 (search "Help" after :test #'char-equal)
                 (not (search "Search: process" after :test #'char-equal))))
           ((string-equal context "process sort")
            (and (search "Sort By" before :test #'char-equal)
                 (not (search "Sort By" after :test #'char-equal))))
           ((string-equal context "expanded widget")
            (and (search "Esc to go back" before :test #'char-equal)
                 (not (search "Esc to go back" after :test #'char-equal))
                 (every (lambda (label) (search label after :test #'char-equal))
                        '("CPU" "Memory" "Network"))))
           ((member context '("kill dialog" "help") :test #'string-equal)
 :manual)
           (t nil)))
        (t nil))))
