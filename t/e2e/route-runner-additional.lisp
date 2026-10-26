(in-package #:ntop.e2e)
(defun %route-wait-for-exit (session timeout-seconds)
  (let ((process (e2e-session-process session)))
    (%e2e-drain-session-until-exit session timeout-seconds "child exit")
    (sb-ext:process-exit-code process)))
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
(defun %route-incomplete-escape-suffix-p (octets)
  (let* ((end (length octets))
         (escape-position (position #x1b octets :from-end t))
         (csi-position (position #x9b octets :from-end t)))
    (labels ((csi-state (start)
               (loop for index from start below end
                     for byte = (aref octets index)
                     do (cond
                          ((<= #x40 byte #x7e) (return :complete))
                          ((<= #x20 byte #x3f))
                          (t (return :invalid)))
                     finally (return :incomplete))))
      (or (and escape-position (= (1+ escape-position) end))
          (and escape-position
               (< (1+ escape-position) end)
               (= (aref octets (1+ escape-position)) #x5b)
               (eq (csi-state (+ escape-position 2)) :incomplete))
          (and csi-position
               (not (%route-utf8-continuation-byte-p octets csi-position))
               (or (null escape-position)
                   (> csi-position escape-position))
               (eq (csi-state (1+ csi-position)) :incomplete))))))
(defun %route-read-through-quiet-period (session quiet-seconds timeout-seconds
                                         &optional (initial-quiet-seconds quiet-seconds)
                                           completion-predicate)
  (%validate-seconds quiet-seconds "QUIET-SECONDS")
  (%validate-seconds initial-quiet-seconds "INITIAL-QUIET-SECONDS")
  (multiple-value-bind (deadline deadline-seconds)
      (%deadline-for-call session "read" timeout-seconds)
    (let ((quiet-deadline (+ (%now) (%seconds-to-ticks initial-quiet-seconds)))
          (buffer (make-array 4096 :element-type (quote (unsigned-byte 8))))
          (output (make-array 0 :element-type (quote (unsigned-byte 8))
                              :adjustable t :fill-pointer 0))
          (query-scan-index 0))
      (loop for now = (%now)
            do (when (>= now deadline)
                 (%signal-timeout "read" deadline-seconds))
               (when (>= now quiet-deadline)
                 (if (or (%route-incomplete-escape-suffix-p output)
                         (%route-incomplete-utf8-suffix-p output)
                         (and completion-predicate
                              (not (funcall completion-predicate output))))
                     (setf quiet-deadline deadline)
                     (return (values output (%elapsed-seconds session now)))))
               (let ((wait-until (min deadline quiet-deadline)))
                 (when (cl-tty-kit:fd-wait
                        (e2e-session-fd session) :input
                        (%ticks-to-seconds (- wait-until now)))
                   (let ((count (cl-tty-kit:fd-read-octets (e2e-session-fd session) buffer)))
                     (cond
                       ((zerop count)
                        (error "PTY closed before the quiet period completed."))
                       (t
                        (loop for index below count do
                          (vector-push-extend (aref buffer index) output))
                        (%e2e-session-record-output session (subseq buffer 0 count))
                        (multiple-value-bind (next-index)
                            (%pty-answer-cursor-query
                             session output query-scan-index deadline)
                          (setf query-scan-index next-index
                                quiet-deadline (+ (%now)
                                                  (%ticks-to-seconds quiet-seconds)))))))))))))
(defun %route-close-session-with-evidence (session evidence-prefix)
  (let* ((ansi-path
           (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (timing-path
           (merge-pathnames (format nil "~a.timing" evidence-prefix)))
         (output (e2e-session-output session))
         (timing-text
           (format nil
                   "first-output-seconds: ~a~%first-visible-screen-output-seconds: ~a~%"
                   (or (e2e-session-first-output-seconds session)
                       "not observed")
                   (or (e2e-session-first-visible-output-seconds session)
                       "not observed"))))
    (unwind-protect
         (progn
           (%route-write-ansi ansi-path output)
           (when (plusp (length output))
             (with-open-file (stream timing-path
                                     :direction :output
                                     :if-exists :supersede
                                     :if-does-not-exist :create)
               (write-string timing-text stream))))
      (close-e2e-session session))
    (namestring ansi-path)))
(defun %route-read-initial-and-action (session rows columns rate timeout
                                               action-text context-text evidence-prefix
                                               route subcase arguments)
  (let ((raw (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (initial-projection nil)
        (initial-grid nil))
    (multiple-value-bind (initial)
        (pty-read-until-idle
         session
         :quiet-seconds *route-frame-quiet-seconds*
         :first-frame-not-before
         (* *route-first-frame-factor* (/ rate 1000d0))
         :timeout-seconds
         (%route-process-startup-timeout route subcase timeout))
      (%append-octets raw initial)
      (when (%route-process-data-required-p route subcase)
        (multiple-value-bind (readiness-result readiness-diagnostic)
            (%route-wait-process-data session raw rows columns arguments)
          (when (eq :timeout readiness-result)
            (let* ((grid (%route-grid raw rows columns))
                   (projection (project-criterion-grid grid))
                   (ansi-path
                    (merge-pathnames (format nil "~a.ansi" evidence-prefix))))
              (return-from %route-read-initial-and-action
                           (values grid projection (%route-write-ansi ansi-path raw)
                                   projection projection grid nil :manual readiness-result
                                   readiness-diagnostic)))))
        (setf initial-grid (%route-grid raw rows columns)
              initial-projection (project-criterion-grid initial-grid))))
    (when (plusp (length context-text))
      (pty-send-input session (%route-octets context-text)
                      :timeout-seconds timeout)
      (multiple-value-bind (context-output)
          (%route-read-widget-state
           session raw rows columns rate timeout
           (when (string-equal (or (getf subcase :context) "")
                               "expanded widget")
             (lambda (grid)
               (%route-expanded-widget-transition-p initial-grid grid))))
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
                (%route-read-through-quiet-period
                 session
                 *route-frame-quiet-seconds*
                 timeout
                 (* *route-first-frame-factor* (/ rate 1000d0)))
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
                      (%route-read-through-quiet-period
                       session
                       *route-frame-quiet-seconds*
                       timeout
                       (* *route-first-frame-factor* (/ rate 1000d0)))
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
                    (%route-read-through-quiet-period
                     session
                     *route-frame-quiet-seconds*
                     timeout
                     (* *route-first-frame-factor* (/ rate 1000d0)))
                  (%append-octets raw preparation-output)))
              (progn
                (when (and (eq (route-kind route) :key)
                           (string= (route-action route) "Select help section")
                           (string= (getf subcase :input) "1"))
                  ;; Start on section 2 so selecting section 1 has a visible transition.
                  (pty-send-input session (%route-octets "2")
                                  :timeout-seconds timeout)
                  (multiple-value-bind (preparation-output)
                      (%route-read-through-quiet-period
                       session
                       *route-frame-quiet-seconds*
                       timeout
                       (* *route-first-frame-factor* (/ rate 1000d0)))
                    (%append-octets raw preparation-output)))
                (when (and (eq (route-kind route) :key)
                           (string= (route-action route) "Select help section")
                           (member (getf subcase :input) '("7" "8" "9") :test #'string=))
                  ;; Start at section 1 so 7-9 produce a measurable jump to the bottom.
                  (pty-send-input session (%route-octets "1")
                                  :timeout-seconds timeout)
                  (multiple-value-bind (preparation-output)
                      (%route-read-through-quiet-period
                       session
                       *route-frame-quiet-seconds*
                       timeout
                       (* *route-first-frame-factor* (/ rate 1000d0)))
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
             ((and (string= (route-id route) "key-36")
                   (string-equal (or (getf subcase :context) "")
                                 "expanded widget"))
              (%route-read-widget-state
               session raw rows columns rate timeout
               (lambda (grid)
                 (%route-expanded-widget-restored-p grid initial-grid))))
             (t
              (%route-read-through-quiet-period
               session
               *route-frame-quiet-seconds*
               timeout
               (* *route-first-frame-factor* (/ rate 1000d0)))))
          (%append-octets raw action-output))
        (push (%route-grid raw rows columns) action-grids))
      (let* ((ansi-path (merge-pathnames
                         (format nil "~a.ansi" evidence-prefix)))
             (grid (%route-grid raw rows columns)))
        (values grid (project-criterion-grid grid)
                (%route-write-ansi ansi-path raw)
                before-action initial-projection before-grid
                (nreverse action-grids) nil)))))
(defun %route-run-interactive-side (program route subcase evidence-prefix
                                    rows columns rate timeout config-path
                                    evidence-directory)
  (let* ((context (or (getf subcase :context) ""))
         (context-text (%route-context-input context route subcase))
         (action-text (%route-action-text route subcase rows columns))
         (args (%route-base-arguments program config-path route subcase evidence-directory))
         (session
          (launch-e2e-process
           program args rows columns
           :environment (%route-environment)
           :timeout-seconds
           (%route-process-startup-timeout route subcase timeout))))
    (unwind-protect
         (multiple-value-bind (grid projection evidence before initial
                               initial-grid action-grids readiness-status
                               readiness-result readiness-diagnostic)
             (%route-read-initial-and-action session rows columns rate timeout
                                             action-text context-text
                                             evidence-prefix route subcase args)
           (list :grid grid :projection projection
                 :initial-projection (or before initial)
                 :initial-grid initial-grid :action-grids action-grids
                 :evidence evidence :readiness-status readiness-status
                 :readiness-result readiness-result
                 :readiness-diagnostic readiness-diagnostic
                 :actual (if readiness-result
                             (format nil "readiness ~a: ~a"
                                     readiness-result readiness-diagnostic)
                           (format nil "captured a ~d by ~d terminal frame" columns rows))
                 :cause readiness-diagnostic))
      (%route-close-session-with-evidence session evidence-prefix))))
(defun %route-run-exit-side (program route subcase evidence-prefix rows columns
                                     rate timeout config-path evidence-directory)
  (let* ((args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session (launch-e2e-process program args rows columns
                                      :environment (%route-environment)
                                      :timeout-seconds timeout))
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (exit-code nil)
         (error-value nil)
         (sent-p nil))
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
              (setf sent-p t
                    exit-code (%route-wait-for-exit session timeout)))
          (error (condition) (setf error-value condition)))
      (%route-close-session-with-evidence session evidence-prefix))
    (list :passed (and (null error-value) (eql exit-code 0))
          :exit-code exit-code
          :capture-error (and error-value (not sent-p) error-value)
          :actual (if error-value (princ-to-string error-value)
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
                (%route-read-through-quiet-period
                 session
                 *route-frame-quiet-seconds*
                 timeout
                 (* *route-first-frame-factor* (/ rate 1000d0))
                 (lambda (octets)
                   (let ((grid (%route-grid octets 36 120)))
                     (and (char= (cell-character (grid-cell-at grid 0 119)) (code-char #x2510))
                          (char= (cell-character (grid-cell-at grid 35 119)) (code-char #x2518))))))
              (%append-octets raw resized))
            (let ((grid (%route-grid raw 36 120)))
              (%route-write-ansi path raw)
              (list :grid grid :projection (project-criterion-grid grid)
                    :initial-projection nil :initial-grid initial-grid
                    :action-grids (list grid) :evidence (namestring path)
                    :actual (format nil "captured a ~d by ~d frame"
                                    (grid-width grid) (grid-height grid))))))
      (%route-close-session-with-evidence session evidence-prefix))))
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
(defun %route-help-navigation-state-p (route subcase before-grid after-grid)
  (when (and (string-equal (or (getf subcase :context) "") "help")
             before-grid after-grid
             (member (route-action route)
                     (list "Go to first row" "Go to last row"
                           "Move to previous row" "Move to next row"
                           "Page up" "Page down" "Half-page up" "Half-page down")
                     :test (function string=)))
    (%route-grid-first-difference before-grid after-grid)))
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
            (and (%route-process-search-open-state-p before nil)
                 (not (%route-process-search-open-state-p after nil))))
           ((string-equal context "help search")
            (and (%route-help-search-query-visible-p before-grid "process")
                 (search "Help" after :test #'char-equal)
                 (not (search "Search: process" after :test #'char-equal))))
           ((string-equal context "process sort")
            (and (search "Sort By" before :test #'char-equal)
                 (not (search "Sort By" after :test #'char-equal))))
           ((string-equal context "expanded widget")
            (and (%route-expanded-widget-p before-grid)
                 (not (%route-expanded-widget-p after-grid))))
           ((member context '("kill dialog" "help") :test #'string-equal)
 :manual)
           (t nil)))
        (t nil))))
