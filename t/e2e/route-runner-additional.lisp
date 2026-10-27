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
(defun %route-read-widget-state
    (session raw rows columns rate timeout state-predicate
             &key (completion-samples-required 2))
  (%route-read-through-quiet-period
   session
   :quiet-seconds *route-frame-quiet-seconds*
   :timeout-seconds timeout
   :initial-quiet-seconds (* *route-first-frame-factor* (/ rate 1000d0))
   :completion-predicate (when state-predicate
                           (lambda (output)
                             (funcall state-predicate
                                      (%route-grid-after-output raw output rows columns))))
   :completion-samples-required completion-samples-required))

(defun %route-context-input-steps (input context)
  (let ((text (or input ""))
        (context (or context "")))
    (cond
      ((and (>= (length text) 2)
            (string= (subseq text 0 2) "?/"))
       (append (list (list :input "?" :state :help)
                     (list :input "/" :state :help-search))
               (when (> (length text) 2)
                 (list (list :input (subseq text 2)
                             :state :help-search-query)))))
      ((and (> (length text) 1)
            (char= (char text 0) #\/))
       (list (list :input "/" :state :process-search)
             (list :input (subseq text 1)
                   :state :process-search-query)))
      ((and (= (length text) 2)
            (char= (char text 0) #\?)
            (digit-char-p (char text 1)))
       (list (list :input "?" :state :help)
             (list :input (subseq text 1) :state :help-section)))
      ((string-equal context "help")
       (list (list :input text :state :help)))
      ((string-equal context "help search")
       (list (list :input text :state :help-search)))
      ((string-equal context "process search")
       (list (list :input text :state :process-search)))
      ((string-equal context "expanded widget")
       (list (list :input text :state :expanded-widget)))
      (t (list (list :input text :state nil))))))
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
                                               route subcase arguments fixture-state)
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
         (%route-process-startup-timeout route subcase timeout)
         :completion-predicate
         (lambda (octets)
           (not (or
                 (%route-incomplete-escape-suffix-p octets)
                 (%route-incomplete-utf8-suffix-p octets)))))
      (%append-octets raw initial)
      (progn
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
                          readiness-diagnostic))))))
        (setf initial-grid (%route-grid raw rows columns)
              initial-projection (project-criterion-grid initial-grid))))
    (when (plusp (length context-text))
      (let ((context-grid initial-grid))
        (dolist (step
                 (%route-context-input-steps
                  context-text (or (getf subcase :context) "")))
          (let ((input (getf step :input))
                (before-grid context-grid))
            (pty-send-input session (%route-octets input)
                            :timeout-seconds timeout)
            (multiple-value-bind (context-output)
                (%route-read-widget-state
                 session raw rows columns rate timeout
                 (when (getf step :state)
                   (lambda (grid)
                     (%route-context-stage-state-p step before-grid grid))))
              (%append-octets raw context-output))
            (setf context-grid (%route-grid raw rows columns))))))
    (let* ((before-grid
            (progn
              (when fixture-state
                (multiple-value-setq (raw fixture-state)
                  (%route-fixture-prepare
                   fixture-state route subcase session raw rows columns rate timeout
                   evidence-prefix)))
              (%route-write-ansi
               (merge-pathnames
                (format nil "~a.before-action.ansi" evidence-prefix))
               raw)
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
               session
               :quiet-seconds *route-frame-quiet-seconds*
               :timeout-seconds timeout))
             ((and (eq (route-kind route) :flag)
                   (string= (ntop.app.domain:parity-flag-snake-case
                             (route-entry route))
                            "disable_keys")
                   (not (getf subcase :invalid-value)))
              (%route-read-through-quiet-period
               session
               :quiet-seconds *route-frame-quiet-seconds*
               :timeout-seconds timeout))
             ((getf subcase :observer-completion)
              (%route-read-widget-state
               session raw rows columns rate timeout
               (lambda (grid)
                 (%route-observer-completion-p
                  (getf subcase :observer-completion)
                  subcase before-grid grid))))
             (t
              (%route-read-through-quiet-period
               session
               :quiet-seconds
               (getf subcase :observer-quiet-seconds
                     *route-frame-quiet-seconds*)
               :timeout-seconds timeout
               :initial-quiet-seconds
               (* *route-first-frame-factor* (/ rate 1000d0)))))
          (%append-octets raw action-output))
        (push (%route-grid raw rows columns) action-grids))
      (multiple-value-bind (updated-raw after-grid)
          (%route-fixture-after-action
           fixture-state session raw rows columns rate timeout)
        (setf raw updated-raw)
        (when after-grid
          (push after-grid action-grids)))
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
         (args (%route-base-arguments program config-path route subcase
                                      evidence-directory))
         (session
          (launch-e2e-process
           program args rows columns
           :environment (%route-environment)
           :timeout-seconds
           (%route-process-startup-timeout route subcase timeout)))
         (fixture-state nil))
    (unwind-protect
         (progn
           (setf fixture-state
                 (and (or (getf subcase :fixture)
                          (getf subcase :probe)
                          (getf subcase :applicability))
                      (%route-fixture-start route subcase)))
           (multiple-value-bind (grid projection evidence before initial
                                 initial-grid action-grids readiness-status
                                 readiness-result readiness-diagnostic)
               (%route-read-initial-and-action
                session rows columns rate timeout action-text context-text
                evidence-prefix route subcase args fixture-state)
             (list :grid grid :projection projection
                   :initial-projection (or before initial)
                   :initial-grid initial-grid :action-grids action-grids
                   :evidence evidence :readiness-status readiness-status
                   :readiness-result readiness-result
                   :readiness-diagnostic readiness-diagnostic
                   :fixture-state fixture-state
                   :fixture-evidence
                   (and fixture-state (getf fixture-state :evidence-path))
                   :actual
                   (if readiness-result
                       (format nil "readiness ~a: ~a"
                               readiness-result readiness-diagnostic)
                     (format nil "captured a ~d by ~d terminal frame"
                             columns rows))
                   :cause readiness-diagnostic)))
      (%route-close-session-with-evidence session evidence-prefix)
      (%route-fixture-cleanup fixture-state))))
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
                 :quiet-seconds *route-frame-quiet-seconds*
                 :timeout-seconds timeout
                 :initial-quiet-seconds (* *route-first-frame-factor* (/ rate 1000d0))
                 :completion-predicate (lambda (octets)
                                         (let ((grid (%route-grid octets 36 120)))
                                           (and (char= (cell-character (grid-cell-at grid 0 119))
                                                       (code-char #x2510))
                                                (char= (cell-character (grid-cell-at grid 35 119))
                                                       (code-char #x2518))))))
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
