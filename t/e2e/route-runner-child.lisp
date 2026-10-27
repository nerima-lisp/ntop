(in-package #:ntop.e2e)
(defun %route-child-capture-frame
    (session raw rows columns stage timeout-seconds
     &key completion-predicate allow-unchanged-p)
  (%route-capture-frame
   session raw rows columns
   :side *route-frame-capture-side*
   :stage stage
   :timeout-seconds timeout-seconds
   :stability-seconds *route-frame-stability-seconds*
   :completion-predicate completion-predicate
   :allow-unchanged-p allow-unchanged-p))

(defun %route-send-action-inputs (session inputs raw timeout rows columns)
  (dolist (input inputs)
    (pty-send-input session (%route-octets input)
                    :timeout-seconds timeout)
    (multiple-value-bind (updated grid metadata)
        (%route-child-capture-frame
         session raw rows columns :owned-child-action timeout
         :allow-unchanged-p t)
      (declare (ignore grid metadata))
      (setf raw updated)))
  (values (and inputs t) raw))

(defun %route-owned-pid-marker (pid)
  (format nil "ntop-owned-~d" pid))

(defun %route-owned-marker-row-p (grid pid)
  (let* ((row (%route-owned-row-index grid pid))
         (text (and row (criterion-row-text grid row)))
         (marker (%route-owned-pid-marker pid))
         (start (and text (search marker text)))
         (end (and start (+ start (length marker))))
         (marker-delimited
           (and start end
                (or (zerop start)
                    (not (alphanumericp (char text (1- start)))))
                (or (= end (length text))
                    (not (alphanumericp (char text end)))))))
    (and row marker-delimited)))

(defun %route-owned-dialog-pid-matches-p (grid pid)
  (= 1
     (loop for row below (grid-height grid)
           for text = (criterion-row-text grid row)
           for pid-label = (search "PID" text :test (function char-equal))
           count (and (search "Kill process" text :test (function char-equal))
                      pid-label
                      (%route-pid-line-p
                       (subseq text (+ pid-label (length "PID")))
                       pid)))))

(defun %route-simple-kill-confirmation-p (grid pid)
  (and (%route-owned-dialog-pid-matches-p grid pid)
       (not (null
             (search "Press ENTER to confirm"
                     (%route-grid-text grid)
                     :test (function char-equal))))))

(defun %route-owned-dialog-safe-cancel-input (grid pid)
  (unless (%route-owned-dialog-pid-matches-p grid pid)
    (error "Refusing to act without exact owned PID ~d dialog proof." pid))
  (let* ((title-row
           (loop for row below (grid-height grid)
                 for text = (criterion-row-text grid row)
                 when (or (search "Confirm Kill Process" text)
                          (search "Select Signal" text))
                   return row))
         (choice-row (and title-row (+ title-row 2)))
         (choice-text (and choice-row (criterion-row-text grid choice-row))))
    (cond
      ((and choice-text
            (or (search "0: Cancel" choice-text)
                (search "No" choice-text)))
       (%route-safe-dialog-click-input grid))
      ((%route-simple-kill-confirmation-p grid pid)
       (%route-key-input "Escape"))
      (t
       (error "No safe No or Cancel choice appears under the owned PID dialog.")))))

(defun %route-wait-owned-marker-row (session raw rows columns pid)
  (handler-case
      (multiple-value-bind (updated grid metadata)
          (%route-child-capture-frame
           session raw rows columns :owned-pid-marker-row-readiness
           *route-process-readiness-timeout-seconds*
           :completion-predicate
           (lambda (stable-grid)
             (%route-owned-marker-row-p stable-grid pid)))
        (declare (ignore metadata))
        (values :ready
                (format nil "Owned PID ~d marker row is visible." pid)
                updated))
    (route-frame-capture-timeout (condition)
      (values :timeout
              (format nil
                      "Owned PID ~d marker row not ready before timeout.~%~a"
                      pid condition)
              (e2e-session-output session)))))
(defun %route-wait-owned-dialog (session raw rows columns pid)
  (handler-case
      (multiple-value-bind (updated grid metadata)
          (%route-child-capture-frame
           session raw rows columns :owned-pid-dialog-readiness
           *route-process-readiness-timeout-seconds*
           :completion-predicate
           (lambda (stable-grid)
             (%route-owned-dialog-pid-matches-p stable-grid pid)))
        (declare (ignore metadata))
        (values :ready
                (format nil "Owned PID ~d kill dialog is visible." pid)
                updated))
    (route-frame-capture-timeout (condition)
      (values :timeout
              (format nil
                      "Owned PID ~d kill dialog not ready before timeout.~%~a"
                      pid condition)
              (e2e-session-output session)))))

(defun %route-owned-marker-selected-p (grid pid)
  (let ((styles (%route-owned-marker-style grid pid)))
    (and styles
         (every (lambda (style)
                  (equal (second style) '(:indexed . 12)))
                styles))))

(defun %route-owned-marker-row-timeout-result (child pid diagnostic path)
  (let ((alive (sb-ext:process-alive-p child)))
    (list :status :manual
          :readiness-result :timeout
          :readiness-stage :owned-pid-marker-row
          :readiness-diagnostic diagnostic
          :passed nil
          :actual
          (format nil
                  "Owned PID ~d marker row timed out; input skipped; child alive ~a."
                  pid (if alive "yes" "no"))
          :cause
          (format nil
                  "~a Route input was skipped; owned PID ~d remains alive: ~a."
                  diagnostic pid (if alive "yes" "no"))
          :evidence path)))

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
                 '("-c" "exec -a ntop-owned-$$ sleep 300")
                 :wait nil))
         (pid (sb-ext:process-pid child))
         (process-route (and (not dialog-context)
                             (member action-name
                                     '("Delete selected process"
                                       "Open process kill dialog"
                                       "read_only"
                                       "disable_advanced_kill")
                                     :test #'string=)))
         (screen-rows rows)
         (base-args (%route-base-arguments program config-path route subcase
                                           evidence-directory))
         (args
          (if (eq (route-kind route) :column)
              base-args
            (append
             (remove "--process-command" base-args :test (function string=))
             (list "--process-command"))))
         (session nil)
         (raw (make-array 0 :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer 0))
         (path (merge-pathnames (format nil "~a.ansi" evidence-prefix)))
         (passed nil)
         (sent nil)
         (actual "owned process row and dialog target were not proven")
         (error-value nil))
    (declare (ignore rate))
    (unwind-protect
        (handler-case
            (progn
              (setf session (launch-e2e-process
                             program args screen-rows columns
                             :environment (%route-environment)
                             :timeout-seconds
                             (%route-process-startup-timeout route subcase timeout)))
              (multiple-value-bind (initial)
                  (%route-child-capture-frame
                   session raw screen-rows columns
                   :owned-child-initial-frame
                   (%route-process-startup-timeout route subcase timeout))
                (progn
                  (setf raw initial)
                  (when (and (eq (route-kind route) :column)
                             (not (sb-ext:process-alive-p child)))
                    (error "Harness-owned column fixture PID ~d exited before the frame." pid))
                  (when (%route-process-data-required-p route subcase)
  (multiple-value-bind (readiness-result readiness-diagnostic)
      (multiple-value-bind (result diagnostic updated)
    (%route-wait-process-data session raw screen-rows columns args)
  (setf raw updated)
  (values result diagnostic))
    (when (eq :timeout readiness-result)
      (let ((alive (sb-ext:process-alive-p child)))
        (return-from %route-run-sleep-child-side
          (list :status :manual
                :readiness-result readiness-result
                :readiness-diagnostic readiness-diagnostic
                :passed nil
                :actual
                (format nil
                        (concatenate
                         'string
                         "~a Readiness ~a after ~,0f seconds; owned PID ~d "
                         "alive ~a; route input was skipped.")
                        readiness-diagnostic readiness-result
                        *route-process-readiness-timeout-seconds*
                        pid (if alive "yes" "no"))
                :cause
                (format nil
                        "~a Harness-owned PID ~d remained alive: ~a."
                        readiness-diagnostic pid
                        (if alive "yes" "no"))
                :evidence path))))))
                  (when (eq (route-kind route) :column)
                    (let ((grid (%route-grid raw screen-rows columns)))
                      (unless (%route-owned-marker-row-p grid pid)
                        (return-from %route-run-sleep-child-side
                                     (list :status :manual
                                           :passed nil
                                           :actual
                                           (format nil
                                                   (concatenate
                                                    'string
                                                    "process rows are visible, but PID ~d and "
                                                    "ntop-owned are not on one row; "
                                                    "owned child alive ~a")
                                                   pid
                                                   (if (sb-ext:process-alive-p child) "yes" "no"))
                                           :cause
                                           (concatenate
                                            'string
                                            "The process table did not show the "
                                            "harness-owned PID and marker on one row.")
                                           :evidence path)))
                      (multiple-value-bind (header-ok expected-header-p)
                          (%route-column-visible-p route grid)
                        (let ((alive (sb-ext:process-alive-p child))
                              (header (ntop.app.domain:parity-column-header
                                       (route-entry route))))
                          (setf passed (and alive header-ok)
                                sent t
                                actual
                                (format nil
                                        (concatenate
                                         'string
                                         "owned PID ~d and ntop-owned share one row; "
                                         "header ~s is ~a, expected ~a; child alive ~a")
                                        pid header (if header-ok "visible" "absent")
                                        (if expected-header-p "visible" "absent")
                                        (if alive "yes" "no")))
                          (return-from %route-run-sleep-child-side
                                       (list :passed (and passed sent)
                                             :actual actual
                                             :evidence path))))))))
              (if select-route
                  (let* ((before-grid
 (progn
   (pty-send-input
    session
    (%route-octets
     (format nil "/~a~c" (%route-owned-pid-marker pid) #\Return))
    :timeout-seconds timeout)
   (multiple-value-bind (searched)
       (%route-child-capture-frame
        session raw screen-rows columns :owned-child-action timeout
        :allow-unchanged-p t)
     (setf raw searched))
   (multiple-value-bind (marker-result marker-diagnostic)
       (multiple-value-bind (result diagnostic updated)
    (%route-wait-owned-marker-row session raw screen-rows columns pid)
  (setf raw updated)
  (values result diagnostic))
     (when (eq :timeout marker-result)
       (return-from %route-run-sleep-child-side
         (%route-owned-marker-row-timeout-result
          child pid marker-diagnostic path))))
   (%route-grid raw screen-rows columns)))
                         (before-style
                          (%route-owned-marker-style before-grid pid)))
                    (unless (and (%route-owned-marker-row-p before-grid pid)
                                 before-style)
                      (error "Owned PID ~d marker is not uniquely visible." pid))
                    (unless (sb-ext:process-alive-p child)
                      (error "Owned PID ~d exited before the click." pid))
                    (multiple-value-bind (action column row)
                        (%route-owned-row-click-input before-grid pid)
                      (pty-send-input session (%route-octets action)
                                      :timeout-seconds timeout)
                      (setf sent t)
                      (multiple-value-bind (clicked)
                          (%route-child-capture-frame
                           session raw screen-rows columns :owned-child-action timeout
                           :allow-unchanged-p t)
                        (setf raw clicked))
                      (let* ((after-grid (%route-grid raw screen-rows columns))
       (selected (%route-owned-marker-selected-p after-grid pid))
       (alive (sb-ext:process-alive-p child))
       (dialog (%route-owned-dialog-pid-matches-p after-grid pid)))
  (setf passed (and alive selected (not dialog))
        actual
        (format nil
                "clicked PID ~d at ~d,~d; selected highlight ~a; kill dialog ~a; child alive ~a"
                pid column row
                (if selected "visible" "absent")
                (if dialog "open" "absent")
                (if alive "yes" "no"))))))
                (progn
                  (pty-send-input
                   session
                   (%route-octets (format nil "/~a~c" (%route-owned-pid-marker pid) #\Return))
                   :timeout-seconds timeout)
                  (multiple-value-bind (searched)
                      (%route-child-capture-frame
                       session raw screen-rows columns :owned-child-action timeout
                       :allow-unchanged-p t)
                    (setf raw searched))
                  (progn
                    (multiple-value-bind (marker-result marker-diagnostic)
                        (multiple-value-bind (result diagnostic updated)
    (%route-wait-owned-marker-row session raw screen-rows columns pid)
  (setf raw updated)
  (values result diagnostic))
                      (when (eq :timeout marker-result)
                        (return-from %route-run-sleep-child-side
                          (%route-owned-marker-row-timeout-result
                           child pid marker-diagnostic path))))
                    (unless (%route-owned-marker-row-p
                             (%route-grid raw screen-rows columns) pid)
                      (error "Owned PID ~d and marker are not on one row." pid)))
                  (pty-send-input session (%route-octets (string #\Return))
                                  :timeout-seconds timeout)
                  (multiple-value-bind (selected)
                      (%route-child-capture-frame
                       session raw screen-rows columns :owned-child-action timeout
                       :allow-unchanged-p t)
                    (setf raw selected))
                  (unless (%route-owned-marker-row-p (%route-grid raw screen-rows columns) pid)
                    (error "Owned PID ~d was not selected after search."
                           pid))
                  (when (or dialog-context sleep-route)
                    (pty-send-input
                     session (%route-octets (%route-key-input "Delete"))
                     :timeout-seconds timeout)
                    (multiple-value-bind (opened)
                        (%route-child-capture-frame
                         session raw screen-rows columns :owned-child-action timeout
                         :allow-unchanged-p t)
                      (setf raw opened))
                    (multiple-value-bind (dialog-result diagnostic)
                        (multiple-value-bind (result diagnostic updated)
    (%route-wait-owned-dialog session raw screen-rows columns pid)
  (setf raw updated)
  (values result diagnostic))
                      (unless (eq dialog-result :ready)
                        (error "~a" diagnostic))))
                  (let* ((grid (%route-grid raw screen-rows columns))
                         (before (project-criterion-grid grid))
                         (action
                          (cond (sleep-route (%route-key-input "Enter"))
                                ((eq (route-kind route) :mouse)
                                 (%route-mouse-action-inputs
                                  route subcase grid screen-rows columns))
                                (t (%route-action-text
                                    route subcase rows columns)))))
                    (when (and (or dialog-context sleep-route)
                               (not (%route-owned-dialog-pid-matches-p grid pid)))
                      (error "Kill action refused without exact PID ~d proof." pid))
                    (when (and (eq (route-kind route) :mouse)
                               (string= action-name "Choose kill action"))
                      (setf action (%route-owned-dialog-safe-cancel-input grid pid)))
                    (setf action (if (stringp action) (list action) action))
                    (unless (and (consp action) (every (function stringp) action))
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
                    (multiple-value-setq (sent raw)
                      (%route-send-action-inputs
                       session action raw timeout screen-rows columns))
                    (let* ((after (%route-grid raw screen-rows columns))
                           (dialog-after (%route-owned-dialog-pid-matches-p after pid))
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
                              (%route-sleep-child-action-actual pid alive dialog-after)))
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
                        (setf passed
                              (and alive
                                   (or (not dialog-after)
                                       (and (string= action-name "Choose kill action")
                                            (not changed)
                                            (%route-simple-kill-confirmation-p after pid))))
                              actual
                              (format nil
                                      "owned PID ~d; dialog ~a; safe input sent; child alive ~a"
                                      pid
                                      (if dialog-after
                                          "retained on simple Enter-confirm prompt"
                                        "closed")
                                      (if alive "yes" "no"))))
                       ((member action-name
                                '("Page up" "Page down"
                                            "Move left within widget"
                                            "Move right within widget"
                                            "Scroll up" "Scroll down")
                                :test #'string=)
                        (setf passed
                              (and alive dialog-after
                                   (or changed
                                       (%route-simple-kill-confirmation-p after pid)))
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
        (close-e2e-session session)
        (when error-value
          (setf raw (copy-seq (e2e-session-output session)))))
      (when (and child (sb-ext:process-alive-p child))
        (progn (sb-ext:process-kill child 9 :pid) (sb-ext:process-wait child)))
      (%route-write-ansi path raw))
      (list :passed (and passed sent (null error-value))
            :actual actual
            :evidence path
            :capture-error error-value)))

(defun %route-sleep-child-action-actual (pid alive dialog-after)
  (format nil
          "owned PID ~d after confirmation input; child alive ~a; exact-PID dialog ~a"
          pid
          (if alive "yes" "no")
          (if dialog-after "open" "closed")))
