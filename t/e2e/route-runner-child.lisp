(in-package #:ntop.e2e)
(defun %route-send-action-inputs (session inputs raw timeout)
  (dolist (input inputs)
    (pty-send-input session (%route-octets input)
                    :timeout-seconds timeout)
    (multiple-value-bind (result)
        (pty-read-until-idle session
                             :quiet-seconds *route-frame-quiet-seconds*
                             :timeout-seconds timeout)
      (%append-octets raw result)))
  (and inputs t))

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
                (progn
                  (%append-octets raw initial)
                  (when (%route-widget-no-data-p
                         (%route-grid raw screen-rows columns) "Processes")
                    (return-from %route-run-sleep-child-side
                                 (list :status :manual
                                       :passed nil
                                       :actual (format nil (concatenate (quote string)
                                                        "~a process widget shows No data;"
                                                        " owned PID ~d cannot be verified.")
                                                       (pathname-name (pathname program)) pid)
                                       :cause "The process table has no rows on this host."
                                       :evidence path)))))
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
                                ((eq (route-kind route) :mouse)
                                 (%route-mouse-action-inputs
                                  route subcase grid screen-rows columns))
                                (t (%route-action-text
                                    route subcase rows columns)))))
                    (when (and dialog-context
                               (not (%route-owned-dialog-p grid pid)))
                      (error "Kill action refused without exact PID ~d proof."
                             pid))
                    (when (and (eq (route-kind route) :mouse)
                               (string= action-name "Choose kill action"))
                      (setf action (%route-safe-dialog-click-input grid)))
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
                    (setf sent (%route-send-action-inputs session action raw timeout))
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
      (%route-write-ansi path raw)) (list :passed (and passed sent (null error-value))
                                          :actual actual :evidence path)))
