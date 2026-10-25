(in-package #:ntop.app.application)

(defun %input-context-for-state (state)
  (let ((active-dialog (ntop.app.domain:app-state-active-dialog state)))
    (ntop.app.domain:make-input-context
     :selected-widget (ntop.app.domain:app-state-selected-widget state)
     :active-dialog active-dialog
     :basic-mode-p (ntop.app.domain:app-state-basic-mode-p state)
     :search-mode-p
     (not
      (null
       (member active-dialog
               (list :process-search :help-search)
               :test (function eq)))))))

(progn
  (defun %contextualize-application-action (action context)
    (let ((kind (ntop.app.domain:app-action-kind action))
          (value (ntop.app.domain:app-action-value action)))
      (if (and (eq kind :navigation)
               (member value (list :left :right) :test (function eq))
               (eq (ntop.app.domain:input-context-selected-widget context)
                   :battery)
               (null (ntop.app.domain:input-context-active-dialog context)))
          (ntop.app.domain:make-app-action
           :kind :select
           :value (list :battery-tab value))
          action)))
  (defun %dispatch-input (raw-event state keymap-state config now on-action)
    (let ((result nil)
          (context (%input-context-for-state state)))
      (ntop.ui:translate-input
       raw-event
       context
       (lambda (meaning)
         (dispatch-application-input
          keymap-state
          meaning
          context
          (lambda (dispatched-action)
            (let ((action
                    (%contextualize-application-action dispatched-action context)))
              (when (eq (ntop.app.domain:app-action-kind action) :quit)
                (setf result :quit))
              (when on-action
                (funcall on-action state action))))
          (lambda (unhandled)
            (declare (ignore unhandled)))
          :now
          (funcall now)
          :disable-keys-p
          (ntop.shared.application:application-config-disable-keys-p config)
          :disable-click-p
          (ntop.shared.application:application-config-disable-click-p config)))
       (lambda (unhandled)
         (declare (ignore unhandled))))
      result)))

(defun %apply-snapshot (snapshot histories on-snapshot)
  (map nil
       (lambda (store)
         (ntop.shared.domain:history-prune
          store (ntop.app.domain:collection-snapshot-sampled-at snapshot)))
       histories)
  (when on-snapshot
    (funcall on-snapshot snapshot)))

(defun %render (render state config snapshot columns rows
                unavailability-reasons)
  (when (equal (uiop:getenv "NTOP_N6_TEST_EXCEPTION")
               "raise-on-first-render")
    (write-line "__NTOP_N6_EXCEPTION_READY__" *error-output*)
    (finish-output *error-output*)
    (error "Deliberate N6 test exception after terminal setup."))
  (funcall render state config snapshot columns rows unavailability-reasons))

(defun run-application-loop
    (state config collection-channel poll-input poll-size render
           &key poll-signal on-action on-snapshot histories
             (now (lambda ()
                    (multiple-value-bind (seconds nanoseconds)
                        (sb-unix:clock-gettime sb-unix:clock-monotonic)
                      (+ seconds (/ nanoseconds 1000000000d0)))))
             (wait (lambda (seconds) (sleep seconds))))
  (unless (functionp render)
    (error "RENDER must be a function."))
  (let ((columns 80)
        (rows 24)
        (snapshot nil)
        (unavailability-reasons nil)
        (keymap-state (make-application-keymap-state))
        (running-p t)
        (result :quit)
        (redraw-p t))
    (multiple-value-bind (new-columns new-rows)
        (funcall poll-size state 0d0)
      (when (and (integerp new-columns) (plusp new-columns)
                 (integerp new-rows) (plusp new-rows))
        (setf columns new-columns rows new-rows)))
    (loop while running-p
          do (when poll-signal
               (let ((terminal-event (funcall poll-signal)))
                 (cond
                   ((eq terminal-event :resize)
                    (setf redraw-p t))
                   ((and (consp terminal-event)
                         (eq (first terminal-event) :terminate))
                    (setf result :signal
                          running-p nil)))))
             (when running-p
               (multiple-value-bind (new-columns new-rows)
                   (funcall poll-size state 0d0)
                 (when (and (integerp new-columns) (plusp new-columns)
                            (integerp new-rows) (plusp new-rows)
                            (or (/= columns new-columns) (/= rows new-rows)))
                   (setf columns new-columns
                         rows new-rows
                         redraw-p t)))
               (loop for raw-event in (funcall poll-input state 0d0)
                     while running-p
                     do (when (eq (%dispatch-input raw-event state keymap-state
                                                   config now on-action)
                                  :quit)
                          (setf result :quit
                                running-p nil))
                        (setf redraw-p t))
               (when running-p
                 (loop
                   (multiple-value-bind (event received-p closed-p)
                       (cl-concurrent-kit:try-recv collection-channel)
                     (cond
                       ((not received-p)
                        (when closed-p
                          (setf result :collection-closed
                                running-p nil))
                        (return))
                       ((and (consp event) (eq (car event) :failure))
                        (error (cdr event)))
                       ((application-collection-result-p event)
                        (let ((received-snapshot
                                (application-collection-result-snapshot event)))
                          (%apply-snapshot received-snapshot histories on-snapshot)
                          (setf snapshot received-snapshot
                                unavailability-reasons
                                (application-collection-result-unavailability-reasons
                                 event)
                                redraw-p t)))
                       ((ntop.app.domain:collection-snapshot-p event)
                        (%apply-snapshot event histories on-snapshot)
                        (setf snapshot event
                              unavailability-reasons nil
                              redraw-p t))
                       (t (error "Unexpected collection event ~S." event))))))
               (when redraw-p
                 (%render render state config snapshot columns rows
                          unavailability-reasons)
                 (setf redraw-p nil))
               (when running-p
                 (funcall wait 0.01d0))))
    result))