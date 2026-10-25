(in-package #:ntop.test)

(cl-weave:describe
 "process kill dialog outcomes"
  (cl-weave:it "closes process kill cancellation outcomes without errors"
    (let* ((simple-config (ntop.shared.application:make-application-config))
           (advanced-config (ntop.shared.application:make-application-config))
           (signal-sample
             (ntop.process.domain:make-process-sample
              :sampled-at 1d0
              :signals
              (list
               (ntop.process.domain:make-process-signal
                :name "SIGTERM" :number 15 :operating-system "fixture"))))
           (empty-sample (%d-sample 0))
           (sent 0)
           (composition
             (ntop.app.application:make-application-composition
              simple-config
              :process-signal-sender
              (lambda (target-pids signal-number)
                (declare (ignore target-pids signal-number))
                (incf sent)
                t))))
      (setf (ntop.shared.application:application-config-disable-advanced-kill-p
             simple-config)
            t
            (ntop.shared.application:application-config-disable-advanced-kill-p
             advanced-config)
            nil
            (ntop.app.application::application-composition-latest-process-sample
             composition)
            signal-sample)
      (flet ((open-dialog (config sample)
               (multiple-value-bind (dialog result)
                   (ntop.process.presentation:open-process-termination-dialog
                    (list 42) "worker" sample config)
                 (%d-expect result :opened)
                 dialog))
             (set-index (dialog index)
               (setf
                (ntop.ui:table-state-selected-index
                 (ntop.process.presentation::process-termination-dialog-state-selection-state
                  dialog))
                index))
             (confirm-without-error (dialog)
               (let ((state (%d-kill-state dialog)))
                 (%d-expect
                  (ntop.app.application::%process-kill-dialog-action
                   composition state (%d-action :process-terminate :confirm))
                  t)
                 (%d-expect (ntop.app.domain:app-state-active-dialog state) nil))))
        (let ((dialog (open-dialog simple-config signal-sample)))
          (%d-expect
           (ntop.ui:table-state-selected-index
            (ntop.process.presentation::process-termination-dialog-state-selection-state
             dialog))
           1)
          (confirm-without-error dialog))
        (let ((dialog (open-dialog advanced-config signal-sample)))
          (cl-weave:expect
           (cdr
            (first
             (ntop.process.presentation::process-termination-dialog-choices
              dialog signal-sample)))
           :to-equal
           "0: Cancel")
          (set-index dialog 0)
          (confirm-without-error dialog))
        (let ((dialog (open-dialog advanced-config signal-sample)))
          (set-index dialog 99)
          (confirm-without-error dialog))
        (setf
         (ntop.app.application::application-composition-latest-process-sample
          composition)
         empty-sample)
        (let ((dialog (open-dialog simple-config empty-sample)))
          (set-index dialog 0)
          (confirm-without-error dialog))
        (%d-expect sent 0))))
  (cl-weave:it "keeps process signal failures in a dismissible kill dialog"
    (let* ((config (ntop.shared.application:make-application-config))
           (sample
             (ntop.process.domain:make-process-sample
              :sampled-at 1d0
              :signals
              (list
               (ntop.process.domain:make-process-signal
                :name "SIGTERM" :number 15 :operating-system "fixture"))))
           (send-failures 0)
           (composition
             (ntop.app.application:make-application-composition
              config
              :process-signal-sender
              (lambda (target-pids signal-number)
                (declare (ignore target-pids signal-number))
                (incf send-failures)
                nil))))
      (setf (ntop.shared.application:application-config-disable-advanced-kill-p
             config)
            t
            (ntop.app.application::application-composition-latest-process-sample
             composition)
            sample)
      (multiple-value-bind (dialog result)
          (ntop.process.presentation:open-process-termination-dialog
           (list 42) "worker" sample config)
        (%d-expect result :opened)
        (let ((state (%d-kill-state dialog))
              (on-action
                (ntop.app.application:application-composition-on-action composition)))
          (setf
           (ntop.ui:table-state-selected-index
            (ntop.process.presentation::process-termination-dialog-state-selection-state
             dialog))
           0)
          (funcall on-action state (%d-action :process-terminate :confirm))
          (%d-expect (ntop.app.domain:app-state-active-dialog state) :process-kill)
          (progn
            (%d-expect
             (ntop.process.presentation:process-termination-dialog-state-error-message dialog)
             nil)
            (cl-weave:expect
 (ntop.process.presentation:process-termination-dialog-state-error-message
  (gethash :process-kill
           (ntop.app.domain:app-state-widget-v-states state)))
 :to-equal
 "The process signal could not be sent."))
          (funcall on-action state (%d-action :process-terminate :confirm))
          (%d-expect (ntop.app.domain:app-state-active-dialog state) nil)
          (%d-expect send-failures 1)))
      (let* ((condition-failures 0)
             (condition-composition
               (ntop.app.application:make-application-composition
                config
                :process-signal-sender
                (lambda (target-pids signal-number)
                  (declare (ignore target-pids signal-number))
                  (incf condition-failures)
                  (error
                   (concatenate (quote string)
                                "Error code 1 - the calling process does not have the permissions "
                                "to terminate the target process(es).")))))
             (dialog nil)
             (state nil)
             (on-action
               (ntop.app.application:application-composition-on-action
                condition-composition)))
        (setf (ntop.app.application::application-composition-latest-process-sample
               condition-composition)
              sample)
        (multiple-value-bind (opened result)
            (ntop.process.presentation:open-process-termination-dialog
             (list 42) "worker" sample config)
          (%d-expect result :opened)
          (setf dialog opened
                state (%d-kill-state opened)))
        (setf
         (ntop.ui:table-state-selected-index
          (ntop.process.presentation::process-termination-dialog-state-selection-state
           dialog))
         0)
        (funcall on-action state (%d-action :process-terminate :confirm))
        (%d-expect (ntop.app.domain:app-state-active-dialog state) :process-kill)
        (progn
          (%d-expect
           (ntop.process.presentation:process-termination-dialog-state-error-message dialog)
           nil)
          (%d-expect
           (not
            (null
             (search
              (concatenate
               (quote string)
               "Error code 1 - the calling process does not have the permissions "
               "to terminate the target process(es).")
              (ntop.process.presentation:process-termination-dialog-state-error-message
               (gethash :process-kill
                        (ntop.app.domain:app-state-widget-v-states state))))))
           t))
        (funcall on-action
                 state
                 (%d-action :dialog (list :close :process-kill)))
        (%d-expect (ntop.app.domain:app-state-active-dialog state) nil)
        (%d-expect
         (gethash :process-kill
                  (ntop.app.domain:app-state-widget-v-states state))
         nil)
        (%d-expect condition-failures 1))))
)
