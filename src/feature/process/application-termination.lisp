(in-package #:ntop.process.application)

(defstruct (process-termination-state
            (:constructor %make-process-termination-state
                (&key selected-signal target-pids (send-status :pending))))
  selected-signal
  (target-pids nil :type list)
  (send-status :pending :type keyword))

(defun make-process-termination-state (&key target-pids)
  (%make-process-termination-state :target-pids target-pids))

(defun copy-process-termination-state-with
    (state &key (selected-signal
                 (process-termination-state-selected-signal state))
                (target-pids (process-termination-state-target-pids state))
                (send-status :pending))
  (%make-process-termination-state
   :selected-signal selected-signal
   :target-pids target-pids
   :send-status send-status))

(defun update-process-termination-targets (state target-pids)
  (copy-process-termination-state-with state :target-pids target-pids))

(defun select-process-signal (state sample signal-name)
  (let ((signal
          (find signal-name
                (ntop.process.domain:process-sample-signals sample)
                :key #'ntop.process.domain:process-signal-name
                :test #'equal)))
    (if signal
        (values (copy-process-termination-state-with
                 state :selected-signal signal)
                :selected)
        (values state :unknown-signal))))

(defun send-process-termination (state config sender)
  (cond
    ((ntop.shared.application:application-config-read-only-p config)
     (values state :read-only))
    ((null (process-termination-state-selected-signal state))
     (values state :signal-not-selected))
    ((null (process-termination-state-target-pids state))
     (values state :no-targets))
    (t
     (handler-case
         (if (funcall sender
                      (process-termination-state-target-pids state)
                      (ntop.process.domain:process-signal-number
                       (process-termination-state-selected-signal state)))
             (values (copy-process-termination-state-with
                      state :send-status :sent)
                     :sent)
             (values state :send-failed))
       (error (condition)
         (values state condition))))))
