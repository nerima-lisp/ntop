(in-package #:ntop.test)

(defun wp4-process-signal (name number operating-system)
  (ntop.process.domain:make-process-signal :name
                                           name
                                           :number
                                           number
                                           :operating-system
                                           operating-system))

(defun wp4-process-sample-with-signals (&rest signals)
  (ntop.process.domain:make-process-sample :sampled-at 0.0 :signals signals))

(defun wp4-process-state-with-signal (target-pids sample name)
  (let ((state
         (ntop.process.application::make-process-termination-state :target-pids
                                                                   target-pids)))
    (ntop.process.application::select-process-signal state sample name)))

(cl-weave:describe
  "process application termination"
  (cl-weave:it-each
    (("Darwin" "SIGTERM" 15) ("Darwin" "SIGINT" 2)
     ("Linux" "SIGTERM" 15)
     ("Linux" "SIGKILL" 9))
    "selects the exact available ~A signal on ~A"
    (operating-system name number)
    (let* ((signal (wp4-process-signal name number operating-system))
           (sample (wp4-process-sample-with-signals signal))
           (state
            (ntop.process.application::make-process-termination-state :target-pids '(101)))
           (selected-state nil)
           (result nil))
      (multiple-value-setq
        (selected-state result)
        (ntop.process.application::select-process-signal state sample name))
      (cl-weave:expect
        result
        :to-be
        :selected)
      (cl-weave:expect
        (ntop.process.application::process-termination-state-selected-signal
         selected-state)
        :to-be
        signal)
      (cl-weave:expect
        (ntop.process.application::process-termination-state-target-pids selected-state)
        :to-equal
        '(101))))
  (cl-weave:it
    "does not select a signal absent from the collected table"
    (let* ((state
            (ntop.process.application::make-process-termination-state :target-pids '(101)))
           (sample
            (wp4-process-sample-with-signals (wp4-process-signal "SIGTERM" 15 "Darwin")))
           (returned-state nil)
           (result nil))
      (multiple-value-setq
        (returned-state result)
        (ntop.process.application::select-process-signal state sample "SIGKILL"))
      (cl-weave:expect
        returned-state
        :to-be
        state)
      (cl-weave:expect
        result
        :to-be
        :unknown-signal)))
  (cl-weave:it
    "updates targets while preserving the chosen signal"
    (let* ((signal (wp4-process-signal "SIGTERM" 15 "Linux"))
           (sample (wp4-process-sample-with-signals signal))
           (state (wp4-process-state-with-signal '(101) sample "SIGTERM"))
           (targets '(201 202 203))
           (updated
            (ntop.process.application::update-process-termination-targets state targets)))
      (cl-weave:expect
        (ntop.process.application::process-termination-state-selected-signal updated)
        :to-be
        signal)
      (cl-weave:expect
        (ntop.process.application::process-termination-state-target-pids updated)
        :to-be
        targets)
      (cl-weave:expect
        (ntop.process.application::process-termination-state-send-status updated)
        :to-be
        :pending)))
  (cl-weave:it
    "sends the exact grouped PID list and selected OS signal number"
    (let* ((signal (wp4-process-signal "SIGKILL" 9 "Linux"))
           (sample (wp4-process-sample-with-signals signal))
           (targets '(301 302 303))
           (state (wp4-process-state-with-signal targets sample "SIGKILL"))
           (calls nil)
           (config (ntop.shared.application:make-application-config)))
      (multiple-value-bind (sent-state result)
          (ntop.process.application::send-process-termination state
                                                              config
                                                              (lambda (pids number)
                                                                (push (list pids number)
                                                                      calls)
                                                                t))
        (cl-weave:expect
          calls
          :to-equal
          (list (list targets 9)))
        (cl-weave:expect
          (first (first calls))
          :to-be
          targets)
        (cl-weave:expect
          result
          :to-be
          :sent)
        (cl-weave:expect
          (ntop.process.application::process-termination-state-send-status sent-state)
          :to-be
          :sent)
        (cl-weave:expect
          (ntop.process.application::process-termination-state-send-status state)
          :to-be
          :pending))))
  (cl-weave:it
    "does not call the sender in read-only mode"
    (let* ((signal (wp4-process-signal "SIGTERM" 15 "Darwin"))
           (sample (wp4-process-sample-with-signals signal))
           (state (wp4-process-state-with-signal '(101) sample "SIGTERM"))
           (called-p nil)
           (config (ntop.shared.application:make-application-config :read-only-p t)))
      (multiple-value-bind (returned-state result)
          (ntop.process.application::send-process-termination state
                                                              config
                                                              (lambda (pids number)
                                                                (declare (ignore pids
                                                                                 number))
                                                                (setf called-p t)
                                                                t))
        (cl-weave:expect
          called-p
          :to-be
          nil)
        (cl-weave:expect
          returned-state
          :to-be
          state)
        (cl-weave:expect
          result
          :to-be
          :read-only))))
  (cl-weave:it
    "returns permission errors and preserves state"
    (let* ((signal (wp4-process-signal "SIGTERM" 15 "Linux"))
           (sample (wp4-process-sample-with-signals signal))
           (state (wp4-process-state-with-signal '(101) sample "SIGTERM"))
           (config (ntop.shared.application:make-application-config))
           (called-with nil)
           (returned-state nil)
           (result nil))
      (multiple-value-setq
        (returned-state result)
        (ntop.process.application::send-process-termination state
                                                            config
                                                            (lambda (pids number)
                                                              (setf called-with (list pids
                                                                                      number))
                                                              (error "permission denied"))))
      (cl-weave:expect
        called-with
        :to-equal
        '((101) 15))
      (cl-weave:expect
        returned-state
        :to-be
        state)
      (cl-weave:expect
        (typep result 'error)
        :to-be-truthy)
      (cl-weave:expect
        (search "permission denied" (princ-to-string result))
        :to-be-truthy)))
  (cl-weave:it
    "preserves state when the synchronous sender returns failure"
    (let* ((signal (wp4-process-signal "SIGTERM" 15 "Linux"))
           (sample (wp4-process-sample-with-signals signal))
           (state (wp4-process-state-with-signal '(101) sample "SIGTERM"))
           (config (ntop.shared.application:make-application-config)))
      (multiple-value-bind (returned-state result)
          (ntop.process.application::send-process-termination state
                                                              config
                                                              (lambda (pids number)
                                                                (declare (ignore pids
                                                                                 number))
                                                                nil))
        (cl-weave:expect
          returned-state
          :to-be
          state)
        (cl-weave:expect
          result
          :to-be
          :send-failed))))
  (cl-weave:it
    "does not call the sender before a signal is selected"
    (let* ((state
            (ntop.process.application::make-process-termination-state :target-pids '(101)))
           (config (ntop.shared.application:make-application-config))
           (called-p nil))
      (multiple-value-bind (returned-state result)
          (ntop.process.application::send-process-termination state
                                                              config
                                                              (lambda (pids number)
                                                                (declare (ignore pids
                                                                                 number))
                                                                (setf called-p t)
                                                                t))
        (cl-weave:expect
          called-p
          :to-be
          nil)
        (cl-weave:expect
          returned-state
          :to-be
          state)
        (cl-weave:expect
          result
          :to-be
          :signal-not-selected))))
  (cl-weave:it
    "does not call the sender without target PIDs"
    (let* ((signal (wp4-process-signal "SIGTERM" 15 "Linux"))
           (sample (wp4-process-sample-with-signals signal))
           (state (wp4-process-state-with-signal nil sample "SIGTERM"))
           (config (ntop.shared.application:make-application-config))
           (called-p nil))
      (multiple-value-bind (returned-state result)
          (ntop.process.application::send-process-termination state
                                                              config
                                                              (lambda (pids number)
                                                                (declare (ignore pids
                                                                                 number))
                                                                (setf called-p t)
                                                                t))
        (cl-weave:expect
          called-p
          :to-be
          nil)
        (cl-weave:expect
          returned-state
          :to-be
          state)
        (cl-weave:expect
          result
          :to-be
          :no-targets))))
  (cl-weave:it
    "starts without a signal or targets and keeps the pending status"
    (let ((state (ntop.process.application::make-process-termination-state)))
      (cl-weave:expect
        (list (ntop.process.application::process-termination-state-selected-signal state)
              (ntop.process.application::process-termination-state-target-pids state)
              (ntop.process.application::process-termination-state-send-status state))
        :to-equal
        (list nil nil :pending)))))
