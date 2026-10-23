(in-package #:ntop.test)

(defun %f8b-process-signal (name number)
  (ntop.process.domain:make-process-signal :name name :number number :operating-system "fixture"))

(defun %f8b-process-sample (&rest signals)
  (ntop.process.domain:make-process-sample :sampled-at 0d0 :signals signals))

(progn
  (defun %f8b-open-termination-dialog (pids name sample config)
    (ntop.process.presentation::open-process-termination-dialog pids name sample config))
  (defun %f8b-opened-termination-dialog (pids name sample config)
    (multiple-value-bind (dialog result) (%f8b-open-termination-dialog pids name sample config)
      (cl-weave:expect result :to-be :opened)
      dialog)))

(cl-weave:describe "Process termination dialog presentation"
                   (cl-weave:it "expands the dialog widget through define-widget"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.ui:define-widget :process-termination-dialog
                                                          (target rectangle state config sample)
                                                          :rendered))
                                 :to-equal
                                 '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key
                                                                   (eql :process-termination-dialog)) target
                                                                  rectangle
                                                                  state
                                                                  config
                                                                  sample)
                                    (declare (ignore ntop.ui::widget-key))
                                    :rendered)))
                   (cl-weave:it "does not open in read-only mode or without targets"
                                (let ((sample (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)))
                                      (read-only (ntop.shared.application:make-application-config :read-only-p t))
                                      (config (ntop.shared.application:make-application-config)))
                                  (cl-weave:expect
                                   (multiple-value-list
                                    (%f8b-open-termination-dialog '(123) "fixture-worker" sample read-only))
                                   :to-equal
                                   '(nil :read-only))
                                  (cl-weave:expect
                                   (multiple-value-list
                                    (%f8b-open-termination-dialog nil "fixture-worker" sample config))
                                   :to-equal
                                   '(nil :no-targets))))
                   (cl-weave:it "uses sample signals and selects SIGTERM by its number on Unix"
                                #+unix
 (let* ((signals (list (%f8b-process-signal "SIGKILL" 9)
                       (%f8b-process-signal "SIGTERM" 15)
                       (%f8b-process-signal "SIGINT" 2)
                       (%f8b-process-signal "SIGZERO" 0)
                       (%f8b-process-signal nil 4)))
        (sample (apply #'%f8b-process-sample signals))
        (config (ntop.shared.application:make-application-config))
        (dialog nil))
   (multiple-value-bind (state result)
       (%f8b-open-termination-dialog '(123) "fixture-worker" sample config)
     (setf dialog state)
     (cl-weave:expect result :to-be :opened))
   (cl-weave:expect
    (ntop.process.presentation::process-termination-dialog-state-mode dialog)
    :to-be :signals)
   (cl-weave:expect
    (mapcar #'cdr
            (ntop.process.presentation::process-termination-dialog-choices
             dialog sample))
    :to-equal '("0: Cancel" "2: SIGINT" "9: SIGKILL" "15: SIGTERM"))
   (cl-weave:expect
    (ntop.ui:table-state-selected-index
     (ntop.process.presentation::process-termination-dialog-state-selection-state
      dialog))
    :to-be 3)
   (cl-weave:expect
    (mapcar #'ntop.process.domain:process-signal-number
            (ntop.process.domain:process-sample-signals sample))
    :to-equal '(9 15 2 0 4))))
                   (cl-weave:it "uses Yes and No choices with No selected when advanced kill is disabled"
                                (let* ((sample (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)))
                                       (config
                                        (ntop.shared.application:make-application-config :disable-advanced-kill-p t))
                                       (dialog nil))
                                  (setf dialog (%f8b-opened-termination-dialog '(123) "fixture-worker" sample config))
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-termination-dialog-state-mode dialog)
                                   :to-be
                                   :simple)
                                  (cl-weave:expect
                                   (mapcar #'cdr
                                           (ntop.process.presentation::process-termination-dialog-choices dialog sample))
                                   :to-equal
                                   '("Yes" "No"))
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index
                                    (ntop.process.presentation::process-termination-dialog-state-selection-state dialog))
                                   :to-be
                                   1)))
                   (cl-weave:it "does not send when No or Cancel is selected"
                                (let* ((sample (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)))
                                       (simple-config
                                        (ntop.shared.application:make-application-config :disable-advanced-kill-p t))
                                       (advanced-config (ntop.shared.application:make-application-config))
                                       (calls nil)
                                       (simple-dialog nil)
                                       (advanced-dialog nil))
                                  (setf simple-dialog (%f8b-opened-termination-dialog '(123)
                                                                                      "fixture-worker"
                                                                                      sample
                                                                                      simple-config))
                                  (multiple-value-bind (termination-state result)
                                      (ntop.process.presentation::confirm-process-termination-dialog simple-dialog
                                                                                                     sample
                                                                                                     simple-config
                                                                                                     (lambda
                                                                                                         (&rest args)
                                                                                                       (push args calls)
                                                                                                       t))
                                    (cl-weave:expect result :to-be :cancel)
                                    (cl-weave:expect calls :to-be nil)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status termination-state)
                                     :to-be
                                     :pending))
                                  (ntop.process.presentation::select-process-termination-dialog-choice simple-dialog
                                                                                                       sample
                                                                                                       0)
                                  (multiple-value-bind (termination-state result)
                                      (ntop.process.presentation::confirm-process-termination-dialog simple-dialog
                                                                                                     sample
                                                                                                     simple-config
                                                                                                     (lambda
                                                                                                         (&rest args)
                                                                                                       (push args calls)
                                                                                                       t))
                                    (cl-weave:expect result :to-be :sent)
                                    (cl-weave:expect (first (first calls)) :to-equal '(123))
                                    (cl-weave:expect (second (first calls)) :to-be 15)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status termination-state)
                                     :to-be
                                     :sent))
                                  (setf advanced-dialog (%f8b-opened-termination-dialog '(123)
                                                                                        "fixture-worker"
                                                                                        sample
                                                                                        advanced-config))
                                  (ntop.process.presentation::select-process-termination-dialog-choice advanced-dialog
                                                                                                       sample
                                                                                                       0)
                                  (multiple-value-bind (termination-state result)
                                      (ntop.process.presentation::confirm-process-termination-dialog advanced-dialog
                                                                                                     sample
                                                                                                     advanced-config
                                                                                                     (lambda
                                                                                                         (&rest args)
                                                                                                       (push args calls)
                                                                                                       t))
                                    (cl-weave:expect result :to-be :cancel)
                                    (cl-weave:expect (length calls) :to-be 1)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status termination-state)
                                     :to-be
                                     :pending))))
                   (cl-weave:it "sends the exact selected sample signal and grouped PIDs"
                                #+unix
 (let* ((sample (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)
                                     (%f8b-process-signal "SIGKILL" 9)))
        (config (ntop.shared.application:make-application-config))
        (calls nil)
        (dialog nil))
   (multiple-value-bind (state result)
       (%f8b-open-termination-dialog '(123 124) "fixture-worker" sample config)
     (setf dialog state)
     (cl-weave:expect result :to-be :opened))
   (ntop.process.presentation::select-process-termination-dialog-choice
    dialog sample 1)
   (multiple-value-bind (termination-state result)
       (ntop.process.presentation::confirm-process-termination-dialog
        dialog sample config
        (lambda (pids signal-number)
          (push (list pids signal-number) calls)
          t))
     (cl-weave:expect result :to-be :sent)
     (cl-weave:expect calls :to-equal '(((123 124) 9)))
     (cl-weave:expect
      (ntop.process.domain:process-signal-name
       (ntop.process.application:process-termination-state-selected-signal
        termination-state))
      :to-equal "SIGKILL"))))
                   (cl-weave:it "does not send Yes when SIGTERM is absent from the sample"
                                (let* ((sample (%f8b-process-sample (%f8b-process-signal "SIGINT" 2)))
                                       (config
                                        (ntop.shared.application:make-application-config :disable-advanced-kill-p t))
                                       (calls 0)
                                       (dialog nil))
                                  (setf dialog (%f8b-opened-termination-dialog '(123) "fixture-worker" sample config))
                                  (ntop.process.presentation::select-process-termination-dialog-choice dialog sample 0)
                                  (multiple-value-bind (termination-state result)
                                      (ntop.process.presentation::confirm-process-termination-dialog dialog
                                                                                                     sample
                                                                                                     config
                                                                                                     (lambda
                                                                                                         (&rest args)
                                                                                                       (declare (ignore
                                                                                                                 args))
                                                                                                       (incf calls)
                                                                                                       t))
                                    (cl-weave:expect result :to-be :unknown-signal)
                                    (cl-weave:expect calls :to-be 0)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status termination-state)
                                     :to-be
                                     :pending))))
                   (cl-weave:it "handles empty signal samples, selection movement, and invalid indexes"
                                (let* ((sample (%f8b-process-sample (%f8b-process-signal "SIGINT" 2)))
                                       (config (ntop.shared.application:make-application-config))
                                       (dialog nil))
                                  (cl-weave:expect (ntop.process.presentation::%process-sample-signals nil) :to-be nil)
                                  (cl-weave:expect
                                   (mapcar #'cdr
                                           (ntop.process.presentation::process-termination-dialog-choices
                                            (ntop.process.presentation::%make-process-termination-dialog-state :mode
                                                                                                               :signals)
                                            nil))
                                   :to-equal
                                   '("0: Cancel"))
                                  (setf dialog (%f8b-opened-termination-dialog '(123) "fixture-worker" sample config))
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index
                                    (ntop.process.presentation::process-termination-dialog-state-selection-state dialog))
                                   :to-be
                                   0)
                                  (dolist (index (list nil -1 2))
                                    (multiple-value-bind (selected result)
                                        (ntop.process.presentation::select-process-termination-dialog-choice dialog
                                                                                                             sample
                                                                                                             index)
                                      (declare (ignore selected))
                                      (cl-weave:expect result :to-be :unknown-choice)))
                                  (ntop.process.presentation::move-process-termination-dialog-selection dialog
                                                                                                        sample
                                                                                                        1
                                                                                                        1)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index
                                    (ntop.process.presentation::process-termination-dialog-state-selection-state dialog))
                                   :to-be
                                   1)
                                  (ntop.process.presentation::move-process-termination-dialog-selection dialog
                                                                                                        sample
                                                                                                        -1
                                                                                                        1)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index
                                    (ntop.process.presentation::process-termination-dialog-state-selection-state dialog))
                                   :to-be
                                   0)
                                  (setf (ntop.ui:table-state-selected-index
                                         (ntop.process.presentation::process-termination-dialog-state-selection-state
                                          dialog)) 2)
                                  (multiple-value-bind (termination-state result)
                                      (ntop.process.presentation::confirm-process-termination-dialog dialog
                                                                                                     sample
                                                                                                     config
                                                                                                     (lambda
                                                                                                         (&rest args)
                                                                                                       (declare (ignore
                                                                                                                 args))
                                                                                                       t))
                                    (cl-weave:expect result :to-be :no-choice)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status termination-state)
                                     :to-be
                                     :pending))))
                   (cl-weave:it "ignores a signal that is absent from the sample"
                                (let* ((sample (%f8b-process-sample))
                                       (termination-state
                                        (ntop.process.application:make-process-termination-state :target-pids '(123)))
                                       (signal (%f8b-process-signal "SIGUSR1" 10))
                                       (config (ntop.shared.application:make-application-config))
                                       (calls 0))
                                  (multiple-value-bind (selected-state result)
                                      (ntop.process.presentation::%send-process-termination-signal termination-state
                                                                                                   sample
                                                                                                   signal
                                                                                                   config
                                                                                                   (lambda (&rest args)
                                                                                                     (declare (ignore
                                                                                                               args))
                                                                                                     (incf calls)
                                                                                                     t))
                                    (cl-weave:expect result :to-be :unknown-signal)
                                    (cl-weave:expect
                                     (ntop.process.application:process-termination-state-send-status selected-state)
                                     :to-be
                                     :pending)
                                    (cl-weave:expect calls :to-be 0)))))
