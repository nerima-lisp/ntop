(in-package #:ntop.test)

(cl-weave:describe "application event loop input and collection"
                   (cl-weave:it "calls the action observer for forwarded unknown and quit actions"
                    (let* ((state (ntop.app.domain:make-app-state))
                           (unknown-action
                             (ntop.app.domain:make-app-action :kind :unknown :value :test))
                           (quit-action (ntop.app.domain:make-app-action :kind :quit))
                           (observed nil)
                           (composition
                             (ntop.app.application:make-application-composition
                              (ntop.shared.application:make-application-config)
                              :on-action
                              (lambda (current action)
                                (push (list current action) observed))))
                           (on-action
                             (ntop.app.application:application-composition-on-action composition)))
                      (cl-weave:expect (funcall on-action state unknown-action) :to-be state)
                      (cl-weave:expect (funcall on-action state quit-action) :to-be state)
                      (cl-weave:expect (nreverse observed)
                                       :to-equal
                                       (list (list state unknown-action)
                                             (list state quit-action)))))
                   (progn
                     (cl-weave:it
                      "dispatches translated key meanings as keymap actions"
                      (let* ((state (ntop.app.domain::make-app-state))
                             (observed nil)
                             (now-calls 0))
                        (defmethod ntop.ui:translate-input ((raw-event
                                                             (eql
                                                              :application-loop-pause-event))
                                                            input-context
                                                            on-meaning
                                                            on-unhandled)
                          (declare (ignore raw-event input-context on-unhandled))
                          (funcall on-meaning
                                   (ntop.ui::make-input-meaning :kind
                                                                :key
                                                                :key
                                                                "f"
                                                                :text
                                                                "f")))
                        (defmethod ntop.ui:translate-input ((raw-event
                                                             (eql
                                                              :application-loop-quit-event))
                                                            input-context
                                                            on-meaning
                                                            on-unhandled)
                          (declare (ignore raw-event input-context on-unhandled))
                          (funcall on-meaning
                                   (ntop.ui::make-input-meaning :kind
                                                                :key
                                                                :key
                                                                "q"
                                                                :text
                                                                "q")))
                        (cl-weave:expect
                         (%run-application-loop-with-fakes
                          (cl-concurrent-kit:make-channel :buffer-size 1)
                          (lambda (current timeout)
                            (declare (ignore current timeout))
                            (values 80 24))
                          (lambda (current config snapshot columns rows)
                            (declare (ignore current
                                             config
                                             snapshot
                                             columns
                                             rows))
                            nil)
                          :state
                          state
                          :poll-input
                          (lambda (current timeout)
                            (declare (ignore current timeout))
                            (list :application-loop-pause-event
                                  :application-loop-quit-event
                                  :application-loop-pause-event))
                          :now
                          (lambda ()
                            (float (incf now-calls) 1d0))
                          :on-action
                          (lambda (current action)
                            (push
                             (list current
                                   (ntop.app.domain:app-action-kind action)
                                   (ntop.app.domain:app-action-value action))
                             observed)))
                         :to-equal
                         :quit)
                        (cl-weave:expect (nreverse observed)
                                         :to-equal
                                         (list (list state :pause nil)
                                               (list state :quit nil)))
                        (cl-weave:expect
                         (ntop.app.domain::app-state-paused-p state)
                         :to-be
                         nil)
                        (cl-weave:expect now-calls :to-be 2)))
                     (cl-weave:it
                      "dispatches printable text for both active search dialogs"
                      (defmethod ntop.ui:translate-input ((raw-event
                                                           (eql
                                                            :application-loop-search-paste-event))
                                                          context
                                                          on-meaning
                                                          on-unhandled)
                        (declare (ignore raw-event on-unhandled))
                        (cl-weave:expect
                         (ntop.app.domain:input-context-search-mode-p context)
                         :to-be
                         t)
                        (funcall on-meaning
                                 (ntop.ui::make-input-meaning :kind
                                                              :paste
                                                              :text
                                                              "needle")))
                      (dolist (dialog (list :process-search :help-search))
                        (let* ((state
                                (ntop.app.domain::make-app-state :active-dialog
                                                                 dialog))
                               (observed nil)
                               (input-count 0)
                               (signal-count 0))
                          (cl-weave:expect
                           (%run-application-loop-with-fakes
                            (cl-concurrent-kit:make-channel :buffer-size 1)
                            (lambda (current timeout)
                              (declare (ignore current timeout))
                              (values 80 24))
                            (lambda (current config snapshot columns rows)
                              (declare (ignore current
                                               config
                                               snapshot
                                               columns
                                               rows))
                              nil)
                            :state
                            state
                            :poll-input
                            (lambda (current timeout)
                              (declare (ignore current timeout))
                              (when (= (incf input-count) 1)
                                (list :application-loop-search-paste-event)))
                            :poll-signal
                            (lambda ()
                              (if (= (incf signal-count) 1)
                                  nil
                                  (list :terminate :test)))
                            :now
                            (lambda ()
                              1d0)
                            :on-action
                            (lambda (current action)
                              (push
                               (list current
                                     (ntop.app.domain:app-action-kind action)
                                     (ntop.app.domain:app-action-value action))
                               observed)))
                           :to-equal
                           :signal)
                          (cl-weave:expect (nreverse observed)
                                           :to-equal
                                           (list (list state :search "needle")))
                          (cl-weave:expect input-count :to-be 1))))
                     (cl-weave:it
                      "passes key and click suppression flags to input dispatch"
                      (defmethod ntop.ui:translate-input ((raw-event
                                                           (eql
                                                            :application-loop-disabled-key-event))
                                                          input-context
                                                          on-meaning
                                                          on-unhandled)
                        (declare (ignore raw-event input-context on-unhandled))
                        (funcall on-meaning
                                 (ntop.ui::make-input-meaning :kind
                                                              :key
                                                              :key
                                                              "q"
                                                              :text
                                                              "q")))
                      (defmethod ntop.ui:translate-input ((raw-event
                                                           (eql
                                                            :application-loop-disabled-click-event))
                                                          input-context
                                                          on-meaning
                                                          on-unhandled)
                        (declare (ignore raw-event input-context on-unhandled))
                        (funcall on-meaning
                                 (ntop.ui::make-input-meaning :kind
                                                              :mouse
                                                              :gesture
                                                              "left-button-down"
                                                              :x
                                                              2
                                                              :y
                                                              3)))
                      (dolist
                          (case '((:keys :application-loop-disabled-key-event)
                                  (:click
                                   :application-loop-disabled-click-event)))
                        (let* ((state (ntop.app.domain::make-app-state))
                               (observed nil)
                               (signal-count 0)
                               (input-count 0)
                               (now-count 0)
                               (config
                                (ecase (first case)
                                  (:keys
                                   (ntop.shared.application:make-application-config
                                    :disable-keys-p
                                    t))
                                  (:click
                                   (ntop.shared.application:make-application-config
                                    :disable-click-p
                                    t)))))
                          (cl-weave:expect
                           (%run-application-loop-with-fakes
                            (cl-concurrent-kit:make-channel :buffer-size 1)
                            (lambda (current timeout)
                              (declare (ignore current timeout))
                              (values 80 24))
                            (lambda
                                (current current-config snapshot columns rows)
                              (declare (ignore current
                                               current-config
                                               snapshot
                                               columns
                                               rows))
                              nil)
                            :state
                            state
                            :config
                            config
                            :poll-input
                            (lambda (current timeout)
                              (declare (ignore current timeout))
                              (when (= (incf input-count) 1)
                                (list (second case))))
                            :poll-signal
                            (lambda ()
                              (if (= (incf signal-count) 1)
                                  nil
                                  '(:terminate :test)))
                            :now
                            (lambda ()
                              (float (incf now-count) 1d0))
                            :on-action
                            (lambda (current action)
                              (push (list current action) observed)))
                           :to-equal
                           :signal)
                          (cl-weave:expect observed :to-equal nil)
                          (cl-weave:expect input-count :to-be 1)
                          (cl-weave:expect now-count :to-be 1)))))
                   (cl-weave:it "returns when the collection channel is closed"
                                (let ((channel
                                       (cl-concurrent-kit:make-channel
                                        :buffer-size
                                        1))
                                      (frames nil))
                                  (cl-concurrent-kit:close-channel channel)
                                  (cl-weave:expect
                                   (%run-application-loop-with-fakes channel
                                                                     (lambda
                                                                         (state
                                                                          timeout)
                                                                       (declare (ignore
                                                                                 state
                                                                                 timeout))
                                                                       (values
                                                                        80
                                                                        24))
                                                                     (lambda
                                                                         (state
                                                                          config
                                                                          snapshot
                                                                          columns
                                                                          rows)
                                                                       (declare (ignore
                                                                                 state
                                                                                 config
                                                                                 snapshot))
                                                                       (push
                                                                        (list
                                                                         columns
                                                                         rows)
                                                                        frames)))
                                   :to-equal
                                   :collection-closed)
                                  (cl-weave:expect (nreverse frames)
                                                   :to-equal
                                                   (list (list 80 24)))))
                   (cl-weave:it "signals a collection failure"
                                (let ((channel
                                       (cl-concurrent-kit:make-channel
                                        :buffer-size
                                        1)))
                                  (cl-concurrent-kit:try-send channel
                                                              (cons :failure
                                                                    (make-condition
                                                                     'simple-error
                                                                     :format-control
                                                                     "test collection failure"
                                                                     :format-arguments
                                                                     nil)))
                                  (let ((failure
                                         (handler-case (progn
                                                         (%run-application-loop-with-fakes
                                                          channel
                                                          (lambda
                                                              (state timeout)
                                                            (declare (ignore
                                                                      state
                                                                      timeout))
                                                            (values 80 24))
                                                          (lambda
                                                              (state config
                                                                     snapshot
                                                                     columns
                                                                     rows)
                                                            (declare (ignore
                                                                      state
                                                                      config
                                                                      snapshot
                                                                      columns
                                                                      rows))
                                                            nil))
                                                         nil)
                                           (error (condition)
                                             condition))))
                                    (cl-weave:expect
                                     (typep failure 'simple-error)
                                     :to-be
                                     t)
                                    (cl-weave:expect
                                     (search "test collection failure"
                                             (format nil "~A" failure))
                                     :to-be
                                     0))))
                   (cl-weave:it "signals an unexpected collection event"
                                (let ((channel
                                       (cl-concurrent-kit:make-channel
                                        :buffer-size
                                        1)))
                                  (cl-concurrent-kit:try-send channel
                                                              :unexpected)
                                  (let ((failure
                                         (handler-case (progn
                                                         (%run-application-loop-with-fakes
                                                          channel
                                                          (lambda
                                                              (state timeout)
                                                            (declare (ignore
                                                                      state
                                                                      timeout))
                                                            (values 80 24))
                                                          (lambda
                                                              (state config
                                                                     snapshot
                                                                     columns
                                                                     rows)
                                                            (declare (ignore
                                                                      state
                                                                      config
                                                                      snapshot
                                                                      columns
                                                                      rows))
                                                            nil))
                                                         nil)
                                           (error (condition)
                                             condition))))
                                    (cl-weave:expect
                                     (typep failure 'simple-error)
                                     :to-be
                                     t)
                                    (cl-weave:expect
                                     (search "Unexpected collection event"
                                             (format nil "~A" failure))
                                     :to-be
                                     0))))
                   (cl-weave:it "renders snapshots without a snapshot callback"
                                (let* ((channel
                                        (cl-concurrent-kit:make-channel
                                         :buffer-size
                                         1))
                                       (snapshot
                                        (ntop.app.domain:make-collection-snapshot
                                         :sampled-at
                                         1d0))
                                       (signal-count 0)
                                       (frames nil))
                                  (cl-weave:expect
                                   (cl-concurrent-kit:try-send channel snapshot)
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (%run-application-loop-with-fakes channel
                                                                     (lambda
                                                                         (state
                                                                          timeout)
                                                                       (declare (ignore
                                                                                 state
                                                                                 timeout))
                                                                       (values
                                                                        80
                                                                        24))
                                                                     (lambda
                                                                         (state
                                                                          config
                                                                          received
                                                                          columns
                                                                          rows)
                                                                       (declare (ignore
                                                                                 state
                                                                                 config
                                                                                 columns
                                                                                 rows))
                                                                       (push
                                                                        received
                                                                        frames))
                                                                     :poll-signal
                                                                     (lambda ()
                                                                       (if (=
                                                                            (incf
                                                                             signal-count)
                                                                            1)
                                                                           nil
                                                                           '(:terminate
                                                                             :test))))
                                   :to-equal
                                   :signal)
                                  (cl-weave:expect (nreverse frames)
                                                   :to-equal
                                                   (list snapshot))))
                   (cl-weave:it "requires a renderer function"
                                (let ((failure
                                       (handler-case (progn
                                                       (%run-application-loop-with-fakes
                                                        (cl-concurrent-kit:make-channel
                                                         :buffer-size
                                                         1)
                                                        (lambda (state timeout)
                                                          (declare (ignore state
                                                                           timeout))
                                                          (values 80 24))
                                                        nil)
                                                       nil)
                                         (error (condition)
                                           condition))))
                                  (cl-weave:expect (typep failure 'simple-error)
                                                   :to-be
                                                   t))))
