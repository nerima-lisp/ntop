(in-package #:ntop.test)

(defun %application-loop-test-history-times (store)
  (map 'list
       #'ntop.shared.domain:history-sample-sampled-at
       (ntop.shared.domain:history-slice
        store 0 :start 0d0 :end 20d0)))

(defun %run-application-loop-with-fakes
    (collection-channel poll-size render
     &key state render-with-reasons-p
       (config (ntop.shared.application:make-application-config))
       (poll-input
        (lambda (state timeout)
          (declare (ignore state timeout))
          nil))
       poll-signal on-action on-snapshot histories
       (now (lambda () 0d0))
       (wait (lambda (seconds) (declare (ignore seconds)))))
  (ntop.app.application:run-application-loop
   state config collection-channel poll-input poll-size
   (cond
     ((or render-with-reasons-p (null render)) render)
     (t (lambda (current-state current-config snapshot columns rows reasons)
          (declare (ignore reasons))
          (funcall render current-state current-config snapshot columns rows))))
   :poll-signal poll-signal
   :on-action on-action
   :on-snapshot on-snapshot
   :histories histories
   :now now
   :wait wait))

(cl-weave:describe "application event loop"
  (cl-weave:it "pauses and resumes the selected histories"
    (let ((state (ntop.app.domain::make-app-state))
          (history (ntop.shared.domain:make-history-store 1)))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :pause :value nil)
       (list history) nil)
      (cl-weave:expect (ntop.app.domain::app-state-paused-p state)
                       :to-be t)
      (cl-weave:expect (ntop.shared.domain:history-paused-p history)
                       :to-be t)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :pause :value nil)
       (list history) nil)
      (cl-weave:expect (ntop.app.domain::app-state-paused-p state)
                       :to-be nil)
      (cl-weave:expect (ntop.shared.domain:history-paused-p history)
                       :to-be nil)))

  (cl-weave:it "dispatches battery tab navigation as selection actions"
    (defmethod ntop.ui:translate-input
        ((raw-event (eql :battery-left-event))
         input-context on-meaning on-unhandled)
      (declare (ignore raw-event input-context on-unhandled))
      (funcall on-meaning
               (ntop.ui::make-input-meaning :kind :key :key "Left")))
    (defmethod ntop.ui:translate-input
        ((raw-event (eql :battery-right-event))
         input-context on-meaning on-unhandled)
      (declare (ignore raw-event input-context on-unhandled))
      (funcall on-meaning
               (ntop.ui::make-input-meaning :kind :key :key "Right")))
    (let* ((state (ntop.app.domain::make-app-state :selected-widget :battery))
           (observed nil)
           (signal-count 0))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        (cl-concurrent-kit:make-channel :buffer-size 1)
        (lambda (current timeout)
          (declare (ignore current timeout))
          (values 80 24))
        (lambda (current config snapshot columns rows)
          (declare (ignore current config snapshot columns rows))
          nil)
        :state state
        :poll-input
        (lambda (current timeout)
          (declare (ignore current timeout))
          (list :battery-left-event :battery-right-event))
        :poll-signal
        (lambda ()
          (if (= (incf signal-count) 1)
              nil
              (list :terminate :test)))
        :on-action
        (lambda (current action)
          (push (list current
                      (ntop.app.domain:app-action-kind action)
                      (ntop.app.domain:app-action-value action))
                observed)))
       :to-equal :signal)
      (cl-weave:expect
       (nreverse observed)
       :to-equal
       (list (list state :select (list :battery-tab :left))
             (list state :select (list :battery-tab :right))))))

  (cl-weave:it "toggles and explicitly sets basic and expanded modes"
    (let ((state (ntop.app.domain::make-app-state)))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value nil)
       nil nil)
      (cl-weave:expect (ntop.app.domain::app-state-basic-mode-p state)
                       :to-be t)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value t)
       nil nil)
      (cl-weave:expect (ntop.app.domain::app-state-basic-mode-p state)
                       :to-be t)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value nil)
       nil nil)
      (cl-weave:expect (ntop.app.domain::app-state-basic-mode-p state)
                       :to-be nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :expand :value t)
       nil nil)
      (cl-weave:expect (ntop.app.domain::app-state-expanded-p state)
                       :to-be t)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :expand :value nil)
       nil nil)
      (cl-weave:expect (ntop.app.domain::app-state-expanded-p state)
                       :to-be nil)))

  (cl-weave:it "returns the quit result for a quit action"
    (let ((state (ntop.app.domain::make-app-state)))
      (cl-weave:expect
       (ntop.app.application:apply-application-action
        state (ntop.app.domain:make-app-action :kind :quit :value nil)
        nil nil)
       :to-equal :quit)))

  (cl-weave:it "delivers snapshots, reasons, and prunes histories on the UI thread"
  (let* ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
         (history
           (ntop.shared.domain:make-history-store
            1 :retention-seconds 5d0 :sample-interval-seconds 1d0))
         (snapshot
           (ntop.app.domain:make-collection-snapshot :sampled-at 10d0))
         (unavailability-reasons
           (ntop.app.application::%make-application-unavailability-reasons
            :gpu :not-present))
         (collection-result
           (ntop.app.application::%make-application-collection-result
            snapshot unavailability-reasons))
         (ui-thread sb-thread:*current-thread*)
         (collector-thread nil)
         (sent-p nil)
         (callback-thread nil)
         (times-in-callback nil)
         (frames nil))
    (dolist (time (list 4d0 5d0 6d0 7d0))
      (ntop.shared.domain:history-append
       history 0 (ntop.shared.domain:make-sample-record :sampled-at time)
       time))
    (setf collector-thread
          (cl-concurrent-kit:make-thread
           (lambda ()
             (setf sent-p
                   (cl-concurrent-kit:try-send channel collection-result)))
           :name "application loop test collector"))
    (cl-concurrent-kit:join-thread collector-thread)
    (cl-weave:expect sent-p :to-be t)
    (cl-weave:expect
     (%run-application-loop-with-fakes
      channel
      (lambda (current timeout)
        (declare (ignore current timeout))
        (values 80 24))
      (lambda (current config received columns rows reasons)
        (declare (ignore current config))
        (push (list columns rows received reasons) frames))
      :render-with-reasons-p t
      :poll-signal
      (let ((poll-count 0))
        (lambda ()
          (case (incf poll-count)
            (1 nil)
            (otherwise (list :terminate :test)))))
      :on-snapshot
      (lambda (received)
        (setf callback-thread sb-thread:*current-thread*
              times-in-callback
              (%application-loop-test-history-times history))
        (cl-weave:expect received :to-be snapshot))
      :histories (list history))
     :to-equal :signal)
    (cl-weave:expect (eq callback-thread ui-thread) :to-be t)
    (cl-weave:expect (eq callback-thread collector-thread) :to-be nil)
    (cl-weave:expect times-in-callback :to-equal (list 5d0 6d0 7d0))
    (cl-weave:expect
     (ntop.app.application:application-unavailability-reason
      unavailability-reasons :gpu)
     :to-equal :not-present)
    (cl-weave:expect
     (nreverse frames)
     :to-equal (list (list 80 24 snapshot unavailability-reasons)))))

  (cl-weave:it "redraws when terminal dimensions change"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
          (size-count 0)
          (signal-count 0)
          (frames nil))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (case (incf size-count)
            ((1 2) (values 80 24))
            (otherwise (values 100 40))))
        (lambda (state config snapshot columns rows)
          (declare (ignore state config snapshot))
          (push (list columns rows) frames))
        :poll-signal
        (lambda ()
          (case (incf signal-count)
            ((1 2) nil)
            (otherwise '(:terminate :test)))))
       :to-equal :signal)
      (cl-weave:expect (nreverse frames) :to-equal '((80 24) (100 40)))))

  (cl-weave:it "redraws after a resize signal without a dimension change"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
          (signal-count 0)
          (frames nil))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (values 80 24))
        (lambda (state config snapshot columns rows)
          (declare (ignore state config snapshot))
          (push (list columns rows) frames))
        :poll-signal
        (lambda ()
          (case (incf signal-count)
            (1 nil)
            (2 :resize)
            (otherwise '(:terminate :test)))))
       :to-equal :signal)
      (cl-weave:expect (nreverse frames) :to-equal '((80 24) (80 24)))))

  (cl-weave:it "returns :signal for a termination event"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
          (input-polls 0))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (values 80 24))
        (lambda (state config snapshot columns rows)
          (declare (ignore state config snapshot columns rows))
          nil)
        :poll-input
        (lambda (state timeout)
          (declare (ignore state timeout))
          (incf input-polls)
          nil)
        :poll-signal (lambda () '(:terminate :test)))
       :to-equal :signal)
      (cl-weave:expect input-polls :to-be 0)))

  (progn
  (cl-weave:it "uses 80 by 24 when initial dimensions are unavailable"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
          (signal-count 0)
          (frames nil))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (values nil nil))
        (lambda (state config snapshot columns rows)
          (declare (ignore state config snapshot))
          (push (list columns rows) frames))
        :poll-signal
        (lambda ()
          (if (= (incf signal-count) 1)
              nil
              '(:terminate :test))))
       :to-equal :signal)
      (cl-weave:expect (nreverse frames) :to-equal '((80 24)))))

  (cl-weave:it "preserves unrelated mode values and toggles widget view states"
    (let ((state (ntop.app.domain::make-app-state)))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :toggle-widget-v :value nil)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :toggle-widget-v :value :network)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :toggle-widget-v :value :network)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value :unchanged)
       nil nil)
      (cl-weave:expect
       (ntop.app.domain::app-state-basic-mode-p state)
       :to-be nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value t)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :basic :value :unchanged)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :expand :value :unchanged)
       nil nil)
      (cl-weave:expect
       (ntop.app.domain::app-state-expanded-p state)
       :to-be nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :expand :value nil)
       nil nil)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :expand :value :unchanged)
       nil nil)
      (cl-weave:expect
       (list
        (gethash :process (ntop.app.domain::app-state-widget-v-states state))
        (gethash :network (ntop.app.domain::app-state-widget-v-states state))
        (ntop.app.domain::app-state-basic-mode-p state)
        (ntop.app.domain::app-state-expanded-p state))
       :to-equal (list t nil t t))))

  (cl-weave:it "applies zoom, history scrolling, and return-to-now actions"
    (let* ((history
             (ntop.shared.domain:make-history-store
              1 :retention-seconds 20d0 :sample-interval-seconds 1d0
              :initial-window-seconds 4d0 :zoom-step-seconds 1d0
              :minimum-window-seconds 1d0))
           (state (ntop.app.domain::make-app-state)))
      (loop for second from 0 to 10
            for sampled-at = (float second 1d0)
            do (ntop.shared.domain:history-append
                history 0
                (ntop.shared.domain:make-sample-record :sampled-at sampled-at)
                sampled-at))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :zoom :value :in)
       (list history) nil)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds history)
       :to-be 3d0)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :zoom :value :out)
       (list history) nil)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds history)
       :to-be 4d0)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :zoom :value :reset)
       (list history) nil)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds history)
       :to-be 4d0)
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :zoom :value 2d0)
       (list history) nil)
      (cl-weave:expect
       (list
        (ntop.shared.domain:history-store-window-width-seconds history)
        (ntop.app.domain::app-state-zoom-seconds state))
       :to-equal (list 2d0 2d0))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :zoom :value :unchanged)
       (list history) nil)
      (cl-weave:expect
       (list
        (ntop.shared.domain:history-store-window-width-seconds history)
        (ntop.app.domain::app-state-zoom-seconds state))
       :to-equal (list 2d0 2d0))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action
             :kind :history-scroll :value :backward)
       (list history) nil)
      (cl-weave:expect
       (multiple-value-list
        (ntop.shared.domain:history-current-window history))
       :to-equal (list 6d0 8d0))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action
             :kind :history-scroll :value :unchanged)
       (list history) nil)
      (cl-weave:expect
       (multiple-value-list
        (ntop.shared.domain:history-current-window history))
       :to-equal (list 6d0 8d0))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action
             :kind :history-scroll :value :forward)
       (list history) nil)
      (cl-weave:expect
       (multiple-value-list
        (ntop.shared.domain:history-current-window history))
       :to-equal (list 8d0 10d0))
      (ntop.app.application:apply-application-action
       state (ntop.app.domain:make-app-action :kind :return-to-now :value nil)
       (list history) nil)
      (ntop.shared.domain:history-append
       history 0
       (ntop.shared.domain:make-sample-record :sampled-at 11d0)
       11d0)
      (cl-weave:expect
       (multiple-value-list
        (ntop.shared.domain:history-current-window history))
       :to-equal (list 9d0 11d0))))

  (cl-weave:it "calls the action observer for unknown and quit actions"
    (let* ((state (ntop.app.domain::make-app-state))
           (unknown-action
             (ntop.app.domain:make-app-action :kind :unknown :value :test))
           (quit-action
             (ntop.app.domain:make-app-action :kind :quit :value nil))
           (observed nil)
           (on-action
             (lambda (current action)
               (push (list current action) observed))))
      (cl-weave:expect
       (ntop.app.application:apply-application-action
        state unknown-action nil on-action)
       :to-be state)
      (cl-weave:expect
       (ntop.app.application:apply-application-action
        state quit-action nil on-action)
       :to-equal :quit)
      (cl-weave:expect
       (nreverse observed)
       :to-equal (list (list state unknown-action)
                       (list state quit-action)))))

  (progn
  (cl-weave:it "dispatches translated key meanings as keymap actions"
    (let* ((state (ntop.app.domain::make-app-state))
           (observed nil)
           (now-calls 0))
      (defmethod ntop.ui:translate-input
          ((raw-event (eql :application-loop-pause-event))
           input-context on-meaning on-unhandled)
        (declare (ignore raw-event input-context on-unhandled))
        (funcall on-meaning
                 (ntop.ui::make-input-meaning
                  :kind :key :key "f" :text "f")))
      (defmethod ntop.ui:translate-input
          ((raw-event (eql :application-loop-quit-event))
           input-context on-meaning on-unhandled)
        (declare (ignore raw-event input-context on-unhandled))
        (funcall on-meaning
                 (ntop.ui::make-input-meaning
                  :kind :key :key "q" :text "q")))
      (cl-weave:expect
       (%run-application-loop-with-fakes
        (cl-concurrent-kit:make-channel :buffer-size 1)
        (lambda (current timeout)
          (declare (ignore current timeout))
          (values 80 24))
        (lambda (current config snapshot columns rows)
          (declare (ignore current config snapshot columns rows))
          nil)
        :state state
        :poll-input
        (lambda (current timeout)
          (declare (ignore current timeout))
          (list :application-loop-pause-event :application-loop-quit-event :application-loop-pause-event))
        :now (lambda () (float (incf now-calls) 1d0))
        :on-action
        (lambda (current action)
          (push
           (list current
                 (ntop.app.domain:app-action-kind action)
                 (ntop.app.domain:app-action-value action))
           observed)))
       :to-equal :quit)
      (cl-weave:expect
       (nreverse observed)
       :to-equal (list (list state :pause nil)
                       (list state :quit nil)))
      (cl-weave:expect (ntop.app.domain::app-state-paused-p state)
                       :to-be nil)
      (cl-weave:expect now-calls :to-be 2)))
  (cl-weave:it "dispatches printable text for both active search dialogs" (defmethod ntop.ui:translate-input ((raw-event (eql :application-loop-search-paste-event)) context on-meaning on-unhandled) (declare (ignore raw-event on-unhandled)) (cl-weave:expect (ntop.app.domain:input-context-search-mode-p context) :to-be t) (funcall on-meaning (ntop.ui::make-input-meaning :kind :paste :text "needle"))) (dolist (dialog (list :process-search :help-search)) (let* ((state (ntop.app.domain::make-app-state :active-dialog dialog)) (observed nil) (input-count 0) (signal-count 0)) (cl-weave:expect (%run-application-loop-with-fakes (cl-concurrent-kit:make-channel :buffer-size 1) (lambda (current timeout) (declare (ignore current timeout)) (values 80 24)) (lambda (current config snapshot columns rows) (declare (ignore current config snapshot columns rows)) nil) :state state :poll-input (lambda (current timeout) (declare (ignore current timeout)) (when (= (incf input-count) 1) (list :application-loop-search-paste-event))) :poll-signal (lambda () (if (= (incf signal-count) 1) nil (list :terminate :test))) :now (lambda () 1d0) :on-action (lambda (current action) (push (list current (ntop.app.domain:app-action-kind action) (ntop.app.domain:app-action-value action)) observed))) :to-equal :signal) (cl-weave:expect (nreverse observed) :to-equal (list (list state :search "needle"))) (cl-weave:expect input-count :to-be 1))))

  (cl-weave:it "passes key and click suppression flags to input dispatch"
    (defmethod ntop.ui:translate-input
        ((raw-event (eql :application-loop-disabled-key-event))
         input-context on-meaning on-unhandled)
      (declare (ignore raw-event input-context on-unhandled))
      (funcall on-meaning
               (ntop.ui::make-input-meaning
                :kind :key :key "q" :text "q")))
    (defmethod ntop.ui:translate-input
        ((raw-event (eql :application-loop-disabled-click-event))
         input-context on-meaning on-unhandled)
      (declare (ignore raw-event input-context on-unhandled))
      (funcall on-meaning
               (ntop.ui::make-input-meaning
                :kind :mouse :gesture "left-button-down" :x 2 :y 3)))
    (dolist (case '((:keys :application-loop-disabled-key-event)
                    (:click :application-loop-disabled-click-event)))
      (let* ((state (ntop.app.domain::make-app-state))
             (observed nil)
             (signal-count 0)
             (input-count 0)
             (now-count 0)
             (config
               (ecase (first case)
                 (:keys
                  (ntop.shared.application:make-application-config
                   :disable-keys-p t))
                 (:click
                  (ntop.shared.application:make-application-config
                   :disable-click-p t)))))
        (cl-weave:expect
         (%run-application-loop-with-fakes
          (cl-concurrent-kit:make-channel :buffer-size 1)
          (lambda (current timeout)
            (declare (ignore current timeout))
            (values 80 24))
          (lambda (current current-config snapshot columns rows)
            (declare (ignore current current-config snapshot columns rows))
            nil)
          :state state
          :config config
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
          :now (lambda () (float (incf now-count) 1d0))
          :on-action
          (lambda (current action)
            (push (list current action) observed)))
         :to-equal :signal)
        (cl-weave:expect observed :to-equal nil)
        (cl-weave:expect input-count :to-be 1)
        (cl-weave:expect now-count :to-be 1)))))

  (cl-weave:it "returns when the collection channel is closed"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
          (frames nil))
      (cl-concurrent-kit:close-channel channel)
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (values 80 24))
        (lambda (state config snapshot columns rows)
          (declare (ignore state config snapshot))
          (push (list columns rows) frames)))
       :to-equal :collection-closed)
      (cl-weave:expect (nreverse frames) :to-equal (list (list 80 24)))))

  (cl-weave:it "signals a collection failure"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1)))
      (cl-concurrent-kit:try-send
       channel
       (cons :failure
             (make-condition 'simple-error
                             :format-control "test collection failure"
                             :format-arguments nil)))
      (let ((failure
              (handler-case
                  (progn
                    (%run-application-loop-with-fakes
                     channel
                     (lambda (state timeout)
                       (declare (ignore state timeout))
                       (values 80 24))
                     (lambda (state config snapshot columns rows)
                       (declare (ignore state config snapshot columns rows))
                       nil))
                    nil)
                (error (condition) condition))))
        (cl-weave:expect (typep failure 'simple-error) :to-be t)
        (cl-weave:expect
         (search "test collection failure" (format nil "~A" failure))
         :to-be 0))))

  (cl-weave:it "signals an unexpected collection event"
    (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1)))
      (cl-concurrent-kit:try-send channel :unexpected)
      (let ((failure
              (handler-case
                  (progn
                    (%run-application-loop-with-fakes
                     channel
                     (lambda (state timeout)
                       (declare (ignore state timeout))
                       (values 80 24))
                     (lambda (state config snapshot columns rows)
                       (declare (ignore state config snapshot columns rows))
                       nil))
                    nil)
                (error (condition) condition))))
        (cl-weave:expect (typep failure 'simple-error) :to-be t)
        (cl-weave:expect
         (search "Unexpected collection event" (format nil "~A" failure))
         :to-be 0))))

  (cl-weave:it "renders snapshots without a snapshot callback"
    (let* ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
           (snapshot
             (ntop.app.domain:make-collection-snapshot :sampled-at 1d0))
           (signal-count 0)
           (frames nil))
      (cl-weave:expect
       (cl-concurrent-kit:try-send channel snapshot)
       :to-be t)
      (cl-weave:expect
       (%run-application-loop-with-fakes
        channel
        (lambda (state timeout)
          (declare (ignore state timeout))
          (values 80 24))
        (lambda (state config received columns rows)
          (declare (ignore state config columns rows))
          (push received frames))
        :poll-signal
        (lambda ()
          (if (= (incf signal-count) 1)
              nil
              '(:terminate :test))))
       :to-equal :signal)
      (cl-weave:expect (nreverse frames) :to-equal (list snapshot))))

  (cl-weave:it "requires a renderer function"
    (let ((failure
            (handler-case
                (progn
                  (%run-application-loop-with-fakes
                   (cl-concurrent-kit:make-channel :buffer-size 1)
                   (lambda (state timeout)
                     (declare (ignore state timeout))
                     (values 80 24))
                   nil)
                  nil)
              (error (condition) condition))))
      (cl-weave:expect (typep failure 'simple-error) :to-be t)))))
