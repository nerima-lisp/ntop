(in-package #:ntop.test)

(defun %application-loop-test-history-times (store)
  (map 'list
       #'ntop.shared.domain:history-sample-sampled-at
       (ntop.shared.domain:history-slice
        store 0 :start 0d0 :end 20d0)))
(defun %application-loop-test-cpu-snapshot (sampled-at usage)
  (ntop.app.domain:make-collection-snapshot
   :sampled-at sampled-at
   :cpu-sample
   (ntop.cpu.domain:make-cpu-sample
    :sampled-at sampled-at
    :average-usage-percent usage)))

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
  (cl-weave:it
   "keeps widget navigation aligned with visible screen placements"
   (let* ((state (ntop.app.domain:make-app-state :selected-widget :cpu))
          (config (ntop.shared.application:make-application-config))
          (screen-layout
           (ntop.app.presentation::%screen-layout-placements
            80 24 state :default))
          (navigation-layout
           (ntop.app.application::%layout-widgets-for-state
            state config 80 24))
          (expected
           (quote ((:cpu 0 0 53 8)
  (:memory 53 0 27 8)
  (:network 0 8 32 8)
  (:disks 32 8 32 8)
  (:temperature 64 8 16 8)
  (:process 0 16 80 8)))))
     (flet ((layout-values (placements)
              (mapcar
               (lambda (entry)
                 (let ((rectangle (cdr entry)))
                   (list (car entry)
                         (ntop.ui:rectangle-x rectangle)
                         (ntop.ui:rectangle-y rectangle)
                         (ntop.ui:rectangle-width rectangle)
                         (ntop.ui:rectangle-height rectangle))))
               placements)))
       (cl-weave:expect (layout-values screen-layout) :to-equal expected)
       (cl-weave:expect (layout-values navigation-layout) :to-equal expected))
     (ntop.app.application::%select-layout-neighbor state config 80 24 :down)
     (cl-weave:expect
      (ntop.app.domain:app-state-selected-widget state)
      :to-equal
      :network)))
  (cl-weave:it "pauses and resumes composition history"
    (let* ((config
             (ntop.shared.application:make-application-config
              :rate-ms 1000
              :retention-seconds 600
              :time-window-seconds 60
              :zoom-window-seconds 15))
           (composition
             (ntop.app.application:make-application-composition config))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot
              composition))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (funcall on-snapshot (%application-loop-test-cpu-snapshot 1d0 50d0))
      (let ((stores
              (ntop.app.application:application-composition-history-stores
               composition)))
        (cl-weave:expect (not (zerop (length stores))) :to-be t)
        (funcall on-action state
                 (ntop.app.domain:make-app-action :kind :pause))
        (cl-weave:expect (ntop.app.domain:app-state-paused-p state) :to-be t)
        (cl-weave:expect
         (every (function ntop.shared.domain:history-paused-p) stores)
         :to-be t)
        (funcall on-action state
                 (ntop.app.domain:make-app-action :kind :pause))
        (cl-weave:expect (ntop.app.domain:app-state-paused-p state) :to-be nil)
        (cl-weave:expect
         (every (lambda (store)
                  (not (ntop.shared.domain:history-paused-p store)))
                stores)
         :to-be t))))
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
  (cl-weave:it "leaves battery navigation unchanged while a dialog is active"
               (let* ((context
                       (ntop.app.domain:make-input-context
                        :selected-widget :battery :active-dialog :help))
                      (action
                       (ntop.app.domain:make-app-action :kind :navigation :value :left)))
                 (cl-weave:expect
                  (ntop.app.application::%contextualize-application-action action context)
                  :to-be action)))

  (cl-weave:it "toggles and explicitly sets basic and expanded modes"
    (let ((state (ntop.app.domain:make-app-state)))
      (flet ((act (kind value)
               (setf state
                     (nth-value
                      0
                      (ntop.app.application::transition-application-state
                       state
                       (ntop.app.domain:make-app-action :kind kind :value value))))))
        (act :basic nil)
        (cl-weave:expect (ntop.app.domain:app-state-basic-mode-p state) :to-be t)
        (act :basic t)
        (cl-weave:expect (ntop.app.domain:app-state-basic-mode-p state) :to-be t)
        (act :basic nil)
        (cl-weave:expect (ntop.app.domain:app-state-basic-mode-p state) :to-be nil)
        (act :expand t)
        (cl-weave:expect (ntop.app.domain:app-state-expanded-p state) :to-be t)
        (act :expand nil)
        (cl-weave:expect (ntop.app.domain:app-state-expanded-p state) :to-be nil))))

  (cl-weave:it "forwards quit actions and preserves selection"
    (let* ((state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (action (ntop.app.domain:make-app-action :kind :quit)))
      (multiple-value-bind (next-state forwarded-action history-intent)
          (ntop.app.application::transition-application-state state action)
        (cl-weave:expect (ntop.app.domain:app-state-selected-widget next-state) :to-equal :cpu)
        (cl-weave:expect forwarded-action :to-be action)
        (cl-weave:expect history-intent :to-be nil)
        (cl-weave:expect (ntop.app.domain:app-state-selected-widget state) :to-be :cpu))))

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

  (cl-weave:it "uses 80 by 24 when initial dimensions are not positive integers"
    (dolist (dimensions (list (list nil nil) (list 80 0) (list 0 24)))
      (let ((channel (cl-concurrent-kit:make-channel :buffer-size 1))
            (signal-count 0)
            (frames nil))
        (cl-weave:expect
         (%run-application-loop-with-fakes
          channel
          (lambda (state timeout)
            (declare (ignore state timeout))
            (values (first dimensions) (second dimensions)))
          (lambda (state config snapshot columns rows)
            (declare (ignore state config snapshot))
            (push (list columns rows) frames))
          :poll-signal
          (lambda ()
            (if (= (incf signal-count) 1)
                nil
                (list :terminate :test))))
         :to-equal
         :signal)
        (cl-weave:expect (nreverse frames) :to-equal (list (list 80 24))))))

  (cl-weave:it "preserves mode values, cycles widget views, and suppresses expand in basic mode"
    (let* ((state (ntop.app.domain:make-app-state :selected-widget :memory))
           (basic-unchanged-action
             (ntop.app.domain:make-app-action
              :kind :basic :value :unchanged)))
      (flet ((act (kind value)
               (setf state
                     (nth-value
                      0
                      (ntop.app.application::transition-application-state
                       state
                       (ntop.app.domain:make-app-action :kind kind :value value))))))
        (act :toggle-widget-v nil)
        (cl-weave:expect
         (gethash :memory (ntop.app.domain:app-state-widget-v-states state))
         :to-be :gpu)
        (act :toggle-widget-v nil)
        (multiple-value-bind (next-state forwarded-action history-intent)
            (ntop.app.application::transition-application-state
             state basic-unchanged-action)
          (cl-weave:expect
           (ntop.app.domain:app-state-basic-mode-p next-state)
           :to-be nil)
          (cl-weave:expect forwarded-action :to-be basic-unchanged-action)
          (cl-weave:expect history-intent :to-be nil)
          (setf state next-state))
        (act :expand t)
        (cl-weave:expect (ntop.app.domain:app-state-expanded-p state) :to-be t)
        (act :basic t)
        (multiple-value-bind (next-state forwarded-action history-intent)
            (ntop.app.application::transition-application-state
             state basic-unchanged-action)
          (cl-weave:expect
           (ntop.app.domain:app-state-basic-mode-p next-state)
           :to-be t)
          (cl-weave:expect forwarded-action :to-be basic-unchanged-action)
          (cl-weave:expect history-intent :to-be nil)
          (setf state next-state))
        (act :expand :unchanged)
        (act :expand nil)
        (act :expand :unchanged)
        (cl-weave:expect
         (list
          (gethash :memory (ntop.app.domain:app-state-widget-v-states state))
          (ntop.app.domain:app-state-basic-mode-p state)
          (ntop.app.domain:app-state-expanded-p state))
         :to-equal '(nil t t)))))

  (cl-weave:it "applies history actions and clamps zoom to the composition minimum of 30 seconds"
    (let* ((config
             (ntop.shared.application:make-application-config
              :rate-ms 1000
              :retention-seconds 600
              :time-window-seconds 60
              :zoom-window-seconds 15))
           (composition
             (ntop.app.application:make-application-composition config))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot
              composition))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (loop for second from 0 to 120
            for sampled-at = (float second 1d0)
            do (funcall on-snapshot
                        (%application-loop-test-cpu-snapshot sampled-at 50d0)))
      (let ((history
              (aref
               (ntop.app.application:application-composition-history-stores
                composition)
               0)))
        (flet ((act (kind value)
                 (funcall on-action state
                          (ntop.app.domain:make-app-action
                           :kind kind :value value))))
          (act :zoom :in)
          (cl-weave:expect
           (ntop.shared.domain:history-store-window-width-seconds history)
           :to-be 45d0)
          (act :zoom :out)
          (cl-weave:expect
           (ntop.shared.domain:history-store-window-width-seconds history)
           :to-be 60d0)
          (act :zoom :reset)
          (cl-weave:expect
           (ntop.shared.domain:history-store-window-width-seconds history)
           :to-be 60d0)
          (act :zoom 40d0)
          (cl-weave:expect
           (ntop.shared.domain:history-store-window-width-seconds history)
           :to-be 40d0)
          (act :zoom 2d0)
          (cl-weave:expect
           (list
            (ntop.shared.domain:history-store-window-width-seconds history)
            (ntop.app.domain:app-state-zoom-seconds state))
           :to-equal '(30d0 30d0))
          (act :zoom :unchanged)
          (cl-weave:expect
           (ntop.shared.domain:history-store-window-width-seconds history)
           :to-be 30d0)
          (act :history-scroll :backward)
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(60d0 90d0))
          (act :history-scroll :unchanged)
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(60d0 90d0))
          (act :history-scroll :forward)
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(90d0 120d0))
          (act :history-scroll :backward)
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(60d0 90d0))
          (act :return-to-now nil)
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(90d0 120d0))
          (funcall on-snapshot
                   (%application-loop-test-cpu-snapshot 130d0 50d0))
          (cl-weave:expect
           (multiple-value-list
            (ntop.shared.domain:history-current-window history))
           :to-equal '(100d0 130d0)))))))

