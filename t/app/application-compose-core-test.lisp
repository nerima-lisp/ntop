(in-package #:ntop.test)

(cl-weave:describe "application composition"
  (cl-weave:it "records samples and passes feature series and reasons to render"
    (let* ((rendered-series nil)
           (first-process (%composition-test-process-sample 10d0 100000000 1000))
           (second-process (%composition-test-process-sample 20d0 200000000 3000))
           (composition (%make-composition-test-instance
                         :on-render (lambda (series)
                                      (setf rendered-series series))))
           (first-snapshot
             (%composition-test-snapshot
              10d0 :cpu-usage 40d0 :memory-used 40
              :gpu-devices
              (list (%composition-test-gpu-device "A" 50 100)
                    (%composition-test-gpu-device "No total" 25 nil))
              :process-sample first-process))
           (snapshot
             (%composition-test-snapshot
              20d0 :cpu-usage 60d0 :memory-used 55
              :gpu-devices
              (list (%composition-test-gpu-device "A" 80 100)
                    (%composition-test-gpu-device "No total" 35 nil))
              :process-sample second-process))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (reasons
             (ntop.app.application::%make-application-unavailability-reasons
              :gpu :disabled)))
      (cl-weave:expect
       (funcall (ntop.app.application:application-composition-on-snapshot
                 composition)
                first-snapshot)
       :to-be first-snapshot)
      (funcall (ntop.app.application:application-composition-on-snapshot
                composition)
               snapshot)
      (%render-composition-test-instance composition state snapshot reasons)
      (let* ((cpu-series (getf (cdr (assoc :cpu rendered-series)) :usage))
             (memory (cdr (assoc :memory rendered-series)))
             (gpu (cdr (assoc :gpu rendered-series)))
             (gpu-memory-series (getf memory :gpu-memory-series))
             (gpu-memory (first gpu-memory-series)))
        (cl-weave:expect
         (map 'list #'ntop.shared.domain:history-sample-value
              (ntop.ui:chart-series-samples (first cpu-series)))
         :to-equal '(40d0 60d0))
        (cl-weave:expect
         (getf (cdr (assoc :process rendered-series)) :previous-sample)
         :to-be first-process)
        (cl-weave:expect (getf gpu :unavailability-reason) :to-be :disabled)
        (cl-weave:expect (length gpu-memory-series) :to-be 1)
        (cl-weave:expect
         (map 'list #'ntop.shared.domain:history-sample-value
              (ntop.ui:chart-series-samples gpu-memory))
         :to-equal '(50d0 80d0))
        (cl-weave:expect (getf memory :gpu-memory-total-bytes) :to-be 100))))
  (cl-weave:it "omits GPU memory series and capacity without a valid device total"
    (let* ((rendered-series nil)
           (composition (%make-composition-test-instance
                         :on-render (lambda (series)
                                      (setf rendered-series series))))
           (snapshot
             (%composition-test-snapshot
              10d0
              :gpu-devices (list (%composition-test-gpu-device "Unknown" 25 nil)))))
      (funcall (ntop.app.application:application-composition-on-snapshot
                composition)
               snapshot)
      (%render-composition-test-instance
       composition (ntop.app.domain:make-app-state) snapshot nil)
      (let ((memory (cdr (assoc :memory rendered-series))))
        (cl-weave:expect (getf memory :gpu-memory-series) :to-be nil)
        (cl-weave:expect
         (member :gpu-memory-total-bytes memory) :to-be nil))))

  (cl-weave:it "pauses all stores and applies zoom only to the selected graph"
    (let* ((composition (%make-composition-test-instance))
           (first-snapshot
             (%composition-test-snapshot 10d0 :cpu-usage 40d0 :memory-used 10))
           (second-snapshot
             (%composition-test-snapshot 20d0 :cpu-usage 50d0 :memory-used 20))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (callback
             (ntop.app.application:application-composition-on-action composition))
           (pause (ntop.app.domain:make-app-action :kind :pause)))
      (funcall (ntop.app.application:application-composition-on-snapshot
                composition)
               first-snapshot)
      (funcall (ntop.app.application:application-composition-on-snapshot
                composition)
               second-snapshot)
      (funcall callback state pause)
      (cl-weave:expect
       (every #'ntop.shared.domain:history-paused-p
              (ntop.app.application:application-composition-history-stores
               composition))
       :to-be t)
      (funcall callback state pause)
      (cl-weave:expect
       (every (lambda (store)
                (not (ntop.shared.domain:history-paused-p store)))
              (ntop.app.application:application-composition-history-stores
               composition))
       :to-be t)
      (funcall callback state
               (ntop.app.domain:make-app-action :kind :zoom :value :in))
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store composition '(:cpu :usage :average)))
       :to-be 45d0)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store composition '(:memory :ram)))
       :to-be 60d0)))

  (cl-weave:it "scrolls network history into view and returns to the live window"
    (let* ((composition (%make-composition-test-instance))
           (state (ntop.app.domain:make-app-state :selected-widget :network))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (loop for sampled-at from 10 to 210 by 20
            for time = (float sampled-at 1d0)
            do (funcall on-snapshot
                        (%composition-test-snapshot time :network-p t)))
      (funcall on-action state
               (ntop.app.domain:make-app-action
                :kind :history-scroll :value :backward))
      (cl-weave:expect
       (mapcar #'ntop.shared.domain:sample-record-sampled-at
               (ntop.app.application::%network-visible-samples composition))
       :to-equal '(90d0 110d0 130d0 150d0))
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :return-to-now))
      (cl-weave:expect
       (mapcar #'ntop.shared.domain:sample-record-sampled-at
               (ntop.app.application::%network-visible-samples composition))
       :to-equal '(150d0 170d0 190d0 210d0))))

  (cl-weave:it "forwards feature and navigation actions while handling selection"
    (let* ((forwarded nil)
           (composition
             (%make-composition-test-instance
              :on-action (lambda (state action)
                           (push (list state action) forwarded))))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (feature-action
             (ntop.app.domain:make-app-action
              :kind :sort-processes :value :cpu-percent))
           (reverse-sort-action
             (ntop.app.domain:make-app-action :kind :reverse-process-sort))
           (navigation-action
             (ntop.app.domain:make-app-action :kind :navigation :value :right)))
      (cl-weave:expect
       (ntop.app.application::application-composition-selection-active-p composition)
       :to-be nil)
      (funcall on-action state feature-action)
      (cl-weave:expect (first (first forwarded)) :to-be state)
      (cl-weave:expect (second (first forwarded)) :to-be feature-action)
      (funcall on-action state reverse-sort-action)
      (cl-weave:expect (second (first forwarded)) :to-be reverse-sort-action)
      (funcall on-action state navigation-action)
      (cl-weave:expect
       (ntop.app.domain:app-state-selected-widget state) :to-be :memory)
      (cl-weave:expect
       (ntop.app.application::application-composition-selection-active-p composition)
       :to-be t)
      (cl-weave:expect (second (first forwarded)) :to-be navigation-action)
      (let ((forward-count (length forwarded)))
        (funcall on-action state
                 (ntop.app.domain:make-app-action :kind :select :value :network))
        (cl-weave:expect
         (ntop.app.domain:app-state-selected-widget state) :to-be :network)
        (cl-weave:expect (length forwarded) :to-be forward-count)))
    (let* ((selection-composition (%make-composition-test-instance))
           (selection-state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (selection-on-action
             (ntop.app.application:application-composition-on-action
              selection-composition)))
      (funcall selection-on-action
               selection-state
               (ntop.app.domain:make-app-action :kind :select :value :network))
      (cl-weave:expect
       (ntop.app.application::application-composition-selection-active-p
        selection-composition)
       :to-be t)))
  (cl-weave:it "keeps graph zoom widths independent when selection changes"
    (let* ((rendered-series nil)
           (snapshot
             (%composition-test-snapshot 10d0 :cpu-usage 40d0 :memory-used 10))
           (composition
             (%make-composition-test-instance
              :on-render (lambda (series)
                           (setf rendered-series series))))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition)))
      (funcall on-snapshot snapshot)
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :zoom :value :in))
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store
         composition (quote (:cpu :usage :average))))
       :to-be 45d0)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store composition (quote (:memory :ram))))
       :to-be 60d0)
      (setf (ntop.app.domain:app-state-selected-widget state) :memory)
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :zoom :value :out))
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store
         composition (quote (:cpu :usage :average))))
       :to-be 45d0)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store composition (quote (:memory :ram))))
       :to-be 75d0)
      (%render-composition-test-instance composition state snapshot nil)
      (let* ((widths (cdr (assoc :graph-window-widths rendered-series)))
             (config (ntop.shared.application:make-application-config)))
        (cl-weave:expect (cdr (assoc :cpu widths)) :to-be 45d0)
        (cl-weave:expect (cdr (assoc :memory widths)) :to-be 75d0)
        (cl-weave:expect
         (ntop.shared.application:application-config-time-window-seconds
          (ntop.app.presentation::%screen-chart-config config rendered-series :cpu))
         :to-be 45d0)
        (cl-weave:expect
         (ntop.shared.application:application-config-time-window-seconds
          (ntop.app.presentation::%screen-chart-config config rendered-series :memory))
         :to-be 75d0))))
  (cl-weave:it "remembers selected graph zoom before stores exist"
    (let* ((rendered-series nil)
           (composition
             (%make-composition-test-instance
              :on-render (lambda (series)
                           (setf rendered-series series))))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition))
           (config (ntop.shared.application:make-application-config)))
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :zoom :value :in))
      (cl-weave:expect
       (length
        (ntop.app.application:application-composition-history-stores composition))
       :to-be 0)
      (cl-weave:expect
       (ntop.app.domain:app-state-zoom-seconds state)
       :to-be 45d0)
      (%render-composition-test-instance composition state nil nil)
      (let ((widths (cdr (assoc :graph-window-widths rendered-series))))
        (cl-weave:expect (cdr (assoc :cpu widths)) :to-be 45d0)
        (cl-weave:expect (cdr (assoc :memory widths)) :to-be 60d0)
        (cl-weave:expect
         (ntop.shared.application:application-config-time-window-seconds
          (ntop.app.presentation::%screen-chart-config config rendered-series :cpu))
         :to-be 45d0)
        (cl-weave:expect
         (ntop.shared.application:application-config-time-window-seconds
          (ntop.app.presentation::%screen-chart-config config rendered-series :memory))
         :to-be 60d0))
      (funcall on-snapshot
               (%composition-test-snapshot 10d0 :cpu-usage 40d0 :memory-used 10))
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store
         composition (quote (:cpu :usage :average))))
       :to-be 45d0)
      (cl-weave:expect
       (ntop.shared.domain:history-store-window-width-seconds
        (%composition-test-history-store composition (quote (:memory :ram))))
       :to-be 60d0))))
