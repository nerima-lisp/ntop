(in-package #:ntop.test)

(defvar *wp12b-composition-help-calls* nil)
(defvar *wp12b-composition-help-capture-p* nil)

(defmethod ntop.ui:draw-widget :around
    ((widget-key (eql :help)) target rectangle state config series)
  (when *wp12b-composition-help-capture-p*
    (push (list target rectangle state config series)
          *wp12b-composition-help-calls*))
  (call-next-method))

(defun %composition-test-gpu-device (name used total)
  (ntop.gpu.domain:make-gpu-device
   :name name :usage-percent 35d0
   :memory-used-bytes used :memory-total-bytes total))

(defun %composition-test-process-sample (sampled-at cpu-time read-bytes)
  (ntop.process.domain:make-process-sample
   :sampled-at sampled-at
   :processes
   (list (ntop.process.domain:make-process-record
          :pid 77 :name "worker" :cpu-time-ns cpu-time
          :read-bytes read-bytes :write-bytes read-bytes))))

(defun %composition-test-snapshot
    (sampled-at &key cpu-usage memory-used gpu-devices network-p process-sample)
  (ntop.app.domain:make-collection-snapshot
   :sampled-at sampled-at
   :cpu-sample
   (when cpu-usage
     (ntop.cpu.domain:make-cpu-sample
      :sampled-at sampled-at :average-usage-percent cpu-usage))
   :memory-sample
   (when memory-used
     (ntop.memory.domain:make-memory-sample
      :sampled-at sampled-at :ram-used-bytes memory-used :ram-total-bytes 100))
   :gpu-sample
   (when gpu-devices
     (ntop.gpu.domain:make-gpu-sample
      :sampled-at sampled-at :devices gpu-devices))
   :network-sample
   (when network-p
     (ntop.network.domain:make-network-sample
      :sampled-at sampled-at :interfaces nil))
   :process-sample process-sample))

(defun %make-composition-test-instance (&key on-action on-render)
  (ntop.app.application:make-application-composition
   (ntop.shared.application:make-application-config)
   :on-action on-action
   :draw-screen
   (lambda (target state config snapshot columns rows reasons series-by-widget)
     (declare (ignore target state config snapshot columns rows reasons))
     (when on-render (funcall on-render series-by-widget)))))

(defun %render-composition-test-instance (composition state snapshot reasons
                                            &optional (rows 24))
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (funcall (ntop.app.application:application-composition-render composition)
               state (ntop.shared.application:make-application-config)
               snapshot 80 rows reasons))))

(defun %composition-test-history-store (composition key)
  (gethash key
           (ntop.app.application::application-composition-history-index
            composition)))

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
      (funcall on-action state feature-action)
      (cl-weave:expect (first (first forwarded)) :to-be state)
      (cl-weave:expect (second (first forwarded)) :to-be feature-action)
      (funcall on-action state reverse-sort-action)
      (cl-weave:expect (second (first forwarded)) :to-be reverse-sort-action)
      (funcall on-action state navigation-action)
      (cl-weave:expect
       (ntop.app.domain:app-state-selected-widget state) :to-be :memory)
      (cl-weave:expect (second (first forwarded)) :to-be navigation-action)
      (let ((forward-count (length forwarded)))
        (funcall on-action state
                 (ntop.app.domain:make-app-action :kind :select :value :network))
        (cl-weave:expect
         (ntop.app.domain:app-state-selected-widget state) :to-be :network)
        (cl-weave:expect (length forwarded) :to-be forward-count))))

  (cl-weave:it "clears owned history in place and forwards reset collection"
    (let* ((forwarded nil)
           (composition
             (%make-composition-test-instance
              :on-action (lambda (state action)
                           (push (list state action) forwarded))))
           (stores
             (ntop.app.application:application-composition-history-stores
              composition))
           (snapshot
             (%composition-test-snapshot
              10d0 :cpu-usage 40d0 :network-p t
              :process-sample
              (%composition-test-process-sample 10d0 100 1000)))
           (state (ntop.app.domain:make-app-state :scroll-offset 5 :zoom-seconds 15))
           (action
             (ntop.app.domain:make-app-action
              :kind :navigation :value :reset-collection)))
      (funcall (ntop.app.application:application-composition-on-snapshot
                composition)
               snapshot)
      (setf (ntop.app.domain:app-state-selected-widget state) :process)
      (funcall (ntop.app.application:application-composition-on-action
                composition)
               state action)
      (cl-weave:expect
       (eq stores
           (ntop.app.application:application-composition-history-stores
            composition))
       :to-be t)
      (cl-weave:expect (fill-pointer stores) :to-be 0)
      (cl-weave:expect
       (hash-table-count
        (ntop.app.application::application-composition-history-index composition))
       :to-be 0)
      (cl-weave:expect
       (length
        (ntop.app.application::application-composition-network-samples
         composition))
       :to-be 0)
      (cl-weave:expect
       (ntop.app.application::application-composition-previous-process-sample
        composition)
       :to-be nil)
      (cl-weave:expect
       (ntop.app.domain:app-state-scroll-offset state) :to-be 0)
      (cl-weave:expect
       (ntop.app.domain:app-state-zoom-seconds state) :to-be 60)
      (cl-weave:expect (first (first forwarded)) :to-be state)
      (cl-weave:expect (second (first forwarded)) :to-be action)))

  (cl-weave:it "draws help with the selected widget context"
    (let* ((*wp12b-composition-help-calls* nil)
           (*wp12b-composition-help-capture-p* t)
           (composition (%make-composition-test-instance))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu)))
      (funcall (ntop.app.application:application-composition-on-action
                composition)
               state (ntop.app.domain:make-app-action :kind :help))
      (%render-composition-test-instance composition state nil nil)
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state) :to-be :help)
      (cl-weave:expect (length *wp12b-composition-help-calls*) :to-be 1)
      (cl-weave:expect
       (ntop.app.presentation:help-state-selected-widget
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-be :cpu)))
  (cl-weave:it "uses the rendered search viewport for help paging"
  (let* ((composition (%make-composition-test-instance))
         (state (ntop.app.domain:make-app-state :selected-widget :cpu))
         (on-action
           (ntop.app.application:application-composition-on-action composition)))
    (funcall on-action
             state
             (ntop.app.domain:make-app-action :kind :search :value :help))
    (%render-composition-test-instance composition state nil nil 12)
    (loop repeat 100
          do (funcall
              on-action
              state
              (ntop.app.domain:make-app-action
               :kind :navigation :value :page-down)))
    (let* ((help-state
             (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
           (visible-lines
             (ntop.app.presentation::%help-visible-lines help-state))
           (expected-offset (max 0 (- (length visible-lines) 9))))
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p help-state)
       :to-be t)
      (cl-weave:expect (> (length visible-lines) 9) :to-be t)
      (cl-weave:expect
       (ntop.app.presentation:help-state-scroll-offset help-state)
       :to-be expected-offset)))

  (cl-weave:it "keeps help search routed and clears it when closed or reopened"
    (let* ((*wp12b-composition-help-calls* nil)
           (*wp12b-composition-help-capture-p* t)
           (forwarded nil)
           (composition
             (%make-composition-test-instance
              :on-action
              (lambda (state action)
                (push (list state action) forwarded))))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (frame nil))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value :help))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value "needle"))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :navigation :value :page-down))
      (setf frame (%render-composition-test-instance composition state nil nil 12))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state) :to-be :help-search)
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-be t)
      (cl-weave:expect
       (ntop.app.presentation:help-state-query
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-equal "needle")
      (cl-weave:expect (not (null (search "Search:" frame))) :to-be t)
      (cl-weave:expect (not (null (search "needle" frame))) :to-be t)
      (cl-weave:expect (length *wp12b-composition-help-calls*) :to-be 1)
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p
        (third (first *wp12b-composition-help-calls*)))
       :to-be t)
      (funcall on-action state (ntop.app.domain:make-app-action :kind :help))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state) :to-be :help)
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-be nil)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value :help))
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-be t)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value (list :close :help-search)))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state) :to-be nil)
      (cl-weave:expect
       (ntop.app.presentation:help-state-search-active-p
        (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
       :to-be nil)
      (cl-weave:expect forwarded :to-be nil)))))

