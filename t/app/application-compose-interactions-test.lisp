(in-package #:ntop.test)

(cl-weave:describe "application composition interactions"
  (cl-weave:it "routes widget and expanded CPU legend pointer selections through rendered areas"
  (let* ((forwarded nil)
         (memory-area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 2 :y 1 :width 1 :height 1)
            :action :screen-widget
            :value :memory))
         (legend-area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 10 :y 1 :width 6 :height 3)
            :action :screen-widget
            :value :cpu-legend))
         (cpu-area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 10 :y 1 :width 6 :height 3)
            :action :screen-widget
            :value :cpu))
         (row-area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 11 :y 2 :width 4 :height 1)
            :action :select-row
            :value 0))
         (areas (list row-area legend-area memory-area))
         (config (ntop.shared.application:make-application-config))
         (composition
           (ntop.app.application:make-application-composition
            config
            :on-action (lambda (state action)
                         (push (list state action) forwarded))
            :draw-screen
            (lambda (target state config snapshot columns rows reasons series)
              (declare (ignore target state config snapshot columns rows reasons series))
              areas)))
         (state (ntop.app.domain:make-app-state :selected-widget :process))
         (on-action
           (ntop.app.application:application-composition-on-action composition)))
    (%render-composition-test-instance composition state nil nil 4)
    (funcall on-action
             state
             (ntop.app.domain:make-app-action
              :kind :select :value (list :widget 2 1)))
    (cl-weave:expect
     (ntop.app.domain:app-state-selected-widget state)
     :to-be :memory)
    (progn
     (cl-weave:expect
      (gethash :screen-selection-id
               (ntop.app.domain:app-state-widget-v-states state))
      :to-be :memory)
     (setf areas (list memory-area)
           (ntop.app.domain:app-state-selected-widget state) :process)
     (remhash :screen-selection-id
              (ntop.app.domain:app-state-widget-v-states state))
     (%render-composition-test-instance composition state nil nil 4)
     (dolist (coordinates (list (list 3 1) (list 2 2)))
       (destructuring-bind (x y) coordinates
         (funcall on-action
                  state
                  (ntop.app.domain:make-app-action
                   :kind :select
                   :value (list :widget x y)))
         (cl-weave:expect
          (ntop.app.domain:app-state-selected-widget state)
          :to-be :process)
         (cl-weave:expect
          (gethash :screen-selection-id
                   (ntop.app.domain:app-state-widget-v-states state))
          :to-be nil)))
     (setf forwarded nil areas (list row-area legend-area memory-area)))
    (setf (ntop.app.domain:app-state-selected-widget state) :cpu
          (gethash :cpu (ntop.app.domain:app-state-widget-v-states state))
          (ntop.ui:make-table-state :selected-index 1))
    (%render-composition-test-instance composition state nil nil 4)
    (funcall on-action
             state
             (ntop.app.domain:make-app-action
              :kind :select :value (list :cpu-series 11 2)))
    (cl-weave:expect
     (ntop.app.domain:app-state-selected-widget state)
     :to-be :cpu)
    (cl-weave:expect
     (gethash :screen-selection-id
              (ntop.app.domain:app-state-widget-v-states state))
     :to-be :cpu-legend)
    (cl-weave:expect
     (ntop.ui:table-state-selected-index
      (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
     :to-be 0)
    (cl-weave:expect
     (ntop.app.application::application-composition-selection-active-p
      composition)
     :to-be t)
    (cl-weave:expect forwarded :to-be nil)
    (setf areas (list row-area cpu-area memory-area)
          (ntop.ui:table-state-selected-index
           (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
          1)
    (%render-composition-test-instance composition state nil nil 4)
    (funcall on-action
             state
             (ntop.app.domain:make-app-action
              :kind :select :value (list :cpu-series 11 2)))
    (cl-weave:expect
     (ntop.app.domain:app-state-selected-widget state)
     :to-be :cpu)
    (cl-weave:expect
     (gethash :screen-selection-id
              (ntop.app.domain:app-state-widget-v-states state))
     :to-be :cpu-legend)
    (cl-weave:expect
     (ntop.ui:table-state-selected-index
      (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
     :to-be 0)
    (cl-weave:expect
     (ntop.app.application::application-composition-selection-active-p
      composition)
     :to-be t)
    (cl-weave:expect forwarded :to-be nil)
    (setf row-area
          (ntop.ui:make-clickable-area
           :rectangle (ntop.ui:make-rectangle :x 11 :y 2 :width 4 :height 1)
           :action :select-row
           :value :header)
          areas (list row-area cpu-area memory-area)
          (ntop.ui:table-state-selected-index
           (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
          1
          (gethash :screen-selection-id
                   (ntop.app.domain:app-state-widget-v-states state))
          :cpu)
    (%render-composition-test-instance composition state nil nil 4)
    (let ((unhandled-action
            (ntop.app.domain:make-app-action
             :kind :select :value (list :cpu-series 11 2))))
      (funcall on-action state unhandled-action)
      (cl-weave:expect
       (ntop.ui:table-state-selected-index
        (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
       :to-be 1)
      (cl-weave:expect
       (gethash :screen-selection-id
                (ntop.app.domain:app-state-widget-v-states state))
       :to-be :cpu)
      (cl-weave:expect
       (and forwarded
            (equal
             (ntop.app.domain:app-action-value (second (first forwarded)))
             '(:cpu-series 11 2)))
       :to-be t))
    (let* ((actual-config (%wp12b-screen-default-config))
           (actual-composition
             (ntop.app.application:make-application-composition actual-config))
           (actual-state
             (ntop.app.domain:make-app-state :selected-widget :cpu))
           (actual-on-action
             (ntop.app.application:application-composition-on-action
              actual-composition))
           (x 149)
           (y 0))
      (with-output-to-string (stream)
        (let ((*standard-output* stream))
          (funcall
           (ntop.app.application:application-composition-render actual-composition)
           actual-state actual-config nil 160 48 nil)))
      (let* ((actual-areas
               (ntop.app.application::application-composition-clickable-areas
                actual-composition))
             (actual-memory-area
               (find-if
                (lambda (area)
                  (and (eq (ntop.ui:clickable-area-action area) :screen-widget)
                       (eq (ntop.ui:clickable-area-value area) :memory)))
                actual-areas))
             (rectangle
               (and actual-memory-area
                    (ntop.ui:clickable-area-rectangle actual-memory-area))))
        (cl-weave:expect
         (and rectangle
              (<= (ntop.ui:rectangle-x rectangle) x)
              (< x (+ (ntop.ui:rectangle-x rectangle)
                      (ntop.ui:rectangle-width rectangle)))
              (<= (ntop.ui:rectangle-y rectangle) y)
              (< y (+ (ntop.ui:rectangle-y rectangle)
                      (ntop.ui:rectangle-height rectangle))))
         :to-be t)
        (funcall actual-on-action
                 actual-state
                 (ntop.app.domain:make-app-action
                  :kind :select :value (list :widget x y)))
        (cl-weave:expect
         (ntop.app.domain:app-state-selected-widget actual-state)
         :to-be :memory)
        (cl-weave:expect
         (gethash :screen-selection-id
                  (ntop.app.domain:app-state-widget-v-states actual-state))
         :to-be :memory)))))
  (cl-weave:it "returns and routes expanded CPU average row areas"
  (let* ((config (ntop.shared.application:make-application-config))
         (composition
           (ntop.app.application:make-application-composition config))
         (state (ntop.app.domain:make-app-state :selected-widget :cpu))
         (snapshot (%composition-test-snapshot 10d0 :cpu-usage 50d0))
         (on-action
           (ntop.app.application:application-composition-on-action composition)))
    (setf (ntop.shared.application:application-config-expanded-p config) t)
    (with-output-to-string (stream)
      (let ((*standard-output* stream))
        (funcall
         (ntop.app.application:application-composition-render composition)
         state config snapshot 80 24 nil)))
    (let* ((areas
             (ntop.app.application::application-composition-clickable-areas
              composition))
           (cpu-area
             (find-if
              (lambda (area)
                (and (eq (ntop.ui:clickable-area-action area) :screen-widget)
                     (eq (ntop.ui:clickable-area-value area) :cpu)))
              areas))
           (average-area
             (find-if
              (lambda (area)
                (and (eq (ntop.ui:clickable-area-action area) :select-row)
                     (eql (ntop.ui:clickable-area-value area) 1)))
              areas)))
      (cl-weave:expect (not (null cpu-area)) :to-be t)
      (cl-weave:expect (not (null average-area)) :to-be t)
      (when (and cpu-area average-area)
        (let* ((cpu-rectangle (ntop.ui:clickable-area-rectangle cpu-area))
               (row-rectangle (ntop.ui:clickable-area-rectangle average-area))
               (x (ntop.ui:rectangle-x row-rectangle))
               (y (ntop.ui:rectangle-y row-rectangle)))
          (cl-weave:expect
           (and (<= (ntop.ui:rectangle-x cpu-rectangle) x)
                (< x (+ (ntop.ui:rectangle-x cpu-rectangle)
                        (ntop.ui:rectangle-width cpu-rectangle)))
                (<= (ntop.ui:rectangle-y cpu-rectangle) y)
                (< y (+ (ntop.ui:rectangle-y cpu-rectangle)
                        (ntop.ui:rectangle-height cpu-rectangle))))
           :to-be t)
          (funcall on-action
                   state
                   (ntop.app.domain:make-app-action
                    :kind :select :value (list :cpu-series x y)))
          (cl-weave:expect
           (ntop.app.domain:app-state-selected-widget state)
           :to-be :cpu)
          (cl-weave:expect
           (gethash :screen-selection-id
                    (ntop.app.domain:app-state-widget-v-states state))
           :to-be :cpu-legend)
          (cl-weave:expect
           (ntop.ui:table-state-selected-index
            (gethash :cpu (ntop.app.domain:app-state-widget-v-states state)))
           :to-be 1)
          (cl-weave:expect
           (ntop.app.application::application-composition-selection-active-p
            composition)
           :to-be t))))))
  (cl-weave:it
   "routes process-row pointer misses to the widget under the pointer"
   (let* ((memory-area
            (ntop.ui:make-clickable-area
             :rectangle (ntop.ui:make-rectangle :x 2 :y 1 :width 1 :height 1)
             :action :screen-widget
             :value :memory))
          (config (ntop.shared.application:make-application-config))
          (composition
            (ntop.app.application:make-application-composition
             config
             :draw-screen
             (lambda (target state config snapshot columns rows reasons series)
               (declare
                (ignore target state config snapshot columns rows reasons series))
               (list memory-area))))
          (state (ntop.app.domain:make-app-state :selected-widget :process))
          (on-action
            (ntop.app.application:application-composition-on-action composition)))
     (with-output-to-string (stream)
       (let ((*standard-output* stream))
         (funcall
          (ntop.app.application:application-composition-render composition)
          state config nil 160 48 nil)))
     (funcall
      on-action
      state
      (ntop.app.domain:make-app-action
       :kind :select :value (list :pointer :process-row 2 1)))
     (cl-weave:expect
      (ntop.app.domain:app-state-selected-widget state) :to-be :memory)
     (cl-weave:expect
      (gethash :screen-selection-id
               (ntop.app.domain:app-state-widget-v-states state))
      :to-be :memory)
     (cl-weave:expect
      (ntop.app.application::application-composition-selection-active-p
       composition)
      :to-be t)))
  (cl-weave:it
   "keeps selection highlighting active through graph navigation"
   (let* ((composition (%make-composition-test-instance))
          (state (ntop.app.domain:make-app-state :selected-widget :cpu))
          (on-action
            (ntop.app.application:application-composition-on-action
             composition)))
     (cl-weave:expect
      (ntop.app.application::application-composition-selection-active-p
       composition)
      :to-be nil)
     (funcall
      on-action
      state
      (ntop.app.domain:make-app-action :kind :zoom :value :in))
     (cl-weave:expect
      (ntop.app.application::application-composition-selection-active-p
       composition)
      :to-be t)
     (funcall
      on-action
      state
      (ntop.app.domain:make-app-action :kind :dialog :value :clear-expanded))
     (cl-weave:expect
      (ntop.app.application::application-composition-selection-active-p
       composition)
      :to-be nil)))

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
             (ntop.app.presentation::%help-visible-lines help-state :width 77))
           (expected-offset (max 0 (- (length visible-lines) 10))))
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
