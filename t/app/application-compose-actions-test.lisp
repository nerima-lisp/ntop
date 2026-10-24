(in-package #:ntop.test)

(defparameter *application-transition-test-cases*
  '((:navigation :left :process nil nil nil nil :same nil nil)
    (:select :cpu :cpu nil nil nil nil nil nil nil)
    (:select (:widget :cpu) :cpu nil nil nil nil nil nil nil)
    (:toggle-widget-v :memory :process nil nil nil nil nil nil :gpu)
    (:expand nil :process t nil nil nil nil nil nil)
    (:expand :toggle :process t nil nil nil nil nil nil)
    (:basic nil :process nil t nil nil nil nil nil)
    (:basic t :process nil t nil nil nil nil nil)
    (:basic :toggle :process nil t nil nil nil nil nil)
    (:basic :unchanged :process nil nil nil nil :same nil nil)
    (:pause nil :process nil nil t nil nil (:pause t) nil)
    (:zoom :in :process nil nil nil nil nil (:history :zoom :in) nil)
    (:history-scroll :backward :process nil nil nil nil nil
     (:history :history-scroll :backward) nil)
    (:return-to-now nil :process nil nil nil nil nil
     (:history :return-to-now nil) nil)
    (:help nil :process nil nil nil :help nil nil nil)
    (:search :help :process nil nil nil :help-search nil nil nil)
    (:dialog :process-kill :process nil nil nil :process-kill nil nil nil)
    (:dialog (:close :help) :process nil nil nil nil nil nil nil)
    (:toggle-process-grouping :probe :process nil nil nil nil :same nil nil)
    (:sort-processes :probe :process nil nil nil nil :same nil nil)
    (:reverse-process-sort :probe :process nil nil nil nil :same nil nil)
    (:toggle-process-tree :probe :process nil nil nil nil :same nil nil)
    (:toggle-process-tree-branch :probe :process nil nil nil nil :same nil nil)
    (:set-process-display-mode :probe :process nil nil nil nil :same nil nil)
    (:process-search :probe :process nil nil nil nil :same nil nil)
    (:process-terminate :probe :process nil nil nil nil :same nil nil)
    (:quit nil :process nil nil nil nil :same nil nil)))

(defun %application-transition-test-state (&key basic-mode-p)
  (let ((views (make-hash-table :test (function eq))))
    (setf (gethash :temperature views) :temperature-graph)
    (ntop.app.domain:make-app-state
     :selected-widget :process
     :basic-mode-p basic-mode-p
     :widget-v-states views)))

(cl-weave:describe "application composition actions"
  (cl-weave:it "declares one transition rule for every application action kind"
    (let ((rule-kinds
            (mapcar (function first)
                    ntop.app.application::*application-action-rules*)))
      (cl-weave:expect rule-kinds :to-equal ntop.app.domain:*app-action-kinds*)
      (cl-weave:expect (length rule-kinds) :to-equal
                       (length (remove-duplicates rule-kinds :test (function eq))))))

  (cl-weave:it "applies every action through the transition table without mutating its input"
    (dolist (test-case *application-transition-test-cases*)
      (destructuring-bind
          (kind value selected expanded basic paused dialog forwarded history memory-view)
          test-case
        (let* ((state (%application-transition-test-state))
               (source-views (ntop.app.domain:app-state-widget-v-states state))
               (action (ntop.app.domain:make-app-action :kind kind :value value)))
          (multiple-value-bind (next-state next-action history-intent)
              (ntop.app.application::transition-application-state state action)
            (cl-weave:expect (eq next-state state) :to-be nil)
            (cl-weave:expect
             (eq (ntop.app.domain:app-state-widget-v-states next-state)
                 source-views)
             :to-be nil)
            (cl-weave:expect
             (list (ntop.app.domain:app-state-selected-widget next-state)
                   (ntop.app.domain:app-state-expanded-p next-state)
                   (ntop.app.domain:app-state-basic-mode-p next-state)
                   (ntop.app.domain:app-state-paused-p next-state)
                   (ntop.app.domain:app-state-active-dialog next-state))
             :to-equal (list selected expanded basic paused dialog))
            (cl-weave:expect (gethash :memory
                                      (ntop.app.domain:app-state-widget-v-states
                                       next-state))
                             :to-be memory-view)
            (cl-weave:expect (gethash :temperature source-views)
                             :to-be :temperature-graph)
            (cl-weave:expect (hash-table-count source-views) :to-be 1)
            (cl-weave:expect
             (list (ntop.app.domain:app-state-selected-widget state)
                   (ntop.app.domain:app-state-expanded-p state)
                   (ntop.app.domain:app-state-basic-mode-p state)
                   (ntop.app.domain:app-state-paused-p state)
                   (ntop.app.domain:app-state-active-dialog state))
             :to-equal '(:process nil nil nil nil))
            (if (eq forwarded :same)
                (cl-weave:expect next-action :to-be action)
                (cl-weave:expect next-action :to-be forwarded))
            (cl-weave:expect history-intent :to-equal history))))))

  (cl-weave:it "cycles the U8 widget views and returns to each default view"
    (cl-weave:expect
     (list (ntop.app.application::next-widget-view :memory nil)
           (ntop.app.application::next-widget-view :memory :gpu)
           (ntop.app.application::next-widget-view :temperature nil)
           (ntop.app.application::next-widget-view :temperature
                                                    :temperature-graph)
           (ntop.app.application::next-widget-view :temperature :battery)
           (ntop.app.application::next-widget-view :disks nil)
           (ntop.app.application::next-widget-view :disks :disk-io-graph)
           (ntop.app.application::next-widget-view :cpu nil))
     :to-equal '(:gpu nil :temperature-graph :battery nil :disk-io-graph nil nil)))

  (cl-weave:it "stores every U8 widget view through its full transition cycle"
    (dolist (test-case
              '((:memory :gpu nil)
                (:temperature :temperature-graph :battery nil)
                (:disks :disk-io-graph nil)))
      (destructuring-bind (widget &rest expected-views) test-case
        (let ((state (ntop.app.domain:make-app-state :selected-widget widget)))
          (dolist (expected-view expected-views)
            (let* ((source-views
                    (ntop.app.domain:app-state-widget-v-states state))
                   (source-view (gethash widget source-views)))
              (multiple-value-bind (next-state forwarded history)
                  (ntop.app.application::transition-application-state
                   state
                   (ntop.app.domain:make-app-action :kind :toggle-widget-v))
                (cl-weave:expect (eq next-state state) :to-be nil)
                (cl-weave:expect forwarded :to-be nil)
                (cl-weave:expect history :to-be nil)
                (cl-weave:expect
                 (ntop.app.domain:app-state-selected-widget next-state)
                 :to-be widget)
                (cl-weave:expect
                 (gethash widget
                          (ntop.app.domain:app-state-widget-v-states next-state))
                 :to-be expected-view)
                (cl-weave:expect
                 (gethash widget source-views) :to-be source-view)
                (setf state next-state))))
          (cl-weave:expect
           (hash-table-count (ntop.app.domain:app-state-widget-v-states state))
           :to-be 0)))))

  (cl-weave:it "ignores expand and view cycling in basic mode"
    (dolist (kind '(:expand :toggle-widget-v))
      (let* ((state (%application-transition-test-state :basic-mode-p t))
             (views (ntop.app.domain:app-state-widget-v-states state)))
        (multiple-value-bind (next-state forwarded history)
            (ntop.app.application::transition-application-state
             state (ntop.app.domain:make-app-action
                    :kind kind :value :temperature))
          (cl-weave:expect forwarded :to-be nil)
          (cl-weave:expect history :to-be nil)
          (cl-weave:expect
           (list (ntop.app.domain:app-state-expanded-p next-state)
                 (ntop.app.domain:app-state-basic-mode-p next-state)
                 (gethash :temperature
                          (ntop.app.domain:app-state-widget-v-states next-state)))
           :to-equal (list nil t :temperature-graph))
          (cl-weave:expect (gethash :temperature views)
                           :to-be :temperature-graph)
          (cl-weave:expect (hash-table-count views) :to-be 1)))))

  (cl-weave:it "forwards feature selection and search actions unchanged"
    (dolist (action
              (list (ntop.app.domain:make-app-action
                     :kind :select :value '(:pointer :process-row 4 8))
                    (ntop.app.domain:make-app-action
                     :kind :search :value "typed text")
                    (ntop.app.domain:make-app-action
                     :kind :dialog :value :choice-down)))
      (multiple-value-bind (next-state forwarded history)
          (ntop.app.application::transition-application-state
           (%application-transition-test-state) action)
        (cl-weave:expect (eq next-state nil) :to-be nil)
        (cl-weave:expect forwarded :to-be action)
        (cl-weave:expect history :to-be nil))))

  (cl-weave:it "closes only the active dialog and routes pause as history intent"
    (let ((state (ntop.app.domain:make-app-state :active-dialog :help)))
      (multiple-value-bind (closed forwarded history)
          (ntop.app.application::transition-application-state
           state (ntop.app.domain:make-app-action
                  :kind :dialog :value '(:close :help)))
        (cl-weave:expect
         (ntop.app.domain:app-state-active-dialog closed) :to-be nil)
        (cl-weave:expect forwarded :to-be nil)
        (cl-weave:expect history :to-be nil)
        (cl-weave:expect (ntop.app.domain:app-state-active-dialog state)
                         :to-be :help))
      (multiple-value-bind (paused forwarded history)
          (ntop.app.application::transition-application-state
           state (ntop.app.domain:make-app-action :kind :pause))
        (cl-weave:expect (ntop.app.domain:app-state-paused-p paused) :to-be t)
        (cl-weave:expect forwarded :to-be nil)
        (cl-weave:expect history :to-equal '(:pause t))
        (cl-weave:expect (ntop.app.domain:app-state-paused-p state)
                         :to-be nil))))
  (cl-weave:describe "application composition state branches"
                   (cl-weave:it
                    "navigates by visible layout and rejects inapplicable actions"
                    (let ((config
                           (ntop.shared.application:make-application-config)))
                      (dolist
                          (case '((:disks :left :network) (:process :up :disks)))
                        (destructuring-bind (selected direction expected) case
                          (let ((state
                                 (ntop.app.domain:make-app-state
                                  :selected-widget
                                  selected)))
                            (ntop.app.application::%select-layout-neighbor state
                                                                           config
                                                                           80
                                                                           24
                                                                           direction)
                            (cl-weave:expect
                             (ntop.app.domain:app-state-selected-widget state)
                             :to-be
                             expected))))
                      (cl-weave:expect
                       (ntop.app.application::%navigation-score-before-p '(0 20)
                                                                         '(0 30))
                       :to-be
                       t)
                      (dolist
                          (case '((:help nil nil :navigation :right)
                                  (nil t nil :navigation :right)
                                  (nil nil t :navigation :right)
                                  (nil nil nil :select :right)
                                  (nil nil nil :navigation :page-down)))
                        (destructuring-bind (dialog basic
                                                    config-basic
                                                    kind
                                                    direction) case
                          (let ((state
                                 (ntop.app.domain:make-app-state
                                  :selected-widget
                                  :cpu
                                  :active-dialog
                                  dialog
                                  :basic-mode-p
                                  basic))
                                (config
                                 (ntop.shared.application:make-application-config)))
                            (setf (ntop.shared.application:application-config-basic-p
                                   config) config-basic)
                            (ntop.app.application::%apply-layout-navigation
                             state
                             (ntop.app.domain:make-app-action :kind
                                                              kind
                                                              :value
                                                              direction)
                             config
                             80
                             24)
                            (cl-weave:expect
                             (ntop.app.domain:app-state-selected-widget state)
                             :to-be
                             :cpu))))
                      (let ((state
                             (ntop.app.domain:make-app-state :selected-widget
                                                             :cpu)))
                        (ntop.app.application::%apply-layout-navigation
                         state
                         (ntop.app.domain:make-app-action
                          :kind :navigation
                          :value :right)
                         config
                         80
                         24)
                        (cl-weave:expect
                         (ntop.app.domain:app-state-selected-widget state)
                         :to-be
                         :memory))))
                   (cl-weave:it
                    "pauses new history stores and trims the retention boundary"
                    (let* ((composition
                            (ntop.app.application:make-application-composition
                             (ntop.shared.application:make-application-config)))
                           (key '(:cpu :usage :average))
                           (record
                            (ntop.shared.domain:make-sample-record :sampled-at
                                                                   1d0)))
                      (setf (ntop.app.application::application-composition-paused-p
                             composition) t)
                      (ntop.app.application::%append-composition-history
                       composition
                       key
                       "AVG"
                       record
                       1d0)
                      (cl-weave:expect
                       (ntop.shared.domain:history-paused-p
                        (gethash key
                                 (ntop.app.application::application-composition-history-index
                                  composition)))
                       :to-be
                       t))
                    (let* ((composition
                            (ntop.app.application:make-application-composition
                             (ntop.shared.application:make-application-config
                              :retention-seconds
                              5d0)))
                           (samples
                            (ntop.app.application::application-composition-network-samples
                             composition)))
                      (loop for sampled-at in '(4d0 5d0 6d0)
                            do (vector-push-extend
                                (ntop.shared.domain:make-sample-record
                                 :sampled-at
                                 sampled-at)
                                samples))
                      (ntop.app.application::%trim-network-samples composition
                                                                   10d0)
                      (cl-weave:expect
                       (map 'list
                            #'ntop.shared.domain:sample-record-sampled-at
                            (ntop.app.application::application-composition-network-samples
                             composition))
                       :to-equal
                       '(5d0 6d0))))
                   (cl-weave:it
                    "replaces equal-time network samples and rejects older samples"
                    (let* ((composition
                            (ntop.app.application:make-application-composition
                             (ntop.shared.application:make-application-config)))
                           (first-sample
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     10d0
                                                                     :interfaces
                                                                     nil))
                           (replacement
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     10d0
                                                                     :interfaces
                                                                     nil))
                           (samples
                            (ntop.app.application::application-composition-network-samples
                             composition)))
                      (ntop.app.application::%record-network-history
                       composition
                       (ntop.shared.domain:make-sample-record :sampled-at 10d0)
                       first-sample)
                      (ntop.app.application::%record-network-history
                       composition
                       (ntop.shared.domain:make-sample-record :sampled-at 10d0)
                       replacement)
                      (cl-weave:expect (length samples) :to-be 1)
                      (cl-weave:expect (aref samples 0) :to-be replacement))
                    (let* ((composition
                            (ntop.app.application:make-application-composition
                             (ntop.shared.application:make-application-config)))
                           (latest
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     10d0
                                                                     :interfaces
                                                                     nil))
                           (older
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     9d0
                                                                     :interfaces
                                                                     nil))
                           (error-text nil))
                      (vector-push-extend
                       latest
                       (ntop.app.application::application-composition-network-samples
                        composition))
                      (handler-case (ntop.app.application::%record-network-history
                                     composition
                                     (ntop.shared.domain:make-sample-record
                                      :sampled-at
                                      9d0)
                                     older)
                        (error (condition)
                          (setf error-text (princ-to-string condition))))
                      (cl-weave:expect
                       (not
                        (null
                         (and error-text
                              (search
                               "Network samples must arrive in chronological order."
                               error-text))))
                       :to-be
                       t)
                      (cl-weave:expect
                       (aref
                        (ntop.app.application::application-composition-network-samples
                         composition)
                        0)
                       :to-be
                       latest)))
                   (cl-weave:it
                    "omits expired GPU memory history from chart series"
                    (let* ((config
                            (ntop.shared.application:make-application-config
                             :retention-seconds
                             5d0))
                           (composition
                            (ntop.app.application:make-application-composition
                             config))
                           (snapshot
                            (ntop.app.domain:make-collection-snapshot
                             :sampled-at
                             1d0
                             :gpu-sample
                             (ntop.gpu.domain:make-gpu-sample :sampled-at
                                                              1d0
                                                              :devices
                                                              (list
                                                               (ntop.gpu.domain:make-gpu-device
                                                                :name
                                                                "GPU"
                                                                :usage-percent
                                                                25d0
                                                                :memory-used-bytes
                                                                25
                                                                :memory-total-bytes
                                                                100)))))
                           (key '(:gpu-memory :used ("GPU" 0))))
                      (funcall
                       (ntop.app.application:application-composition-on-snapshot
                        composition)
                       snapshot)
                      (let ((store
                             (gethash key
                                      (ntop.app.application::application-composition-history-index
                                       composition))))
                        (cl-weave:expect (not (null store)) :to-be t)
                        (ntop.shared.domain:history-prune store 10d0)
                        (cl-weave:expect
                         (length (ntop.shared.domain:history-slice store 0))
                         :to-be
                         0))
                      (cl-weave:expect
                       (ntop.app.application::%gpu-memory-series-data
                        composition)
                       :to-be
                       nil)))))
