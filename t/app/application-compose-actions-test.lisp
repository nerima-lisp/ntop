(in-package #:ntop.test)

(defparameter *application-transition-test-cases*
  '((:navigation :left :process nil nil nil nil :same nil nil)
    (:select :cpu :cpu nil nil nil nil nil nil nil)
    (:toggle-widget-v :memory :process nil nil nil nil nil nil :gpu)
    (:expand nil :process t nil nil nil nil nil nil)
    (:basic nil :process nil t nil nil nil nil nil)
    (:pause nil :process nil nil t nil nil (:pause t) nil)
    (:zoom :in :process nil nil nil nil nil (:history :zoom :in) nil)
    (:history-scroll :backward :process nil nil nil nil nil
     (:history :history-scroll :backward) nil)
    (:return-to-now nil :process nil nil nil nil nil
     (:history :return-to-now nil) nil)
    (:help nil :process nil nil nil :help nil nil nil)
    (:search :help :process nil nil nil :help-search nil nil nil)
    (:dialog :process-kill :process nil nil nil :process-kill nil nil nil)
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
                         :to-be nil)))))
