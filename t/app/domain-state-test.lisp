(in-package #:ntop.test)

(cl-weave:describe "application domain state"
  (cl-weave:it "starts with the default process view and neutral modes"
    (let ((state (ntop.app.domain::make-app-state)))
      (cl-weave:expect
       (list (ntop.app.domain::app-state-selected-widget state)
             (ntop.app.domain::app-state-expanded-p state)
             (ntop.app.domain::app-state-basic-mode-p state)
             (ntop.app.domain::app-state-paused-p state)
             (ntop.app.domain::app-state-zoom-seconds state)
             (ntop.app.domain::app-state-scroll-offset state)
             (ntop.app.domain::app-state-active-dialog state)
             (hash-table-p
              (ntop.app.domain::app-state-widget-v-states state)))
       :to-equal '(:process nil nil nil 15 0 nil t))))

  (cl-weave:it "stores input context and action values"
    (let ((context
            (ntop.app.domain::make-input-context
             :selected-widget :cpu
             :active-dialog :help
             :basic-mode-p t
             :search-mode-p nil))
          (action
            (ntop.app.domain::make-app-action
             :kind :zoom
             :value 30)))
      (cl-weave:expect
       (list (ntop.app.domain::input-context-selected-widget context)
             (ntop.app.domain::input-context-active-dialog context)
             (ntop.app.domain::input-context-basic-mode-p context)
             (ntop.app.domain::input-context-search-mode-p context)
             (ntop.app.domain::app-action-kind action)
             (ntop.app.domain::app-action-value action))
       :to-equal '(:cpu :help t nil :zoom 30))))

  (cl-weave:it
    "lists the action kinds shared with the event loop"
    (cl-weave:expect
     ntop.app.domain::*app-action-kinds*
     :to-equal
     '(:navigation :select :toggle-widget-v :expand :basic :pause :zoom
       :history-scroll :return-to-now :help :search :dialog
       :toggle-process-grouping :sort-processes :reverse-process-sort
       :toggle-process-tree :toggle-process-tree-branch
       :set-process-display-mode :process-search :process-terminate :quit))))
