(in-package #:ntop.test)
(cl-weave:describe "process dialogs in application composition"
  (cl-weave:it "routes search edits and submit through the active dialog"
    (let* ((forwarded nil)
           (config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition
              config
              :on-action (lambda (state action)
                           (push (list state action) forwarded))))
           (state (ntop.app.domain:make-app-state :selected-widget :process))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value :process))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value "worker"))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value :toggle-case))
      (let* ((field (gethash :process-search
                             (ntop.app.domain:app-state-widget-v-states state)))
             (search-state
               (ntop.process.presentation:process-search-field-state-application-state
                field)))
        (cl-weave:expect
         (ntop.process.application:process-search-state-query search-state)
         :to-equal
         "worker")
        (cl-weave:expect
         (ntop.process.presentation:process-search-field-state-case-sensitive-p
          field)
         :to-be
         t))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action :kind :search :value :submit))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state)
       :to-be
       nil)
      (cl-weave:expect forwarded :to-be nil)))

  (cl-weave:it "moves and applies sort choices without forwarding them"
    (let* ((forwarded nil)
           (config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition
              config
              :on-action (lambda (state action)
                           (push (list state action) forwarded))))
           (state (ntop.app.domain:make-app-state :selected-widget :process))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value :process-sort))
      (let* ((sort-state
               (ntop.app.application::%process-sort-state state config))
             (sort-config
               (ntop.app.application::%process-dialog-table-config
                config nil sort-state))
             (choices
               (ntop.process.presentation:process-table-sort-options sort-config))
             (menu-state
               (gethash :process-sort
                        (ntop.app.domain:app-state-widget-v-states state)))
             (next-index
               (min (1- (length choices))
                    (1+ (ntop.ui:table-state-selected-index menu-state)))))
        (funcall on-action
                 state
                 (ntop.app.domain:make-app-action
                  :kind :navigation :value :down))
        (cl-weave:expect
         (ntop.ui:table-state-selected-index
          (gethash :process-sort
                   (ntop.app.domain:app-state-widget-v-states state)))
         :to-be
         next-index)
        (funcall on-action
                 state
                 (ntop.app.domain:make-app-action
                  :kind :sort-processes :value :apply))
        (cl-weave:expect
         (ntop.process.domain:process-sort-state-key
          (gethash :process-sort-state
                   (ntop.app.domain:app-state-widget-v-states state)))
         :to-be
         (car (nth next-index choices)))
        (cl-weave:expect
         (ntop.app.domain:app-state-active-dialog state)
         :to-be
         nil)
        (cl-weave:expect forwarded :to-be nil))))

  (cl-weave:it "retains rendered click areas and routes row, sort, and kill clicks"
    (let* ((row-area
             (ntop.ui:make-clickable-area
              :rectangle (ntop.ui:make-rectangle :x 2 :y 3 :width 1 :height 1)
              :action :select-row
              :value 0))
           (sort-area
             (ntop.ui:make-clickable-area
              :rectangle (ntop.ui:make-rectangle :x 4 :y 5 :width 1 :height 1)
              :action :sort-choice
              :value :memory))
           (kill-area
             (ntop.ui:make-clickable-area
              :rectangle (ntop.ui:make-rectangle :x 6 :y 7 :width 1 :height 1)
              :action :kill-choice
              :value 0))
           (forwarded nil)
           (config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition
              config
              :on-action (lambda (state action)
                           (push (list state action) forwarded))
              :draw-screen
              (lambda (target state draw-config snapshot columns rows reasons series)
                (declare
                 (ignore target draw-config snapshot columns rows reasons series))
                (case (ntop.app.domain:app-state-active-dialog state)
                  (:process-sort (list sort-area))
                  (:process-kill (list kill-area))
                  (otherwise (list row-area))))))
           (state (ntop.app.domain:make-app-state :selected-widget :process))
           (sample
             (ntop.process.domain:make-process-sample
              :sampled-at 1d0
              :processes
              (list
               (ntop.process.domain:make-process-record
                :pid 42 :ppid 1 :name "worker" :command "worker"
                :cpu-time-ns 100000000 :resident-memory-bytes 1024
                :virtual-memory-bytes 2048 :read-bytes 100 :write-bytes 20))))
           (snapshot
             (ntop.app.domain:make-collection-snapshot
              :sampled-at 1d0 :process-sample sample))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (setf (ntop.shared.application:application-config-disable-advanced-kill-p
             config)
            t)
      (funcall
       (ntop.app.application:application-composition-on-snapshot composition)
       snapshot)
      (let ((returned-areas nil))
        (with-output-to-string (stream)
          (let ((*standard-output* stream))
            (setf returned-areas
                  (funcall
                   (ntop.app.application:application-composition-render composition)
                   state config snapshot 80 24 nil))))
        (cl-weave:expect (first returned-areas) :to-be row-area))
      (cl-weave:expect
         (first
          (ntop.app.application::application-composition-clickable-areas
           composition))
         :to-be
         row-area)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :select :value (list :pointer :process-row 2 3)))
      (cl-weave:expect
       (ntop.ui:table-state-selected-index
        (gethash :process (ntop.app.domain:app-state-widget-v-states state)))
       :to-be
       0)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value :process-sort))
      (with-output-to-string (stream)
        (let ((*standard-output* stream))
          (funcall
           (ntop.app.application:application-composition-render composition)
           state config snapshot 80 24 nil)))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :sort-processes
                :value (list :pointer :sort-choice 4 5)))
      (cl-weave:expect
       (ntop.process.domain:process-sort-state-key
        (gethash :process-sort-state
                 (ntop.app.domain:app-state-widget-v-states state)))
       :to-be
       :memory)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value :process-kill))
      (with-output-to-string (stream)
        (let ((*standard-output* stream))
          (funcall
           (ntop.app.application:application-composition-render composition)
           state config snapshot 80 24 nil)))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :process-terminate
                :value (list :pointer :kill-choice 6 7)))
      (cl-weave:expect
       (ntop.ui:table-state-selected-index
        (ntop.process.presentation::process-termination-dialog-state-selection-state
         (gethash :process-kill
                  (ntop.app.domain:app-state-widget-v-states state))))
       :to-be
       0)
      (cl-weave:expect forwarded :to-be nil)))

  (cl-weave:it "sends kill confirmation through the configured sender"
    (let* ((forwarded nil)
           (sent nil)
           (config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition
              config
              :on-action (lambda (state action)
                           (push (list state action) forwarded))
              :process-signal-sender
              (lambda (target-pids signal-number)
                (push (list target-pids signal-number) sent)
                t)))
           (state (ntop.app.domain:make-app-state :selected-widget :process))
           (snapshot
             (ntop.app.domain:make-collection-snapshot
              :sampled-at 1d0
              :process-sample
              (ntop.process.domain:make-process-sample
               :sampled-at 1d0
               :signals
               (list
                (ntop.process.domain:make-process-signal
                 :name "SIGTERM" :number 15 :operating-system "fixture"))
               :processes
               (list (ntop.process.domain:make-process-record
                      :pid 42 :ppid 1 :name "worker" :command "worker"
                      :cpu-time-ns 100000000
                      :resident-memory-bytes 1024
                      :virtual-memory-bytes 2048
                      :read-bytes 100 :write-bytes 20)))))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (confirm
             (ntop.app.domain:make-app-action
              :kind :process-terminate :value :confirm)))
      (setf (ntop.shared.application:application-config-disable-advanced-kill-p
             config)
            t)
      (funcall
       (ntop.app.application:application-composition-on-snapshot composition)
       snapshot)
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value :process-kill))
      (funcall on-action
               state
               (ntop.app.domain:make-app-action
                :kind :dialog :value :choice-up))
      (funcall on-action state confirm)
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state)
       :to-be nil)
      (cl-weave:expect sent :to-equal (quote (((42) 15))))
      (cl-weave:expect forwarded :to-be nil)))

  (cl-weave:it "copies nested kill selection for move and select transitions"
    (let* ((config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition config))
           (dialog nil)
           (states (make-hash-table :test (function eq)))
           (source-state nil)
           (selection nil)
           (initial-index nil))
      (setf (ntop.shared.application:application-config-disable-advanced-kill-p
             config)
            t)
      (multiple-value-bind (opened result)
          (ntop.process.presentation:open-process-termination-dialog
           (list 42) "worker" nil config)
        (declare (ignore result))
        (setf dialog opened))
      (setf selection
            (ntop.process.presentation::process-termination-dialog-state-selection-state
             dialog)
            initial-index (ntop.ui:table-state-selected-index selection)
            (gethash :process-kill states) dialog
            source-state
            (ntop.app.domain:make-app-state
             :selected-widget :process
             :active-dialog :process-kill
             :widget-v-states states))
      (dolist (entry
               (list
                (list :dialog :choice-up)
                (list :navigation :first-row)))
        (let* ((kind (first entry))
               (value (second entry))
               (action
                 (ntop.app.domain:make-app-action :kind kind :value value)))
          (multiple-value-bind (next-state forwarded history-intent)
              (ntop.app.application::transition-application-state source-state action)
            (declare (ignore forwarded history-intent))
            (ntop.app.application::%update-process-dialog-state
             composition next-state action)
            (let* ((next-dialog
                     (gethash :process-kill
                              (ntop.app.domain:app-state-widget-v-states
                               next-state)))
                   (next-selection
                     (ntop.process.presentation::process-termination-dialog-state-selection-state
                      next-dialog)))
              (cl-weave:expect
               (gethash :process-kill
                        (ntop.app.domain:app-state-widget-v-states source-state))
               :to-be
               dialog)
              (cl-weave:expect
               (ntop.ui:table-state-selected-index selection)
               :to-be
               initial-index)
              (cl-weave:expect (not (eq next-dialog dialog)) :to-be t)
              (cl-weave:expect (not (eq next-selection selection)) :to-be t)
              (cl-weave:expect
               (ntop.ui:table-state-selected-index next-selection)
               :to-be
               0)))))))
  (cl-weave:it
  "applies process row navigation while leaving up and down for widget focus"
  (let* ((forwarded nil)
         (config (ntop.shared.application:make-application-config :process-default-grouped-p nil))
         (composition
           (ntop.app.application:make-application-composition
            config
            :on-action (lambda (state action)
                         (declare (ignore state))
                         (push action forwarded))))
         (records
           (loop for pid from 1 to 20
                 collect
                   (ntop.process.domain:make-process-record
                    :pid pid :ppid 1
                    :name (format nil "process-~D" pid)
                    :command (format nil "process-~D" pid)
                    :cpu-time-ns (* pid 1000000)
                    :resident-memory-bytes (* pid 1024)
                    :virtual-memory-bytes (* pid 2048)
                    :read-bytes (* pid 100) :write-bytes (* pid 50)
                    :ps-fallback-cpu-percent (float pid 1d0))))
         (snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 1d0
            :process-sample
            (ntop.process.domain:make-process-sample
             :sampled-at 1d0 :processes records)))
         (on-snapshot
           (ntop.app.application:application-composition-on-snapshot
            composition))
         (on-action
           (ntop.app.application:application-composition-on-action composition))
         (start-index 8)
         (row-navigation-values
           (list :previous-row :next-row :first-row :last-row
                 :page-up :page-down :half-page-up :half-page-down
                 :scroll-up :scroll-down)))
    (setf (ntop.shared.application:application-config-process-sort-column config)
          :pid
          (ntop.shared.application:application-config-process-sort-order config)
          :ascending)
    (funcall on-snapshot snapshot)
    (dolist (value row-navigation-values)
      (let* ((state (ntop.app.domain:make-app-state :selected-widget :process))
             (states (ntop.app.domain:app-state-widget-v-states state))
             (viewport
               (ntop.app.application::%process-row-navigation-viewport-height
                composition state config))
             (half-page (max 1 (floor viewport 2)))
             (expected
               (case value
                 (:previous-row (1- start-index))
                 (:next-row (1+ start-index))
                 (:first-row 0)
                 (:last-row 19)
                 (:page-up (max 0 (- start-index viewport)))
                 (:page-down (min 19 (+ start-index viewport)))
                 (:half-page-up (max 0 (- start-index half-page)))
                 (:half-page-down (min 19 (+ start-index half-page)))
                 (:scroll-up (1- start-index))
                 (:scroll-down (1+ start-index)))))
        (setf (gethash :process states)
              (ntop.ui:make-table-state :selected-index start-index)
              (gethash :process-sort-state states)
              (ntop.process.domain:make-process-sort-state
               :key :pid :order :ascending :memory-mode :percent))
        (funcall
         on-action
         state
         (ntop.app.domain:make-app-action :kind :navigation :value value))
        (cl-weave:expect
         (ntop.ui:table-state-selected-index (gethash :process states))
         :to-be
         expected)
        (cl-weave:expect
         (gethash :process-selected-key states)
         :to-equal
         (list :pid (1+ expected)))))
    (cl-weave:expect forwarded :to-be nil)
    (dolist (direction (list :up :down))
      (let* ((state (ntop.app.domain:make-app-state :selected-widget :process))
             (states (ntop.app.domain:app-state-widget-v-states state))
             (action
               (ntop.app.domain:make-app-action
                :kind :navigation :value direction)))
        (setf (gethash :process states)
              (ntop.ui:make-table-state :selected-index start-index))
        (cl-weave:expect
         (ntop.app.application::%process-row-navigation-action
          composition state action)
         :to-be
         nil)
        (funcall on-action state action)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index (gethash :process states))
         :to-be
         start-index)))
    (cl-weave:expect (length forwarded) :to-be 2)
    (let* ((source-state (ntop.app.domain:make-app-state
                          :selected-widget :process))
           (source-states
             (ntop.app.domain:app-state-widget-v-states source-state))
           (source-table (ntop.ui:make-table-state :selected-index start-index))
           (action
             (ntop.app.domain:make-app-action
              :kind :navigation :value :next-row)))
      (setf (gethash :process source-states) source-table)
      (multiple-value-bind (next-state forwarded-action history-intent)
          (ntop.app.application::transition-application-state
           source-state action)
        (declare (ignore forwarded-action history-intent))
        (cl-weave:expect
         (eq source-states
             (ntop.app.domain:app-state-widget-v-states next-state))
         :to-be
         nil)
        (ntop.app.application::%process-row-navigation-action
         composition next-state action)
        (cl-weave:expect
         (gethash :process source-states)
         :to-be
         source-table)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index source-table)
         :to-be
         start-index)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index
          (gethash :process
                   (ntop.app.domain:app-state-widget-v-states next-state)))
         :to-be
         (1+ start-index))))))

  (cl-weave:it "matches BTM help geometry (src/canvas.rs:138-194)"
    (dolist (case '((40 12 0 0 40 12)
                    (80 24 0 0 80 24)
                    (160 48 32 10 96 27)))
      (destructuring-bind (columns rows x y width height) case
        (let ((rectangle
                (ntop.app.application::%help-dialog-rectangle columns rows)))
          (cl-weave:expect
           (list (ntop.ui:rectangle-x rectangle)
                 (ntop.ui:rectangle-y rectangle)
                 (ntop.ui:rectangle-width rectangle)
                 (ntop.ui:rectangle-height rectangle))
           :to-equal
           (list x y width height)))))))
(load
 (asdf:system-relative-pathname
  "ntop/test"
  "t/app/application-compose-dialogs-extra-test.lisp"))
(load
 (asdf:system-relative-pathname
  "ntop/test"
  "t/app/application-compose-kill-test.lisp"))

