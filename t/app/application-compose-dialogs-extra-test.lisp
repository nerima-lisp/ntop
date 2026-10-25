(in-package #:ntop.test)
(progn
  (progn (defun %d-action (kind value) (ntop.app.domain:make-app-action :kind kind :value value))
    (defun %d-expect (actual expected) (cl-weave:expect actual :to-be expected)))
  (defun %d-sample (&optional (count 3))
    (ntop.process.domain:make-process-sample
     :sampled-at 1d0
     :processes
     (loop for pid from 42 below (+ 42 count)
           collect
           (ntop.process.domain:make-process-record
            :pid pid :ppid 1 :name (format nil "worker-~D" pid)
            :command (format nil "worker-~D" pid)
            :cpu-time-ns (* pid 1000000)
            :resident-memory-bytes 1024 :virtual-memory-bytes 2048
            :read-bytes 10 :write-bytes 5))))

  (defun %d-snapshot (&optional (sample (%d-sample)))
    (ntop.app.domain:make-collection-snapshot
     :sampled-at 1d0 :process-sample sample))

  (defun %d-composition (&key config snapshot sample areas rows)
    (let ((composition (ntop.app.application:make-application-composition
                        (or config (ntop.shared.application:make-application-config))
                        :process-signal-sender (constantly t))))
      (setf
       (ntop.app.application::application-composition-latest-snapshot composition)
       snapshot
       (ntop.app.application::application-composition-latest-process-sample composition)
       sample
       (ntop.app.application::application-composition-clickable-areas composition)
       areas
       (ntop.app.application::application-composition-screen-rows composition)
       rows)
      composition))

  (defun %d-kill-dialog (config)
    (setf
     (ntop.shared.application:application-config-disable-advanced-kill-p config)
     t)
    (ntop.process.presentation:open-process-termination-dialog
     (list 42 43) "worker" (%d-sample) config))

  (defun %d-kill-state (dialog)
    (let ((state
            (ntop.app.domain:make-app-state
             :selected-widget :process :active-dialog :process-kill)))
      (setf (gethash :process-kill
                     (ntop.app.domain:app-state-widget-v-states state))
            dialog)
      state))

  (cl-weave:describe "application compose dialogs extra coverage"
    (cl-weave:it "checks pointer values, hit boxes, and process row clicks"
      (let* ((area-1
               (ntop.ui:make-clickable-area
                :rectangle (ntop.ui:make-rectangle :x 2 :y 3 :width 1 :height 1)
                :action :select-row :value 1))
             (area-2
               (ntop.ui:make-clickable-area
                :rectangle (ntop.ui:make-rectangle :x 2 :y 3 :width 1 :height 1)
                :action :select-row :value 2))
             (composition (%d-composition :areas (list area-1 area-2)))
             (state (ntop.app.domain:make-app-state :selected-widget :process))
             (states (ntop.app.domain:app-state-widget-v-states state)))
        (multiple-value-bind (x y hit)
            (ntop.app.application::%process-dialog-pointer-coordinates
             (list :pointer :process-row 2 3) :process-row)
          (cl-weave:expect (list x y hit) :to-equal (list 2 3 t)))
        (dolist (value
                 (list nil :bad (list :pointer :process-row 2)
                       (list :pointer :other 2 3)
                       (list :pointer :process-row "2" 3)
                       (list :pointer :process-row 2 nil)))
          (cl-weave:expect
           (nth-value
            2
            (ntop.app.application::%process-dialog-pointer-coordinates
             value :process-row))
           :to-be
           nil))
        (cl-weave:expect
         (ntop.ui:clickable-area-value
          (ntop.app.application::%process-dialog-area-at
           composition :select-row 2 3))
         :to-be
         2)
        (dolist (point
                 (list (list :other 2 3) (list :select-row 1 3)
                       (list :select-row 3 3) (list :select-row 2 2)
                       (list :select-row 2 4)))
          (cl-weave:expect
           (ntop.app.application::%process-dialog-area-at
            composition (first point) (second point) (third point))
           :to-be
           nil))
        (setf (gethash :process states) (ntop.ui:make-table-state)
              (gethash :process-selected-key states) :stale)
        (cl-weave:expect
         (ntop.app.application::%process-row-pointer-action
          composition state (%d-action :select
                                       (list :pointer :process-row 2 3)))
         :to-be
         t)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index (gethash :process states))
         :to-be
         2)
        (cl-weave:expect (nth-value 1 (gethash :process-selected-key states))
                         :to-be nil)
        (cl-weave:expect
         (ntop.app.application::%process-row-pointer-action
          composition state (%d-action :select
                                       (list :pointer :process-row 9 9)))
         :to-be
         t)
        (cl-weave:expect
         (ntop.app.application::%process-row-pointer-action
          composition state (%d-action :select :bad))
         :to-be
         nil)))

    (cl-weave:it "covers search edits, navigation, submit, and close transitions"
      (let ((config (ntop.shared.application:make-application-config))
            (state (ntop.app.domain:make-app-state)))
        (dolist (value
                 (list :toggle-case :toggle-whole-word :toggle-regex
                       :cursor-start :cursor-end :clear
                       :delete-word-backward :delete-backward :delete-forward))
          (cl-weave:expect
           (ntop.app.application::%process-search-transition-action
            state (%d-action :search value) config)
           :to-be
           t))
        (cl-weave:expect
         (ntop.app.application::%process-search-transition-action
          state (%d-action :search "worker") config)
         :to-be
         t)
        (dolist (value (list :left :right))
          (cl-weave:expect
           (ntop.app.application::%process-search-transition-action
            state (%d-action :navigation value) config)
           :to-be
           t))
        (dolist (action
                 (list (%d-action :search :submit)
                       (%d-action :dialog (list :close :process-search))))
          (cl-weave:expect
           (ntop.app.application::%process-search-transition-action
            state action config)
           :to-be
           t))
        (dolist (action
                 (list (%d-action :search :unknown)
                       (%d-action :navigation :up)
                       (%d-action :dialog :close)
                       (%d-action :dialog (list :close :other))
                       (%d-action :quit nil)))
          (cl-weave:expect
           (ntop.app.application::%process-search-transition-action
            state action config)
           :to-be
           nil))
        (cl-weave:expect
         (ntop.app.application::%ensure-process-search-field state config)
         :to-be
         (gethash :process-search
                  (ntop.app.domain:app-state-widget-v-states state)))))

    (cl-weave:it "covers navigation deltas, table config, and sort keys"
  (let* ((config (ntop.shared.application:make-application-config))
         (state (ntop.app.domain:make-app-state))
         (stored
           (ntop.process.domain:make-process-sort-state
            :key :name :order :ascending :memory-mode :bytes))
         (gpu
           (ntop.gpu.domain:make-gpu-sample
            :sampled-at 1d0
            :devices
            (list (ntop.gpu.domain:make-gpu-device
                   :name "gpu" :usage-percent 10d0
                   :memory-used-bytes 1 :memory-total-bytes 2))))
         (snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 1d0 :gpu-sample gpu))
         (table-config nil))
    (flet ((check (actual expected)
             (cl-weave:expect actual :to-be expected))
           (same (actual expected)
             (cl-weave:expect actual :to-equal expected))
           (delta (value height)
             (ntop.app.application::%process-dialog-navigation-delta
              value height))
           (sort-key (value table-config)
             (ntop.app.application::%process-sort-action-key
              value table-config)))
      (dolist (entry
               (list (list :up 8 -1) (list :page-up 8 -8)
                     (list :page-down 0 1) (list :half-page-up 8 -4)
                     (list :half-page-down 1 1)))
        (check (delta (first entry) (second entry)) (third entry)))
      (check (delta :unknown 8) nil)
      (setf (gethash :process-sort-state
                     (ntop.app.domain:app-state-widget-v-states state))
            stored)
      (check (ntop.app.application::%process-sort-state state config) stored)
      (setf (gethash :process-sort-state
                     (ntop.app.domain:app-state-widget-v-states state))
            :invalid)
      (check
       (ntop.process.domain:process-sort-state-key
        (ntop.app.application::%process-sort-state state config))
       (ntop.shared.application:application-config-process-sort-column config))
      (setf
       (ntop.shared.application:application-config-process-default-memory-value-p
        config)
       t
       (ntop.shared.application:application-config-process-default-tree-p config)
       nil
       (ntop.shared.application:application-config-process-default-grouped-p config)
       t
       (ntop.shared.application:application-config-process-show-command-p config)
       t
       (ntop.shared.application:application-config-show-table-scroll-position-p
        config)
       t)
      (setf table-config
            (ntop.app.application::%process-dialog-table-config
             config snapshot stored))
      (same
       (list
        (ntop.process.presentation:process-table-config-memory-mode table-config)
        (ntop.process.presentation:process-table-config-grouped-p table-config)
        (ntop.process.presentation:process-table-config-show-command-p table-config)
        (ntop.process.presentation:process-table-config-gpu-available-p table-config))
       (list :bytes t t t))
      (setf
       (ntop.shared.application:application-config-disable-gpu-p config)
       t
       (ntop.shared.application:application-config-process-default-tree-p config)
       t)
      (setf table-config
            (ntop.app.application::%process-dialog-table-config
             config snapshot stored))
      (same
       (list
        (ntop.process.presentation:process-table-config-gpu-available-p table-config)
        (ntop.process.presentation:process-table-config-grouped-p table-config))
       (list nil nil))
      (setf
       (ntop.shared.application:application-config-process-default-memory-value-p
        config)
       nil
       (ntop.shared.application:application-config-process-default-tree-p config)
       nil
       (ntop.shared.application:application-config-process-default-grouped-p config)
       nil)
      (setf table-config
            (ntop.app.application::%process-dialog-table-config
             config nil stored))
      (same
       (list
        (ntop.process.presentation:process-table-config-memory-mode table-config)
        (ntop.process.presentation:process-table-config-grouped-p table-config)
        (ntop.process.presentation:process-table-config-show-scroll-position-p
         table-config))
       (list :percent nil t))
      (let ((menu
              (ntop.app.application::%process-sort-dialog-state
               state table-config)))
        (check (typep menu 'ntop.ui:table-state) t)
        (check
         (ntop.app.application::%process-sort-dialog-state state table-config)
         menu))
      (dolist (entry
               (list (list :cpu :cpu-percent) (list :memory :memory)
                     (list :gpu-utilization :gpu-usage-percent)
                     (list :gpu-memory :gpu-memory)))
        (check (sort-key (first entry) table-config) (second entry)))
      (dolist (entry
               (list
                (list :pid-or-count
                      (ntop.process.presentation:make-process-table-config
                       :grouped-p t)
                      :count)
                (list :pid-or-count
                      (ntop.process.presentation:make-process-table-config
                       :grouped-p nil)
                      :pid)
                (list :name
                      (ntop.process.presentation:make-process-table-config
                       :show-command-p t)
                      :command)
                (list :name
                      (ntop.process.presentation:make-process-table-config
                       :show-command-p nil)
                      :name)))
        (check (sort-key (first entry) (second entry)) (third entry)))
      (check (sort-key :cpu-percent table-config) :cpu-percent)
      (check (sort-key :unknown table-config) nil))))

    (cl-weave:it "covers sort dialog choices and navigation actions"
  (let* ((config (ntop.shared.application:make-application-config))
         (area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 4 :y 5 :width 1 :height 1)
            :action :sort-choice :value :memory))
         (composition (%d-composition :areas (list area) :rows 1))
         (state (ntop.app.domain:make-app-state
                 :selected-widget :process :active-dialog :process-sort)))
    (flet ((check (actual expected)
             (cl-weave:expect actual :to-be expected))
           (run (action)
             (ntop.app.application::%process-sort-dialog-action
              composition state action config)))
      (check (ntop.app.application::%open-process-sort-dialog
              state config nil)
             t)
      (check (typep (gethash :process-sort
                             (ntop.app.domain:app-state-widget-v-states state))
                    'ntop.ui:table-state)
             t)
      (check (ntop.app.application::%apply-process-sort-choice
              state config nil 0)
             t)
      (dolist (index (list -1 999 1.5 nil))
        (check (ntop.app.application::%apply-process-sort-choice
                state config nil index)
               nil))
      (dolist (action
               (list (%d-action :sort-processes :apply)
                     (%d-action :navigation :first-row)
                     (%d-action :navigation :last-row)
                     (%d-action :sort-processes (list :pointer :sort-choice 4 5))
                     (%d-action :sort-processes (list :pointer :sort-choice 9 9))
                     (%d-action :sort-processes :unknown)
                     (%d-action :navigation :down)
                     (%d-action :navigation :page-up)
                     (%d-action :dialog :choice-up)
                     (%d-action :dialog :choice-down)))
        (check (run action) t))
      (dolist (action-spec
               (quote ((:navigation :unknown) (:dialog :close) (:help nil))))
        (check (run (apply (function %d-action) action-spec)) nil))
      (check
       (ntop.process.domain:process-sort-state-key
        (gethash :process-sort-state
                 (ntop.app.domain:app-state-widget-v-states state)))
       :memory))))

    (cl-weave:it "covers kill opening, movement, selection, and close actions"
  (let* ((sample (%d-sample))
         (snapshot (%d-snapshot sample))
         (config (ntop.shared.application:make-application-config))
         (area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 6 :y 7 :width 1 :height 1)
            :action :kill-choice :value most-positive-fixnum))
         (composition
           (%d-composition :config config :snapshot snapshot :sample sample
                           :areas (list area) :rows 8))
         (dialog (%d-kill-dialog config)))
    (flet ((act (state kind value)
             (ntop.app.application::%process-kill-dialog-action
              composition state (%d-action kind value))))
      (%d-expect
       (ntop.app.application::%open-process-kill-dialog
        composition (ntop.app.domain:make-app-state :selected-widget :process)
        config)
       t)
      (let* ((state (%d-kill-state dialog))
             (states (ntop.app.domain:app-state-widget-v-states state)))
        (dolist (entry
                 (list (list :dialog :choice-up t)
                       (list :dialog :choice-down t)
                       (list :dialog :other nil)
                       (list :navigation :first-row t)
                       (list :navigation :last-row t)
                       (list :navigation :next-row t)
                       (list :navigation :unknown nil)
                       (list :process-terminate
                             (list :pointer :kill-choice 6 7) t)
                       (list :process-terminate
                             (list :pointer :kill-choice 9 9) t)
                       (list :process-terminate
                             (list :pointer :wrong 6 7) nil)
                       (list :process-terminate :confirm t)
                       (list :help nil nil)))
          (%d-expect (act state (first entry) (second entry)) (third entry)))
        (%d-expect (ntop.app.domain:app-state-active-dialog state) nil)
        (%d-expect
         (ntop.ui:table-state-selected-index
          (ntop.process.presentation::process-termination-dialog-state-selection-state
           dialog))
         1)
        (%d-expect
         (ntop.app.application::%move-process-kill-selection
          composition state nil 1 2)
         nil)
        (%d-expect
         (ntop.app.application::%select-process-kill-choice
          composition state nil 0)
         nil)
        (%d-expect
         (ntop.app.application::%select-process-kill-choice
          composition state dialog most-positive-fixnum)
         (gethash :process-kill states))
        (let ((empty-state (%d-kill-state nil)))
          (%d-expect (act empty-state :dialog :choice-up) nil)
          (%d-expect (act empty-state :navigation :next-row) nil)
          (%d-expect
           (act empty-state :process-terminate
                (list :pointer :kill-choice 6 7))
           t)))
      (let* ((composition-without-snapshot (%d-composition :sample sample))
             (state (ntop.app.domain:make-app-state
                     :selected-widget :process :active-dialog :process-kill)))
        (setf (gethash :process-kill
                       (ntop.app.domain:app-state-widget-v-states state))
              dialog)
        (%d-expect
         (ntop.app.application::%open-process-kill-dialog
          composition-without-snapshot state config)
         t)
        (%d-expect (ntop.app.domain:app-state-active-dialog state) nil)
        (%d-expect
         (nth-value
          1
          (gethash :process-kill
                   (ntop.app.domain:app-state-widget-v-states state)))
         nil))
      (let* ((read-only-config
               (ntop.shared.application:make-application-config))
             (read-only-composition
               (%d-composition :config read-only-config :snapshot snapshot))
             (state (ntop.app.domain:make-app-state
                     :selected-widget :process :active-dialog :process-kill)))
        (setf
         (ntop.shared.application:application-config-read-only-p read-only-config)
         t)
        (%d-expect
         (ntop.app.application::%open-process-kill-dialog
          read-only-composition state read-only-config)
         t)
        (%d-expect (ntop.app.domain:app-state-active-dialog state) nil)))))

    (cl-weave:it "covers dialog dispatch, direct sorting, and reverse sorting"
  (let* ((sample (%d-sample))
         (snapshot (%d-snapshot sample))
         (config (ntop.shared.application:make-application-config))
         (row-area
           (ntop.ui:make-clickable-area
            :rectangle (ntop.ui:make-rectangle :x 2 :y 3 :width 1 :height 1)
            :action :select-row :value 0))
         (composition (%d-composition :config config :snapshot snapshot
                                      :sample sample :areas (list row-area)
                                      :rows 8))
         (state (ntop.app.domain:make-app-state :selected-widget :process)))
    (flet ((check (actual expected)
             (cl-weave:expect actual :to-be expected))
           (send (action)
             (ntop.app.application::%update-process-dialog-state
              composition state action)))
      (check (send (%d-action :search :process)) t)
      (setf (ntop.app.domain:app-state-active-dialog state) :process-search)
      (check (send (%d-action :search "worker")) t)
      (check (send (%d-action :dialog (list :close :process-search))) t)
      (setf (ntop.app.domain:app-state-active-dialog state) :process-sort)
      (check (send (%d-action :navigation :down)) t)
      (setf (ntop.app.domain:app-state-active-dialog state) nil)
      (check (send (%d-action :dialog :process-sort)) t)
      (setf (ntop.app.domain:app-state-active-dialog state) :process-kill)
      (check (send (%d-action :dialog :process-kill)) t)
      (check (send (%d-action :process-terminate :confirm)) t)
      (check
       (send (%d-action :select (list :pointer :process-row 2 3)))
       t)
      (check
       (ntop.ui:table-state-selected-index
        (gethash :process (ntop.app.domain:app-state-widget-v-states state)))
       0)
      (check (send (%d-action :navigation :next-row)) t)
      (dolist (value (list :cpu :memory :gpu-utilization
                           :gpu-memory :pid-or-count :name))
        (check (send (%d-action :sort-processes value)) t))
      (dolist (value (list :apply :temperature-name :temperature-value :unknown))
        (check (send (%d-action :sort-processes value)) nil))
      (progn
  (setf
   (ntop.app.domain:app-state-selected-widget state)
   :cpu)
  (check (send (%d-action :sort-processes :cpu)) nil)
  (setf
   (ntop.app.domain:app-state-selected-widget state)
   :process))
      (check (send (%d-action :reverse-process-sort nil)) t)
      (setf state (ntop.app.domain:make-app-state :selected-widget :cpu))
      (check (send (%d-action :reverse-process-sort nil)) nil)
      (check (send (%d-action :help nil)) nil))))))
