(in-package #:ntop.test)

(cl-weave:describe
 "process modal composition"
 (cl-weave:it
  "matches btm process search geometry and click bounds"
  ;; btm 0.14.9: src/canvas/widgets/process_table.rs:29-43, 206-245.
  (let* ((state (ntop.app.domain:make-app-state))
         (config (%wp12b-screen-config))
         (basic-config
           (ntop.shared.application:make-application-config :basic-p t))
         (composition
           (ntop.app.application:make-application-composition config))
         (on-action
           (ntop.app.application:application-composition-on-action composition))
         (*wp12b-screen-widget-calls* nil)
         (*wp12b-screen-recording-p* t))
    (funcall on-action
             state
             (ntop.app.domain:make-app-action :kind :search :value :process))
    (funcall on-action
             state
             (ntop.app.domain:make-app-action :kind :search :value "alpha"))
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 48 :state state :config config)
      (declare (ignorable screen))
      (setf *wp12b-screen-widget-calls*
            (nreverse *wp12b-screen-widget-calls*))
      (let* ((search-call
               (find :process-search *wp12b-screen-widget-calls*
                     :key #'first :test #'eq))
             (search-rectangle (second search-call))
             (process-call
               (find :process-table *wp12b-screen-widget-calls*
                     :key #'first :test #'eq))
             (process-rectangle (second process-call))
             (rows (third process-call))
             (row-areas
               (remove-if-not
                (lambda (area)
                  (eq (ntop.ui:clickable-area-action area) :select-row))
                areas))
             (row-area (first row-areas))
             (row-rectangle
               (and row-area
                    (ntop.ui:clickable-area-rectangle row-area))))
        (cl-weave:expect
         (list (ntop.ui:rectangle-x search-rectangle)
               (ntop.ui:rectangle-y search-rectangle)
               (ntop.ui:rectangle-width search-rectangle)
               (ntop.ui:rectangle-height search-rectangle))
         :to-equal
         (list 0 43 80 5))
        (progn
          (cl-weave:expect
           (list (ntop.ui:rectangle-x process-rectangle)
                 (ntop.ui:rectangle-y process-rectangle)
                 (ntop.ui:rectangle-width process-rectangle)
                 (ntop.ui:rectangle-height process-rectangle))
           :to-equal
           (list 1 24 78 18))
          (cl-weave:expect
           (ntop.process.presentation:process-table-config-focused-p
            (fifth process-call))
           :to-be nil)
          (cl-weave:expect
           (cl-tty-kit:cell-style
            (cl-tty-kit:screen-cell
             screen
             (1- (ntop.ui:rectangle-x process-rectangle))
             (ntop.ui:rectangle-y process-rectangle)))
           :to-equal
           (ntop.ui:presentation-theme-style :default :border)))
        (cl-weave:expect
         (and
          (equal (mapcar #'ntop.process.domain:process-row-name rows)
                 (list "alpha"))
          (not
           (null
            (search
             "alpha"
             (cl-tty-kit:screen-row-string
              screen
              (1+ (ntop.ui:rectangle-y search-rectangle))))))
          (let* ((body-bottom-row
                   (cl-tty-kit:screen-row-string
                    screen
                    (1- (ntop.ui:rectangle-y search-rectangle))))
                 (search-top-row
                   (cl-tty-kit:screen-row-string
                    screen
                    (ntop.ui:rectangle-y search-rectangle))))
            (and (char= (char body-bottom-row 0) #\└)
                 (char= (char body-bottom-row 79) #\┘)
                 (char= (char search-top-row 0) #\┌)
                 (char= (char search-top-row 79) #\┐))))
         :to-be t)
        (cl-weave:expect (not (null row-area)) :to-be t)
        (cl-weave:expect
         (< (ntop.ui:rectangle-y row-rectangle)
            (ntop.ui:rectangle-y search-rectangle))
         :to-be t)
        (setf (ntop.app.application::application-composition-clickable-areas
               composition)
              areas)
        (cl-weave:expect
         (not
          (null
           (ntop.app.application::%process-dialog-area-at
            composition :select-row
            (ntop.ui:rectangle-x row-rectangle)
            (ntop.ui:rectangle-y row-rectangle))))
         :to-be t)
        (cl-weave:expect
         (ntop.app.application::%process-dialog-area-at
          composition :select-row
          (ntop.ui:rectangle-x row-rectangle)
          (ntop.ui:rectangle-y search-rectangle))
         :to-be nil)
        (cl-weave:expect
         (first (car (last *wp12b-screen-widget-calls*)))
         :to-equal :process-search))
    (labels
        ((dimensions (rectangle)
           (list (ntop.ui:rectangle-x rectangle)
                 (ntop.ui:rectangle-y rectangle)
                 (ntop.ui:rectangle-width rectangle)
                 (ntop.ui:rectangle-height rectangle)))
         (layout (rectangle config)
           (multiple-value-bind (body search)
               (ntop.app.presentation::%screen-process-search-layout
                rectangle config)
             (list (dimensions body) (dimensions search)))))
      (cl-weave:expect
       (list
        (layout (ntop.ui:make-rectangle :x 2 :y 6 :width 40 :height 8)
                config)
        (layout (ntop.ui:make-rectangle :x 4 :y 10 :width 50 :height 8)
                basic-config)
        (layout (ntop.ui:make-rectangle :x 3 :y 20 :width 30 :height 2)
                config)
        (layout (ntop.ui:make-rectangle :x 7 :y 8 :width 24 :height 1)
                basic-config))
       :to-equal
       (list
        (list (list 2 6 40 3) (list 2 9 40 5))
        (list (list 4 10 50 5) (list 5 15 48 3))
        (list (list 3 20 30 0) (list 3 20 30 2))
        (list (list 7 8 24 0) (list 8 8 22 1))))))))
 (cl-weave:it
  "draws the centered process termination dialog and returns its choices"
  (let* ((state (ntop.app.domain:make-app-state))
         (config (%wp12b-screen-config))
         (snapshot (%wp12b-screen-fixture-snapshot))
         (*wp12b-screen-widget-calls* nil)
         (*wp12b-screen-recording-p* t))
    (multiple-value-bind (dialog-state status)
        (ntop.process.presentation:open-process-termination-dialog
         (list 100)
         "alpha"
         (ntop.app.domain:collection-snapshot-process-sample snapshot)
         config)
      (declare (ignore status))
      (setf (ntop.app.domain:app-state-active-dialog state) :process-kill
            (gethash :process-kill
                     (ntop.app.domain:app-state-widget-v-states state))
            dialog-state))
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24 :state state :config config)
      (declare (ignore screen))
      (setf *wp12b-screen-widget-calls*
            (nreverse *wp12b-screen-widget-calls*))
      (let* ((kill-call
               (find :process-termination-dialog
                     *wp12b-screen-widget-calls*
                     :key (function first)
                     :test (function eq)))
             (rectangle (second kill-call)))
        (cl-weave:expect (ntop.ui:rectangle-x rectangle) :to-equal 0)
        (cl-weave:expect (ntop.ui:rectangle-y rectangle) :to-equal 0)
        (cl-weave:expect (ntop.ui:rectangle-width rectangle) :to-equal 80)
        (cl-weave:expect (ntop.ui:rectangle-height rectangle) :to-equal 24)
        (cl-weave:expect
         (not
          (null
           (find :kill-choice areas
                 :key (function ntop.ui:clickable-area-action)
                 :test (function eq))))
         :to-be
         t)
        (cl-weave:expect
         (first (car (last *wp12b-screen-widget-calls*)))
         :to-equal
         :process-termination-dialog)))))
 (cl-weave:it
  "resolves grouped process rows to every matching PID"
  (let* ((source-snapshot (%wp12b-screen-fixture-snapshot))
         (source-sample
           (ntop.app.domain:collection-snapshot-process-sample
            source-snapshot))
         (source-records
           (ntop.process.domain:process-sample-processes source-sample))
         (duplicate
           (ntop.process.domain:make-process-record
            :pid 101
            :ppid 1
            :name "alpha"
            :command "alpha --serve"
            :cpu-time-ns 200000000
            :resident-memory-bytes (* 512 1024 1024)
            :virtual-memory-bytes (* 1024 1024 1024)
            :read-bytes 8000
            :write-bytes 2000
            :ps-fallback-cpu-percent 8.5d0))
         (snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 3d0
            :process-sample
            (ntop.process.domain:make-process-sample
             :sampled-at 3d0
             :processes (append source-records (list duplicate)))))
         (config (%wp12b-screen-config))
         (state (ntop.app.domain:make-app-state)))
    (setf
     (ntop.shared.application:application-config-process-default-grouped-p
      config)
     t
     (gethash :process-sort-state
              (ntop.app.domain:app-state-widget-v-states state))
     (ntop.process.domain:make-process-sort-state
      :key :pid
      :order :ascending
      :memory-mode :percent))
    (multiple-value-bind (pids name)
        (ntop.app.presentation::resolve-selected-process-targets
         state
         config
         snapshot
         nil)
      (cl-weave:expect pids :to-equal (quote (100 101)))
      (cl-weave:expect name :to-equal "alpha"))))
 (cl-weave:it
  "keeps the selected PID when process rows are reordered"
  (let* ((state (ntop.app.domain:make-app-state))
         (config (%wp12b-screen-config))
         (snapshot (%wp12b-screen-fixture-snapshot))
         (states (ntop.app.domain:app-state-widget-v-states state))
         (original-table (ntop.ui:make-table-state :selected-index 1))
         (ascending
           (ntop.process.domain:make-process-sort-state
            :key :pid :order :ascending :memory-mode :percent))
         (descending
           (ntop.process.domain:make-process-sort-state
            :key :pid :order :descending :memory-mode :percent)))
    (setf (gethash :process states) original-table)
    (ntop.app.presentation::%screen-process-rows
     snapshot state config ascending nil)
    (cl-weave:expect
     (gethash :process-selected-key states)
     :to-equal
     (list :pid 200))
    (multiple-value-bind (rows raw-rows)
        (ntop.app.presentation::%screen-process-rows
         snapshot state config descending nil)
      (declare (ignore raw-rows))
      (let* ((table (gethash :process states))
             (selected-index (ntop.ui:table-state-selected-index table)))
        (cl-weave:expect selected-index :to-be 0)
        (cl-weave:expect
         (ntop.process.domain:process-row-pid (nth selected-index rows))
         :to-be
         200)
        (cl-weave:expect
         (ntop.ui:table-state-selected-index original-table)
         :to-be
         1)
        (cl-weave:expect (not (eq table original-table)) :to-be t)))))

(cl-weave:it
  "keeps a grouped selection when its representative PID and row order change"
  (let* ((snapshot (%wp12b-screen-fixture-snapshot))
         (sample (ntop.app.domain:collection-snapshot-process-sample snapshot))
         (records (ntop.process.domain:process-sample-processes sample))
         (replacement
           (ntop.process.domain:make-process-record
            :pid 101 :ppid 1 :name "alpha" :command "alpha --serve"
            :cpu-time-ns 200000000 :resident-memory-bytes 1024
            :virtual-memory-bytes 2048 :read-bytes 8000 :write-bytes 2000
            :ps-fallback-cpu-percent 8.5d0))
         (reordered-snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 4d0
            :process-sample
            (ntop.process.domain:make-process-sample
             :sampled-at 4d0 :processes (list (second records) replacement))))
         (config (%wp12b-screen-config))
         (state (ntop.app.domain:make-app-state))
         (states (ntop.app.domain:app-state-widget-v-states state))
         (ascending
           (ntop.process.domain:make-process-sort-state
            :key :pid :order :ascending :memory-mode :percent))
         (descending
           (ntop.process.domain:make-process-sort-state
            :key :pid :order :descending :memory-mode :percent)))
    (setf
     (ntop.shared.application:application-config-process-default-grouped-p
      config)
     t
     (gethash :process states) (ntop.ui:make-table-state :selected-index 0))
    (ntop.app.presentation::%screen-process-rows
     snapshot state config ascending nil)
    (cl-weave:expect
     (gethash :process-selected-key states)
     :to-equal
     (list :group :name "alpha"))
    (multiple-value-bind (rows raw-rows)
        (ntop.app.presentation::%screen-process-rows
         reordered-snapshot state config descending nil)
      (declare (ignore raw-rows))
      (let* ((table (gethash :process states))
             (selected-index (ntop.ui:table-state-selected-index table))
             (selected-row (nth selected-index rows)))
        (cl-weave:expect selected-index :to-be 1)
        (cl-weave:expect
         (ntop.process.domain:process-row-pid selected-row)
         :to-be
         101)
        (cl-weave:expect
         (gethash :process-selected-key states)
         :to-equal
         (list :group :name "alpha"))))))

(cl-weave:it
  "clamps a removed filtered selection to a visible process row"
  (let* ((state (ntop.app.domain:make-app-state))
         (config (%wp12b-screen-config))
         (snapshot (%wp12b-screen-fixture-snapshot))
         (states (ntop.app.domain:app-state-widget-v-states state))
         (search-field
           (ntop.process.presentation:make-process-search-field-state
            :config config)))
    (ntop.process.presentation:transition-process-search-field
     search-field :insert-text "beta")
    (setf (gethash :process states)
          (ntop.ui:make-table-state :selected-index 99)
          (gethash :process-selected-key states) (list :pid 100)
          (gethash :process-search states) search-field)
    (multiple-value-bind (rows raw-rows)
        (ntop.app.presentation::%screen-process-rows
         snapshot
         state
         config
         (ntop.app.presentation::%screen-process-sort-state config state)
         nil)
      (declare (ignore raw-rows))
      (cl-weave:expect
       (mapcar (function ntop.process.domain:process-row-name) rows)
       :to-equal
       (list "beta"))
      (cl-weave:expect
       (ntop.ui:table-state-selected-index (gethash :process states))
       :to-be
       0)
      (cl-weave:expect
       (gethash :process-selected-key states)
       :to-equal
       (list :pid 200)))))
(cl-weave:it
  "clamps kill selection and refuses to open when no process rows remain"
  (let* ((config (ntop.shared.application:make-application-config :process-default-grouped-p nil))
         (composition (ntop.app.application:make-application-composition config))
         (records
           (list
            (ntop.process.domain:make-process-record
             :pid 100 :ppid 1 :name "alpha" :command "alpha"
             :cpu-time-ns 100000000 :resident-memory-bytes 1024
             :virtual-memory-bytes 2048 :read-bytes 100 :write-bytes 20
             :ps-fallback-cpu-percent 2d0)
            (ntop.process.domain:make-process-record
             :pid 200 :ppid 1 :name "beta" :command "beta"
             :cpu-time-ns 200000000 :resident-memory-bytes 2048
             :virtual-memory-bytes 4096 :read-bytes 200 :write-bytes 40
             :ps-fallback-cpu-percent 4d0)))
         (snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 1d0
            :process-sample
            (ntop.process.domain:make-process-sample
             :sampled-at 1d0 :processes records)))
         (state (ntop.app.domain:make-app-state :selected-widget :process))
         (states (ntop.app.domain:app-state-widget-v-states state))
         (on-action
           (ntop.app.application:application-composition-on-action composition))
         (on-snapshot
           (ntop.app.application:application-composition-on-snapshot
            composition)))
    (setf
     (ntop.shared.application:application-config-disable-advanced-kill-p config)
     t
     (gethash :process states) (ntop.ui:make-table-state :selected-index 99)
     (gethash :process-sort-state states)
     (ntop.process.domain:make-process-sort-state
      :key :pid :order :ascending :memory-mode :percent))
    (funcall on-snapshot snapshot)
    (funcall
     on-action
     state
     (ntop.app.domain:make-app-action :kind :dialog :value :process-kill))
    (let* ((dialog (gethash :process-kill states))
           (termination-state
             (ntop.process.presentation::process-termination-dialog-state-termination-state
              dialog)))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog state)
       :to-be
       :process-kill)
      (cl-weave:expect
       (ntop.process.application:process-termination-state-target-pids
        termination-state)
       :to-equal
       (list 200))
      (cl-weave:expect
       (ntop.ui:table-state-selected-index (gethash :process states))
       :to-be
       1)
      (cl-weave:expect
       (gethash :process-selected-key states)
       :to-equal
       (list :pid 200)))
    (let* ((empty-state (ntop.app.domain:make-app-state
                         :selected-widget :process))
           (empty-states
             (ntop.app.domain:app-state-widget-v-states empty-state))
           (search-field
             (ntop.process.presentation:make-process-search-field-state
              :config config)))
      (ntop.process.presentation:transition-process-search-field
       search-field :insert-text "no-matching-process")
      (setf (gethash :process empty-states)
            (ntop.ui:make-table-state :selected-index 99)
            (gethash :process-search empty-states) search-field)
      (funcall
       on-action
       empty-state
       (ntop.app.domain:make-app-action :kind :dialog :value :process-kill))
      (cl-weave:expect
       (ntop.app.domain:app-state-active-dialog empty-state)
       :to-be
       nil)
      (cl-weave:expect (gethash :process-kill empty-states) :to-be nil)
      (cl-weave:expect
       (gethash :process-selected-key empty-states)
       :to-be
       nil))))
(cl-weave:it
  "renders elapsed process time from the supplied Unix clock"
  (let* ((snapshot
           (ntop.app.domain:make-collection-snapshot
            :sampled-at 3d0
            :process-sample
            (ntop.process.domain:make-process-sample
             :sampled-at 3d0
             :processes
             (list
              (ntop.process.domain:make-process-record
               :pid 100
               :ppid 1
               :name "alpha"
               :command "alpha --serve"
               :start-time-unix-seconds 100)))))
         (state
           (ntop.app.domain:make-app-state
            :selected-widget :process :expanded-p t))
         (config
           (ntop.shared.application:make-application-config
            :theme :default
            :process-default-grouped-p nil
            :process-columns
            (quote (:pid-or-count :name-or-command :time))))
         (screen (cl-tty-kit:make-screen 80 24)))
    (ntop.app.presentation:draw-application-screen
     screen state config snapshot 80 24 nil nil 175)
    (cl-weave:expect
     (not
      (null
       (search "1m 15.00s" (cl-tty-kit:screen-to-string screen))))
     :to-be t))))
