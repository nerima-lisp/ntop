(in-package #:ntop.test)

(cl-weave:describe "process sort menu placement"
                   (cl-weave:it
                    "matches btm left sort layout"
                    ;; btm 0.14.9: src/canvas.rs:264-272;
                    ;; src/canvas/widgets/process_table.rs:20, 45-55, 73-100, 260-289;
                    ;; src/widgets/process_table.rs:239-255.
                    (let* ((state (ntop.app.domain:make-app-state))
                           (config (%wp12b-screen-config))
                           (*wp12b-screen-widget-calls* nil)
                           (*wp12b-screen-recording-p* t))
                      (setf (ntop.app.domain:app-state-active-dialog state) :process-sort)
                      (multiple-value-bind (areas screen)
                          (%wp12b-screen-render 80
                                                24
                                                :state
                                                state
                                                :config
                                                config)
                        (declare (ignorable screen))
                        (setf *wp12b-screen-widget-calls* (nreverse
                                                           *wp12b-screen-widget-calls*))
                        (let* ((placements
                                (nth-value 0
                                           (ntop.app.presentation::%screen-layout-placements
                                            80
                                            24
                                            state
                                            (ntop.app.presentation::%screen-layout-mode
                                             state
                                             config))))
                               (process-area
                                (ntop.app.presentation::%screen-inner-rectangle
                                 (cdr
                                  (assoc :process
                                         placements
                                         :test
                                         (function eq)))))
                               (sort-rectangle
                                (second
                                 (find :process-sort-menu
                                       *wp12b-screen-widget-calls*
                                       :key
                                       (function first)
                                       :test
                                       (function eq))))
                               (table-rectangle
                                (second
                                 (find :process-table
                                       *wp12b-screen-widget-calls*
                                       :key
                                       (function first)
                                       :test
                                       (function eq))))
                               (sort-choice-areas
                                (remove-if-not
                                 (lambda (area)
                                   (eq (ntop.ui:clickable-area-action area)
                                       :sort-choice))
                                 areas)))
                          (progn
                           (cl-weave:expect
                            (list
                             (list (ntop.ui:rectangle-x sort-rectangle)
                               (ntop.ui:rectangle-y sort-rectangle)
                               (ntop.ui:rectangle-width sort-rectangle)
                               (ntop.ui:rectangle-height sort-rectangle))
                             (list (ntop.ui:rectangle-x table-rectangle)
                               (ntop.ui:rectangle-y table-rectangle)
                               (ntop.ui:rectangle-width table-rectangle)
                               (ntop.ui:rectangle-height table-rectangle)))
                            :to-equal
                            (list
                             (list (ntop.ui:rectangle-x process-area)
                               (1+ (ntop.ui:rectangle-y process-area))
                               9
                               (1- (ntop.ui:rectangle-height process-area)))
                             (list (+ (ntop.ui:rectangle-x process-area) 11)
                               (ntop.ui:rectangle-y process-area)
                               (- (ntop.ui:rectangle-width process-area) 11)
                               (ntop.ui:rectangle-height process-area))))
                           (cl-weave:expect
                            (ntop.process.presentation:process-table-config-focused-p
                             (fifth
                              (find :process-table *wp12b-screen-widget-calls*
                                :key (function first)
                                :test (function eq))))
                            :to-be nil)
                           (cl-weave:expect
                            (cl-tty-kit:cell-style
                             (cl-tty-kit:screen-cell
                              screen
                              (1- (ntop.ui:rectangle-x table-rectangle))
                              (ntop.ui:rectangle-y table-rectangle)))
                            :to-equal
                            (ntop.ui:presentation-theme-style :default :border))
                           (cl-weave:expect
                            (cl-tty-kit:cell-style
                             (cl-tty-kit:screen-cell
                              screen
                              (1- (ntop.ui:rectangle-x sort-rectangle))
                              (1- (ntop.ui:rectangle-y sort-rectangle))))
                            :to-equal
                            (ntop.ui:presentation-theme-style
                             :default :highlighted-border)))
                          (cl-weave:expect
                           (and sort-choice-areas
                                (every
                                 (lambda (area)
                                   (let ((rectangle
                                          (ntop.ui:clickable-area-rectangle
                                           area)))
                                     (and
                                      (= (ntop.ui:rectangle-x rectangle)
                                         (ntop.ui:rectangle-x sort-rectangle))
                                      (= (ntop.ui:rectangle-width rectangle) 9)
                                      (= (ntop.ui:rectangle-height rectangle) 1)
                                      (> (ntop.ui:rectangle-y rectangle)
                                         (ntop.ui:rectangle-y sort-rectangle))
                                      (< (ntop.ui:rectangle-y rectangle)
                                         (+ (ntop.ui:rectangle-y sort-rectangle)
                                            (ntop.ui:rectangle-height
                                             sort-rectangle))))))
                                 sort-choice-areas))
                           :to-be
                           t)
                          (multiple-value-bind (basic-areas basic-screen)
                           (%wp12b-screen-render 80 24 :state state
                            :config (ntop.shared.application:make-application-config :basic-p t))
                           (declare (ignorable basic-areas))
                           (setf *wp12b-screen-widget-calls*
                            (nreverse *wp12b-screen-widget-calls*))
                           (let* ((basic-process-call
                            (find :process-table *wp12b-screen-widget-calls*
                              :key (function first)
                              :test (function eq)))
                             (basic-menu-call
                              (find :process-sort-menu *wp12b-screen-widget-calls*
                                :key (function first)
                                :test (function eq)))
                             (basic-menu-rectangle (second basic-menu-call))
                             (basic-config
                              (ntop.shared.application:make-application-config :basic-p t))
                             (process-placement
                              (cdr
                               (assoc :process
                                (nth-value 0
                                 (ntop.app.presentation::%screen-layout-placements
                                  80 24 state
                                  (ntop.app.presentation::%screen-layout-mode
                                   state basic-config))))))
                             (process-right-edge-x
                              (+ (ntop.ui:rectangle-x process-placement)
                               (1- (ntop.ui:rectangle-width process-placement))))
                             (process-top-y (ntop.ui:rectangle-y process-placement)))
                            (cl-weave:expect
                             (ntop.process.presentation:process-table-config-focused-p
                              (fifth basic-process-call))
                             :to-be nil)
                            (cl-weave:expect
                             (cl-tty-kit:cell-style
                              (cl-tty-kit:screen-cell
                               basic-screen
                               (1- (ntop.ui:rectangle-x basic-menu-rectangle))
                               (1- (ntop.ui:rectangle-y basic-menu-rectangle))))
                             :to-equal
                             (ntop.ui:presentation-theme-style
                              :default :highlighted-border))
                            (cl-weave:expect
                             (not
                              (member
                               (cl-tty-kit:cell-char
                                (cl-tty-kit:screen-cell
                                 basic-screen process-right-edge-x process-top-y))
                               (list #\┌ #\┐ #\└ #\┘ #\─ #\│)))
                             :to-be t))))))))

(cl-weave:describe "process search over process sort"
  (cl-weave:it
   "keeps the btm sort sidebar beside the table while process search is open"
   ;; btm 0.14.9: src/app.rs:333-349; src/canvas/widgets/process_table.rs:35-55.
   (let* ((state (ntop.app.domain:make-app-state :active-dialog :process-sort))
          (config (%wp12b-screen-config))
          (states (ntop.app.domain:app-state-widget-v-states state))
          (sort-state
            (ntop.app.presentation::%screen-process-sort-state config state)))
     (multiple-value-bind (table-state process-config)
         (ntop.app.presentation::%screen-process-widget-config
          nil state config nil sort-state)
       (declare (ignore table-state))
       (setf (gethash :process-sort states)
             (ntop.process.presentation:make-process-sort-menu-state
              process-config)))
     (multiple-value-bind (search-state forwarded history)
         (ntop.app.application::transition-application-state
          state
          (ntop.app.domain:make-app-action :kind :search :value :process))
       (declare (ignore forwarded history))
       (setf (gethash :process-search
                      (ntop.app.domain:app-state-widget-v-states search-state))
             (ntop.process.presentation:make-process-search-field-state
              :config config))
       (let ((*wp12b-screen-widget-calls* nil)
             (*wp12b-screen-recording-p* t))
         (multiple-value-bind (areas screen)
             (%wp12b-screen-render 80 48 :state search-state :config config)
           (declare (ignore areas))
           (setf *wp12b-screen-widget-calls*
                 (nreverse *wp12b-screen-widget-calls*))
           (let* ((placements
                    (nth-value
                     0
                     (ntop.app.presentation::%screen-layout-placements
                      80 48 search-state
                      (ntop.app.presentation::%screen-layout-mode
                       search-state config)
                      nil config)))
                  (process-placement
                    (cdr (assoc :process placements :test #'eq)))
                  (search-layout
                    (multiple-value-list
                     (ntop.app.presentation::%screen-process-search-layout
                      process-placement config)))
                  (sort-layout
                    (multiple-value-list
                     (ntop.app.presentation::%screen-process-sort-layout
                      (first search-layout) config)))
                  (sort-menu-call
                    (find :process-sort-menu *wp12b-screen-widget-calls*
                     :key (function first) :test (function eq)))
                  (table-call
                    (find :process-table *wp12b-screen-widget-calls*
                     :key (function first) :test (function eq)))
                  (search-call
                    (find :process-search *wp12b-screen-widget-calls*
                     :key (function first) :test (function eq)))
                  (sort-rectangle (second sort-menu-call))
                  (table-rectangle (second table-call))
                  (search-rectangle (second search-call)))
             (cl-weave:expect
              (ntop.app.domain:app-state-active-dialog search-state)
              :to-be :process-search)
             (cl-weave:expect
              (gethash :process-sort-background-p
                       (ntop.app.domain:app-state-widget-v-states search-state))
              :to-be t)
             (cl-weave:expect
              (list (ntop.ui:rectangle-x sort-rectangle)
                    (ntop.ui:rectangle-y sort-rectangle)
                    (ntop.ui:rectangle-width sort-rectangle)
                    (ntop.ui:rectangle-height sort-rectangle))
              :to-equal
              (list (ntop.ui:rectangle-x (second sort-layout))
                    (ntop.ui:rectangle-y (second sort-layout))
                    (ntop.ui:rectangle-width (second sort-layout))
                    (ntop.ui:rectangle-height (second sort-layout))))
             (cl-weave:expect
              (list (ntop.ui:rectangle-x table-rectangle)
                    (ntop.ui:rectangle-y table-rectangle)
                    (ntop.ui:rectangle-width table-rectangle)
                    (ntop.ui:rectangle-height table-rectangle))
              :to-equal
              (list (ntop.ui:rectangle-x (first sort-layout))
                    (ntop.ui:rectangle-y (first sort-layout))
                    (ntop.ui:rectangle-width (first sort-layout))
                    (ntop.ui:rectangle-height (first sort-layout))))
             (cl-weave:expect
              (list (ntop.ui:rectangle-x search-rectangle)
                    (ntop.ui:rectangle-y search-rectangle)
                    (ntop.ui:rectangle-width search-rectangle)
                    (ntop.ui:rectangle-height search-rectangle))
              :to-equal
              (list (ntop.ui:rectangle-x (second search-layout))
                    (ntop.ui:rectangle-y (second search-layout))
                    (ntop.ui:rectangle-width (second search-layout))
                    (ntop.ui:rectangle-height (second search-layout))))
             (cl-weave:expect
              (cl-tty-kit:cell-char
               (cl-tty-kit:screen-cell
                screen
                (1- (ntop.ui:rectangle-x sort-rectangle))
                (ntop.ui:rectangle-y sort-rectangle)))
              :to-be #\│)
             (ntop.app.application::%process-search-transition-action
              search-state
              (ntop.app.domain:make-app-action :kind :search :value :submit)
              config)
             (cl-weave:expect
              (ntop.app.domain:app-state-active-dialog search-state)
              :to-be :process-sort)
             (cl-weave:expect
              (gethash :process-sort-background-p
                       (ntop.app.domain:app-state-widget-v-states search-state))
              :to-be nil))))))))
