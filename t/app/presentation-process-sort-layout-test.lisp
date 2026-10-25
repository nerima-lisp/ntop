(in-package #:ntop.test)

(cl-weave:describe "process sort menu placement"
                   (cl-weave:it
                    "matches btm left sort layout"
                    ;; btm 0.14.9: src/canvas/widgets/process_table.rs:20, 45-55.
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
                        (declare (ignore screen))
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
                           t))))))
