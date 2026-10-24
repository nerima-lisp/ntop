(in-package #:ntop.test)

(cl-weave:describe "application screen config modes"
                   (cl-weave:it
                    "uses a basic config with the default application state"
                    (let* ((*wp12b-screen-widget-calls* nil)
                           (*wp12b-screen-recording-p* t)
                           (config (%wp12b-screen-config)))
                      (setf
                       (ntop.shared.application:application-config-basic-p
                        config)
                       t)
                      (%wp12b-screen-render 80 24 :config config)
                      (cl-weave:expect
                       (not
                       (null
                        (member :basic-table-arrows
                                *wp12b-screen-widget-calls*
                                :key (function first)
                                :test (function eq))))
                       :to-be
                       t)))
                   (cl-weave:it
                    "selects alternate GPU, temperature, battery and disk graph views"
                    (dolist (case '((:memory :gpu :gpu :memory)
                                   (:temperature :temperature-graph
                                                 :temperature-graph :temperature)
                                   (:temperature :temperature :temperature
                                                 :temperature-graph)
                                   (:temperature :battery :battery :temperature)
                                   (:disks :disk-io-graph :disk-io-graph :disks)))
                      (destructuring-bind (base view expected default-widget) case
                        (let* ((state (ntop.app.domain:make-app-state
                                       :selected-widget base))
                               (temperature-series
                                 (and (eq base :temperature)
                                      (list (ntop.ui:make-chart-series
                                             :name "CPU" :samples
                                             (%wp12b-screen-history '(40 50))))))
                               (*wp12b-screen-widget-calls* nil)
                               (*wp12b-screen-recording-p* t)
                               (reasons (and (eq view :gpu)
                                     (ntop.app.application::%make-application-unavailability-reasons
                                      :gpu :not-present))))
                          (setf (gethash base
                                         (ntop.app.domain:app-state-widget-v-states
                                          state))
                                view)
                          (%wp12b-screen-render
                           80 24 :state state
                           :series (and temperature-series
                                        (list (cons :temperature temperature-series)))
                           :unavailability-reasons reasons)
                          (cl-weave:expect
                           (not (null (member expected
                                              *wp12b-screen-widget-calls*
                                              :key #'first :test #'eq)))
                           :to-be t)
                          (cl-weave:expect
                           (member default-widget *wp12b-screen-widget-calls*
                                   :key #'first :test #'eq)
                           :to-be nil)
                          (when (member view '(:temperature :temperature-graph)
                                        :test #'eq)
                            (let ((input (third (find expected
                                                      *wp12b-screen-widget-calls*
                                                      :key #'first :test #'eq))))
                              (cl-weave:expect
                               (if (eq view :temperature-graph)
                                   (eq input temperature-series)
                                   (typep input
                                          'ntop.temperature.domain:temperature-sample))
                               :to-be t)))
                          (when (eq view :gpu)
                            (cl-weave:expect
                             (getf (third (find :gpu
                                                *wp12b-screen-widget-calls*
                                                :key #'first :test #'eq))
                                   :unavailability-reason)
                             :to-equal :not-present))))))
                   (cl-weave:it
                    "uses expanded layout from state and config and preserves table state"
                    (dolist (case (list (list t nil) (list nil t)))
                      (destructuring-bind (state-expanded config-expanded)
                          case
                        (let* ((state
                                (ntop.app.domain:make-app-state
                                 :selected-widget :cpu
                                 :expanded-p state-expanded))
                               (config (%wp12b-screen-config))
                               (stored
                                 (ntop.ui:make-table-state :selected-index 3))
                               (states
                                 (ntop.app.domain:app-state-widget-v-states state))
                               (*wp12b-screen-widget-calls* nil)
                               (*wp12b-screen-recording-p* t))
                          (setf
                           (ntop.shared.application:application-config-expanded-p
                            config)
                           config-expanded
                           (gethash :cpu states)
                           stored)
                          (cl-weave:expect
                           (ntop.app.presentation::%screen-layout-mode state config)
                           :to-be :expanded)
                          (%wp12b-screen-render 80 24 :state state :config config)
                          (let ((call
                                 (find :cpu *wp12b-screen-widget-calls*
                                       :key (function first)
                                       :test (function eq))))
                            (cl-weave:expect (fourth call) :to-be stored))))))
                   (cl-weave:it
                    "applies saved process, memory, GPU, command grouping, and tree modes"
                    (let* ((fixture (%wp12b-screen-fixture-snapshot))
                           (records
                            (ntop.process.domain:process-sample-processes
                             (ntop.app.domain:collection-snapshot-process-sample fixture)))
                           (worker
                            (ntop.process.domain:make-process-record :pid
                                                                     101
                                                                     :ppid
                                                                     1
                                                                     :name
                                                                     "worker"
                                                                     :command
                                                                     "alpha --serve"
                                                                     :ps-fallback-cpu-percent
                                                                     9d0))
                           (snapshot
                             (ntop.app.domain:make-collection-snapshot
                              :sampled-at 3d0
                              :gpu-sample
                              (ntop.gpu.domain:make-gpu-sample
                               :sampled-at 3d0
                               :devices
                               (list
                                (ntop.gpu.domain:make-gpu-device
                                 :name "GPU"
                                 :memory-total-bytes 500)
                                (ntop.gpu.domain:make-gpu-device :name "Unknown")))
                              :process-sample
                              (ntop.process.domain:make-process-sample
                               :sampled-at 3d0
                               :processes (append records (list worker)))))
                           (config
                            (ntop.shared.application:make-application-config
                             :process-default-memory-value-p
                             t
                             :process-default-grouped-p
                             t
                             :process-show-command-p
                             t
                             :process-sort-column
                             :pid
                             :process-sort-order
                             :ascending))
                           (state (ntop.app.domain:make-app-state :selected-widget :process))
                           (states (ntop.app.domain:app-state-widget-v-states state))
                           (table (ntop.ui:make-table-state :selected-index 0))
                           (sort-state
                            (ntop.process.domain:make-process-sort-state :key
                                                                         :pid
                                                                         :order
                                                                         :ascending
                                                                         :memory-mode
                                                                         :bytes)))
                      (setf (gethash :process states) table
                            (gethash :process-sort-state states) sort-state)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-process-sort-state config state)
                       :to-be
                       sort-state)
                      (multiple-value-bind (saved-state table-config)
                          (ntop.app.presentation::%screen-process-widget-config snapshot
                                                                                state
                                                                                config
                                                                                t
                                                                                sort-state)
                        (cl-weave:expect saved-state :to-be table)
                        (cl-weave:expect
                         (ntop.process.presentation:process-table-config-memory-mode table-config)
                         :to-be
                         :bytes)
                        (cl-weave:expect
                         (ntop.process.presentation:process-table-config-gpu-available-p
                          table-config)
                         :to-be
                         t))
                      (multiple-value-bind (pids name)
                          (ntop.app.presentation::resolve-selected-process-targets state
                                                                                  config
                                                                                  snapshot
                                                                                  nil)
                        (cl-weave:expect pids :to-equal (list 100 101))
                        (cl-weave:expect name :to-equal "alpha"))
                      (setf (ntop.shared.application:application-config-process-default-tree-p
                             config) t)
                      (multiple-value-bind (rows raw-rows)
                          (ntop.app.presentation::%screen-process-rows snapshot
                                                                       state
                                                                       config
                                                                       sort-state
                                                                       nil)
                        (declare (ignore raw-rows))
                        (cl-weave:expect (ntop.process.domain:process-tree-entry-p (first rows))
                                         :to-be
                                         t))
                      (multiple-value-bind (pids name)
                          (ntop.app.presentation::resolve-selected-process-targets state
                                                                                  config
                                                                                  snapshot
                                                                                  nil)
                        (cl-weave:expect (length pids) :to-be 1)
                        (cl-weave:expect (not (null name)) :to-be t))
                      (setf (ntop.shared.application:application-config-disable-gpu-p config) t)
                      (multiple-value-bind (saved-state table-config)
                          (ntop.app.presentation::%screen-process-widget-config snapshot
                                                                                state
                                                                                config
                                                                                t
                                                                                sort-state)
                        (declare (ignore saved-state))
                        (cl-weave:expect
                         (ntop.process.presentation:process-table-config-gpu-available-p
                          table-config)
                         :to-be
                         nil))
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-process-gpu-memory-total snapshot)
                       :to-be
                       500)
                      (cl-weave:expect (ntop.app.presentation::%screen-process-gpu-memory-total nil)
                                       :to-be
                                       nil)))
                   (cl-weave:it
                    "falls back from invalid saved views and skips empty rectangles"
                    (let* ((state (ntop.app.domain:make-app-state :selected-widget :memory))
                           (states (ntop.app.domain:app-state-widget-v-states state))
                           (target (cl-tty-kit:make-screen 2 2))
                           (config (ntop.shared.application:make-application-config)))
                      (dolist (case (list (list :memory :unknown-view :memory)
                                          (list :temperature :unknown-view :temperature)
                                          (list :disks :unknown-view :disks)))
                        (destructuring-bind (widget view expected) case
                          (setf (gethash widget states) view)
                          (cl-weave:expect
                           (ntop.app.presentation::%screen-effective-widget widget state)
                           :to-equal expected)))
                      (cl-weave:expect
                       (assoc :cpu-legend
                              (ntop.app.presentation::%screen-layout-placements
                               1 24 state :default))
                       :to-be nil)
                      (dolist (dimensions (list (list 0 2) (list 2 0)))
                        (cl-weave:expect
                         (ntop.app.presentation::%screen-draw-frame
                          target
                          (ntop.ui:make-rectangle :x 0 :y 0
                                                  :width (first dimensions)
                                                  :height (second dimensions))
                          config nil)
                         :to-be nil))
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-draw-widget
                        target :basic-table-arrows
                        (ntop.ui:make-rectangle :x 0 :y 0 :width 2 :height 2)
                        nil state config nil nil nil)
                       :to-equal nil))))
