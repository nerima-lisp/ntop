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
                             :to-equal :not-present)))))))
