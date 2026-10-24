(in-package #:ntop.test)

(cl-weave:describe "application domain state"
                   (cl-weave:it
                    "starts with the default process view and neutral modes"
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
                       :to-equal
                       '(:process nil nil nil 15 0 nil t))))
                   (cl-weave:it "stores input context and action values"
                                (let ((context
                                       (ntop.app.domain::make-input-context
                                        :selected-widget
                                        :cpu
                                        :active-dialog
                                        :help
                                        :basic-mode-p
                                        t
                                        :search-mode-p
                                        nil))
                                      (action
                                       (ntop.app.domain::make-app-action :kind
                                                                         :zoom
                                                                         :value
                                                                         30)))
                                  (cl-weave:expect
                                   (list
                                    (ntop.app.domain::input-context-selected-widget
                                     context)
                                    (ntop.app.domain::input-context-active-dialog
                                     context)
                                    (ntop.app.domain::input-context-basic-mode-p
                                     context)
                                    (ntop.app.domain::input-context-search-mode-p
                                     context)
                                    (ntop.app.domain::app-action-kind action)
                                    (ntop.app.domain::app-action-value action))
                                   :to-equal
                                   '(:cpu :help t nil :zoom 30))))
                   (cl-weave:it
                    "lists the action kinds shared with the event loop"
                    (cl-weave:expect ntop.app.domain::*app-action-kinds*
                                     :to-equal
                                     '(:navigation :select
                                                   :toggle-widget-v
                                                   :expand
                                                   :basic
                                                   :pause
                                                   :zoom
                                                   :history-scroll
                                                   :return-to-now
                                                   :help
                                                   :search
                                                   :dialog
                                                   :toggle-process-grouping
                                                   :sort-processes
                                                   :reverse-process-sort
                                                   :toggle-process-tree
                                                   :toggle-process-tree-branch
                                                   :set-process-display-mode
                                                   :process-search
                                                   :process-terminate
                                                   :quit)))
                   (cl-weave:it
                    "bundles each feature sample with its collection timestamp"
                    (let* ((sampled-at 45.0)
                           (cpu
                            (ntop.cpu.domain:make-cpu-sample :sampled-at
                                                             sampled-at))
                           (memory
                            (ntop.memory.domain:make-memory-sample :sampled-at
                                                                   sampled-at))
                           (gpu
                            (ntop.gpu.domain:make-gpu-sample :sampled-at
                                                             sampled-at))
                           (network
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     sampled-at))
                           (disk
                            (ntop.disk.domain:make-disk-sample :sampled-at
                                                               sampled-at))
                           (temperature
                            (ntop.temperature.domain:make-temperature-sample
                             :sampled-at
                             sampled-at))
                           (battery
                            (ntop.battery.domain:make-battery-sample :sampled-at
                                                                     sampled-at))
                           (process
                            (ntop.process.domain:make-process-sample :sampled-at
                                                                     sampled-at))
                           (snapshot
                            (ntop.app.domain:make-collection-snapshot
                             :sampled-at
                             sampled-at
                             :cpu-sample
                             cpu
                             :memory-sample
                             memory
                             :gpu-sample
                             gpu
                             :network-sample
                             network
                             :disk-sample
                             disk
                             :temperature-sample
                             temperature
                             :battery-sample
                             battery
                             :process-sample
                             process))
                           (samples
                            (list
                             (list
                              #'ntop.app.domain:collection-snapshot-cpu-sample
                              cpu
                              'ntop.cpu.domain:cpu-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-memory-sample
                              memory
                              'ntop.memory.domain:memory-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-gpu-sample
                              gpu
                              'ntop.gpu.domain:gpu-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-network-sample
                              network
                              'ntop.network.domain:network-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-disk-sample
                              disk
                              'ntop.disk.domain:disk-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-temperature-sample
                              temperature
                              'ntop.temperature.domain:temperature-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-battery-sample
                              battery
                              'ntop.battery.domain:battery-sample)
                             (list
                              #'ntop.app.domain:collection-snapshot-process-sample
                              process
                              'ntop.process.domain:process-sample))))
                      (cl-weave:expect
                       (list (ntop.app.domain:collection-snapshot-p snapshot)
                             (ntop.app.domain:collection-snapshot-sampled-at
                              snapshot))
                       :to-equal
                       (list t sampled-at))
                      (dolist (entry samples)
                        (cl-weave:expect (funcall (first entry) snapshot)
                                         :to-be
                                         (second entry))
                        (cl-weave:expect
                         (typep (funcall (first entry) snapshot) (third entry))
                         :to-be
                         t))))
                   (cl-weave:it
                    "keeps missing samples distinct from zero measurements"
                    (let* ((cpu
                            (ntop.cpu.domain:make-cpu-sample :sampled-at
                                                             0.0
                                                             :average-usage-percent
                                                             0))
                           (snapshot
                            (ntop.app.domain:make-collection-snapshot
                             :sampled-at
                             1.0
                             :cpu-sample
                             cpu)))
                      (cl-weave:expect
                       (list
                        (ntop.app.domain:collection-snapshot-cpu-sample
                         snapshot)
                        (ntop.cpu.domain:cpu-sample-average-usage-percent
                         (ntop.app.domain:collection-snapshot-cpu-sample
                          snapshot))
                        (ntop.shared.domain:measurement-available-p
                         (ntop.cpu.domain:cpu-sample-average-usage-percent
                          (ntop.app.domain:collection-snapshot-cpu-sample
                           snapshot)))
                        (ntop.app.domain:collection-snapshot-memory-sample
                         snapshot))
                       :to-equal
                       (list cpu 0 t nil))))
                   (cl-weave:it "does not expose snapshot slot writers"
                                (let ((accessors
                                       '(ntop.app.domain:collection-snapshot-sampled-at
                                         ntop.app.domain:collection-snapshot-cpu-sample
                                         ntop.app.domain:collection-snapshot-memory-sample
                                         ntop.app.domain:collection-snapshot-gpu-sample
                                         ntop.app.domain:collection-snapshot-network-sample
                                         ntop.app.domain:collection-snapshot-disk-sample
                                         ntop.app.domain:collection-snapshot-temperature-sample
                                         ntop.app.domain:collection-snapshot-battery-sample
                                         ntop.app.domain:collection-snapshot-process-sample)))
                                  (cl-weave:expect
                                   (every
                                    (lambda (accessor)
                                      (not (fboundp (list 'setf accessor))))
                                    accessors)
                                   :to-be
                                   t)))
                   (cl-weave:it "requires a snapshot collection timestamp"
                                (cl-weave:signals simple-error
                                                  (funcall
                                                   (symbol-function
                                                    (find-symbol
                                                     "MAKE-COLLECTION-SNAPSHOT"
                                                     "NTOP.APP.DOMAIN")))))
                   (cl-weave:it-each
                    ((ntop.app.domain:collection-snapshot-cpu-sample :cpu-sample
                                                                     ntop.cpu.domain:make-cpu-sample
                                                                     nil
                                                                     nil)
                     (ntop.app.domain:collection-snapshot-memory-sample
                      :memory-sample
                      ntop.memory.domain:make-memory-sample
                      nil
                      nil)
                     (ntop.app.domain:collection-snapshot-gpu-sample :gpu-sample
                                                                     ntop.gpu.domain:make-gpu-sample
                                                                     nil
                                                                     nil)
                     (ntop.app.domain:collection-snapshot-network-sample
                      :network-sample
                      ntop.network.domain:make-network-sample
                      nil
                      nil)
                     (ntop.app.domain:collection-snapshot-disk-sample
                      :disk-sample
                      ntop.disk.domain:make-disk-sample
                      nil
                      nil)
                     (ntop.app.domain:collection-snapshot-temperature-sample
                      :temperature-sample
                      ntop.temperature.domain:make-temperature-sample
                      nil
                      nil)
                     (ntop.app.domain:collection-snapshot-battery-sample
                      :battery-sample
                      ntop.battery.domain:make-battery-sample
                      nil
                      nil)
                     (ntop.app.domain:collection-snapshot-process-sample
                      :process-sample
                      ntop.process.domain:make-process-sample
                      nil
                      nil))
                    "validates optional collection sample slot ~S"
                    (accessor initarg
                              sample-constructor
                              default-value
                              nil-value)
                    (let ((sample
                           (funcall (symbol-function sample-constructor)
                                    :sampled-at
                                    0.0)))
                      (cl-weave:expect
                       (constructor-slot-outcomes
                        'ntop.app.domain:make-collection-snapshot
                        accessor
                        '(:sampled-at 0.0)
                        initarg
                        nil-value
                        sample
                        :invalid)
                       :to-equal
                       (list default-value nil-value sample :type-error)))))
