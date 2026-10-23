(in-package #:ntop.test)

(cl-weave:describe "Process table columns"
                   (cl-weave:it "expands the widget and process table column macros"
                                (let ((column-expansion
                                       (macroexpand-1
                                        '(ntop.process.presentation::define-process-table-columns
                                          test-columns
                                          (:cpu-percent "CPU%" :soft :cpu-percent)))))
                                  (cl-weave:expect
                                   (macroexpand-1
                                    '(ntop.ui:define-widget :process-table
                                                            (target rectangle state config rows)
                                                            :rendered))
                                   :to-equal
                                   '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key
                                                                     (eql :process-table)) target
                                                                    rectangle
                                                                    state
                                                                    config
                                                                    rows)
                                      (declare (ignore ntop.ui::widget-key))
                                      :rendered))
                                  (cl-weave:expect (first column-expansion)
                                                   :to-be
                                                   'ntop.ui:define-table-columns)
                                  (cl-weave:expect
                                   (not (null (member :value-function (third column-expansion))))
                                   :to-be
                                   t)))
                   (cl-weave:it "declares the default btm columns, width bounds, and CPU sort"
                                (let* ((config (%f8b-config :column-ids nil))
                                       (columns (%f8b-process-table-column config)))
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-table-config-column-ids
                                    config)
                                   :to-equal
                                   '(:pid-or-count :name-or-command
                                                   :cpu
                                                   :memory
                                                   :read-per-second
                                                   :write-per-second
                                                   :total-read
                                                   :total-write
                                                   :user
                                                   :state
                                                   :time
                                                   :priority))
                                  (cl-weave:expect (mapcar #'ntop.ui:table-column-id columns)
                                                   :to-equal
                                                   '(:count :name
                                                            :cpu-percent
                                                            :memory
                                                            :read-bytes-per-second
                                                            :write-bytes-per-second
                                                            :total-read-bytes
                                                            :total-write-bytes
                                                            :user
                                                            :state
                                                            :runtime-seconds
                                                            :priority))
                                  (cl-weave:expect
                                   (ntop.process.domain:process-sort-state-key
                                    (ntop.process.presentation::process-table-config-sort-state
                                     config))
                                   :to-be
                                   :cpu-percent)
                                  (cl-weave:expect
                                   (ntop.process.domain:process-sort-state-order
                                    (ntop.process.presentation::process-table-config-sort-state
                                     config))
                                   :to-be
                                   :descending)
                                  (cl-weave:expect
                                   (mapcar
                                    (lambda (id)
                                      (ntop.ui:table-column-width
                                       (%f8b-process-table-column config id)))
                                    '(:read-bytes-per-second :write-bytes-per-second
                                                             :total-read-bytes
                                                             :total-write-bytes
                                                             :state))
                                   :to-equal
                                   '(8 8 8 8 9))
                                  (cl-weave:expect
                                   (ntop.ui:table-column-max-percent
                                    (%f8b-process-table-column config :name))
                                   :to-be
                                   3/10)
                                  (cl-weave:expect
                                   (ntop.ui:table-column-max-percent
                                    (%f8b-process-table-column config :user))
                                   :to-be
                                   1/20)))
                   (cl-weave:it-each ((t nil :count 12 t) (nil nil :pid 12 t) (t t :count 10 nil))
                                     "distinguishes default grouping from Tab grouping"
                                     (grouped-p grouped-by-tab-p
                                                first-column
                                                column-count
                                                user-state-visible-p)
                                     (let* ((config
                                             (%f8b-config :grouped-p
                                                          grouped-p
                                                          :grouped-by-tab-p
                                                          grouped-by-tab-p))
                                            (columns (%f8b-process-table-column config))
                                            (ids (mapcar #'ntop.ui:table-column-id columns)))
                                       (cl-weave:expect (first ids) :to-be first-column)
                                       (cl-weave:expect (length columns) :to-be column-count)
                                       (cl-weave:expect
                                        (not (null (and (member :user ids) (member :state ids))))
                                        :to-be
                                        user-state-visible-p)))
                   (cl-weave:it
                    "exposes current sortable choices from the configured visible columns"
                    (let* ((config (%f8b-config))
                           (columns (%f8b-process-table-column config))
                           (options (ntop.process.presentation::process-table-sort-options config)))
                      (cl-weave:expect options
                                       :to-equal
                                       (mapcar
                                        (lambda (column)
                                          (cons (ntop.ui:table-column-id column)
                                                (ntop.ui:table-column-header column)))
                                        columns))))
                   (cl-weave:it "switches Name to Command when configured"
                                (let* ((config
                                        (%f8b-config :show-command-p
                                                     t
                                                     :column-ids
                                                     '(:name-or-command)))
                                       (column (first (%f8b-process-table-column config)))
                                       (display-row (first (%f8b-display-rows config +f8b-rows+)))
                                       (value (ntop.ui::table-column-cell-text column display-row)))
                                  (cl-weave:expect (ntop.ui:table-column-id column) :to-be :command)
                                  (cl-weave:expect (ntop.ui:table-column-header column)
                                                   :to-equal
                                                   "Command(n)")
                                  (cl-weave:expect value :to-equal "worker --demo")))
                   (cl-weave:it-each ((:percent "Mem%(m)" "GMem%") (:bytes "Mem(m)" "GMem"))
                                     "renders process and GPU memory in ~A mode"
                                     (memory-mode memory-header gpu-header)
                                     (let* ((config
                                             (%f8b-config :memory-mode
                                                          memory-mode
                                                          :gpu-available-p
                                                          t
                                                          :column-ids
                                                          '(:memory :gpu-memory :gpu-util)))
                                            (columns (%f8b-process-table-column config))
                                            (display-row
                                             (first
                                              (%f8b-display-rows config (list (first +f8b-rows+)))))
                                            (formatter
                                             (if (eq memory-mode :bytes)
                                                 #'ntop.ui:format-process-binary-bytes
                                                 #'ntop.ui:format-process-memory-percent)))
                                       (cl-weave:expect
                                        (mapcar #'ntop.ui:table-column-header columns)
                                        :to-equal
                                        (list memory-header gpu-header "GPU%"))
                                       (cl-weave:expect
                                        (ntop.ui::table-column-cell-text (first columns)
                                                                         display-row)
                                        :to-equal
                                        (funcall formatter
                                                 (if (eq memory-mode :bytes)
                                                     1024
                                                     10.0d0)))
                                       (cl-weave:expect
                                        (ntop.ui::table-column-cell-text (second columns)
                                                                         display-row)
                                        :to-equal
                                        (funcall formatter
                                                 (if (eq memory-mode :bytes)
                                                     2048
                                                     4.0d0)))
                                       (cl-weave:expect
                                        (ntop.ui::table-column-cell-text (third columns)
                                                                         display-row)
                                        :to-equal
                                        (ntop.ui:format-process-cpu-percent 3.0d0))))
                   (cl-weave:it "omits GPU columns when GPU data is unavailable"
                                (let ((columns
                                       (%f8b-process-table-column
                                        (%f8b-config :gpu-available-p
                                                     nil
                                                     :column-ids
                                                     '(:gpu-memory :gpu-util :cpu)))))
                                  (cl-weave:expect (mapcar #'ntop.ui:table-column-id columns)
                                                   :to-equal
                                                   '(:cpu-percent)))))
