(in-package #:ntop.test)

(cl-weave:describe "Process table rendering"
                   (cl-weave:it-each ((:ascending "CPU%(c)▲") (:descending "CPU%(c)▼"))
                                     "shows the ~A arrow for the active sort column"
                                     (order expected-header)
                                     (let* ((config
                                             (%f8b-config :sort-state
                                                          (%f8b-sort-state :cpu-percent order)
                                                          :column-ids
                                                          '(:cpu)))
                                            (target (%f8b-process-table-target 32 3)))
                                       (%f8b-draw-process-table target
                                                                (ntop.ui:make-table-state)
                                                                config
                                                                (list (first +f8b-rows+)))
                                       (cl-weave:expect
                                        (search expected-header
                                                (second (%f8b-process-table-lines target)))
                                        :to-be
                                        0)))
                   (cl-weave:it "routes header text through the table role and default header theme"
                                (let* ((target (%f8b-process-table-target 32 3))
                                       (config (%f8b-config :column-ids '(:cpu))))
                                  (%f8b-draw-process-table target
                                                           (ntop.ui:make-table-state)
                                                           config
                                                           (list (first +f8b-rows+)))
                                  (cl-weave:expect
                                   (aref (%f8b-process-table-target-roles target) 1 0)
                                   :to-be
                                   :header)
                                  (cl-weave:expect
                                   (ntop.ui:presentation-theme-style :default :table-header)
                                   :to-equal
                                   '(:bold (:fg 12)))))
                   (cl-weave:it-each
                    ((95 11 :runtime-seconds 6 8 5 8) (96 11 :runtime-seconds 7 8 5 8)
                     (104 11 :runtime-seconds 7 9 6 9)
                     (105 12 :priority 6 8 6 8))
                    "matches btm column sizing and first non-fitting drop at ~A cells"
                    (width expected-count expected-last first-width name-width user-width io-width)
                    (let* ((config (%f8b-config))
                           (display-rows (%f8b-display-rows config +f8b-rows+))
                           (layout
                            (ntop.ui:compute-table-layout (%f8b-process-table-column config)
                                                          width
                                                          :rows
                                                          display-rows))
                           (ids (mapcar #'ntop.ui:table-column-layout-id layout))
                           (widths (mapcar #'ntop.ui:table-column-layout-width layout)))
                      (cl-weave:expect (length layout) :to-be expected-count)
                      (cl-weave:expect (car (last ids)) :to-be expected-last)
                      (cl-weave:expect (first widths) :to-be first-width)
                      (cl-weave:expect (second widths) :to-be name-width)
                      (cl-weave:expect (ninth widths) :to-be user-width)
                      (cl-weave:expect (elt widths 10) :to-be 8)
                      (cl-weave:expect (subseq widths 4 8)
                                       :to-equal
                                       (make-list 4 :initial-element io-width))
                      (cl-weave:expect (tenth widths) :to-be 9)))
                   (cl-weave:it
                    "renders the supplied selection and scroll window without reordering rows"
                    (let* ((rows +f8b-rows+)
                           (config
                            (%f8b-config :grouped-p
                                         nil
                                         :show-scroll-position-p
                                         t
                                         :column-ids
                                         '(:pid-or-count)))
                           (state (ntop.ui:make-table-state :selected-index 4))
                           (target (%f8b-process-table-target 24 5)))
                      (%f8b-draw-process-table target state config rows)
                      (cl-weave:expect (ntop.ui:table-state-selected-index state) :to-be 4)
                      (cl-weave:expect (ntop.ui:table-state-scroll-offset state) :to-be 2)
                      (cl-weave:expect
                       (mapcar
                        (lambda (line)
                          (subseq line 0 3))
                        (subseq (%f8b-process-table-lines target) 2 5))
                       :to-equal
                       '("103" "104" "105"))
                      (cl-weave:expect
                       (not (null (search "5 of 5" (first (%f8b-process-table-lines target)))))
                       :to-be
                       t)
                      (cl-weave:expect (aref (%f8b-process-table-target-roles target) 4 0)
                                       :to-be
                                       :selected)))
                   (cl-weave:it
                    "renders tree branch connectors, continuation lines, and collapsed markers"
                    (let* ((rows
                            (list (%f8b-process-table-entry "worker" 0 t t nil)
                                  (%f8b-process-table-entry "shell" 1 nil t nil)
                                  (%f8b-process-table-entry "monitor" 2 t nil nil)
                                  (%f8b-process-table-entry "runner" 1 t nil nil)))
                           (config (%f8b-config :grouped-p nil :column-ids '(:name-or-command)))
                           (target (%f8b-process-table-target 64 6)))
                      (%f8b-draw-process-table target (ntop.ui:make-table-state) config rows)
                      (let ((lines (%f8b-process-table-lines target)))
                        (cl-weave:expect (search "├─ shell" (fourth lines)) :to-be 0)
                        (cl-weave:expect (search "│  └─ monitor" (fifth lines)) :to-be 0)
                        (cl-weave:expect (search "└─ runner" (sixth lines)) :to-be 0))
                      (let* ((collapsed-config
                              (%f8b-config :grouped-p nil :column-ids '(:name-or-command)))
                             (collapsed-target (%f8b-process-table-target 32 3)))
                        (%f8b-draw-process-table collapsed-target
                                                 (ntop.ui:make-table-state)
                                                 collapsed-config
                                                 (list (%f8b-process-table-entry "worker" 0 t t t)))
                        (cl-weave:expect
                         (search "+ worker" (third (%f8b-process-table-lines collapsed-target)))
                         :to-be
                         0))))
                   (cl-weave:it "adds a btm-style ellipsis when the Time value exceeds its column"
                                (let* ((config
                                        (%f8b-config :sort-state
                                                     (%f8b-sort-state :runtime-seconds :ascending)
                                                     :column-ids
                                                     '(:time)))
                                       (row
                                        (%f8b-process-table-row 104
                                                                "monitor"
                                                                2.0d0
                                                                7.0d0
                                                                4096
                                                                3600d0))
                                       (target (%f8b-process-table-target 6 3)))
                                  (%f8b-draw-process-table target
                                                           (ntop.ui:make-table-state)
                                                           config
                                                           (list row))
                                  (cl-weave:expect (third (%f8b-process-table-lines target))
                                                   :to-equal
                                                   "1h 0m…")))
                   (cl-weave:it "matches the deterministic synthetic screen golden"
                                (let* ((target (%f8b-process-table-target 96 4))
                                       (config (%f8b-config))
                                       (rows (subseq +f8b-rows+ 0 2))
                                       (golden-path +f8b-table-golden-path+))
                                  (%f8b-draw-process-table target
                                                           (ntop.ui:make-table-state)
                                                           config
                                                           rows)
                                  (cl-weave:expect
                                   (format nil "~{~A~%~}" (%f8b-process-table-lines target))
                                   :to-equal
                                   (uiop:read-file-string golden-path))
                                  (let* ((tiny-target (%f8b-process-table-target 1 1))
                                         (tiny-config (%f8b-config :column-ids '(:name-or-command)))
                                         (tiny-state (ntop.ui:make-table-state)))
                                    (%f8b-draw-process-table tiny-target tiny-state tiny-config nil)
                                    (cl-weave:expect (ntop.ui:table-state-selected-index tiny-state)
                                                     :to-be
                                                     0)
                                    (cl-weave:expect (ntop.ui:table-state-scroll-offset tiny-state)
                                                     :to-be
                                                     0)
                                    (cl-weave:expect
                                     (aref (%f8b-process-table-target-roles tiny-target) 0 0)
                                     :to-be
                                     :title))))
                   (cl-weave:it "rejects invalid process table inputs"
                                (dolist
                                    (flag
                                     '(:grouped-p :grouped-by-tab-p
                                                  :show-command-p
                                                  :gpu-available-p
                                                  :focused-p
                                                  :show-current-entry-when-unfocused-p
                                                  :show-scroll-position-p))
                                  (cl-weave:expect
                                   (lambda ()
                                     (%f8b-config flag :invalid))
                                   :to-throw
                                   'error))
                                (dolist
                                    (arguments
                                     '((:grouped-p nil :grouped-by-tab-p t) (:memory-mode :invalid)
                                       (:column-ids "pid")
                                       (:column-ids (:unsupported))))
                                  (cl-weave:expect
                                   (lambda ()
                                     (apply #'%f8b-config arguments))
                                   :to-throw
                                   'error))
                                (dolist
                                    (rows
                                     (list (list :invalid)
                                           (list (%f8b-process-table-entry "child" 2 t nil nil))))
                                  (cl-weave:expect
                                   (lambda ()
                                     (%f8b-display-rows (%f8b-config) rows))
                                   :to-throw
                                   'error))))
