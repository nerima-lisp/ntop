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
                     ((95 ((:count 0 7) (:name 8 9) (:cpu-percent 18 9) (:memory 28 8)
                            (:read-bytes-per-second 37 8) (:write-bytes-per-second 46 8)
                            (:total-read-bytes 55 8) (:total-write-bytes 64 8)
                            (:user 73 5) (:state 79 9) (:runtime-seconds 89 5)))
                      (96 ((:count 0 7) (:name 8 9) (:cpu-percent 18 9) (:memory 28 9)
                            (:read-bytes-per-second 38 8) (:write-bytes-per-second 47 8)
                            (:total-read-bytes 56 8) (:total-write-bytes 65 8)
                            (:user 74 5) (:state 80 9) (:runtime-seconds 90 5)))
                      (104 ((:count 0 7) (:name 8 8) (:cpu-percent 17 8) (:memory 26 8)
                             (:read-bytes-per-second 35 8) (:write-bytes-per-second 44 8)
                             (:total-read-bytes 53 8) (:total-write-bytes 62 8)
                             (:user 71 6) (:state 78 9) (:runtime-seconds 88 5)
                             (:priority 94 9)))
                      (105 ((:count 0 7) (:name 8 9) (:cpu-percent 18 8) (:memory 27 8)
                             (:read-bytes-per-second 36 8) (:write-bytes-per-second 45 8)
                             (:total-read-bytes 54 8) (:total-write-bytes 63 8)
                             (:user 72 6) (:state 79 9) (:runtime-seconds 89 5)
                             (:priority 95 9))))
                     "matches BTM 0.14.9 column.rs:161-242 and draw.rs:217-229 at ~A cells"
                     (width expected-columns)
                     (let* ((config (%f8b-config))
                            (display-rows (%f8b-display-rows config +f8b-rows+))
                            (layout (ntop.ui:compute-table-layout
                                     (%f8b-process-table-column config)
                                     width
                                     :rows
                                     display-rows))
                            (actual (mapcar
                                     (lambda (column)
                                       (list (ntop.ui:table-column-layout-id column)
                                             (ntop.ui:table-column-layout-x column)
                                             (ntop.ui:table-column-layout-width column)))
                                     layout)))
                       (cl-weave:expect actual :to-equal expected-columns)))
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
                   (cl-weave:it
                    "uses BTM 0.14.9 ellipsis for allocated process cells"
                    (cl-weave:expect (ntop.ui::table-cell-within-width "sample-user" 5)
                                     :to-equal
                                     "samp…")
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
                                       "1h 0…")))
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
