(in-package #:ntop.test)

(defstruct (%f8b-process-table-target (:constructor %f8b-make-process-table-target (cells roles)))
  cells
  roles)

(defmethod ntop.ui:draw-table-text ((target %f8b-process-table-target) x y text role)
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%f8b-process-table-target-cells target) 0))
                  (<= 0 column)
                  (< column (array-dimension (%f8b-process-table-target-cells target) 1)))
          do (setf (aref (%f8b-process-table-target-cells target) y column) character
                   (aref (%f8b-process-table-target-roles target) y column) role)))

(defun %f8b-process-table-target (width height)
  (%f8b-make-process-table-target (make-array (list height width) :initial-element #\Space)
                                  (make-array (list height width) :initial-element nil)))

(defun %f8b-process-table-lines (target)
  (loop for row below (array-dimension (%f8b-process-table-target-cells target) 0)
        collect (string-right-trim '(#\Space #\Return)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%f8b-process-table-target-cells target)
                                                            1)
                                          collect (aref (%f8b-process-table-target-cells target)
                                                        row
                                                        column))
                                    'string))))

(defun %f8b-process-table-row (pid name cpu memory-percent memory-bytes elapsed-seconds)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :name
                                        name
                                        :command
                                        (format nil "~A --demo" name)
                                        :cpu-percent
                                        cpu
                                        :memory-percent
                                        memory-percent
                                        :memory-bytes
                                        memory-bytes
                                        :virtual-memory-bytes
                                        (* memory-bytes 4)
                                        :read-bytes-per-second
                                        pid
                                        :write-bytes-per-second
                                        (* pid 2)
                                        :total-read-bytes
                                        (* pid 100)
                                        :total-write-bytes
                                        (* pid 200)
                                        :user-name
                                        "sample-user"
                                        :state
                                        "sleeping"
                                        :elapsed-seconds
                                        elapsed-seconds
                                        :priority
                                        20
                                        :gpu-usage-percent
                                        3d0
                                        :gpu-memory-bytes
                                        2048
                                        :gpu-memory-percent
                                        4d0))

(defparameter +f8b-rows+
  (loop for row-data in '((101 "worker" 12.5d0 10.0d0 1024 60d0) (102 "shell" 3.0d0 5.0d0 2048 3d0)
                          (103 "runner" 8.0d0 15.0d0 3072 42d0)
                          (104 "monitor" 2.0d0 7.0d0 4096 3600d0)
                          (105 "service" 1.0d0 2.0d0 5120 86400d0))
        collect (apply #'%f8b-process-table-row row-data)))

(defparameter +f8b-table-golden-path+
  (asdf:system-relative-pathname "ntop/test"
                                 "t/feature/process/goldens/presentation-table-96x4.txt"))

(defun %f8b-process-table-entry (name depth last-child-p has-children-p collapsed-p)
  (ntop.process.domain::%make-process-tree-entry (%f8b-process-table-row 111 name 1d0 1d0 1024 1d0)
                                                 depth
                                                 last-child-p
                                                 has-children-p
                                                 collapsed-p))

(defun %f8b-process-table-column (config &optional id)
  (let ((columns (ntop.process.presentation::process-table-visible-columns config)))
    (if id
        (find id columns :key (function ntop.ui:table-column-id))
        columns)))

(defun %f8b-display-rows (config rows)
  (ntop.process.presentation::%process-table-display-rows config rows))

(defun %f8b-config (&rest args)
  (apply #'ntop.process.presentation::make-process-table-config args))

(defun %f8b-sort-state (key order)
  (ntop.process.domain:make-process-sort-state :key key :order order))

(defun %f8b-draw-process-table (target state config rows)
  (let ((rect
         (ntop.ui:make-rectangle :x
                                 0
                                 :y
                                 0
                                 :width
                                 (array-dimension (%f8b-process-table-target-cells target) 1)
                                 :height
                                 (array-dimension (%f8b-process-table-target-cells target) 0))))
    (ntop.ui:draw-widget :process-table target rect state config rows)))

(cl-weave:describe "Process table presentation"
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
                                                   '(:cpu-percent))))
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
