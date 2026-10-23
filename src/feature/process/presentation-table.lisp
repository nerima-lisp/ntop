(in-package #:ntop.process.presentation)

(defparameter +default-process-table-column-ids+
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

(defparameter +process-table-column-ids+
  '(:pid-or-count :name-or-command
                  :cpu
                  :memory
                  :virtual-memory
                  :read-per-second
                  :write-per-second
                  :total-read
                  :total-write
                  :user
                  :state
                  :time
                  :priority
                  :nice
                  :gpu-memory
                  :gpu-util))

(defparameter +process-table-static-column-specs+
  '((:cpu :cpu-percent "CPU%(c)") (:virtual-memory :virtual-memory-bytes "Virt")
    (:read-per-second :read-bytes-per-second "R/s")
    (:write-per-second :write-bytes-per-second "W/s")
    (:total-read :total-read-bytes "T.Read")
    (:total-write :total-write-bytes "T.Write")
    (:user :user "User")
    (:state :state "State")
    (:time :runtime-seconds "Time")
    (:priority :priority "Priority")
    (:nice :nice "Nice")))

(defparameter +process-table-static-value-specs+
  '((:cpu-percent ntop.process.domain:process-row-cpu-percent ntop.ui:format-process-cpu-percent)
    (:virtual-memory-bytes ntop.process.domain:process-row-virtual-memory-bytes
                           ntop.ui:format-process-binary-bytes)
    (:read-bytes-per-second ntop.process.domain:process-row-read-bytes-per-second
                            ntop.ui:format-process-io-rate)
    (:write-bytes-per-second ntop.process.domain:process-row-write-bytes-per-second
                             ntop.ui:format-process-io-rate)
    (:total-read-bytes ntop.process.domain:process-row-total-read-bytes
                       ntop.ui:format-process-decimal-bytes)
    (:total-write-bytes ntop.process.domain:process-row-total-write-bytes
                        ntop.ui:format-process-decimal-bytes)
    (:user ntop.process.domain:process-row-user-name)
    (:state ntop.process.domain:process-row-state)
    (:priority ntop.process.domain:process-row-priority)
    (:nice ntop.process.domain:process-row-nice)
    (:gpu-usage-percent ntop.process.domain:process-row-gpu-usage-percent
                        ntop.ui:format-process-cpu-percent)))

(defstruct
    (process-table-config
     (:constructor %make-process-table-config
                   (sort-state grouped-p
                               grouped-by-tab-p
                               show-command-p
                               memory-mode
                               gpu-available-p
                               focused-p
                               show-current-entry-when-unfocused-p
                               show-scroll-position-p
                               column-ids))
     (:copier nil))
  (sort-state nil :read-only t)
  (grouped-p nil :read-only t)
  (grouped-by-tab-p nil :read-only t)
  (show-command-p nil :read-only t)
  (memory-mode :percent :read-only t)
  (gpu-available-p nil :read-only t)
  (focused-p t :read-only t)
  (show-current-entry-when-unfocused-p nil :read-only t)
  (show-scroll-position-p nil :read-only t)
  (column-ids nil :read-only t))

(defstruct
    (%process-table-display-row
     (:constructor %make-process-table-display-row (row prefix config time-override))
     (:copier nil))
  row
  prefix
  config
  time-override)

(defmacro define-process-table-columns (name &body definitions)
  `(ntop.ui:define-table-columns ,name
                                 ,@(mapcar
                                    (lambda (definition)
                                      (destructuring-bind (id header
                                                              width-kind
                                                              value-key
                                                              &rest
                                                              options) definition
                                        `(,id ,header
                                              ,width-kind
                                              ,@options
                                              :value-function
                                              (lambda (row)
                                                (%process-table-column-value row ,value-key)))))
                                    definitions)))

(defun make-process-table-config (&key sort-state
                                       (grouped-p t)
                                       (grouped-by-tab-p nil)
                                       (show-command-p nil)
                                       memory-mode
                                       gpu-available-p
                                       (focused-p t)
                                       (show-current-entry-when-unfocused-p nil)
                                       show-scroll-position-p
                                       column-ids)
  (when sort-state
    (check-type sort-state ntop.process.domain:process-sort-state))
  (unless
      (and (member grouped-p '(t nil))
           (member grouped-by-tab-p '(t nil))
           (member show-command-p '(t nil))
           (member gpu-available-p '(t nil))
           (member focused-p '(t nil))
           (member show-current-entry-when-unfocused-p '(t nil))
           (member show-scroll-position-p '(t nil)))
    (error "Process table flags must be true or false."))
  (when (and grouped-by-tab-p (not grouped-p))
    (error "Tab grouping origin requires grouped rows."))
  (unless (member memory-mode '(nil :bytes :percent))
    (error "Unsupported process memory display mode: ~S." memory-mode))
  (unless (and (typep column-ids 'sequence) (not (stringp column-ids)))
    (error "Process column IDs must be a list or vector: ~S." column-ids))
  (let* ((sort-state*
          (or sort-state
              (ntop.process.domain:make-process-sort-state :memory-mode (or memory-mode :percent))))
         (effective-memory-mode
          (or memory-mode (ntop.process.domain:process-sort-state-memory-mode sort-state*)))
         (effective-column-ids
          (remove-duplicates
           (if column-ids
               (coerce column-ids 'list)
               +default-process-table-column-ids+)
           :test
           (function eq)
           :from-end
           t)))
    (unless (member effective-memory-mode '(:bytes :percent))
      (error "Unsupported process memory display mode: ~S." effective-memory-mode))
    (unless
        (every
         (lambda (column-id)
           (member column-id +process-table-column-ids+ :test #'eq))
         effective-column-ids)
      (error "Unsupported process table column IDs: ~S." effective-column-ids))
    (%make-process-table-config sort-state*
                                grouped-p
                                grouped-by-tab-p
                                show-command-p
                                effective-memory-mode
                                gpu-available-p
                                focused-p
                                show-current-entry-when-unfocused-p
                                show-scroll-position-p
                                effective-column-ids)))

(defun %process-table-row (display-row)
  (%process-table-display-row-row display-row))

(defun %process-table-value (display-row reader &optional formatter)
  (let ((value (funcall reader (%process-table-row display-row))))
    (if value
        (if formatter
            (funcall formatter value)
            (princ-to-string value))
        "N/A")))

(defun %process-table-name-value (display-row reader)
  (let ((value (funcall reader (%process-table-row display-row))))
    (concatenate 'string (%process-table-display-row-prefix display-row) (or value "N/A"))))

(defun %process-table-memory-value (display-row bytes-reader percent-reader)
  (let ((config (%process-table-display-row-config display-row)))
    (if (eq (process-table-config-memory-mode config) :bytes)
        (%process-table-value display-row bytes-reader #'ntop.ui:format-process-binary-bytes)
        (%process-table-value display-row percent-reader #'ntop.ui:format-process-memory-percent))))

(defun %process-table-column-value (display-row column-id)
  (case column-id
    ((:pid :count)
     (%process-table-value display-row
                           (if (eq column-id :pid)
                               (function ntop.process.domain:process-row-pid)
                               (function ntop.process.domain:process-row-count))))
    ((:name :command)
     (%process-table-name-value display-row
                                (if (eq column-id :name)
                                    (function ntop.process.domain:process-row-name)
                                    (function ntop.process.domain:process-row-command))))
    (:memory
     (%process-table-memory-value display-row
                                  (function ntop.process.domain:process-row-memory-bytes)
                                  (function ntop.process.domain:process-row-memory-percent)))
    (:gpu-memory
     (%process-table-memory-value display-row
                                  (function ntop.process.domain:process-row-gpu-memory-bytes)
                                  (function ntop.process.domain:process-row-gpu-memory-percent)))
    (:runtime-seconds
     (or (%process-table-display-row-time-override display-row)
         (%process-table-value display-row
                               (function ntop.process.domain:process-row-elapsed-seconds)
                               (lambda (seconds)
                                 (ntop.ui:format-process-time
                                  (ntop.shared.domain:make-quantity seconds :second))))))
    (otherwise
     (let ((spec (assoc column-id +process-table-static-value-specs+ :test #'eq)))
       (if spec
           (%process-table-value display-row
                                 (symbol-function (second spec))
                                 (when (third spec)
                                   (symbol-function (third spec))))
           (error "Unsupported process table column: ~S." column-id))))))

(define-process-table-columns *process-table-column-prototypes*
                              (:pid "PID(p)" :soft :pid :sortable-p t)
                              (:count "Count" :soft :count :sortable-p t)
                              (:name "Name(n)" :soft :name :max-percent 3/10 :sortable-p t)
                              (:command "Command(n)" :soft :command :max-percent 3/10 :sortable-p t)
                              (:cpu-percent "CPU%(c)" :soft :cpu-percent :sortable-p t)
                              (:memory "Mem%(m)" :soft :memory :sortable-p t)
                              (:virtual-memory-bytes "Virt"
                                                     :soft
                                                     :virtual-memory-bytes
                                                     :sortable-p
                                                     t)
                              (:read-bytes-per-second "R/s"
                                                      :hard
                                                      :read-bytes-per-second
                                                      :width
                                                      8
                                                      :sortable-p
                                                      t)
                              (:write-bytes-per-second "W/s"
                                                       :hard
                                                       :write-bytes-per-second
                                                       :width
                                                       8
                                                       :sortable-p
                                                       t)
                              (:total-read-bytes "T.Read"
                                                 :hard
                                                 :total-read-bytes
                                                 :width
                                                 8
                                                 :sortable-p
                                                 t)
                              (:total-write-bytes "T.Write"
                                                  :hard
                                                  :total-write-bytes
                                                  :width
                                                  8
                                                  :sortable-p
                                                  t)
                              (:user "User" :soft :user :max-percent 1/20 :sortable-p t)
                              (:state "State" :hard :state :width 9 :sortable-p t)
                              (:runtime-seconds "Time" :soft :runtime-seconds :sortable-p t)
                              (:priority "Priority" :soft :priority :sortable-p t)
                              (:nice "Nice" :soft :nice :sortable-p t)
                              (:gpu-memory "GMem%" :soft :gpu-memory :sortable-p t)
                              (:gpu-usage-percent "GPU%" :soft :gpu-usage-percent :sortable-p t))

(defun %process-table-column-spec (column-id config)
  (case column-id
    (:pid-or-count
     (if (process-table-config-grouped-p config)
         (values :count "Count")
         (values :pid "PID(p)")))
    (:name-or-command
     (if (process-table-config-show-command-p config)
         (values :command "Command(n)")
         (values :name "Name(n)")))
    (:memory
     (values :memory
             (if (eq (process-table-config-memory-mode config) :bytes)
                 "Mem(m)"
                 "Mem%(m)")))
    (:gpu-memory
     (if (process-table-config-gpu-available-p config)
         (values :gpu-memory
                 (if (eq (process-table-config-memory-mode config) :bytes)
                     "GMem"
                     "GMem%"))
         (values nil nil)))
    (:gpu-util
     (if (process-table-config-gpu-available-p config)
         (values :gpu-usage-percent "GPU%")
         (values nil nil)))
    (otherwise
     (let ((spec (assoc column-id +process-table-static-column-specs+ :test #'eq)))
       (values (second spec) (third spec))))))

(defun %copy-process-table-column (column-id header)
  (let ((prototype
         (find column-id
               *process-table-column-prototypes*
               :key
               #'ntop.ui:table-column-id
               :test
               #'eq)))
    (ntop.ui:make-table-column :id
                               column-id
                               :header
                               header
                               :width-kind
                               (ntop.ui:table-column-width-kind prototype)
                               :width
                               (ntop.ui:table-column-width prototype)
                               :max-percent
                               (ntop.ui:table-column-max-percent prototype)
                               :sortable-p
                               (ntop.ui:table-column-sortable-p prototype)
                               :value-function
                               (ntop.ui:table-column-value-function prototype))))

(defun process-table-visible-columns (config)
  (check-type config process-table-config)
  (loop for selector in (process-table-config-column-ids config)
        unless (and (process-table-config-grouped-by-tab-p config)
                    (or (eq selector :user) (eq selector :state)))
          append (multiple-value-bind (column-id header)
                     (%process-table-column-spec selector config)
                   (when column-id
                     (list (%copy-process-table-column column-id header))))))

(defun process-table-sort-options (config)
  (loop for column in (process-table-visible-columns config)
        when (ntop.ui:table-column-sortable-p column)
          collect (cons (ntop.ui:table-column-id column) (ntop.ui:table-column-header column))))

(defun %process-tree-entry-prefixes (entries)
  (let ((ancestor-last-child (make-array 0 :adjustable t :fill-pointer 0)))
    (loop for entry in entries
          for depth = (ntop.process.domain:process-tree-entry-depth entry)
          collect (progn
                    (when (> depth (fill-pointer ancestor-last-child))
                      (error "Process tree entries skip a parent depth."))
                    (let ((prefix
                           (with-output-to-string (output)
                             (loop for ancestor-depth from 1 below depth
                                   do (write-string
                                       (if (aref ancestor-last-child ancestor-depth)
                                           "   "
                                           "│  ")
                                       output))
                             (when (plusp depth)
                               (write-string
                                (if (ntop.process.domain:process-tree-entry-last-child-p entry)
                                    "└"
                                    "├")
                                output)
                               (write-string "─ " output))
                             (when (ntop.process.domain:process-tree-entry-collapsed-p entry)
                               (write-string "+ " output)))))
                      (when (> (fill-pointer ancestor-last-child) depth)
                        (setf (fill-pointer ancestor-last-child) depth))
                      (vector-push-extend
                       (ntop.process.domain:process-tree-entry-last-child-p entry)
                       ancestor-last-child)
                      prefix)))))

(defun %process-table-display-rows (config rows)
  (let ((row-list (coerce rows 'list)))
    (cond
      ((every #'ntop.process.domain:process-tree-entry-p row-list)
       (loop for entry in row-list
             for prefix in (%process-tree-entry-prefixes row-list)
             collect (%make-process-table-display-row
                      (ntop.process.domain:process-tree-entry-row entry)
                      prefix
                      config
                      nil)))
      ((every #'ntop.process.domain:process-row-p row-list)
       (mapcar
        (lambda (row)
          (%make-process-table-display-row row "" config nil))
        row-list))
      (t (error "Process table rows must be process rows or tree entries.")))))

(defun %truncate-process-time (text width)
  (if (<= (ntop.ui:measure-table-text text) width)
      text
      (concatenate 'string (ntop.ui:truncate-table-text text (max 0 (1- width))) "…")))

(defun %process-table-rows-with-time-width (display-rows layout)
  (let ((time-layout
         (find :runtime-seconds layout :key #'ntop.ui:table-column-layout-id :test #'eq)))
    (if time-layout
        (mapcar
         (lambda (display-row)
           (%make-process-table-display-row (%process-table-display-row-row display-row)
                                            (%process-table-display-row-prefix display-row)
                                            (%process-table-display-row-config display-row)
                                            (%truncate-process-time
                                             (%process-table-column-value display-row
                                                                          :runtime-seconds)
                                             (ntop.ui:table-column-layout-width time-layout))))
         display-rows)
        display-rows)))

(defun %process-table-hard-columns (columns layout)
  (let ((columns-by-id (make-hash-table :test #'eq)))
    (dolist (column columns)
      (setf (gethash (ntop.ui:table-column-id column) columns-by-id) column))
    (loop for column-layout in layout
          for column = (gethash (ntop.ui:table-column-layout-id column-layout) columns-by-id)
          collect (ntop.ui:make-table-column :id
                                             (ntop.ui:table-column-layout-id column-layout)
                                             :header
                                             (ntop.ui:table-column-header column)
                                             :width-kind
                                             :hard
                                             :width
                                             (ntop.ui:table-column-layout-width column-layout)
                                             :sortable-p
                                             (ntop.ui:table-column-sortable-p column)
                                             :value-function
                                             (ntop.ui:table-column-value-function column)))))

(defun %process-table-ui-config (config columns)
  (let ((sort-state (process-table-config-sort-state config)))
    (ntop.ui:make-table-config :columns
                               columns
                               :title
                               " Processes "
                               :show-scroll-position-p
                               (process-table-config-show-scroll-position-p config)
                               :sort-column
                               (ntop.process.domain:process-sort-state-key sort-state)
                               :sort-order
                               (ntop.process.domain:process-sort-state-order sort-state)
                               :focused-p
                               (process-table-config-focused-p config)
                               :show-current-entry-when-unfocused-p
                               (process-table-config-show-current-entry-when-unfocused-p config))))

(ntop.ui:define-widget :process-table
                       (target rectangle state config rows)
                       (check-type config process-table-config)
                       (let* ((columns (process-table-visible-columns config))
                              (display-rows (%process-table-display-rows config rows))
                              (layout
                               (ntop.ui:compute-table-layout columns
                                                             (ntop.ui:rectangle-width rectangle)
                                                             :rows
                                                             display-rows))
                              (render-columns (%process-table-hard-columns columns layout))
                              (render-rows
                               (%process-table-rows-with-time-width display-rows layout))
                              (table-config (%process-table-ui-config config render-columns)))
                         (ntop.ui:draw-table target rectangle state table-config render-rows)))
