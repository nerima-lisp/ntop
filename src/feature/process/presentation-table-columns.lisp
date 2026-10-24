(in-package #:ntop.process.presentation)

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
        (%process-table-value display-row percent-reader #'ntop.ui:format-percent-value))))

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
