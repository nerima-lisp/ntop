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
  '((:cpu-percent ntop.process.domain:process-row-cpu-percent ntop.ui:format-percent-value)
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
                        ntop.ui:format-percent-value)))

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
                               basic-p
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
  (basic-p nil :read-only t)
  (column-ids nil :read-only t))

(defun make-process-table-config (&key sort-state
                                       (grouped-p t)
                                       (grouped-by-tab-p nil)
                                       (show-command-p nil)
                                       memory-mode
                                       gpu-available-p
                                       (focused-p t)
                                       (show-current-entry-when-unfocused-p nil)
                                       show-scroll-position-p
                                       (basic-p nil)
                                       column-ids)
  (when sort-state
    (check-type sort-state ntop.process.domain:process-sort-state))
  (unless
      (and (member grouped-p (list t nil))
           (member grouped-by-tab-p (list t nil))
           (member show-command-p (list t nil))
           (member gpu-available-p (list t nil))
           (member focused-p (list t nil))
           (member show-current-entry-when-unfocused-p (list t nil))
           (member show-scroll-position-p (list t nil))
           (member basic-p (list t nil)))
    (error "Process table flags must be true or false."))
  (when (and grouped-by-tab-p (not grouped-p))
    (error "Tab grouping origin requires grouped rows."))
  (unless (member memory-mode (list nil :bytes :percent))
    (error "Unsupported process memory display mode: ~S." memory-mode))
  (unless (and (typep column-ids (quote sequence)) (not (stringp column-ids)))
    (error "Process column IDs must be a list or vector: ~S." column-ids))
  (let* ((sort-state*
          (or sort-state
              (ntop.process.domain:make-process-sort-state
               :memory-mode (or memory-mode :percent))))
         (effective-memory-mode
          (or memory-mode
              (ntop.process.domain:process-sort-state-memory-mode sort-state*)))
         (effective-column-ids
          (remove-duplicates
           (if column-ids
               (coerce column-ids (quote list))
               +default-process-table-column-ids+)
           :test
           (function eq)
           :from-end
           t)))
    (unless (member effective-memory-mode (list :bytes :percent))
      (error "Unsupported process memory display mode: ~S." effective-memory-mode))
    (unless
        (every
         (lambda (column-id)
           (member column-id +process-table-column-ids+ :test (function eq)))
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
                                basic-p
                                effective-column-ids)))
