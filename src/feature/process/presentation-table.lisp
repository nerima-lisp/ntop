(in-package #:ntop.process.presentation)

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
