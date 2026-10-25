(in-package #:ntop.disk.presentation)

(defun %disk-row-text (row key)
  (or (getf row key) "N/A"))

(defun %disk-row-capacity (row key)
  (let ((value (getf row key)))
    (if value
        (ntop.ui:format-disk-capacity value)
        "N/A")))

(defun %disk-row-percent (row key)
  (let ((value (getf row key)))
    (if value
        (ntop.ui:format-percent-value value)
        "N/A")))

(defun %disk-row-rate (row key)
  (let ((value (getf row key)))
    (if value
        (ntop.ui:format-disk-io-rate value)
        "N/A")))

(ntop.ui:define-table-columns *disk-table-columns*
                              (:disk "Disk(d)"
                                     :soft
                                     :max-percent
                                     1/5
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-text row :disk)))
                              (:mount "Mount(m)"
                                      :soft
                                      :max-percent
                                      1/5
                                      :sortable-p
                                      t
                                      :value-function
                                      (lambda (row)
                                        (%disk-row-text row :mount)))
                              (:used "Used(u)"
                                     :hard
                                     :width
                                     8
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-capacity row :used)))
                              (:free "Free(n)"
                                     :hard
                                     :width
                                     8
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-capacity row :free)))
                              (:total "Total(t)"
                                      :hard
                                      :width
                                      9
                                      :sortable-p
                                      t
                                      :value-function
                                      (lambda (row)
                                        (%disk-row-capacity row :total)))
                              (:used-percent "Used%(p)"
                                             :hard
                                             :width
                                             9
                                             :sortable-p
                                             t
                                             :value-function
                                             (lambda (row)
                                               (%disk-row-percent row
                                                                  :used-percent)))
                              (:read-rate "R/s(r)"
                                          :hard
                                          :width
                                          10
                                          :sortable-p
                                          t
                                          :value-function
                                          (lambda (row)
                                            (%disk-row-rate row :read-rate)))
                              (:write-rate "W/s(w)"
                                           :hard
                                           :width
                                           11
                                           :sortable-p
                                           t
                                           :value-function
                                           (lambda (row)
                                             (%disk-row-rate row :write-rate))))

(ntop.ui:define-table-columns *disk-table-columns-with-free-percent*
                              (:disk "Disk(d)"
                                     :soft
                                     :max-percent
                                     1/5
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-text row :disk)))
                              (:mount "Mount(m)"
                                      :soft
                                      :max-percent
                                      1/5
                                      :sortable-p
                                      t
                                      :value-function
                                      (lambda (row)
                                        (%disk-row-text row :mount)))
                              (:used "Used(u)"
                                     :hard
                                     :width
                                     8
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-capacity row :used)))
                              (:free "Free(n)"
                                     :hard
                                     :width
                                     8
                                     :sortable-p
                                     t
                                     :value-function
                                     (lambda (row)
                                       (%disk-row-capacity row :free)))
                              (:total "Total(t)"
                                      :hard
                                      :width
                                      9
                                      :sortable-p
                                      t
                                      :value-function
                                      (lambda (row)
                                        (%disk-row-capacity row :total)))
                              (:used-percent "Used%(p)"
                                             :hard
                                             :width
                                             9
                                             :sortable-p
                                             t
                                             :value-function
                                             (lambda (row)
                                               (%disk-row-percent row
                                                                  :used-percent)))
                              (:free-percent "Free%"
                                             :hard
                                             :width
                                             9
                                             :sortable-p
                                             t
                                             :value-function
                                             (lambda (row)
                                               (%disk-row-percent row
                                                                  :free-percent)))
                              (:read-rate "R/s(r)"
                                          :hard
                                          :width
                                          10
                                          :sortable-p
                                          t
                                          :value-function
                                          (lambda (row)
                                            (%disk-row-rate row :read-rate)))
                              (:write-rate "W/s(w)"
                                           :hard
                                           :width
                                           11
                                           :sortable-p
                                           t
                                           :value-function
                                           (lambda (row)
                                             (%disk-row-rate row :write-rate))))

(defun make-disk-table-config (&key (show-free-percent-p nil)
                                    (title " Disks ")
                                    (show-header-p t)
                                    empty-message
                                    (sort-column :disk)
                                    (sort-order :ascending)
                                    (focused-p t)
                                    (show-current-entry-when-unfocused-p nil))
  (ntop.ui:make-table-config :columns
                             (if show-free-percent-p
                                 *disk-table-columns-with-free-percent*
                                 *disk-table-columns*)
                             :title title
                             :show-header-p show-header-p
                             :empty-message empty-message
                             :sort-column sort-column
                             :sort-order sort-order
                             :focused-p focused-p
                             :show-current-entry-when-unfocused-p
                             show-current-entry-when-unfocused-p))

(ntop.ui:define-widget :disk
                       (target rectangle state config rows)
                       (ntop.ui:draw-table target
                                           rectangle
                                           state
                                           config
                                           (ntop.disk.domain:sort-disk-rows
                                            (coerce rows 'list)
                                            :sort-key
                                            (or
                                             (ntop.ui:table-config-sort-column
                                              config)
                                             :disk)
                                            :sort-order
                                            (or
                                             (ntop.ui:table-config-sort-order
                                              config)
                                             :ascending))))
