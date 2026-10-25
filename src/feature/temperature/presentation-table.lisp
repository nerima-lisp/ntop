(in-package #:ntop.temperature.presentation)

(ntop.ui:define-table-columns *temperature-table-columns*
                              (:sensor "Sensor(s)"
                                       :soft
                                       :max-percent
                                       4/5
                                       :sortable-p
                                       t
                                       :value-function
                                       (lambda (row)
                                         (or (getf row :sensor) "")))
                              (:temperature "Temp(t)" :soft :sortable-p t))

(defun %temperature-table-columns (unit)
  (mapcar
   (lambda (column)
     (if (eq (ntop.ui:table-column-id column) :temperature)
         (ntop.ui:make-table-column :id
                                    :temperature
                                    :header
                                    (ntop.ui:table-column-header column)
                                    :width-kind
                                    (ntop.ui:table-column-width-kind column)
                                    :width
                                    (ntop.ui:table-column-width column)
                                    :max-percent
                                    (ntop.ui:table-column-max-percent column)
                                    :hidden-p
                                    (ntop.ui:table-column-hidden-p column)
                                    :sortable-p
                                    (ntop.ui:table-column-sortable-p column)
                                    :value-function
                                    (lambda (row)
                                      (let ((celsius
                                             (getf row :temperature-celsius)))
                                        (if celsius
                                            (ntop.ui:format-temperature celsius
                                                                        unit)
                                            "N/A"))))
         column))
   *temperature-table-columns*))

(defun make-temperature-table-config (&key (temperature-unit :celsius)
                                           (title " Temperatures ")
                                           (show-header-p t)
                                           empty-message
                                           (sort-column :sensor)
                                           (sort-order :ascending)
                                           (focused-p t)
                                           (show-current-entry-when-unfocused-p nil))
  (ntop.ui:make-table-config :columns
                             (%temperature-table-columns temperature-unit)
                             :title title
                             :show-header-p show-header-p
                             :empty-message empty-message
                             :sort-column sort-column
                             :sort-order sort-order
                             :focused-p focused-p
                             :show-current-entry-when-unfocused-p
                             show-current-entry-when-unfocused-p))

(defun %temperature-table-rows (sample)
  (when sample
    (loop for sensor-label in
            (ntop.temperature.domain:temperature-sensor-labels
             (ntop.temperature.domain:temperature-sample-sensors sample))
          collect
          (list :sensor (cdr sensor-label)
                :temperature-celsius
                (ntop.temperature.domain:temperature-sensor-temperature-celsius
                 (car sensor-label))))))

(defun %temperature-row-comparison (left right sort-column)
  (ecase sort-column
    (:sensor
     (let ((left-name (or (getf left :sensor) ""))
           (right-name (or (getf right :sensor) "")))
       (cond
         ((string= left-name right-name) 0)
         ((string< left-name right-name) -1)
         (t 1))))
    (:temperature
     (let ((left-temperature (getf left :temperature-celsius))
           (right-temperature (getf right :temperature-celsius)))
       (cond
         ((and (null left-temperature) (null right-temperature)) 0)
         ((null left-temperature) -1)
         ((null right-temperature) 1)
         ((= left-temperature right-temperature) 0)
         ((< left-temperature right-temperature) -1)
         (t 1))))))

(defun %temperature-row-less-p (left right sort-column sort-order)
  (let ((comparison (%temperature-row-comparison left right sort-column)))
    (if (eq sort-order :ascending)
        (minusp comparison)
        (if (eq sort-order :descending)
            (plusp comparison)
            (error "Unsupported temperature table sort order ~S." sort-order)))))

(defun %sort-temperature-table-rows (rows sort-column sort-order)
  (stable-sort rows
               (lambda (left right)
                 (%temperature-row-less-p left right sort-column sort-order))))

(ntop.ui:define-widget :temperature
                       (target rectangle state config sample)
                       (ntop.ui:draw-table target
                                           rectangle
                                           state
                                           config
                                           (%sort-temperature-table-rows
                                            (%temperature-table-rows sample)
                                            (or
                                             (ntop.ui:table-config-sort-column
                                              config)
                                             :sensor)
                                            (or
                                             (ntop.ui:table-config-sort-order
                                              config)
                                             :ascending))))
