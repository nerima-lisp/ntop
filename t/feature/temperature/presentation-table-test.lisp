(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load
   (merge-pathnames
    "helpers-presentation.lisp"
    (uiop:pathname-directory-pathname
     (asdf:component-pathname
      (asdf:find-component
       "ntop/test"
       "t/feature/temperature/presentation-table-test"))))))

(cl-weave:describe "Temperature table presentation"
                   (cl-weave:it
                    "defines the btm headers and sortable soft columns"
                    (let ((columns
                           (ntop.ui:table-config-columns
                            (ntop.temperature.presentation:make-temperature-table-config))))
                      (cl-weave:expect
                       (list (mapcar (function ntop.ui:table-column-id) columns)
                             (mapcar (function ntop.ui:table-column-header) columns)
                             (mapcar (function ntop.ui:table-column-width-kind)
                                     columns)
                             (mapcar (function ntop.ui:table-column-sortable-p)
                                     columns)
                             (ntop.ui:table-column-max-percent (first columns))
                             (ntop.ui:table-column-max-percent (second columns)))
                       :to-equal
                       (list (list :sensor :temperature)
                             (list "Sensor(s)" "Temp(t)")
                             (list :soft :soft)
                             (list t t)
                             4/5
                             nil))))
                   (cl-weave:it
                    "defaults to Sensor ascending"
                    (let ((config (ntop.temperature.presentation:make-temperature-table-config)))
                     (cl-weave:expect (ntop.ui:table-config-sort-column config) :to-be :sensor)
                     (cl-weave:expect (ntop.ui:table-config-sort-order config) :to-be :ascending)))
                   (cl-weave:it
                    "uses the shared temperature unit and ceiling formatter"
                    (cl-weave:expect
                                     (list (%temperature-column-text :celsius 37.2)
                                           (%temperature-column-text :fahrenheit 20)
                                           (%temperature-column-text :kelvin 0)
                                           (%temperature-column-text :celsius nil))
                                     :to-equal
                                     (list "38°C" "68°F" "274K" "N/A")))
                   (cl-weave:it "sorts the rendered rows by Sensor ASCII order"
                                (let
                                 ((target
                                   (%render-temperature-table
                                    (ntop.temperature.presentation:make-temperature-table-config))))
                                 (cl-weave:expect (%temperature-screen-row-labels target)
                                                  :to-equal
                                                  (list "" "CPU (1)" "CPU (2)" "GPU"))))
                   (cl-weave:it "sorts the rendered rows by temperature"
                                (let ((target
                                       (%render-temperature-table
                                        (ntop.temperature.presentation:make-temperature-table-config
                                         :sort-column
                                         :temperature))))
                                  (cl-weave:expect
                                   (%temperature-screen-row-labels target)
                                   :to-equal
                                   (list "GPU" "" "CPU (1)" "CPU (2)"))))
                   (cl-weave:it
                    "matches BTM empty and unavailable temperature rows"
                    (let* ((config
                            (ntop.temperature.presentation:make-temperature-table-config
                             :empty-message "No data"))
                           (unavailable-sample nil)
                           (empty-sample
                            (ntop.temperature.domain:make-temperature-sample
                             :sampled-at 0
                             :sensors nil))
                           (unavailable-rows
                            (ntop.temperature.presentation::%temperature-table-rows
                             unavailable-sample))
                           (empty-rows
                            (ntop.temperature.presentation::%temperature-table-rows
                             empty-sample))
                           (unavailable-screen
                            (%render-temperature-table config unavailable-sample))
                           (empty-screen
                            (%render-temperature-table config empty-sample))
                           (missing-reading-sample
                            (ntop.temperature.domain:make-temperature-sample
                             :sampled-at 0
                             :sensors
                             (list
                              (ntop.temperature.domain:make-temperature-sensor
                               :name "CPU"
                               :temperature-celsius nil))))
                           (missing-reading-rows
                            (ntop.temperature.presentation::%temperature-table-rows
                             missing-reading-sample))
                           (missing-reading-screen
                            (%render-temperature-table config
                                                       missing-reading-sample)))
                      (cl-weave:expect (list unavailable-rows empty-rows)
                                       :to-equal
                                       (list nil nil))
                      (cl-weave:expect (%temperature-screen-golden unavailable-screen)
                                       :to-equal
                                       (%temperature-screen-golden empty-screen))
                      (cl-weave:expect
                       (not
                        (null
                         (search "No data"
                                (%temperature-screen-golden unavailable-screen))))
                       :to-be t)
                      (cl-weave:expect missing-reading-rows
                                       :to-equal
                                       (list
                                        (list :sensor "CPU"
                                              :temperature-celsius nil)))
                      (cl-weave:expect
                       (not
                        (null
                         (search "N/A"
                                (%temperature-screen-golden
                                 missing-reading-screen))))
                       :to-be t)))
                   (cl-weave:it
                    "sorts descending and preserves equal row comparisons"
                    (let* ((first-cpu '(:sensor "CPU" :temperature-celsius 20 :id 1))
                           (second-cpu '(:sensor "CPU" :temperature-celsius 20 :id 2))
                           (gpu '(:sensor "GPU" :temperature-celsius 30 :id 3))
                           (rows (list first-cpu second-cpu gpu)))
                      (cl-weave:expect
                       (list (ntop.temperature.presentation::%temperature-row-comparison
                              first-cpu
                              second-cpu
                              :sensor)
                             (ntop.temperature.presentation::%temperature-row-comparison
                              first-cpu
                              second-cpu
                              :temperature))
                       :to-equal
                       (list 0 0))
                      (cl-weave:expect
                       (mapcar (lambda (row) (getf row :id))
                               (ntop.temperature.presentation::%sort-temperature-table-rows
                                (copy-list rows)
                                :sensor
                                :descending))
                       :to-equal
                       '(3 1 2))
                      (cl-weave:expect
                       (mapcar (lambda (row) (getf row :temperature-celsius))
                               (ntop.temperature.presentation::%sort-temperature-table-rows
                                (copy-list rows)
                                :temperature
                                :descending))
                       :to-equal
                       '(30 20 20))
                      (cl-weave:signals
                       simple-error
                       (ntop.temperature.presentation::%temperature-row-less-p
                        first-cpu
                        gpu
                        :sensor
                        :sideways))))
                   (cl-weave:it-each
                    ((nil nil 0) (nil 50 -1))
                    "compares missing temperatures as ~A and ~A"
                    (left-temperature right-temperature expected)
                    (cl-weave:expect
                     (ntop.temperature.presentation::%temperature-row-comparison
                      (list :temperature-celsius left-temperature)
                      (list :temperature-celsius right-temperature)
                      :temperature)
                     :to-be
                     expected))
                   (cl-weave:it
                    "rejects unsupported temperature sort columns"
                    (cl-weave:signals
                     type-error
                     (ntop.temperature.presentation::%temperature-row-comparison
                      nil
                      nil
                      :unsupported))))

