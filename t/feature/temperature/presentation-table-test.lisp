(in-package #:ntop.test)

(defstruct
    (%temperature-screen-target
     (:constructor %make-temperature-screen-target (cells)))
  cells)

(defmethod ntop.ui:draw-table-text ((target %temperature-screen-target) x
                                    y
                                    text
                                    role)
  (declare (ignore role))
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y
                     (array-dimension (%temperature-screen-target-cells target)
                                      0))
                  (<= 0 column)
                  (< column
                     (array-dimension (%temperature-screen-target-cells target)
                                      1)))
          do (setf (aref (%temperature-screen-target-cells target) y column) character)))

(defun %temperature-screen-target (width height)
  (%make-temperature-screen-target
   (make-array (list height width) :initial-element #\Space)))

(defun %temperature-screen-lines (target)
  (loop for row below (array-dimension (%temperature-screen-target-cells target)
                                       0)
        collect (string-right-trim '(#\Space)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%temperature-screen-target-cells
                                                             target)
                                                            1)
                                          collect (aref (%temperature-screen-target-cells target) row column))
                                    'string))))

(defun %temperature-screen-golden (target)
  (format nil "~{~A~%~}" (%temperature-screen-lines target)))

(defun %temperature-table-fixture-sample ()
  (ntop.temperature.domain:make-temperature-sample :sampled-at
                                                   0
                                                   :sensors
                                                   (list
                                                    (ntop.temperature.domain:make-temperature-sensor
                                                     :name
                                                     "GPU"
                                                     :temperature-celsius
                                                     nil)
                                                    (ntop.temperature.domain:make-temperature-sensor
                                                     :name
                                                     "CPU"
                                                     :temperature-celsius
                                                     50.3)
                                                    (ntop.temperature.domain:make-temperature-sensor
                                                     :name
                                                     "CPU"
                                                     :temperature-celsius
                                                     51.2)
                                                    (ntop.temperature.domain:make-temperature-sensor
                                                     :temperature-celsius
                                                     42.1))))

(defun %render-temperature-table (config &optional
                                         (sample
                                          (%temperature-table-fixture-sample)))
  (let* ((target (%temperature-screen-target 32 6))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 6)))
    (ntop.ui:draw-widget :temperature
                         target
                         rectangle
                         (ntop.ui:make-table-state)
                         config
                         sample)
    target))

(defun %temperature-screen-row-labels (target)
  (mapcar
   (lambda (line)
     (string-trim '(#\Space) (subseq line 0 16)))
   (subseq (%temperature-screen-lines target) 2)))

(defun %temperature-column-text (unit celsius)
  (funcall
   (ntop.ui:table-column-value-function
    (second
     (ntop.ui:table-config-columns
      (ntop.temperature.presentation:make-temperature-table-config
       :temperature-unit
       unit))))
   (list :temperature-celsius celsius)))

(cl-weave:describe "Temperature table presentation"
                   (cl-weave:it
                    "defines the btm headers and sortable soft columns"
                    (let ((columns
                           (ntop.ui:table-config-columns
                            (ntop.temperature.presentation:make-temperature-table-config))))
                      (cl-weave:expect
                       (mapcar (function ntop.ui:table-column-id) columns)
                       :to-equal
                       (list :sensor :temperature))
                      (cl-weave:expect
                       (mapcar (function ntop.ui:table-column-header) columns)
                       :to-equal
                       (list "Sensor(s)" "Temp(t)"))
                      (cl-weave:expect
                       (mapcar (function ntop.ui:table-column-width-kind)
                               columns)
                       :to-equal
                       (list :soft :soft))
                      (cl-weave:expect
                       (mapcar (function ntop.ui:table-column-sortable-p)
                               columns)
                       :to-equal
                       (list t t))
                      (cl-weave:expect
                       (ntop.ui:table-column-max-percent (first columns))
                       :to-be
                       4/5)
                      (cl-weave:expect
                       (ntop.ui:table-column-max-percent (second columns))
                       :to-be
                       nil)))
                   (cl-weave:it "defaults to Sensor ascending"
                                (let ((config
                                       (ntop.temperature.presentation:make-temperature-table-config)))
                                  (cl-weave:expect
                                   (ntop.ui:table-config-sort-column config)
                                   :to-be
                                   :sensor)
                                  (cl-weave:expect
                                   (ntop.ui:table-config-sort-order config)
                                   :to-be
                                   :ascending)))
                   (cl-weave:it
                    "uses the shared temperature unit and ceiling formatter"
                    (cl-weave:expect (%temperature-column-text :celsius 37.2)
                                     :to-equal
                                     "38°C")
                    (cl-weave:expect (%temperature-column-text :fahrenheit 20)
                                     :to-equal
                                     "68°F")
                    (cl-weave:expect (%temperature-column-text :kelvin 0)
                                     :to-equal
                                     "274K")
                    (cl-weave:expect (%temperature-column-text :celsius nil)
                                     :to-equal
                                     "N/A"))
                   (cl-weave:it "sorts the rendered rows by Sensor ASCII order"
                                (let ((target
                                       (%render-temperature-table
                                        (ntop.temperature.presentation:make-temperature-table-config))))
                                  (cl-weave:expect
                                   (%temperature-screen-row-labels target)
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
                    "distinguishes an unavailable sample from an empty sample"
                    (let* ((config
                            (ntop.temperature.presentation:make-temperature-table-config))
                           (unavailable-lines
                            (%temperature-screen-lines
                             (%render-temperature-table config nil)))
                           (empty-sample
                            (ntop.temperature.domain:make-temperature-sample
                             :sampled-at
                             0
                             :sensors
                             nil))
                           (empty-lines
                            (%temperature-screen-lines
                             (%render-temperature-table config empty-sample))))
                      (cl-weave:expect
                       (string-trim " " (subseq (third unavailable-lines) 0 16))
                       :to-equal
                       "N/A")
                      (cl-weave:expect
                       (string-trim " " (subseq (third unavailable-lines) 17))
                       :to-equal
                       "N/A")
                      (cl-weave:expect (subseq unavailable-lines 3)
                                       :to-equal
                                       (make-list 3 :initial-element ""))
                      (cl-weave:expect (subseq empty-lines 2)
                                       :to-equal
                                       (make-list 4 :initial-element ""))))
                   (cl-weave:it "matches the fixture-data screen golden"
                                (let* ((golden-path
                                        (asdf:system-relative-pathname "ntop"
                                                                       "t/feature/temperature/fixtures/screen-golden.txt"))
                                       (expected
                                        (string-right-trim (format nil "~%")
                                                           (uiop:read-file-string
                                                            golden-path))))
                                  (cl-weave:expect
                                   (%temperature-screen-golden
                                    (%render-temperature-table
                                     (ntop.temperature.presentation:make-temperature-table-config)))
                                   :to-equal
                                   (format nil "~A~%" expected)))))
