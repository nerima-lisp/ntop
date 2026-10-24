(in-package #:ntop.test)

(defun %f6-temperature-graph-history (name values)
  "Build a chart series named NAME from indexed readings in VALUES."
  (ntop.ui:make-chart-series :name
                             name
                             :samples
                             (loop for value in values
                                   for index from 0
                                   collect (ntop.shared.domain::%make-history-sample
                                            (float index 1d0)
                                            (and value (float value 1d0))))))

(defun %f6-temperature-graph-fixture-series ()
  "Provide stable processor and graphics sensor histories for graph rendering tests."
  (list (%f6-temperature-graph-history "CPU Package" '(25.0 58.0 74.2))
        (%f6-temperature-graph-history "GPU Core" '(40.0 53.0 61.8))))

(defun %f6-temperature-graph-screen-lines (screen)
  "Return each screen row without trailing spaces."
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim '(#\Space) (cl-tty-kit:screen-row-string screen row))))

(defun %f6-temperature-graph-golden-lines ()
  "Load the expected rows for the graph fixture."
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test" "t/feature/temperature/fixtures/graph-golden.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun %f6-render-temperature-graph (config)
  "Render fixture histories with CONFIG into a fixed-size screen."
  (let* ((screen (cl-tty-kit:make-screen 44 16))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 44 :height 16)))
    (ntop.ui:draw-widget :temperature-graph
                         screen
                         rectangle
                         nil
                         config
                         (%f6-temperature-graph-fixture-series))
    screen))

(defstruct (%temperature-screen-target (:constructor %make-temperature-screen-target (cells)))
  "Store the character grid used by the table rendering fixture."
  cells)

(defun %temperature-screen-target (width height)
  "Create a character grid sized for the temperature table fixture."
  (%make-temperature-screen-target (make-array (list height width) :initial-element #\Space)))

(defun %temperature-screen-lines (target)
  "Serialize the grid as rows with trailing spaces removed."
  (loop for row below (array-dimension (%temperature-screen-target-cells target) 0)
        collect (string-right-trim '(#\Space)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%temperature-screen-target-cells
                                                             target)
                                                            1)
                                          collect (aref (%temperature-screen-target-cells target)
                                                        row
                                                        column))
                                    'string))))

(defun %temperature-screen-golden (target)
  "Serialize rendered table rows with a newline after each row."
  (format nil "~{~A~%~}" (%temperature-screen-lines target)))

(defun %temperature-table-fixture-sample ()
  "Build a sample containing duplicate, missing, and unnamed sensors."
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

(defun %render-temperature-table (config &optional (sample (%temperature-table-fixture-sample)))
  "Draw the optional sample with CONFIG in a fixed-size table grid."
  (let* ((target (%temperature-screen-target 32 6))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 6)))
    (ntop.ui:draw-widget :temperature target rectangle (ntop.ui:make-table-state) config sample)
    target))

(defun %temperature-screen-row-labels (target)
  "Extract sensor labels from the rendered table rows."
  (mapcar
   (lambda (line)
     (string-trim '(#\Space) (subseq line 0 16)))
   (subseq (%temperature-screen-lines target) 2)))

(defun %temperature-column-text (unit celsius)
  "Format a Celsius reading in the requested unit, preserving missing values."
  (funcall
   (ntop.ui:table-column-value-function
    (second
     (ntop.ui:table-config-columns
      (ntop.temperature.presentation:make-temperature-table-config :temperature-unit unit))))
   (list :temperature-celsius celsius)))

(defmethod ntop.ui:draw-table-text ((target %temperature-screen-target) x y text role)
  (declare (ignore role))
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%temperature-screen-target-cells target) 0))
                  (<= 0 column)
                  (< column (array-dimension (%temperature-screen-target-cells target) 1)))
          do (setf (aref (%temperature-screen-target-cells target) y column) character)))
