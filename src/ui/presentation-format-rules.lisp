(in-package #:ntop.ui)

(defparameter *presentation-format-rules*
  '((:binary-prefixes :prefix-system
                      :binary
                      :prefixes
                      ((:none 0 "") (:kibi 1024 "Ki")
                       (:mebi 1048576 "Mi")
                       (:gibi 1073741824 "Gi")
                       (:tebi 1099511627776 "Ti"))
                      :sources
                      ("bottom 0.14.9 src/utils/data_units.rs:36-47"
                       "bottom 0.14.9 src/utils/data_units.rs:80-93"))
    (:decimal-prefixes :prefix-system
                       :decimal
                       :prefixes
                       ((:none 0 "") (:kilo 1000 "K")
                        (:mega 1000000 "M")
                        (:giga 1000000000 "G")
                        (:tera 1000000000000 "T"))
                       :sources
                       ("bottom 0.14.9 src/utils/data_units.rs:50-61"
                        "bottom 0.14.9 src/utils/data_units.rs:80-101"))
    (:cpu-basic-percent :decimal-precision
                        1
                        :decimal-width
                        5
                        :integer-precision
                        0
                        :integer-width
                        3
                        :integer-rounding
                        :half-up
                        :sources
                        ("bottom 0.14.9 src/canvas/widgets/cpu_basic.rs:159-173"))
    (:cpu-title :precision
                2
                :sources
                ("bottom 0.14.9 src/canvas/widgets/cpu_graph.rs:191-202"))
    (:memory-legend :percentage-precision
                    0
                    :percentage-width
                    3
                    :quantity-precision
                    1
                    :zero-total
                    "0%   0.0B/0.0B"
                    :sources
                    ("bottom 0.14.9 src/canvas/widgets/mem_graph.rs:21-34"
                     "bottom 0.14.9 src/canvas/widgets/mem_basic.rs:15-31"
                     "bottom 0.14.9 src/utils/conversion.rs:6-20"))
    (:process-percent :precision
                      1
                      :sources
                      ("bottom 0.14.9 src/widgets/process_table/process_data.rs:123-129"
                       "bottom 0.14.9 src/widgets/process_table/process_data.rs:319-345"))
    (:disk-capacity :precision
                    0
                    :sources
                    ("bottom 0.14.9 src/widgets/disk_table.rs:30-54"))
    (:disk-io-rate :precision-threshold
                   1000000000
                   :precision-below
                   0
                   :precision-at-least
                   1
                   :sources
                   ("bottom 0.14.9 src/utils/conversion.rs:23-33"
                    "bottom 0.14.9 src/widgets/disk_table.rs:86-95"))
    (:process-binary-bytes :precision-threshold
                           1073741824
                           :precision-below
                           0
                           :precision-at-least
                           1
                           :sources
                           ("bottom 0.14.9 src/widgets/process_table/process_data.rs:189-200"))
    (:process-decimal-bytes :precision-threshold
                            1000000000
                            :precision-below
                            0
                            :precision-at-least
                            1
                            :sources
                            ("bottom 0.14.9 src/widgets/process_table/process_data.rs:202-210"))
    (:network-rate :precision
                   1
                   :sources
                   ("bottom 0.14.9 src/canvas/widgets/network_graph.rs:103-113"
                    "bottom 0.14.9 src/canvas/widgets/network_graph.rs:134-137"
                    "bottom 0.14.9 src/utils/data_units.rs:80-103"))
    (:network-total :precision
                    1
                    :sources
                    ("bottom 0.14.9 src/canvas/widgets/network_graph.rs:110-113"
                     "bottom 0.14.9 src/utils/data_units.rs:64-78"))
    (:network-legend :rate-width
                     10
                     :rate-alignment
                     :left
                     :total-width
                     8
                     :total-alignment
                     :left
                     :sources
                     ("bottom 0.14.9 src/canvas/widgets/network_graph.rs:134-182"))
    (:disk-io-legend-rate :precision
                          1
                          :width
                          11
                          :alignment
                          :right
                          :sources
                          ("bottom 0.14.9 src/canvas/widgets/disk_io_graph.rs:255-269"))
    (:network-axis :precision
                   1
                   :label-width
                   5
                   :upper-scale
                   3/2
                   :sources
                   ("bottom 0.14.9 src/canvas/widgets/network_graph.rs:360-439"))
    (:disk-io-axis :precision
                   1
                   :label-width
                   8
                   :upper-scale
                   3/2
                   :sources
                   ("bottom 0.14.9 src/canvas/widgets/disk_io_graph.rs:272-302"))
    (:process-time :elapsed-unit
                   :millisecond
                   :seconds-per-minute
                   60
                   :minutes-per-hour
                   60
                   :hours-per-day
                   24
                   :milliseconds-per-second
                   1000
                   :minute-fraction-precision
                   2
                   :second-fraction-precision
                   3
                   :sources
                   ("bottom 0.14.9 src/widgets/process_table/process_data.rs:132-187"
                    "bottom 0.14.9 src/widgets/process_table/process_data.rs:436-463"
                    "bottom 0.14.9 src/utils/strings.rs:4-10"
                    "bottom 0.14.9 src/canvas/components/data_table/draw.rs:234-242"))
    (:temperature-table :rounding
                        :ceiling
                        :units
                        ((:celsius "°C") (:fahrenheit "°F") (:kelvin "K"))
                        :sources
                        ("bottom 0.14.9 src/app/data/temperature.rs:28-47"
                         "bottom 0.14.9 src/app/data/temperature.rs:69-75"
                         "bottom 0.14.9 src/widgets/temperature_table.rs:68-74"))
    (:temperature-graph :precision
                        0
                        :sources
                        ("bottom 0.14.9 src/canvas/widgets/temperature_graph.rs:51-59"))
    (:temperature-axis :default-celsius
                       100
                       :rounding
                       :ceiling
                       :sources
                       ("bottom 0.14.9 src/canvas/widgets/temperature_graph.rs:111-138"
                        "bottom 0.14.9 src/app/data/temperature.rs:28-58"))))

(defun format-rule (key)
  (or (cdr (assoc key *presentation-format-rules*))
      (error "Unknown presentation format rule ~S." key)))

(defun format-rule-value (key property)
  (getf (format-rule key) property))

(defun format-rule-sources (key)
  (format-rule-value key :sources))

(defun format-fixed (value precision &optional width)
  (if (zerop precision)
      (if width
          (format nil "~vD" width (round value))
          (format nil "~D" (round value)))
      (format nil "~v,vF" width precision (float value 1d0))))

(defun format-prefix-entry (prefix-rule value)
  (loop with selected = (first (format-rule-value prefix-rule :prefixes))
        for entry in (format-rule-value prefix-rule :prefixes)
        when (<= (second entry) (abs value))
          do (setf selected entry)
        finally (return selected)))

(defun format-scaled-in-prefix (value unit
                                      prefix-rule
                                      prefix
                                      precision
                                      &optional
                                      width)
  (let* ((prefix-entry
          (find prefix (format-rule-value prefix-rule :prefixes) :key #'first))
         (scaled
          (ntop.shared.domain:scale-quantity
           (ntop.shared.domain:make-quantity value unit)
           (format-rule-value prefix-rule :prefix-system)
           prefix)))
    (values
     (format-fixed (ntop.shared.domain:scaled-quantity-value scaled)
                   precision
                   width)
     (third prefix-entry))))

(defun format-scaled-value (value unit prefix-rule precision &optional width)
  (let ((prefix-entry (format-prefix-entry prefix-rule value)))
    (format-scaled-in-prefix value
                             unit
                             prefix-rule
                             (first prefix-entry)
                             precision
                             width)))

(defun format-rounded-value (value rounding)
  (ecase rounding
    (:half-up (floor (+ value 1/2)))
    (:ceiling (ceiling value))))
