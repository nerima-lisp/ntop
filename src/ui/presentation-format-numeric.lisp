(in-package #:ntop.ui)

(defun format-cpu-basic-percent (usage show-decimal)
  (let* ((precision
          (format-rule-value :cpu-basic-percent
                             (if show-decimal
                                 :decimal-precision
                                 :integer-precision)))
         (width
          (format-rule-value :cpu-basic-percent
                             (if show-decimal
                                 :decimal-width
                                 :integer-width)))
         (value
          (if show-decimal
              usage
              (format-rounded-value usage
                                    (format-rule-value :cpu-basic-percent
                                                       :integer-rounding)))))
    (format nil "~A%" (format-fixed value precision width))))

(defun format-cpu-title (one-minute five-minutes fifteen-minutes)
  (let ((precision (format-rule-value :cpu-title :precision)))
    (format nil
            " CPU ─ ~A ~A ~A "
            (format-fixed one-minute precision)
            (format-fixed five-minutes precision)
            (format-fixed fifteen-minutes precision))))

(defun format-process-cpu-percent (usage)
  (concatenate 'string
               (format-fixed usage
                             (format-rule-value :process-percent :precision))
               "%"))

(defun format-process-memory-percent (usage)
  (concatenate 'string
               (format-fixed usage
                             (format-rule-value :process-percent :precision))
               "%"))

(defun format-memory-percentage (used-bytes total-bytes)
  (let ((percentage (* 100d0 (/ (float used-bytes 1d0) total-bytes))))
    (concatenate 'string
                 (format-fixed percentage
                               (format-rule-value :memory-legend
                                                  :percentage-precision)
                               (format-rule-value :memory-legend
                                                  :percentage-width))
                 "%")))

(defun format-memory-fraction (used-bytes total-bytes)
  (let* ((prefix-entry (format-prefix-entry :binary-prefixes total-bytes))
         (prefix (first prefix-entry))
         (precision (format-rule-value :memory-legend :quantity-precision)))
    (multiple-value-bind (used-text prefix-text)
        (format-scaled-in-prefix used-bytes
                                 :byte
                                 :binary-prefixes
                                 prefix
                                 precision)
      (multiple-value-bind (total-text ignored-prefix-text)
          (format-scaled-in-prefix total-bytes
                                   :byte
                                   :binary-prefixes
                                   prefix
                                   precision)
        (declare (ignore ignored-prefix-text))
        (format nil "~A~AB/~A~AB" used-text prefix-text total-text prefix-text)))))

(defun format-memory-legend (name used-bytes total-bytes)
  (if (zerop total-bytes)
      (format nil
              "~A:   ~A"
              name
              (format-rule-value :memory-legend :zero-total))
      (format nil
              "~A:~A   ~A"
              name
              (format-memory-percentage used-bytes total-bytes)
              (format-memory-fraction used-bytes total-bytes))))

(defun format-process-binary-bytes (bytes)
  (let* ((precision
          (if (>= bytes
                  (format-rule-value :process-binary-bytes :precision-threshold))
              (format-rule-value :process-binary-bytes :precision-at-least)
              (format-rule-value :process-binary-bytes :precision-below))))
    (multiple-value-bind (number prefix)
        (format-scaled-value bytes :byte :binary-prefixes precision)
      (format nil "~A~AB" number prefix))))

(defun format-process-decimal-bytes (bytes)
  (let* ((precision
          (if (>= bytes
                  (format-rule-value :process-decimal-bytes
                                     :precision-threshold))
              (format-rule-value :process-decimal-bytes :precision-at-least)
              (format-rule-value :process-decimal-bytes :precision-below))))
    (multiple-value-bind (number prefix)
        (format-scaled-value bytes :byte :decimal-prefixes precision)
      (format nil "~A~AB" number prefix))))

(defun format-disk-capacity (bytes)
  (multiple-value-bind (number prefix)
      (format-scaled-value bytes
                           :byte
                           :decimal-prefixes
                           (format-rule-value :disk-capacity :precision))
    (format nil "~A~AB" number prefix)))

(defun format-decimal-io-rate (bytes-per-second)
  (let* ((precision
          (if (>= bytes-per-second
                  (format-rule-value :disk-io-rate :precision-threshold))
              (format-rule-value :disk-io-rate :precision-at-least)
              (format-rule-value :disk-io-rate :precision-below))))
    (multiple-value-bind (number prefix)
        (format-scaled-value bytes-per-second :byte :decimal-prefixes precision)
      (format nil "~A~AB/s" number prefix))))

(defun format-disk-io-rate (bytes-per-second)
  (format-decimal-io-rate bytes-per-second))

(defun format-process-io-rate (bytes-per-second)
  (format-decimal-io-rate bytes-per-second))

(defun format-network-rate (rate prefix-system data-unit)
  (let ((prefix-rule
         (ecase prefix-system
           (:decimal :decimal-prefixes)
           (:binary :binary-prefixes)))
        (unit
         (ecase data-unit
           (:bit "b/s")
           (:byte "B/s"))))
    (multiple-value-bind (number prefix)
        (format-scaled-value rate
                             (ecase data-unit
                               (:bit :bit)
                               (:byte :byte))
                             prefix-rule
                             (format-rule-value :network-rate :precision))
      (format nil "~A~A~A" number prefix unit))))

(defun format-network-total (total-bits prefix-system)
  (let ((prefix-rule
         (ecase prefix-system
           (:decimal :decimal-prefixes)
           (:binary :binary-prefixes))))
    (multiple-value-bind (number prefix)
        (format-scaled-value (floor total-bits 8)
                             :byte
                             prefix-rule
                             (format-rule-value :network-total :precision))
      (format nil "~A~AB" number prefix))))

(defun format-network-legend (direction rate total-bits prefix-system data-unit)
  (let ((rate-text (format-network-rate rate prefix-system data-unit))
        (total-text (format-network-total total-bits prefix-system)))
    (format nil
            "~A: ~A All: ~A"
            direction
            (format-aligned rate-text
                            (format-rule-value :network-legend :rate-width)
                            (format-rule-value :network-legend :rate-alignment))
            (format-aligned total-text
                            (format-rule-value :network-legend :total-width)
                            (format-rule-value :network-legend :total-alignment)))))

(defun format-disk-io-legend-rate (bytes-per-second)
  (let ((rate
         (multiple-value-bind (number prefix)
             (format-scaled-value bytes-per-second
                                  :byte
                                  :binary-prefixes
                                  (format-rule-value :disk-io-legend-rate
                                                     :precision))
           (format nil "~A~AB/s" number prefix))))
    (format-aligned rate
                    (format-rule-value :disk-io-legend-rate :width)
                    (format-rule-value :disk-io-legend-rate :alignment))))

(defun format-temperature-value (celsius unit)
  (ecase unit
    (:celsius celsius)
    (:fahrenheit (+ (* celsius 9/5) 32))
    (:kelvin (+ celsius 273.15d0))))

(defun format-temperature-unit (unit)
  (or (second (assoc unit (format-rule-value :temperature-table :units)))
      (error "Unknown temperature unit ~S." unit)))

(defun format-temperature (celsius unit)
  (format nil
          "~D~A"
          (format-rounded-value (format-temperature-value celsius unit)
                                (format-rule-value :temperature-table :rounding))
          (format-temperature-unit unit)))

(defun format-temperature-graph-value (celsius unit)
  (format nil
          "~A~A"
          (format-fixed (format-temperature-value celsius unit)
                        (format-rule-value :temperature-graph :precision))
          (format-temperature-unit unit)))

(defun format-temperature-axis-labels (max-entry unit &optional upper-limit)
  (let* ((rounding (format-rule-value :temperature-axis :rounding))
         (default-upper
          (format-temperature-value
           (format-rule-value :temperature-axis :default-celsius)
           unit))
         (upper (or upper-limit (max max-entry default-upper)))
         (unit-text (format-temperature-unit unit)))
    (values upper
            (list (format nil "0~A" unit-text)
                  (format nil
                          "~D~A"
                          (format-rounded-value (/ upper 2) rounding)
                          unit-text)
                  (format nil
                          "~D~A"
                          (format-rounded-value upper rounding)
                          unit-text)))))

(defun format-network-axis-labels (max-entry prefix-system data-unit)
  (let* ((upper
          (if (zerop max-entry)
              1d0
              (* max-entry (format-rule-value :network-axis :upper-scale))))
         (prefix-rule
          (ecase prefix-system
            (:decimal :decimal-prefixes)
            (:binary :binary-prefixes)))
         (prefix-entry (format-prefix-entry prefix-rule upper))
         (prefix (first prefix-entry))
         (unit
          (ecase data-unit
            (:bit :bit)
            (:byte :byte)))
         (prefix-denominator
          (/ 1
             (ntop.shared.domain:scaled-quantity-value
              (ntop.shared.domain:scale-quantity
               (ntop.shared.domain:make-quantity 1 unit)
               (format-rule-value prefix-rule :prefix-system)
               prefix))))
         (scaled-max (/ (float max-entry 1d0) prefix-denominator))
         (prefix-text (third prefix-entry))
         (unit-text
          (ecase data-unit
            (:bit "b")
            (:byte "B")))
         (width (format-rule-value :network-axis :label-width))
         (precision (format-rule-value :network-axis :precision)))
    (values upper
            (list (format nil "0~A~A" prefix-text unit-text)
                  (format-fixed (* scaled-max 1/2) precision width)
                  (format-fixed scaled-max precision width)
                  (format-fixed (* scaled-max 3/2) precision width)))))

(defun format-disk-io-axis-labels (max-entry)
  (let* ((upper
          (if (zerop max-entry)
              1d0
              (* max-entry (format-rule-value :disk-io-axis :upper-scale))))
         (prefix-entry (format-prefix-entry :binary-prefixes upper))
         (prefix (first prefix-entry))
         (prefix-text (third prefix-entry))
         (prefix-denominator
          (/ 1
             (ntop.shared.domain:scaled-quantity-value
              (ntop.shared.domain:scale-quantity
               (ntop.shared.domain:make-quantity 1 :byte)
               :binary
               prefix))))
         (scaled-max (/ (float max-entry 1d0) prefix-denominator))
         (unit "B/s")
         (width (format-rule-value :disk-io-axis :label-width))
         (precision (format-rule-value :disk-io-axis :precision)))
    (values upper
            (list (format nil "0~A~A" prefix-text unit)
                  (format-fixed (* scaled-max 1/2) precision width)
                  (format-fixed scaled-max precision width)
                  (format-fixed (* scaled-max 3/2) precision width)))))

;; Source: btm network_graph.rs log-axis tiers, lines 443-568.
(defun format-network-log-axis-labels (max-entry prefix-system data-unit)
  (let* ((binary-p (ecase prefix-system
                     (:decimal nil)
                     (:binary t)))
         (base (if binary-p 2d0 10d0))
         (thresholds (if binary-p
                         '(20d0 30d0 40d0 50d0)
                         '(6d0 9d0 12d0 15d0)))
         (prefixes (if binary-p
                       '("Ki" "Mi" "Gi" "Ti" "Pi")
                       '("K" "M" "G" "T" "P")))
         (unit (ecase data-unit
                 (:bit "b")
                 (:byte "B")))
         (zero-padding (if binary-p "  " " "))
         (tier (cond
                 ((< max-entry (expt base (first thresholds))) 0)
                 ((< max-entry (expt base (second thresholds))) 1)
                 ((< max-entry (expt base (third thresholds))) 2)
                 (t 3))))
    (values (nth tier thresholds)
            (loop for index below (+ tier 3)
                  collect (if (zerop index)
                              (format nil "~A0~A" zero-padding unit)
                              (format nil "1~A~A"
                                      (nth (1- index) prefixes)
                                      unit)))))

;; Source: btm disk_io_graph.rs log-axis tiers, lines 305-340.
) (defun format-disk-io-log-axis-labels (max-entry)
  (let* ((thresholds '(20d0 30d0 40d0 50d0))
         (tier (cond
                 ((< max-entry (expt 2d0 (first thresholds))) 0)
                 ((< max-entry (expt 2d0 (second thresholds))) 1)
                 ((< max-entry (expt 2d0 (third thresholds))) 2)
                 (t 3))))
    (values (nth tier thresholds)
            (loop for index below (+ tier 3)
                  collect (if (zerop index)
                              "  0B/s"
                              (format nil "1~A/s"
                                      (nth (1- index)
                                           '("KiB" "MiB" "GiB" "TiB" "PiB")))))))

) (defun format-aligned (value width alignment)
  (ecase alignment
    (:left (format nil "~vA" width value))
    (:right (format nil "~v@A" width value))))