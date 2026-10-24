(in-package #:ntop.network.presentation)

(defun %network-chart-config (rectangle latest maximum config)
  (multiple-value-bind (y-maximum y-labels) (%network-axis-data maximum config)
    (let* ((logarithmic-p
            (ntop.shared.application:application-config-network-use-log-p config))
           (binary-prefix-p
            (ntop.shared.application:application-config-binary-prefix-p config))
           (legend-position
            (ntop.shared.application:application-config-network-legend-position config))
           (scale
            (ntop.network.domain:network-axis-scaling logarithmic-p binary-prefix-p))
           (window
            (ntop.shared.application:application-config-time-window-seconds config))
           (latest-time (and latest (ntop.shared.domain:sample-record-sampled-at latest)))
           (labels-visible-p
            (and (not (ntop.shared.application:application-config-hide-time-p config))
                 (or
                  (not
                   (ntop.shared.application:application-config-auto-hide-time-p config))
                  (>= (ntop.ui:rectangle-height rectangle) 6))))
           (old-legend-p
            (%network-old-legend-p config)))
      (ntop.ui:make-chart-config :x-start
                                 (and latest-time (- latest-time window))
                                 :x-end latest-time
                                 :y-min 0d0
                                 :y-max y-maximum
                                 :y-scale scale
                                 :y-labels y-labels
                                 :x-labels
                                 (and labels-visible-p
                                      latest-time
                                      (list (format nil "-~As" (round window)) "now"))
                                 :theme-name
                                 (string-downcase
                                  (string
                                   (ntop.shared.application:application-config-theme
                                    config)))
                                 :legend-position
                                 (unless old-legend-p legend-position)
                                 :legend-width-ratio (if old-legend-p 0 9/10)
                                 :legend-height-ratio (if old-legend-p 0 3/4)
                                 :dot-marker-p
                                 (ntop.shared.application:application-config-dot-marker-p
                                  config)))))

(defun %network-axis-data (maximum config)
  (let ((unit (ntop.shared.application:application-config-data-unit config))
        (prefix-system (%network-prefix-system config)))
    (if (ntop.shared.application:application-config-network-use-log-p config)
        (%network-log-axis-data maximum prefix-system unit)
        (ntop.ui:format-network-axis-labels maximum prefix-system unit))))

(defun %network-log-axis-data (maximum prefix-system unit)
  (let* ((base
          (if (eq prefix-system :binary)
              2d0
              10d0))
         (levels (cdr (assoc prefix-system +network-log-axis-levels+)))
         (prefixes (cdr (assoc prefix-system +network-log-axis-prefixes+)))
         (level-count
          (or
           (position-if
            (lambda (level)
              (< maximum (expt base level)))
            levels)
           (1- (length levels))))
         (upper (nth level-count levels))
         (unit-text
          (if (eq unit :bit)
              "b"
              "B"))
         (zero-padding
          (if (eq prefix-system :binary)
              "  "
              " "))
         (labels (cons
                  (format nil
                    "~A0~A"
                    zero-padding
                    unit-text)
                  (loop for
                    prefix
                    in
                    (subseq prefixes 0 (min (+ level-count 2) (length prefixes)))
                    collect
                    (format nil "1~A~A" prefix unit-text)))))
    (values upper labels)))

(defun %draw-network-old-legend (target rectangle latest previous baseline config)
  (let* ((show-packets-p
          (ntop.shared.application:application-config-network-show-packets-p config))
         (headers
          (if show-packets-p
              '("RX" "TX" "Total RX" "Total TX" "RX Pkts" "TX Pkts" "Avg RX" "Avg TX")
              '("RX" "TX" "Total RX" "Total TX")))
         (values (%network-legend-values latest previous baseline config show-packets-p))
         (theme (ntop.shared.application:application-config-theme config))
         (column-width
          (floor (max 0 (- (ntop.ui:rectangle-width rectangle) 2)) (length headers))))
    (when
        (and (plusp (ntop.ui:rectangle-width rectangle))
             (plusp (ntop.ui:rectangle-height rectangle)))
      (cl-tty-kit:screen-draw-box target
                                  (ntop.ui:rectangle-x rectangle)
                                  (ntop.ui:rectangle-y rectangle)
                                  (ntop.ui:rectangle-width rectangle)
                                  (ntop.ui:rectangle-height rectangle)
                                  :border
                                  :single
                                  :style
                                  (ntop.ui:presentation-theme-style theme :border))
      (when (>= (ntop.ui:rectangle-height rectangle) 2)
        (%network-draw-columns target
                               rectangle
                               headers
                               (make-list (length headers)
                                          :initial-element
                                          (ntop.ui:presentation-theme-style theme
                                                                            :table-header))
                               column-width
                               1))
      (when (>= (ntop.ui:rectangle-height rectangle) 3)
        (%network-draw-columns target
                               rectangle
                               values
                               (%network-value-styles theme show-packets-p)
                               column-width
                               2)))))

(defun %network-legend-values (latest previous baseline config show-packets-p)
  (let* ((unit (ntop.shared.application:application-config-data-unit config))
         (rx-rate
          (and previous
               latest
               (ntop.network.domain:network-rate previous latest :rx :unit unit)))
         (tx-rate
          (and previous
               latest
               (ntop.network.domain:network-rate previous latest :tx :unit unit)))
         (prefix (%network-prefix-system config))
         (rx-total (%network-total-bytes latest :rx baseline config))
         (tx-total (%network-total-bytes latest :tx baseline config)))
    (append
     (list (%network-rate-text rx-rate prefix config)
           (%network-rate-text tx-rate prefix config)
           (%network-total-text rx-total prefix)
           (%network-total-text tx-total prefix))
     (when show-packets-p
       (list (%network-packet-rate-text previous latest :rx)
             (%network-packet-rate-text previous latest :tx)
             (%network-average-packet-size-text
              (and previous
                   latest
                   (ntop.network.domain:network-average-packet-size previous latest :rx))
              config)
             (%network-average-packet-size-text
              (and previous
                   latest
                   (ntop.network.domain:network-average-packet-size previous latest :tx))
              config))))))

(defun %network-value-styles (theme show-packets-p)
  (append
   (list (ntop.ui:presentation-theme-style theme :rx)
         (ntop.ui:presentation-theme-style theme :tx)
         (ntop.ui:presentation-theme-style theme :total-rx)
         (ntop.ui:presentation-theme-style theme :total-tx))
   (when show-packets-p
     (list (ntop.ui:presentation-theme-style theme :rx)
           (ntop.ui:presentation-theme-style theme :tx)
           (ntop.ui:presentation-theme-style theme :total-rx)
           (ntop.ui:presentation-theme-style theme :total-tx)))))

(defun %network-draw-columns (target rectangle values styles column-width row-offset)
  (loop for value in values
        for style in styles
        for index from 0
        for x = (+ (ntop.ui:rectangle-x rectangle) 1 (* index column-width))
        for y = (+ (ntop.ui:rectangle-y rectangle) row-offset)
        do (%network-write-text target rectangle x y value column-width style)))

(defun %network-rate-text (quantity prefix-system config)
  (if quantity
      (ntop.ui:format-network-rate (ntop.shared.domain:quantity-value quantity)
                                   prefix-system
                                   (ntop.shared.application:application-config-data-unit
                                    config))
      "N/A"))

(defun %network-total-text (bytes prefix-system)
  (if bytes
      (ntop.ui:format-network-total (* bytes 8) prefix-system)
      "N/A"))

(defun %network-packet-rate-text (previous latest direction)
  (let ((rate
         (and previous
              latest
              (ntop.network.domain:network-packet-rate-per-second previous
                                                                  latest
                                                                  direction))))
    (if rate
        (format nil "~Dpkt/s" (round rate))
        "N/A")))

(defun %draw-network-basic (target rectangle latest previous baseline config)
  (let* ((theme (ntop.shared.application:application-config-theme config))
         (unit (ntop.shared.application:application-config-data-unit config))
         (content-rectangle (%network-content-rectangle rectangle))
         (body-rectangle
          (ntop.ui:make-rectangle :x
                                  (1+ (ntop.ui:rectangle-x content-rectangle))
                                  :y
                                  (1+ (ntop.ui:rectangle-y content-rectangle))
                                  :width
                                  (max 0
                                       (- (ntop.ui:rectangle-width content-rectangle) 2))
                                  :height
                                  (max 0
                                       (- (ntop.ui:rectangle-height content-rectangle) 2))))
         (show-packets-p
          (ntop.shared.application:application-config-network-show-packets-p config))
         (rx-rate
          (%network-rate-text
           (and previous
                latest
                (ntop.network.domain:network-rate previous latest :rx :unit unit))
           (%network-prefix-system config)
           config))
         (tx-rate
          (%network-rate-text
           (and previous
                latest
                (ntop.network.domain:network-rate previous latest :tx :unit unit))
           (%network-prefix-system config)
           config))
         (values (%network-legend-values latest previous baseline config show-packets-p))
         (styles (%network-value-styles theme show-packets-p))
         (content-x (ntop.ui:rectangle-x body-rectangle))
         (content-y (ntop.ui:rectangle-y body-rectangle))
         (content-width (ntop.ui:rectangle-width body-rectangle))
         (content-height (ntop.ui:rectangle-height body-rectangle)))
    (%draw-network-title target rectangle "Network" theme)
    (when
        (and (plusp (ntop.ui:rectangle-width content-rectangle))
             (plusp (ntop.ui:rectangle-height content-rectangle)))
      (cl-tty-kit:screen-draw-box target
                                  (ntop.ui:rectangle-x content-rectangle)
                                  (ntop.ui:rectangle-y content-rectangle)
                                  (ntop.ui:rectangle-width content-rectangle)
                                  (ntop.ui:rectangle-height content-rectangle)
                                  :border
                                  :single
                                  :style
                                  (ntop.ui:presentation-theme-style theme :border)))
    (if (not show-packets-p)
        (let ((column-width (floor content-width 2)))
          (%network-write-text target body-rectangle content-x content-y
                               (format nil "RX: ~A" rx-rate)
                               column-width (first styles))
          (%network-write-text target body-rectangle content-x (+ content-y 1)
                               (format nil "TX: ~A" tx-rate)
                               column-width (second styles))
          (%network-write-text target body-rectangle (+ content-x column-width) content-y
                               (format nil "Total RX: ~A" (third values))
                               (- content-width column-width) (third styles))
          (%network-write-text target body-rectangle (+ content-x column-width)
                               (+ content-y 1)
                               (format nil "Total TX: ~A" (fourth values))
                               (- content-width column-width) (fourth styles)))
        (%draw-network-basic-packets target rectangle body-rectangle values styles
                                     content-x content-y content-width content-height))))

(defun %draw-network-basic-packets (target layout-rectangle
                                           text-rectangle
                                           values
                                           styles
                                           content-x
                                           content-y
                                           content-width
                                           content-height)
  (if (< (ntop.ui:rectangle-width layout-rectangle) 60)
      (let ((column-width (floor content-width 2)))
        (loop for left in (subseq values 0 4)
              for right in (subseq values 4 8)
              for left-style in (subseq styles 0 4)
              for right-style in (subseq styles 4 8)
              for row from 0 below (min content-height 4)
              do (%network-write-text target
                                      text-rectangle
                                      content-x
                                      (+ content-y row)
                                      (format nil
                                              "~A ~A"
                                              (nth row
                                                   '("RX:" "TX:" "Total RX:" "Total TX:"))
                                              left)
                                      column-width
                                      left-style) (%network-write-text target
                                                                       text-rectangle
                                                                       (+ content-x
                                                                          column-width)
                                                                       (+ content-y row)
                                                                       (format nil
                                                                               "~A ~A"
                                                                               (nth row
                                                                                    '("RX Pkt:"
                                                                                      "TX Pkt:"
                                                                                      "Avg RX:"
                                                                                      "Avg TX:"))
                                                                               right)
                                                                       (- content-width
                                                                          column-width)
                                                                       right-style)))
      (let ((column-width (floor content-width 4))
            (labels '("RX:" "Total RX:" "RX Pkt:" "Avg RX:")))
        (loop for index below 4
              for value = (nth (* index 2) values)
              for label = (nth index labels)
              for style = (nth (* index 2) styles)
              for x = (+ content-x (* index column-width))
              do (%network-write-text target
                                      text-rectangle
                                      x
                                      content-y
                                      label
                                      column-width
                                      style) (%network-write-text target
                                                                  text-rectangle
                                                                  x
                                                                  (+ content-y 1)
                                                                  value
                                                                  column-width
                                                                  style))
        (loop for index below 4
              for value = (nth (1+ (* index 2)) values)
              for label = (nth index '("TX:" "Total TX:" "TX Pkt:" "Avg TX:"))
              for style = (nth (1+ (* index 2)) styles)
              for x = (+ content-x (* index column-width))
              do (%network-write-text target
                                      text-rectangle
                                      x
                                      (+ content-y 2)
                                      label
                                      column-width
                                      style) (%network-write-text target
                                                                  text-rectangle
                                                                  x
                                                                  (+ content-y 3)
                                                                  value
                                                                  column-width
                                                                  style)))))

(defun %network-write-text (target rectangle x y text width style)
  (let* ((left (ntop.ui:rectangle-x rectangle))
         (top (ntop.ui:rectangle-y rectangle))
         (right
          (min (+ left (ntop.ui:rectangle-width rectangle))
               (cl-tty-kit:screen-width target)))
         (bottom
          (min (+ top (ntop.ui:rectangle-height rectangle))
               (cl-tty-kit:screen-height target)))
         (visible-width (max 0 (min width (- right x))))
         (visible (ntop.ui::chart-clip-text text visible-width)))
    (when (and (plusp (length visible)) (< y bottom))
      (cl-tty-kit:screen-write-string target x y visible :style style))))
