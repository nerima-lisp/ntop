(in-package #:ntop.ui)

(defparameter +braille-dot-bits+
  #2A((1 8) (2 16) (4 32) (64 128)))

(defun draw-time-chart (target rectangle config series)
  (let* ((items (coerce series 'list))
         (theme (chart-config-theme-name config))
         (graph-style (presentation-theme-style theme :graph))
         (background-style (presentation-theme-style theme :general-widget))
         (y-label-width
          (loop for label in (chart-config-y-labels config)
                maximize (chart-text-width label) into width
                finally (return (or width 0))))
         (x-label-width
          (if (and (chart-config-x-labels config)
                   (>= (rectangle-height rectangle) 3))
              (chart-text-width (first (chart-config-x-labels config)))
              0))
         (left-label-width
          (min (floor (rectangle-width rectangle) 3)
               (max y-label-width
                    (max 0 (- x-label-width
                              (if (chart-config-y-labels config) 1 0))))))
         (y-axis-p (and (chart-config-y-labels config)
                        (< left-label-width (rectangle-width rectangle))))
         (left-width left-label-width)
         (axis-width (if y-axis-p 1 0))
         (x-labels-p (and (chart-config-x-labels config)
                          (>= (rectangle-height rectangle) 3)))
         (bottom-height (if x-labels-p 2 0))
         (plot-rectangle
          (make-rectangle
           :x (+ (rectangle-x rectangle) left-width axis-width)
           :y (rectangle-y rectangle)
           :width (max 0 (- (rectangle-width rectangle) left-width axis-width))
           :height (max 0 (- (rectangle-height rectangle) bottom-height))))
         (x-range (%chart-x-range config items)))
    (cl-tty-kit:screen-fill-rect target
                                 (rectangle-x rectangle)
                                 (rectangle-y rectangle)
                                 (rectangle-width rectangle)
                                 (rectangle-height rectangle)
                                 #\Space
                                 :style background-style)
    (when y-axis-p
      (%draw-chart-y-labels target rectangle plot-rectangle config left-width graph-style)
      (%draw-chart-y-axis target plot-rectangle graph-style))
    (when x-labels-p
      (%draw-chart-x-axis target
                          plot-rectangle
                          rectangle
                          config
                          graph-style
                          y-axis-p))
    (when (and x-range
               (plusp (rectangle-width plot-rectangle))
               (plusp (rectangle-height plot-rectangle)))
      (%draw-chart-series target plot-rectangle config items x-range))
    (multiple-value-bind (legend-rectangle visible-series)
        (chart-legend-layout plot-rectangle items config)
      (when legend-rectangle
        (%draw-chart-legend target legend-rectangle visible-series theme))
      (values target legend-rectangle))))

(defun %chart-x-range (config series)
  (if (chart-config-x-start config)
      (list (chart-config-x-start config) (chart-config-x-end config))
      (let ((samples
              (loop for item in series append (coerce (chart-series-samples item) 'list))))
        (when samples
          (list (loop for sample in samples
                      minimize (ntop.shared.domain:history-sample-sampled-at sample))
                (loop for sample in samples
                      maximize (ntop.shared.domain:history-sample-sampled-at sample)))))))

(defun %draw-chart-y-labels (target rectangle plot config label-width style)
  (let* ((labels (chart-config-y-labels config))
         (count (length labels))
         (plot-bottom (+ (rectangle-y plot) (rectangle-height plot) -1)))
    (when (plusp (rectangle-height plot))
      (loop for label in labels
            for index from 0
            for offset = (if (= count 1)
                             0
                             (floor (* index (1- (rectangle-height plot)))
                                    (1- count)))
            for y = (- plot-bottom offset)
            do (%write-chart-text target
                                  (rectangle-x rectangle)
                                  y
                                  label
                                  label-width
                                  :right
                                  style)))))

(defun %draw-chart-y-axis (target plot style)
  (let ((x (1- (rectangle-x plot))))
    (loop for y from (rectangle-y plot)
          below (+ (rectangle-y plot) (rectangle-height plot))
          do (cl-tty-kit:screen-write-string target x y "│" :style style))))

(defun %draw-chart-x-axis (target plot rectangle config style y-axis-p)
  (let* ((axis-y (+ (rectangle-y plot) (rectangle-height plot)))
         (axis-x (rectangle-x plot))
         (labels-y (1+ axis-y))
         (labels (chart-config-x-labels config))
         (end-x (+ axis-x (rectangle-width plot))))
    (when (plusp (rectangle-width plot))
      (cl-tty-kit:screen-write-string
       target
       (if y-axis-p (1- axis-x) axis-x)
       axis-y
       "└"
       :style style)
      (loop for x from axis-x below end-x
            do (cl-tty-kit:screen-write-string target x axis-y "─" :style style))
      (destructuring-bind (left-label right-label) labels
        (%write-chart-text target
                           (rectangle-x rectangle)
                           labels-y
                           left-label
                           (- axis-x (rectangle-x rectangle))
                           :right
                           style)
        (%write-chart-text target
                           axis-x
                           labels-y
                           right-label
                           (rectangle-width plot)
                           :right
                           style)))))

(defun %draw-chart-series (target plot config series x-range)
  (let* ((width (rectangle-width plot))
         (height (rectangle-height plot))
         (dot-width (* width 2))
         (dot-height (* height 4))
         (dots (make-array (list dot-width dot-height) :initial-element 0))
         (markers (make-array (list width height) :initial-element nil))
         (styles (make-array (list width height) :initial-element nil)))
    (dolist (item series)
      (%rasterize-chart-series item config x-range width height dots markers styles))
    (loop for y below height
          do (loop for x below width
                   for marker = (aref markers x y)
                   for bits = (%cell-braille-bits dots x y)
                   when (or marker (plusp bits))
                     do (cl-tty-kit:screen-write-string
                         target
                         (+ (rectangle-x plot) x)
                         (+ (rectangle-y plot) y)
                         (string (or marker (code-char (+ #x2800 bits))))
                         :style (or (aref styles x y)
                                    (presentation-theme-style
                                     (chart-config-theme-name config)
                                     :graph)))))))
 (defun %rasterize-chart-series (series config x-range width height dots markers styles)
  (let ((style (or (chart-series-style series)
                   (presentation-theme-style
                    (chart-config-theme-name config)
                    :graph)))
        (previous nil))
    (loop for sample across (chart-series-samples series)
          for value = (ntop.shared.domain:history-sample-value sample)
          do (when (and previous value)
               (multiple-value-bind (start-time start-value end-time end-value)
                   (%clip-chart-segment previous sample x-range)
                 (when start-time
                   (multiple-value-bind (x0 y0)
                       (%chart-point start-time start-value config x-range width height)
                     (multiple-value-bind (x1 y1)
                         (%chart-point end-time end-value config x-range width height)
                       (%draw-braille-line dots styles x0 y0 x1 y1 style))))))
             (when value
               (let ((timestamp
                       (ntop.shared.domain:history-sample-sampled-at sample)))
                 (when (<= (first x-range) timestamp (second x-range))
                   (multiple-value-bind (x y)
                       (%chart-point timestamp value config x-range width height)
                     (%set-chart-dot dots styles x y style)
                     (when (chart-config-dot-marker-p config)
                       (setf (aref markers (floor x 2) (floor y 4)) #\•
                             (aref styles (floor x 2) (floor y 4)) style))))))
             (setf previous (and value sample)))))

(defun %clip-chart-segment (left right x-range)
  (let* ((left-time (ntop.shared.domain:history-sample-sampled-at left))
         (right-time (ntop.shared.domain:history-sample-sampled-at right))
         (start (max left-time (first x-range)))
         (end (min right-time (second x-range))))
    (when (<= start end)
      (values start
              (%interpolate-chart-value left-time
                                        (ntop.shared.domain:history-sample-value left)
                                        right-time
                                        (ntop.shared.domain:history-sample-value right)
                                        start)
              end
              (%interpolate-chart-value left-time
                                        (ntop.shared.domain:history-sample-value left)
                                        right-time
                                        (ntop.shared.domain:history-sample-value right)
                                        end)))))

(defun %interpolate-chart-value (start-time start-value end-time end-value time)
  (if (= start-time end-time)
      end-value
      (+ start-value
         (* (/ (- time start-time) (- end-time start-time))
            (- end-value start-value)))))

(defun %chart-point (timestamp value config x-range width height)
  (let* ((x-span (- (second x-range) (first x-range)))
         (x-ratio (if (zerop x-span)
                      1d0
                      (/ (- timestamp (first x-range)) x-span)))
         (minimum (chart-config-y-min config))
         (maximum (chart-config-y-max config))
         (scaled (%scale-chart-value value (chart-config-y-scale config)))
         (y-ratio (if (= minimum maximum)
                      0d0
                      (/ (- scaled minimum) (- maximum minimum))))
         (x (floor (+ (* (max 0d0 (min 1d0 x-ratio)) (1- (* width 2)))
                      0.5d0)))
         (y (floor (+ (* (- 1d0 (max 0d0 (min 1d0 y-ratio)))
                         (1- (* height 4)))
                      0.5d0))))
    (values x y)))

(defun %scale-chart-value (value scale)
  (case scale
    (:linear value)
    (:log2 (if (plusp value) (log (float value 1d0) 2d0) 0d0))
    (:log10 (if (plusp value) (log (float value 1d0) 10d0) 0d0))))

(defun %draw-braille-line (dots styles x0 y0 x1 y1 style)
  (let* ((delta-x (abs (- x1 x0)))
         (step-x (if (< x0 x1) 1 -1))
         (delta-y (- (abs (- y1 y0))))
         (step-y (if (< y0 y1) 1 -1))
         (error (+ delta-x delta-y)))
    (loop
      (%set-chart-dot dots styles x0 y0 style)
      (when (and (= x0 x1) (= y0 y1))
        (return))
      (let ((twice-error (* 2 error)))
        (when (>= twice-error delta-y)
          (incf error delta-y)
          (incf x0 step-x))
        (when (<= twice-error delta-x)
          (incf error delta-x)
          (incf y0 step-y))))
  (values)))

(defun %set-chart-dot (dots styles x y style)
  (let ((dot-x (floor x 2))
        (dot-y (floor y 4))
        (bit (aref +braille-dot-bits+ (mod y 4) (mod x 2))))
    (setf (aref dots x y) 1
          (aref styles dot-x dot-y) style)
    bit))

(defun %cell-braille-bits (dots cell-x cell-y)
  (loop for column below 2
        sum (loop for row below 4
                  for x = (+ (* cell-x 2) column)
                  for y = (+ (* cell-y 4) row)
                  when (= 1 (aref dots x y))
                    sum (aref +braille-dot-bits+ row column))))

(defun %draw-chart-legend (target rectangle series theme)
  (let ((style (presentation-theme-style theme :graph-legend)))
    (cl-tty-kit:screen-draw-box target
                                (rectangle-x rectangle)
                                (rectangle-y rectangle)
                                (rectangle-width rectangle)
                                (rectangle-height rectangle)
                                :border :single
                                :style style)
    (loop for item in series
          for row from (1+ (rectangle-y rectangle))
          below (+ (rectangle-y rectangle) (rectangle-height rectangle) -1)
          for name = (chart-truncate-text
                      (chart-series-name item)
                      (- (rectangle-width rectangle) 2))
          do (%write-chart-text target
                                (1+ (rectangle-x rectangle))
                                row
                                name
                                (- (rectangle-width rectangle) 2)
                                :left
                                (or (chart-series-style item) style)))))

(defun %write-chart-text (target x y text width alignment style)
  (let* ((visible (chart-clip-text text width))
         (visible-width (chart-text-width visible))
         (draw-x (if (eq alignment :right)
                     (+ x (max 0 (- width visible-width)))
                     x)))
    (when (and (plusp width) (plusp visible-width))
      (cl-tty-kit:screen-write-string target draw-x y visible :style style))))
