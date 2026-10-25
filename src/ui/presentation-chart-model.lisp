(in-package #:ntop.ui)

(defstruct
    (chart-series
     (:constructor %make-chart-series (name style samples)))
  name
  style
  samples)

(defun make-chart-series (&key name style samples)
  (when (and name (not (stringp name)))
    (error "A chart series name must be a string or NIL."))
  (let ((sample-vector (map 'vector #'identity samples)))
    (loop with previous-time = nil
          for sample across sample-vector
          do (unless (typep sample 'ntop.shared.domain:history-sample)
               (error "Chart samples must be history samples."))
             (let ((sampled-at
                     (ntop.shared.domain:history-sample-sampled-at sample)))
               (when (and previous-time (< sampled-at previous-time))
                 (error "Chart samples must have nondecreasing timestamps."))
               (setf previous-time sampled-at)))
    (%make-chart-series name style sample-vector)))

(defstruct
    (chart-config
     (:constructor %make-chart-config
         (x-start x-end y-min y-max y-scale y-labels x-labels theme-name
          legend-position legend-width-ratio legend-height-ratio dot-marker-p)))
  x-start
  x-end
  y-min
  y-max
  y-scale
  y-labels
  x-labels
  theme-name
  legend-position
  legend-width-ratio
  legend-height-ratio
  dot-marker-p)

(defun make-chart-config (&key x-start x-end
                               (y-min 0d0)
                               (y-max 1d0)
                               (y-scale :linear)
                               y-labels
                               x-labels
                               (theme-name "default")
                               (legend-position :top-right)
                               (legend-width-ratio 1/4)
                               (legend-height-ratio 1/4)
                               dot-marker-p)
  (unless (and (or (null x-start) (realp x-start))
               (or (null x-end) (realp x-end)))
    (error "Chart X bounds must be real numbers or NIL."))
  (unless (eq (null x-start) (null x-end))
    (error "X-START and X-END must both be supplied or both be NIL."))
  (when (and x-start (> x-start x-end))
    (error "X-START must not be later than X-END."))
  (unless (and (realp y-min) (realp y-max) (< y-min y-max))
    (error "Y-MIN must be less than Y-MAX."))
  (unless (member y-scale '(:linear :log2 :log10))
    (error "Unknown chart Y scale ~S." y-scale))
  (unless (and (typep y-labels 'sequence)
               (every #'stringp y-labels))
    (error "Y-LABELS must be a sequence of strings."))
  (unless (or (stringp theme-name)
              (keywordp theme-name))
    (error "A chart theme name must be a string or keyword."))
  (unless (or (null legend-position)
              (member legend-position
                      '(:top :top-left :top-right :left :right
                        :bottom-left :bottom :bottom-right)))
    (error "Unknown chart legend position ~S." legend-position))
  (dolist (ratio (list legend-width-ratio legend-height-ratio))
    (unless (and (realp ratio) (<= 0 ratio 1))
      (error "Chart legend ratios must be between zero and one.")))
  (let ((label-list (and x-labels (coerce x-labels 'list))))
    (unless (or (null label-list)
                (and (= (length label-list) 2)
                     (every #'stringp label-list)))
      (error "X-LABELS must be NIL or a pair of strings."))
    (%make-chart-config x-start
                        x-end
                        y-min
                        y-max
                        y-scale
                        (coerce y-labels 'list)
                        (and label-list (copy-list label-list))
                        theme-name
                        legend-position
                        legend-width-ratio
                        legend-height-ratio
                        (not (null dot-marker-p)))))

(defmacro define-chart-config (name &rest options)
  `(defparameter ,name (make-chart-config ,@options)))

(define-chart-config *default-chart-config*)

(defun chart-legend-layout (area series config)
  (let* ((named-series (remove-if-not #'chart-series-name series))
         (width-limit
          (floor (+ (* (rectangle-width area)
                       (chart-config-legend-width-ratio config))
                    1/2)))
         (height-limit
          (floor (+ (* (rectangle-height area)
                       (chart-config-legend-height-ratio config))
                    1/2)))
         (entry-count (min (length named-series) (max 0 (- height-limit 2))))
         (visible-series (subseq named-series 0 entry-count)))
    (when (and visible-series (chart-config-legend-position config))
      (let* ((widths (mapcar (lambda (item)
                               (chart-text-width (chart-series-name item)))
                             visible-series))
             (minimum-required-width (+ 2 (apply #'min widths)))
             (legend-width (min width-limit (+ 2 (apply #'max widths))))
             (legend-height (+ 2 entry-count)))
        (when (and (plusp (apply #'max widths))
                   (>= width-limit minimum-required-width))
          (let ((position
                  (%chart-legend-position
                   area
                   legend-width
                   legend-height
                   (chart-config-legend-position config))))
            (values position (and position visible-series))))))))

(defun %chart-legend-position (area width height position)
  (when (and (<= width (rectangle-width area))
             (<= height (rectangle-height area)))
    (let* ((x (rectangle-x area))
           (y (rectangle-y area))
           (right (+ x (rectangle-width area)))
           (bottom (+ y (rectangle-height area))))
      (multiple-value-bind (legend-x legend-y)
          (case position
            (:top-left (values x y))
            (:top-right (values (- right width) y))
            (:top (values (+ x (floor (- (rectangle-width area) width) 2)) y))
            (:left (values x (+ y (floor (- (rectangle-height area) height) 2))))
            (:right (values (- right width)
                            (+ y (floor (- (rectangle-height area) height) 2))))
            (:bottom-left (values x (- bottom height)))
            (:bottom-right (values (- right width) (- bottom height)))
            (:bottom (values (+ x (floor (- (rectangle-width area) width) 2))
                             (- bottom height))))
        (make-rectangle :x legend-x :y legend-y :width width :height height)))))

(defun chart-text-width (text)
  (loop for character across text
        sum (cl-tty-kit:char-width character)))

(defun chart-clip-text (text width)
  (check-type width (integer 0 *))
  (with-output-to-string (output)
    (loop with used = 0
          for character across text
          for character-width = (cl-tty-kit:char-width character)
          while (<= (+ used character-width) width)
          do (write-char character output)
             (incf used character-width))))

(defun chart-truncate-text (text width)
  (check-type width (integer 0 *))
  (if (<= (chart-text-width text) width)
      text
      (concatenate 'string
                   (chart-clip-text text (max 0 (1- width)))
                   (if (plusp width) "…" ""))))

(defun chart-series-maximum-value (series)
  (loop for item in series
        append (coerce (ntop.ui:chart-series-samples item) 'list) into samples
        finally (return
                 (loop for sample in samples
                       for value = (ntop.shared.domain:history-sample-value
                                    sample)
                       when value
                         maximize value into maximum
                       finally (return (or maximum 0d0))))))
