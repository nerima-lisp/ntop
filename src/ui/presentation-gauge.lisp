(in-package #:ntop.ui)

(defstruct
    (gauge-value
     (:constructor %make-gauge-value
         (start-label inner-label ratio label-style gauge-style)))
  start-label
  inner-label
  ratio
  label-style
  gauge-style)

(defun make-gauge-value (&key (start-label "") (inner-label "") ratio
                              label-style gauge-style)
  (check-type start-label string)
  (check-type inner-label string)
  (unless (and (realp ratio)
               (= ratio ratio)
               (<= (abs ratio) most-positive-double-float))
    (error "A gauge ratio must be a finite real number."))
  (%make-gauge-value start-label
                     inner-label
                     (max 0d0 (min 1d0 (float ratio 1d0)))
                     label-style
                     gauge-style))

(defun %fill-gauge-rectangle (target rectangle style)
  (let ((blank-row
          (make-string (rectangle-width rectangle) :initial-element #\Space)))
    (loop for y from (rectangle-y rectangle)
          below (+ (rectangle-y rectangle) (rectangle-height rectangle))
          do (cl-tty-kit:screen-write-string target
                                             (rectangle-x rectangle)
                                             y
                                             blank-row
                                             :style
                                             style)))
  target)

(defun draw-pipe-gauge (target rectangle gauge &key (label-limit :none))
  (unless (member label-limit '(:none :bars :start-label))
    (error "Unknown pipe gauge label limit ~S." label-limit))
  (when (and (plusp (rectangle-width rectangle))
             (plusp (rectangle-height rectangle)))
    (let* ((x (rectangle-x rectangle))
           (y (rectangle-y rectangle))
           (start-label (gauge-value-start-label gauge))
           (inner-label (gauge-value-inner-label gauge))
           (label-style (gauge-value-label-style gauge))
           (gauge-style (gauge-value-gauge-style gauge)))
      (%fill-gauge-rectangle target rectangle label-style)
      (case label-limit
        (:start-label
         (%draw-gauge-text target x y start-label
                           (rectangle-width rectangle) label-style))
        (:bars
         (%draw-gauge-pipes target rectangle gauge 0 0
                            label-style gauge-style))
        (:none
         (%draw-gauge-text target x y start-label
                           (rectangle-width rectangle) label-style)
         (%draw-gauge-pipes target
                            rectangle
                            gauge
                            (chart-text-width start-label)
                            (chart-text-width inner-label)
                            label-style
                            gauge-style)))))
  target)

(defun %draw-gauge-pipes (target rectangle gauge start-width inner-width
                          label-style gauge-style)
  (let* ((x (rectangle-x rectangle))
         (y (rectangle-y rectangle))
         (right (+ x (rectangle-width rectangle)))
         (open-x (+ x start-width))
         (bar-start (1+ open-x))
         (close-x (1- right)))
    (when (< open-x right)
      (%draw-gauge-text target open-x y "[" 1 label-style))
    (when (and (< bar-start close-x)
               (>= close-x x))
      (%draw-gauge-text target close-x y "]" 1 label-style)
      (let* ((pipe-width (- close-x bar-start))
             (filled-width (floor (* pipe-width (gauge-value-ratio gauge)))))
        (when (plusp filled-width)
          (cl-tty-kit:screen-write-string
           target
           bar-start
           y
           (make-string filled-width :initial-element (code-char 124))
           :style gauge-style))
        (when (and (plusp inner-width) (< inner-width pipe-width))
          (%draw-gauge-text target
                            (max x (- right inner-width 1))
                            y
                            (gauge-value-inner-label gauge)
                            (max 0 (- right (max x (- right inner-width 1))))
                            label-style))))))

(defun %draw-gauge-text (target x y text width style)
  (let ((visible (chart-clip-text text (max 0 width))))
    (when (and (plusp width) (plusp (chart-text-width visible)))
      (cl-tty-kit:screen-write-string target x y visible :style style))))

(defun draw-basic-cpu-gauges (target rectangle gauges)
  (let* ((items (coerce gauges 'list))
         (column-count 4)
         (base-width (floor (rectangle-width rectangle) column-count))
         (extra-width (mod (rectangle-width rectangle) column-count))
         (label-limit (cond
                        ((>= base-width 12) :none)
                        ((>= base-width 10) :bars)
                        (t :start-label))))
    (loop with index = 0
          with remaining = (length items)
          with x-offset = 0
          for column from 0 below column-count
          for columns-left = (- column-count column)
          for column-width = (+ base-width (if (< column extra-width) 1 0))
          for row-count = (min (rectangle-height rectangle)
                               (ceiling remaining columns-left))
          do (loop repeat row-count
                   for gauge = (nth index items)
                   for y-offset from 0
                   while gauge
                   for gauge-rectangle =
                     (make-rectangle
                      :x (+ (rectangle-x rectangle) x-offset 1)
                      :y (+ (rectangle-y rectangle) y-offset)
                      :width (max 0 (- column-width 2))
                      :height 1)
                   do (unless (eq label-limit :none)
                        (%fill-gauge-rectangle
                         target
                         gauge-rectangle
                         (gauge-value-label-style gauge)))
                      (if (eq label-limit :bars)
                          (let* ((start-label
                                   (gauge-value-start-label gauge))
                                 (inner-label
                                   (gauge-value-inner-label gauge))
                                 (label-style
                                   (gauge-value-label-style gauge))
                                 (x (rectangle-x gauge-rectangle))
                                 (y (rectangle-y gauge-rectangle))
                                 (width (rectangle-width gauge-rectangle))
                                 (inner-width (chart-text-width inner-label)))
                            (%draw-gauge-text target x y start-label width label-style)
                            (%draw-gauge-text
                             target
                             (max x (- (+ x width) inner-width 1))
                             y
                             inner-label
                             inner-width
                             label-style))
                          (if (eq label-limit :start-label)
                              (%draw-gauge-text target
                                                (rectangle-x gauge-rectangle)
                                                (rectangle-y gauge-rectangle)
                                                (gauge-value-inner-label gauge)
                                                (rectangle-width gauge-rectangle)
                                                (gauge-value-label-style gauge))
                              (draw-pipe-gauge target
                                               gauge-rectangle
                                               gauge
                                               :label-limit label-limit)))
                      (incf index)
                      (decf remaining))
             (incf x-offset column-width))
    target))

(defun draw-basic-memory-gauge (target rectangle used total label style
                                &optional (start-label "RAM"))
  (unless (and (realp used) (not (minusp used))
               (realp total) (not (minusp total)))
    (error "Memory gauge values must be non-negative real numbers."))
  (draw-pipe-gauge
   target
   rectangle
   (make-gauge-value :start-label start-label
                     :inner-label label
                     :ratio (if (zerop total) 0d0 (/ used total))
                     :label-style style
                     :gauge-style style)))
