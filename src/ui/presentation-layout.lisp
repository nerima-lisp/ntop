(in-package #:ntop.ui)

(defmacro define-layout (name data)
  `(defparameter ,name
     ',data))

(define-layout *default-layout*
               (:split :vertical
                       (((:fill 1)
                         (:split :horizontal
                                 (((:fill 2) (:widget :cpu-container))
                                  ((:fill 1) (:widget :memory)))))
                        ((:fill 1)
                         (:split :horizontal
                                 (((:fill 2) (:widget :network))
                                  ((:fill 2) (:widget :disks))
                                  ((:fill 1) (:widget :temperature)))))
                        ((:fill 1) (:widget :process)))))

(define-layout *basic-layout*
               (:split :vertical
                       (((:length :cpu-height) (:widget :cpu))
                        ((:length :memory-rows)
                         (:split :horizontal
                                 (((:percentage 50) (:widget :memory))
                                  ((:percentage 50) (:widget :network)))))
                        ((:length 2) (:widget :basic-table-arrows))
                        ((:min 5) (:parameter :table-widget)))))

(define-layout *expanded-layout* (:selected-widget))

(defun layout-widgets (width height
                             &key
                             (mode :default)
                             selected-widget
                             (cpu-height 1)
                             (memory-rows 2)
                             (table-widget :process))
  (ensure-layout-dimensions width height)
  (let ((rectangle (make-rectangle :x 0 :y 0 :width width :height height)))
    (case mode
      (:default
       (loop for placement in (layout-node *default-layout* rectangle nil)
             for widget = (car placement)
             for widget-rectangle = (cdr placement)
             if (eq widget :cpu-container)
               append
               (let* ((legend-width
                       (floor (* (rectangle-width widget-rectangle) 15) 100))
                      (legend-visible-p (>= legend-width 6))
                      (graph-width
                       (if legend-visible-p
                           (- (rectangle-width widget-rectangle) legend-width)
                           (rectangle-width widget-rectangle))))
                 (if legend-visible-p
                     (list (cons :cpu
                                 (offset-rectangle widget-rectangle
                                                   :horizontal
                                                   0
                                                   graph-width))
                           (cons :cpu-legend
                                 (offset-rectangle widget-rectangle
                                                   :horizontal
                                                   graph-width
                                                   legend-width)))
                     (list (cons :cpu widget-rectangle))))
             else
               collect placement))
      (:basic
       (ensure-layout-dimensions 0 cpu-height)
       (ensure-layout-dimensions 0 memory-rows)
       (layout-node *basic-layout*
                    rectangle
                    (list :cpu-height cpu-height
                          :memory-rows memory-rows
                          :table-widget table-widget)))
      (:expanded
       (if selected-widget
           (layout-node *expanded-layout*
                        rectangle
                        (list :selected-widget selected-widget))
           nil))
      (otherwise (error "Unknown layout mode ~S." mode)))))

(defun ensure-layout-dimensions (width height)
  (unless
      (and (integerp width)
           (not (minusp width))
           (integerp height)
           (not (minusp height)))
    (error "Layout dimensions must be non-negative integers: ~S x ~S."
           width
           height)))

(defun layout-node (node rectangle parameters)
  (case (first node)
    (:widget (list (cons (second node) rectangle)))
    (:parameter
     (let ((key (getf parameters (second node))))
       (if key
           (list (cons key rectangle))
           nil)))
    (:selected-widget
     (let ((key (getf parameters :selected-widget)))
       (if key
           (list (cons key rectangle))
           nil)))
    (:split (layout-split rectangle (second node) (third node) parameters))
    (otherwise (error "Unknown layout node ~S." node))))

(defun layout-split (rectangle direction children parameters)
  (let* ((constraints (mapcar #'first children))
         (sizes
          (split-sizes (rectangle-axis-size rectangle direction)
                       constraints
                       parameters)))
    (loop with offset = 0
          for child in children
          for size in sizes
          for child-rectangle = (offset-rectangle rectangle
                                                  direction
                                                  offset
                                                  size)
          append (layout-node (second child) child-rectangle parameters)
          do (incf offset size))))

(defun rectangle-axis-size (rectangle direction)
  (ecase direction
    (:horizontal (rectangle-width rectangle))
    (:vertical (rectangle-height rectangle))))

(defun offset-rectangle (rectangle direction offset size)
  (ecase direction
    (:horizontal
     (make-rectangle :x
                     (+ (rectangle-x rectangle) offset)
                     :y
                     (rectangle-y rectangle)
                     :width
                     size
                     :height
                     (rectangle-height rectangle)))
    (:vertical
     (make-rectangle :x
                     (rectangle-x rectangle)
                     :y
                     (+ (rectangle-y rectangle) offset)
                     :width
                     (rectangle-width rectangle)
                     :height
                     size))))

(defun split-sizes (total constraints parameters)
  (cond
    ((every
      (lambda (constraint)
        (eq (first constraint) :fill))
      constraints)
     (proportional-sizes total
                         (mapcar
                          (lambda (constraint)
                            (max 1
                                 (layout-value (second constraint) parameters)))
                          constraints)))
    ((every
      (lambda (constraint)
        (member (first constraint) '(:percentage :ratio)))
      constraints) (target-ratio-sizes total constraints))
    (t (mixed-sizes total constraints parameters))))

(defun mixed-sizes (total constraints parameters)
  (let* ((flexible-p
          (mapcar
           (lambda (constraint)
             (member (first constraint) '(:fill :min)))
           constraints))
         (minimums
          (mapcar
           (lambda (constraint)
             (case (first constraint)
               ((:length :min) (layout-value (second constraint) parameters))
               (:fill 0)
               ((:percentage :ratio) (constraint-target-size total constraint))
               (otherwise (error "Unknown layout constraint ~S." constraint))))
           constraints))
         (fixed-total
          (loop for size in minimums
                for flexible in flexible-p
                unless flexible
                  sum size))
         (flexible-minimum-total
          (loop for size in minimums
                for flexible in flexible-p
                when flexible
                  sum size))
         (flexible-indices
          (loop for flexible in flexible-p
                for index from 0
                when flexible
                  collect index))
         (flexible-weights
          (mapcar
           (lambda (index)
             (if (eq (first (nth index constraints)) :min)
                 1
                 (max 1
                      (layout-value (second (nth index constraints)) parameters))))
           flexible-indices))
         (sizes (copy-list minimums)))
    (cond
      ((> fixed-total total)
       (setf sizes (compressed-fixed-sizes total sizes flexible-p)))
      ((> (+ fixed-total flexible-minimum-total) total)
       (let ((shares
              (proportional-sizes (- total fixed-total) flexible-weights)))
         (loop for index in flexible-indices
               for share in shares
               do (setf (nth index sizes) share))))
      (flexible-indices
       (let* ((extra (- total fixed-total flexible-minimum-total))
              (shares (proportional-sizes extra flexible-weights)))
         (loop for index in flexible-indices
               for share in shares
               do (incf (nth index sizes) share)))))
    sizes))

(defun compressed-fixed-sizes (total sizes flexible-p)
  (let* ((indices
          (loop for flexible in flexible-p
                for index from 0
                unless flexible
                  collect index))
         (weights
          (mapcar
           (lambda (index)
             (max 1 (nth index sizes)))
           indices))
         (shares (proportional-sizes total weights))
         (result (make-list (length sizes) :initial-element 0)))
    (loop for index in indices
          for share in shares
          do (setf (nth index result) share))
    result))

(defun target-ratio-sizes (total constraints)
  (let* ((fractions (mapcar #'constraint-fraction constraints))
         (denominator (reduce #'lcm fractions :key #'second :initial-value 1))
         (weights
          (mapcar
           (lambda (fraction)
             (* (first fraction) (/ denominator (second fraction))))
           fractions))
         (cumulative 0)
         (previous 0))
    (mapcar
     (lambda (weight)
       (incf cumulative weight)
       (let ((endpoint (rounded-boundary total cumulative denominator)))
         (prog1
           (- endpoint previous)
           (setf previous endpoint))))
     weights)))

(defun constraint-fraction (constraint)
  (case (first constraint)
    (:percentage (list (second constraint) 100))
    (:ratio (list (second constraint) (max 1 (third constraint))))))

(defun constraint-target-size (total constraint)
  (destructuring-bind (numerator denominator) (constraint-fraction constraint)
    (rounded-boundary total numerator denominator)))

(defun layout-value (value parameters)
  (let ((resolved
         (if (numberp value)
             value
             (getf parameters value :missing))))
    (unless (and (integerp resolved) (not (minusp resolved)))
      (error "Layout value ~S must resolve to a non-negative integer." value))
    resolved))

(defun proportional-sizes (total weights)
  (let ((weight-total
         (loop for weight in weights
               sum weight))
        (cumulative 0)
        (previous 0))
    (mapcar
     (lambda (weight)
       (incf cumulative weight)
       (let ((endpoint (rounded-boundary total cumulative weight-total)))
         (prog1
           (- endpoint previous)
           (setf previous endpoint))))
     weights)))

(defun rounded-boundary (total numerator denominator)
  (floor (+ (* 2 total numerator) denominator) (* 2 denominator)))
