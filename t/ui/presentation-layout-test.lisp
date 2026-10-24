(in-package #:ntop.test)

(defparameter *btm-layout-width-reference*
  '((40 27 13 16 16 8) (50 33 17 20 20 10)
    (60 40 20 24 24 12)
    (70 47 23 28 28 14)
    (80 53 27 32 32 16)
    (90 60 30 36 36 18)
    (100 67 33 40 40 20)
    (110 73 37 44 44 22)
    (120 80 40 48 48 24)
    (130 87 43 52 52 26)
    (140 93 47 56 56 28)
    (150 100 50 60 60 30)
    (160 107 53 64 64 32)
    (170 113 57 68 68 34)
    (180 120 60 72 72 36)
    (190 127 63 76 76 38)
    (200 133 67 80 80 40)
    (210 140 70 84 84 42)
    (220 147 73 88 88 44)))

(defparameter *btm-layout-height-reference*
  '((12 4 4 4) (18 6 6 6) (24 8 8 8)
    (30 10 10 10) (36 12 12 12) (42 14 14 14)
    (48 16 16 16) (54 18 18 18) (60 20 20 20)))

(defun layout-test-rectangle (layout widget)
  (cdr (assoc widget layout)))

(defun layout-test-rectangle-fields (rectangle)
  (list (ntop.ui:rectangle-x rectangle)
        (ntop.ui:rectangle-y rectangle)
        (ntop.ui:rectangle-width rectangle)
        (ntop.ui:rectangle-height rectangle)))

(defun layout-test-width-fields (layout)
  (list (+ (ntop.ui:rectangle-width (layout-test-rectangle layout :cpu))
           (if (layout-test-rectangle layout :cpu-legend)
               (ntop.ui:rectangle-width
                (layout-test-rectangle layout :cpu-legend))
               0))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :memory))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :network))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :disks))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :temperature))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :process))))

(defun layout-test-grid-differences (width-reference height-reference)
  (destructuring-bind (width cpu-panel-width memory-width
                            network-width disk-width temperature-width)
      width-reference
    (destructuring-bind (height top-row-height
                                 middle-row-height process-height)
        height-reference
      (let* ((layout (ntop.ui:layout-widgets width height))
             (legend-width (floor (* cpu-panel-width 15) 100))
             (legend-visible-p (>= legend-width 6))
             (cpu-width
              (if legend-visible-p
                  (- cpu-panel-width legend-width)
                  cpu-panel-width))
             (expected
              (append
               (list (list :cpu
                           (list 0 0 cpu-width top-row-height)))
               (when legend-visible-p
                 (list (list :cpu-legend
                             (list cpu-width 0 legend-width top-row-height))))
               (list (list :memory
                           (list cpu-panel-width 0
                                 memory-width top-row-height))
                     (list :network
                           (list 0 top-row-height
                                 network-width middle-row-height))
                     (list :disks
                           (list network-width top-row-height
                                 disk-width middle-row-height))
                     (list :temperature
                           (list (+ network-width disk-width)
                                 top-row-height
                                 temperature-width
                                 middle-row-height))
                     (list :process
                           (list 0 (+ top-row-height middle-row-height)
                                 width process-height)))))
             (expected-widgets
              (mapcar (lambda (entry) (first entry)) expected))
             (actual-widgets
              (mapcar (lambda (placement) (car placement)) layout))
             (widget-differences
              (unless (equal expected-widgets actual-widgets)
                (list (list :widgets expected-widgets actual-widgets))))
             (coordinate-differences
              (loop for (widget fields) in expected
                    for actual-rectangle =
                      (layout-test-rectangle layout widget)
                    for actual =
                      (when actual-rectangle
                        (layout-test-rectangle-fields actual-rectangle))
                    unless (equal fields actual)
                      collect (list widget fields actual))))
        (append widget-differences coordinate-differences)))))

(cl-weave:describe "ratatui-compatible screen layouts"
                   (cl-weave:it
                    "expands declarative layout data into a parameter definition"
                    (cl-weave:expect
                     (macroexpand-1
                      '(ntop.ui:define-layout example (:widget :cpu)))
                     :to-equal
                     '(defparameter example
                        '(:widget :cpu))))
                   (cl-weave:it-each
                    ((40 27 13 16 16 8) (50 33 17 20 20 10)
                     (60 40 20 24 24 12)
                     (70 47 23 28 28 14)
                     (80 53 27 32 32 16)
                     (90 60 30 36 36 18)
                     (100 67 33 40 40 20)
                     (110 73 37 44 44 22)
                     (120 80 40 48 48 24)
                     (130 87 43 52 52 26)
                     (140 93 47 56 56 28)
                     (150 100 50 60 60 30)
                     (160 107 53 64 64 32)
                     (170 113 57 68 68 34)
                     (180 120 60 72 72 36)
                     (190 127 63 76 76 38)
                     (200 133 67 80 80 40)
                     (210 140 70 84 84 42)
                     (220 147 73 88 88 44))
                    "matches btm horizontal spans at width ~A"
                    (width cpu-width
                           memory-width
                           network-width
                           disk-width
                           temperature-width)
                    (let ((layout (ntop.ui:layout-widgets width 18)))
                      (cl-weave:expect (layout-test-width-fields layout)
                                       :to-equal
                                       (list cpu-width
                                             memory-width
                                             network-width
                                             disk-width
                                             temperature-width
                                             width))))
                   (cl-weave:it-each
                    ((12 4 4 4) (18 6 6 6)
                     (24 8 8 8) (30 10 10 10)
                     (36 12 12 12) (42 14 14 14)
                     (48 16 16 16) (54 18 18 18)
                     (60 20 20 20))
                    "matches btm row spans at height ~A"
                    (height top-row-height middle-row-height process-height)
                    (let ((layout (ntop.ui:layout-widgets 80 height)))
                      (cl-weave:expect
                       (list
                        (list 0 0
                              (first (layout-test-width-fields layout))
                              top-row-height)
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :network))
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :process)))
                       :to-equal
                       (list
                        (list 0 0 53 top-row-height)
                        (list 0 top-row-height 32 middle-row-height)
                        (list 0 (+ top-row-height middle-row-height)
                              80 process-height)))))
                   (cl-weave:it
                    "matches the outer-frame split at 40x12"
                    (let* ((layout (ntop.ui:layout-widgets 40 12))
                           (cpu (layout-test-rectangle layout :cpu))
                           (network (layout-test-rectangle layout :network))
                           (process (layout-test-rectangle layout :process)))
                      (cl-weave:expect
                       (list (1- (ntop.ui:rectangle-height cpu))
                             (ntop.ui:rectangle-y network)
                             (ntop.ui:rectangle-height network)
                             (ntop.ui:rectangle-y process)
                             (ntop.ui:rectangle-height process))
                       :to-equal
                       (quote (3 4 4 8 4)))))
                   (cl-weave:it "matches the full 171-size btm rectangle grid"
                                (let ((count 0)
                                      (differences nil))
                                  (dolist
                                      (width-reference
                                       *btm-layout-width-reference*)
                                    (dolist
                                        (height-reference
                                         *btm-layout-height-reference*)
                                      (incf count)
                                      (setf differences (nconc differences
                                                               (layout-test-grid-differences
                                                                width-reference
                                                                height-reference)))))
                                  (cl-weave:expect (list count differences)
                                                   :to-equal
                                                   '(171 nil))))
                   (cl-weave:it-each
                    ((80 24 4 4 :disk 14) (80 24 1 2 :process 19)
                     (40 12 1 2 :temperature 7)
                     (100 30 3 4 :battery 21))
                    "places the basic ~A table with data-dependent rows"
                    (width height
                           cpu-height
                           memory-rows
                           table-widget
                           table-height)
                    (let* ((layout
                            (ntop.ui:layout-widgets width
                                                    height
                                                    :mode
                                                    :basic
                                                    :cpu-height
                                                    cpu-height
                                                    :memory-rows
                                                    memory-rows
                                                    :table-widget
                                                    table-widget))
                           (table (layout-test-rectangle layout table-widget)))
                      (cl-weave:expect
                       (list
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :cpu))
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :memory))
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :network))
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout :basic-table-arrows))
                        (layout-test-rectangle-fields table))
                       :to-equal
                       (let ((table-y (+ cpu-height memory-rows 2)))
                         (list (list 0 0 width cpu-height)
                               (list 0 cpu-height (/ width 2) memory-rows)
                               (list (/ width 2)
                                     cpu-height
                                     (/ width 2)
                                     memory-rows)
                               (list 0 (+ cpu-height memory-rows) width 2)
                               (list 0 table-y width table-height))))))
                   (cl-weave:it-each ((:network) (:process))
                                     "expands the selected ~A widget to the screen"
                                     (widget)
                                     (cl-weave:expect
                                      (layout-test-rectangle-fields
                                       (layout-test-rectangle
                                        (ntop.ui:layout-widgets 70
                                                                24
                                                                :mode
                                                                :expanded
                                                                :selected-widget
                                                                widget)
                                        widget))
                                      :to-equal
                                      '(0 0 70 24)))
                   (cl-weave:it
                    "leaves the screen unassigned when expansion has no selection"
                    (cl-weave:expect
                     (ntop.ui:layout-widgets 70 24 :mode :expanded)
                     :to-equal
                     nil))
                   (cl-weave:it
                    "matches Ratatui Fill boundaries rounded independently"
                    (let ((layout (ntop.ui:layout-widgets 6 3)))
                      (cl-weave:expect
                       (list
                        (ntop.ui:rectangle-width
                         (layout-test-rectangle layout :network))
                        (ntop.ui:rectangle-width
                         (layout-test-rectangle layout :disks))
                        (ntop.ui:rectangle-width
                         (layout-test-rectangle layout :temperature)))
                       :to-equal
                       (list 2 3 1))))
                   (cl-weave:it-property
                    "all horizontal Fill splits conserve their widths"
                    ((width (cl-weave:gen-integer :min 0 :max 500)))
                    (let* ((layout (ntop.ui:layout-widgets width 18))
                           (widths (layout-test-width-fields layout)))
                      (cl-weave:expect
                       (list (+ (first widths) (second widths))
                             (+ (third widths) (fourth widths) (fifth widths))
                             (sixth widths))
                       :to-equal
                       (list width width width))))
                   (cl-weave:it-property
                    "higher Fill weights receive at least as much width"
                    ((width (cl-weave:gen-integer :min 0 :max 500)))
                    (let ((widths
                           (layout-test-width-fields
                            (ntop.ui:layout-widgets width 18))))
                      (cl-weave:expect
                       (and (>= (first widths) (second widths))
                            (>= (third widths) (fifth widths))
                            (>= (fourth widths) (fifth widths)))
                       :to-be
                       t)))
                   (cl-weave:it
                    "returns zero-sized placements for a zero-sized screen"
                    (let ((layout (ntop.ui:layout-widgets 0 0)))
                      (cl-weave:expect
                       (mapcar
                        (lambda (widget)
                          (layout-test-rectangle-fields
                           (layout-test-rectangle layout widget)))
                        '(:cpu :memory :network :disks :temperature :process))
                       :to-equal
                       (make-list 6 :initial-element '(0 0 0 0)))))
                   (cl-weave:it "rejects negative screen dimensions"
                                (cl-weave:expect
                                 (handler-case (ntop.ui:layout-widgets -1 12)
                                   (error ()
                                     :rejected))
                                 :to-be
                                 :rejected)))
(cl-weave:describe "layout solver edge cases"
  (cl-weave:it "compresses fixed basic rows when the screen is too short"
    (let ((layout
           (ntop.ui:layout-widgets 40 4
                                   :mode :basic
                                   :cpu-height 2
                                   :memory-rows 3)))
      (cl-weave:expect
       (mapcar
        (lambda (widget)
          (ntop.ui:rectangle-height
           (layout-test-rectangle layout widget)))
        '(:cpu :memory :basic-table-arrows :process))
       :to-equal
       '(1 2 1 0))))
  (cl-weave:it "shrinks a minimum row when the fixed rows leave too little space"
    (let ((layout
           (ntop.ui:layout-widgets 40 7
                                   :mode :basic
                                   :cpu-height 1
                                   :memory-rows 2)))
      (cl-weave:expect
       (mapcar
        (lambda (widget)
          (ntop.ui:rectangle-height
           (layout-test-rectangle layout widget)))
        '(:cpu :memory :basic-table-arrows :process))
       :to-equal
       '(1 2 2 2))))
  (cl-weave:it "distributes mixed Fill and Percentage constraints"
    (cl-weave:expect
     (ntop.ui::mixed-sizes
      10
      '((:length 1) (:fill 2) (:percentage 25) (:min 3))
      nil)
     :to-equal
     '(1 2 3 4)))
  (cl-weave:it "rounds Ratio constraints at cumulative boundaries"
    (cl-weave:expect
     (ntop.ui::split-sizes
      6
      '((:ratio 2 5) (:ratio 2 5) (:ratio 1 5))
      nil)
     :to-equal
     '(2 3 1)))
  (cl-weave:it "omits unresolved widget references"
    (let ((rectangle
           (ntop.ui::make-rectangle :x 0 :y 0 :width 1 :height 1)))
      (cl-weave:expect
       (list (ntop.ui::layout-node '(:parameter :missing) rectangle nil)
             (ntop.ui::layout-node '(:selected-widget) rectangle nil))
       :to-equal
       '(nil nil))))
  (cl-weave:it "rejects invalid basic row dimensions"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.ui:layout-widgets 10 10 :mode :basic :memory-rows -1)
           :returned)
       (error () :signaled))
     :to-be
     :signaled))
  (cl-weave:it "rejects an unknown layout mode"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.ui:layout-widgets 10 10 :mode :unknown)
           :returned)
       (error () :signaled))
     :to-be
     :signaled))
  (cl-weave:it "rejects an unknown layout node"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.ui::layout-node '(:unknown) nil nil)
           :returned)
       (error () :signaled))
     :to-be
     :signaled))
  (cl-weave:it "rejects an unknown layout constraint"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.ui::split-sizes 1 '((:unknown 1)) nil)
           :returned)
       (error () :signaled))
     :to-be
     :signaled))
  (cl-weave:it "rejects an unresolved layout value"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.ui::layout-value :missing nil)
           :returned)
       (error () :signaled))
     :to-be
     :signaled)))
