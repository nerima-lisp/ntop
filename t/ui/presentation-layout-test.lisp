(in-package #:ntop.test)
(defparameter *btm-layout-width-reference*
  ;; BTM 0.14.9 DEFAULT_LAYOUT groups CPU across the first row, Memory 4:3
  ;; against the stacked Temperature/Disk area, and Network:Process as 1:1.
  ;; Source: bottom-0.14.9/src/constants.rs:195-218 and src/canvas.rs:432-445.
  '((40 40 23 17 20 20) (50 50 29 21 25 25)
    (60 60 34 26 30 30) (70 70 40 30 35 35)
    (80 80 46 34 40 40) (90 90 51 39 45 45)
    (100 100 57 43 50 50) (110 110 63 47 55 55)
    (120 120 69 51 60 60) (130 130 74 56 65 65)
    (140 140 80 60 70 70) (150 150 86 64 75 75)
    (160 160 91 69 80 80) (170 170 97 73 85 85)
    (180 180 103 77 90 90) (190 190 109 81 95 95)
    (200 200 114 86 100 100) (210 210 120 90 105 105)
    (220 220 126 94 110 110)))

(defparameter *btm-layout-height-reference*
  ;; BTM 0.14.9 DEFAULT_LAYOUT uses three outer row ratios 30:40:30.
  ;; Source: bottom-0.14.9/src/constants.rs:195-218; row splits use Ratatui
  ;; Layout in src/canvas.rs:432-445.
  '((12 4 4 4) (18 5 8 5) (24 7 10 7)
    (30 9 12 9) (36 11 14 11) (42 13 16 13)
    (48 14 20 14) (54 16 22 16) (60 18 24 18)))

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
        (ntop.ui:rectangle-width (layout-test-rectangle layout :temperature))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :network))
        (ntop.ui:rectangle-width (layout-test-rectangle layout :process))))

(defun layout-test-grid-differences (width-reference height-reference)
  (destructuring-bind (width cpu-panel-width memory-width side-width
                            network-width process-width)
      width-reference
    (destructuring-bind (height top-row-height middle-row-height bottom-row-height)
        height-reference
      (let* ((layout (ntop.ui:layout-widgets width height))
             (legend-width (floor (* cpu-panel-width 15) 100))
             (legend-visible-p (>= legend-width 6))
             (cpu-width (if legend-visible-p
                            (- cpu-panel-width legend-width)
                            cpu-panel-width))
             (temperature-height (ceiling middle-row-height 2))
             (disk-height (- middle-row-height temperature-height))
             (expected
              (append
               (list (list :cpu (list 0 0 cpu-width top-row-height)))
               (when legend-visible-p
                 (list (list :cpu-legend
                             (list cpu-width 0 legend-width top-row-height))))
               (list (list :memory
                           (list 0 top-row-height memory-width middle-row-height))
                     (list :temperature
                           (list memory-width top-row-height
                                 side-width temperature-height))
                     (list :disks
                           (list memory-width (+ top-row-height temperature-height)
                                 side-width disk-height))
                     (list :network
                           (list 0 (+ top-row-height middle-row-height)
                                 network-width bottom-row-height))
                     (list :process
                           (list network-width (+ top-row-height middle-row-height)
                                 process-width bottom-row-height)))))
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

(cl-weave:describe "layout solver edge cases"
  (cl-weave:it "compresses CPU-first basic rows when the screen is too short"
  (let ((layout
          (ntop.ui:layout-widgets 40 4
                                  :mode :basic
                                  :memory-rows 3
                                  :table-widget :process)))
    (cl-weave:expect
     (list
      (layout-test-rectangle-fields (layout-test-rectangle layout :cpu))
      (layout-test-rectangle-fields (layout-test-rectangle layout :memory))
      (layout-test-rectangle-fields (layout-test-rectangle layout :network))
      (layout-test-rectangle-fields
       (layout-test-rectangle layout :basic-table-arrows))
      (layout-test-rectangle-fields (layout-test-rectangle layout :process)))
     :to-equal
     (quote ((0 0 40 1)
             (0 1 20 2)
             (20 1 20 2)
             (0 3 40 1)
             (0 4 40 0))))))
  (cl-weave:it "places CPU-first basic widgets at BTM rows"
  (let ((layout
          (ntop.ui:layout-widgets 40 7
                                  :mode :basic
                                  :cpu-height 1
                                  :memory-rows 2
                                  :table-widget :process)))
    (cl-weave:expect
     (list
      (layout-test-rectangle-fields (layout-test-rectangle layout :cpu))
      (layout-test-rectangle-fields (layout-test-rectangle layout :memory))
      (layout-test-rectangle-fields (layout-test-rectangle layout :network))
      (layout-test-rectangle-fields
       (layout-test-rectangle layout :basic-table-arrows))
      (layout-test-rectangle-fields (layout-test-rectangle layout :process)))
     :to-equal
     (quote ((0 0 40 1)
             (0 1 20 2)
             (20 1 20 2)
             (0 3 40 2)
             (0 5 40 2)))))
  (dolist (case (quote ((40 12 3 5) (80 24 5 15))))
    (destructuring-bind (width height cpu-height table-height) case
      (let ((layout
              (ntop.ui:layout-widgets width height
                                      :mode :basic
                                      :cpu-height cpu-height
                                      :memory-rows 2
                                      :table-widget :process)))
        (cl-weave:expect
         (mapcar (function car) layout)
         :to-equal
         (quote (:cpu :memory :network :basic-table-arrows :process)))
        (cl-weave:expect
         (list
          (layout-test-rectangle-fields (layout-test-rectangle layout :cpu))
          (layout-test-rectangle-fields (layout-test-rectangle layout :memory))
          (layout-test-rectangle-fields (layout-test-rectangle layout :network))
          (layout-test-rectangle-fields
           (layout-test-rectangle layout :basic-table-arrows))
          (layout-test-rectangle-fields (layout-test-rectangle layout :process)))
         :to-equal
         (list (list 0 0 width cpu-height)
               (list 0 cpu-height (/ width 2) 2)
               (list (/ width 2) cpu-height (/ width 2) 2)
               (list 0 (+ cpu-height 2) width 2)
               (list 0 (+ cpu-height 4) width table-height)))))))
  (cl-weave:it "distributes mixed Fill and Percentage constraints"
    (cl-weave:expect
     (ntop.ui::mixed-sizes
      10
      '((:length 1) (:fill 2) (:percentage 25) (:min 3))
      nil)
     :to-equal
     '(1 2 3 4)))
  (cl-weave:it "rounds Ratio constraints and places the CPU legend on the left like BTM 0.14.9"
    (cl-weave:expect
     (ntop.ui::split-sizes
      6
      '((:ratio 2 5) (:ratio 2 5) (:ratio 1 5))
      nil)
     :to-equal
     '(2 3 1))
    ;; BTM 0.14.9: src/canvas/widgets/cpu_graph.rs:31-56,70-79.
    (let* ((layout (ntop.ui:layout-widgets 80 24 :cpu-left-legend-p t))
           (cpu (layout-test-rectangle layout :cpu))
           (legend (layout-test-rectangle layout :cpu-legend)))
      (cl-weave:expect (ntop.ui:rectangle-x legend) :to-be 0)
      (cl-weave:expect
       (+ (ntop.ui:rectangle-x legend) (ntop.ui:rectangle-width legend))
       :to-equal
       (ntop.ui:rectangle-x cpu))))
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
