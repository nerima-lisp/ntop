(in-package #:ntop.test)
(defparameter *btm-layout-width-reference*
  ;; The active config is preferred over DEFAULT_LAYOUT in
  ;; bottom/src/options.rs:860-870. Its configured widget ratios are applied in
  ;; bottom/src/options/config/layout.rs:74-76,125-203 and become Fill
  ;; constraints in bottom/src/app/layout_manager.rs:729-731,764-766.
  ;; bottom/src/canvas.rs:432-445 splits each level with Ratatui Layout.
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
  ;; The effective FR-001 profile sets outer row ratios to 1:1:2: CPU+Mem,
  ;; Net+Disks+Temp, Process. BTM defaults omitted ratios to 1 and applies
  ;; BottomRow ratios in layout.rs:64-76,209-211; layout_manager.rs:729-731
  ;; maps them to Fill constraints, split vertically in canvas.rs:432-433.
  ;; Ratatui Fill boundaries yield the weighted cell heights below.
  (quote ((12 3 3 6) (18 5 4 9) (24 6 6 12)
          (30 8 7 15) (36 9 9 18) (42 11 10 21)
          (48 12 12 24) (54 14 13 27) (60 15 15 30))))

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
