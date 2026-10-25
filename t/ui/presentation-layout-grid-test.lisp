(in-package #:ntop.test)

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
 ((12 3 3 6) (18 5 4 9) (24 6 6 12)
  (30 8 7 15) (36 9 9 18) (42 11 10 21)
  (48 12 12 24) (54 14 13 27) (60 15 15 30))
 "matches the effective BTM 1:1:2 row spans at height ~A"
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
 "matches the effective BTM 1:1:2 outer split at 40x12"
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
    (quote (2 3 3 6 6)))))
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
                    ((40 12 :default :process 3 5)
                     (80 24 :process :process 5 15)
                     (40 12 :temperature :temperature 3 5)
                     (80 24 :disks :disks 5 15))
                    "places the selected basic table at ~Ax~A"
                    (width height selection table-widget cpu-height table-height)
                    (let* ((layout
                             (if (eq selection :default)
                                 (ntop.ui:layout-widgets width height
                                                         :mode :basic
                                                         :cpu-height cpu-height
                                                         :memory-rows 2)
                                 (ntop.ui:layout-widgets width height
                                                         :mode :basic
                                                         :cpu-height cpu-height
                                                         :memory-rows 2
                                                         :table-widget table-widget)))
                           (keys (mapcar (function car) layout)))
                      (cl-weave:expect
                       keys
                       :to-equal
                       (list :cpu :memory :network :basic-table-arrows table-widget))
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
                        (layout-test-rectangle-fields
                         (layout-test-rectangle layout table-widget)))
                       :to-equal
                       (list (list 0 0 width cpu-height)
                             (list 0 cpu-height (/ width 2) 2)
                             (list (/ width 2) cpu-height (/ width 2) 2)
                             (list 0 (+ cpu-height 2) width 2)
                             (list 0 (+ cpu-height 4) width table-height)))))
                   (cl-weave:it-each
                    ((40 12 8 3 26) (80 24 16 5 66))
                    "renders basic table labels at the CPU-derived rows and selects tables at ~Ax~A"
                    (width height core-count cpu-height expected-right-start)
                    (let* ((state
                             (ntop.app.domain:make-app-state
                              :selected-widget :cpu
                              :basic-mode-p t))
                           (config
                             (ntop.shared.application:make-application-config))
                           (snapshot
                             (ntop.app.domain:make-collection-snapshot
                              :sampled-at 1d0
                              :cpu-sample
                              (ntop.cpu.domain:make-cpu-sample
                               :sampled-at 1d0
                               :core-values
                               (loop for core-index below core-count
                                     collect
                                     (ntop.cpu.domain:make-cpu-core-value
                                      :core-index core-index
                                      :usage-percent 10d0))
                               :average-usage-percent 10d0)))
                           (right
                             (ntop.app.domain:make-app-action
                              :kind :navigation :value :right))
                           (left
                             (ntop.app.domain:make-app-action
                              :kind :navigation :value :left)))
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-selection state)
                       :to-equal :process)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-heading state)
                       :to-equal
                       (list "◄ Disks" "Temperature ►"))
                      (multiple-value-bind (placements cpu-frames)
                          (ntop.app.presentation::%screen-layout-placements
                           width height state :basic snapshot config)
                        (declare (ignore cpu-frames))
                        (cl-weave:expect
                         (mapcar (function car) placements)
                         :to-equal
                         (list :cpu :memory :network
                               :basic-table-arrows :process))
                        (cl-weave:expect
                         (list
                          (layout-test-rectangle-fields
                           (layout-test-rectangle placements :cpu))
                          (layout-test-rectangle-fields
                           (layout-test-rectangle placements :memory))
                          (layout-test-rectangle-fields
                           (layout-test-rectangle placements :network))
                          (layout-test-rectangle-fields
                           (layout-test-rectangle placements :basic-table-arrows))
                          (layout-test-rectangle-fields
                           (layout-test-rectangle placements :process)))
                         :to-equal
                         (list (list 0 0 width cpu-height)
                               (list 0 cpu-height (/ width 2) 2)
                               (list (/ width 2) cpu-height (/ width 2) 2)
                               (list 0 (+ cpu-height 2) width 2)
                               (list 0 (+ cpu-height 4) width
                                     (- height cpu-height 4))))
                        (let* ((rectangle
                                 (layout-test-rectangle placements
                                                        :basic-table-arrows))
                               (screen (cl-tty-kit:make-screen width height))
                               (labels
                                 (ntop.app.presentation::%screen-basic-table-heading
                                  state))
                               (left-label (first labels))
                               (right-label (second labels))
                               (left-width
                                 (ntop.ui::chart-text-width left-label))
                               (right-width
                                 (ntop.ui::chart-text-width right-label))
                               (left-start
                                 (1+ (ntop.ui:rectangle-x rectangle)))
                               (right-start
                                 (- (+ (ntop.ui:rectangle-x rectangle)
                                       (ntop.ui:rectangle-width rectangle))
                                    right-width
                                    1)))
                          (ntop.ui:draw-widget :basic-table-arrows
                                               screen
                                               rectangle
                                               nil
                                               config
                                               labels)
                          (let ((row
                                  (cl-tty-kit:screen-row-string
                                   screen
                                   (+ (ntop.ui:rectangle-y rectangle)
                                      (1- (ntop.ui:rectangle-height rectangle))))))
                            (cl-weave:expect
                             (layout-test-rectangle-fields rectangle)
                             :to-equal
                             (list 0 (+ cpu-height 2) width 2))
                            (cl-weave:expect
                             (list left-start right-start)
                             :to-equal
                             (list 1 expected-right-start))
                            (cl-weave:expect
                             (subseq row left-start (+ left-start left-width))
                             :to-equal
                             left-label)
                            (cl-weave:expect
                             (subseq row right-start (+ right-start right-width))
                             :to-equal
                             right-label)
                            (cl-weave:expect
                             (every (lambda (character)
                                      (char= character #\Space))
                                    (subseq row
                                            (+ left-start left-width)
                                            right-start))
                             :to-be-truthy)
                            (cl-weave:expect (char row 0)
                                             :to-equal #\Space)
                            (cl-weave:expect (char row (1- width))
                                             :to-equal #\Space))))
                      (ntop.app.application::%apply-layout-navigation
                       state right config width height)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-selection state)
                       :to-equal :temperature)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-heading state)
                       :to-equal
                       (list "◄ Process" "Disks ►"))
                      (multiple-value-bind (placements cpu-frames)
                          (ntop.app.presentation::%screen-layout-placements
                           width height state :basic snapshot config)
                        (declare (ignore cpu-frames))
                        (cl-weave:expect
                         (mapcar (function car) placements)
                         :to-equal
                         (list :cpu :memory :network
                               :basic-table-arrows :temperature))
                        (cl-weave:expect
                         (layout-test-rectangle-fields
                          (layout-test-rectangle placements :temperature))
                         :to-equal
                         (list 0 (+ cpu-height 4) width
                               (- height cpu-height 4))))
                      (ntop.app.application::%apply-layout-navigation
                       state left config width height)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-selection state)
                       :to-equal :process)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-heading state)
                       :to-equal
                       (list "◄ Disks" "Temperature ►"))
                      (ntop.app.application::%apply-layout-navigation
                       state left config width height)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-selection state)
                       :to-equal :disks)
                      (cl-weave:expect
                       (ntop.app.presentation::%screen-basic-table-heading state)
                       :to-equal
                       (list "◄ Temperature" "Process ►"))
                      (multiple-value-bind (placements cpu-frames)
                          (ntop.app.presentation::%screen-layout-placements
                           width height state :basic snapshot config)
                        (declare (ignore cpu-frames))
                        (cl-weave:expect
                         (mapcar (function car) placements)
                         :to-equal
                         (list :cpu :memory :network
                               :basic-table-arrows :disks))
                        (cl-weave:expect
                         (layout-test-rectangle-fields
                          (layout-test-rectangle placements :disks))
                         :to-equal
                         (list 0 (+ cpu-height 4) width
                               (- height cpu-height 4))))
                      (cl-weave:expect
                       (ntop.app.domain:app-state-selected-widget state)
                       :to-equal :cpu)))
                   (cl-weave:it-each
                    ((40 12) (80 24))
                    "renders BTM basic network values at ~Ax~A"
                    (width height)
                    (let* ((screen (cl-tty-kit:make-screen width height))
                           (rectangle
                             (ntop.ui:make-rectangle :x (/ width 2)
                                                     :y 1
                                                     :width (/ width 2)
                                                     :height 2))
                           (config
                             (ntop.shared.application:make-application-config
                              :basic-p t))
                           (samples
                             (vector
                              (ntop.network.domain:make-network-sample
                               :sampled-at 1d0
                               :interfaces
                               (list
                                (ntop.network.domain:make-network-interface
                                 :name "en0" :rx-bytes 1000 :tx-bytes 2000
                                 :rx-packets 10 :tx-packets 20)))
                              (ntop.network.domain:make-network-sample
                               :sampled-at 3d0
                               :interfaces
                               (list
                                (ntop.network.domain:make-network-interface
                                 :name "en0" :rx-bytes 2100 :tx-bytes 6000
                                 :rx-packets 30 :tx-packets 40)))))
                           (total-rx
                             (if (= width 40) "Total RX" "Total RX: 2.1KB"))
                           (total-tx
                             (if (= width 40) "Total TX" "Total TX: 6.0KB")))
                      (ntop.ui:draw-widget :network screen rectangle nil config samples)
                      (let ((top (cl-tty-kit:screen-row-string screen 1))
                            (bottom (cl-tty-kit:screen-row-string screen 2))
                            (snapshot (cl-tty-kit:screen-to-string screen)))
                        (cl-weave:expect
                         (list (search (if (= width 40) "RX: 550." "RX: 550.0/s") top)
                               (search total-rx top)
                               (search (if (= width 40) "TX: 2.0K" "TX: 2.0K/s") bottom)
                               (search total-tx bottom)
                               (search "Network" snapshot)
                               (find-if
                                (lambda (character)
                                  (find character "┌┐└┘─│"))
                                snapshot))
                         :to-equal
                         (list (+ 1 (/ width 2))
                               (+ 1 (/ width 2) (floor (/ width 2) 2))
                               (+ 1 (/ width 2))
                               (+ 1 (/ width 2) (floor (/ width 2) 2))
                               nil
                               nil)))))
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
