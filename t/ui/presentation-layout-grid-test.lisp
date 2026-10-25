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
                    ((40 40 23 17 20 20) (50 50 29 21 25 25)
                     (60 60 34 26 30 30) (70 70 40 30 35 35)
                     (80 80 46 34 40 40) (90 90 51 39 45 45)
                     (100 100 57 43 50 50) (110 110 63 47 55 55)
                     (120 120 69 51 60 60) (130 130 74 56 65 65)
                     (140 140 80 60 70 70) (150 150 86 64 75 75)
                     (160 160 91 69 80 80) (170 170 97 73 85 85)
                     (180 180 103 77 90 90) (190 190 109 81 95 95)
                     (200 200 114 86 100 100) (210 210 120 90 105 105)
                     (220 220 126 94 110 110))
                    "matches BTM 0.14.9 built-in horizontal spans at width ~A"
                    (width cpu-panel-width memory-width side-width network-width process-width)
                    (let ((layout (ntop.ui:layout-widgets width 18)))
                      (cl-weave:expect
                       (layout-test-width-fields layout)
                       :to-equal
                       (list cpu-panel-width memory-width side-width
                             network-width process-width))))
                   (cl-weave:it-each
                    ((12 4 4 4) (18 5 8 5) (24 7 10 7)
                     (30 9 12 9) (36 11 14 11) (42 13 16 13)
                     (48 14 20 14) (54 16 22 16) (60 18 24 18))
                    "matches BTM 0.14.9 built-in 30:40:30 row spans at height ~A"
                    (height top-row-height middle-row-height bottom-row-height)
                    (let ((layout (ntop.ui:layout-widgets 80 height)))
                      (cl-weave:expect
                       (list
                        (ntop.ui:rectangle-height
                         (layout-test-rectangle layout :cpu))
                        (ntop.ui:rectangle-y
                         (layout-test-rectangle layout :memory))
                        (ntop.ui:rectangle-height
                         (layout-test-rectangle layout :memory))
                        (ntop.ui:rectangle-y
                         (layout-test-rectangle layout :network))
                        (ntop.ui:rectangle-height
                         (layout-test-rectangle layout :network))
                        (ntop.ui:rectangle-y
                         (layout-test-rectangle layout :process))
                        (ntop.ui:rectangle-height
                         (layout-test-rectangle layout :process)))
                       :to-equal
                       (list top-row-height top-row-height middle-row-height
                             (+ top-row-height middle-row-height)
                             bottom-row-height
                             (+ top-row-height middle-row-height)
                             bottom-row-height))))
                   (cl-weave:it
                    "matches BTM 0.14.9 DEFAULT_LAYOUT at 220x60"
                    (let ((layout (ntop.ui:layout-widgets 220 60)))
                      (cl-weave:expect
                       (mapcar
                        (lambda (widget)
                          (layout-test-rectangle-fields
                           (layout-test-rectangle layout widget)))
                        '(:cpu :cpu-legend :memory :temperature :disks
                          :network :process))
                       :to-equal
                       '((0 0 187 18)
                         (187 0 33 18)
                         (0 18 126 24)
                         (126 18 94 12)
                         (126 30 94 12)
                         (0 42 110 18)
                         (110 42 110 18)))))
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
                    ((40 12 8 3 26) (40 12 16 3 26) (80 24 16 5 66))
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
                    "matches BTM Fill boundaries for the default split ratios"
                    (let* ((layout (ntop.ui:layout-widgets 6 3))
                           (widths (layout-test-width-fields layout)))
                      (cl-weave:expect widths :to-equal '(6 3 3 3 3))))
                   (cl-weave:it-property
                    "conserves each horizontal default-layout split"
                    ((width (cl-weave:gen-integer :min 0 :max 500)))
                    (let ((widths
                           (layout-test-width-fields
                            (ntop.ui:layout-widgets width 18))))
                      (cl-weave:expect
                       (list (first widths)
                             (+ (second widths) (third widths))
                             (+ (fourth widths) (fifth widths)))
                       :to-equal
                       (list width width width))))
                   (cl-weave:it-property
                    "default Fill ratios preserve their relative widths"
                    ((width (cl-weave:gen-integer :min 0 :max 500)))
                    (let ((widths
                           (layout-test-width-fields
                            (ntop.ui:layout-widgets width 18))))
                      (cl-weave:expect
                       (and (>= (first widths) (second widths))
                            (>= (second widths) (third widths))
                            (<= (abs (- (fourth widths) (fifth widths))) 1))
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
