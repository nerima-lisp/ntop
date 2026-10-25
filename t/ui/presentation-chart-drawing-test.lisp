(in-package #:ntop.test)
;; BTM 0.14.9 allocates and clips chart axes in
;; bottom/src/canvas/components/time_series/vendored.rs:658-688,770-792.
(cl-weave:describe "time-chart drawing"
                   (cl-weave:it "draws axes, series, and a positioned legend"
                                (let* ((screen (cl-tty-kit:make-screen 30 12))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :name
                                                                    "CPU"
                                                                    :samples
                                                                    (%chart-test-samples
                                                                     '((0d0 0d0)
                                                                       (5d0
                                                                        60d0)
                                                                       (10d0
                                                                        100d0))))))
                                       (config
                                        (ntop.ui:make-chart-config :x-start
                                                                   0d0
                                                                   :x-end
                                                                   10d0
                                                                   :y-min
                                                                   0d0
                                                                   :y-max
                                                                   100d0
                                                                   :y-labels
                                                                   '("0" "50"
                                                                         "100")
                                                                   :x-labels
                                                                   '("10s" "0s")
                                                                   :legend-width-ratio
                                                                   1
                                                                   :legend-height-ratio
                                                                   1)))
                                  (multiple-value-bind (target legend)
                                      (ntop.ui:draw-time-chart screen
                                                               (%chart-test-rectangle
                                                                0
                                                                0
                                                                30
                                                                12)
                                                               config
                                                               series)
                                    (let ((text
                                           (%chart-test-screen-text screen 12)))
                                      (cl-weave:expect (eq target screen)
                                                       :to-be
                                                       t)
                                      (cl-weave:expect
                                       (and legend
                                            (plusp
                                             (ntop.ui:rectangle-width legend)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "CPU" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "└" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "─" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "10s" text)))
                                       :to-be
                                       t)
                                      (cl-weave:expect
                                       (not (null (search "0s" text)))
                                       :to-be
                                       t)))))
                   (cl-weave:it "draws dot markers when enabled"
                                (let* ((screen (cl-tty-kit:make-screen 12 5))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :samples
                                                                    (%chart-test-samples
                                                                     '((0d0 1d0)
                                                                       (1d0 2d0))))))
                                       (config
                                        (ntop.ui:make-chart-config :x-start
                                                                   0d0
                                                                   :x-end
                                                                   1d0
                                                                   :y-min
                                                                   0d0
                                                                   :y-max
                                                                   2d0
                                                                   :dot-marker-p
                                                                   t)))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            12
                                                            5)
                                                           config
                                                           series)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "•"
                                             (%chart-test-screen-text screen 5))))
                                   :to-be
                                   t)))
                   (cl-weave:it "draws an X axis without Y labels"
                                (let ((screen (cl-tty-kit:make-screen 16 5)))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            16
                                                            5)
                                                           (ntop.ui:make-chart-config
                                                            :x-start
                                                            0
                                                            :x-end
                                                            1
                                                            :x-labels
                                                            '("1s" "0s"))
                                                           nil)
                                  (cl-weave:expect
                                   (not
                                    (null
                                     (search "─"
                                             (%chart-test-screen-text screen 5))))
                                   :to-be
                                   t)))
                   (cl-weave:it
                    "skips the X axis when the rectangle is too short"
                    (let ((screen (cl-tty-kit:make-screen 12 2)))
                      (ntop.ui:draw-time-chart screen
                                               (%chart-test-rectangle 0 0 12 2)
                                               (ntop.ui:make-chart-config
                                                :x-start
                                                0
                                                :x-end
                                                1
                                                :x-labels
                                                '("1s" "0s"))
                                               nil)
                      (cl-weave:expect
                       (search "─" (%chart-test-screen-text screen 2))
                       :to-be
                       nil)))
                   (cl-weave:it "clears empty charts and returns no legend"
                                (let ((screen (cl-tty-kit:make-screen 10 4)))
                                  (multiple-value-bind (target legend)
                                      (ntop.ui:draw-time-chart screen
                                                               (%chart-test-rectangle
                                                                0
                                                                0
                                                                10
                                                                4)
                                                               (ntop.ui:make-chart-config)
                                                               nil)
                                    (cl-weave:expect (eq target screen)
                                                     :to-be
                                                     t)
                                    (cl-weave:expect legend :to-be nil)
                                    (cl-weave:expect
                                     (%chart-test-screen-text screen 4)
                                     :to-equal
                                     (format nil
                                             "          ~%          ~%          ~%          ~%")))))
                   (cl-weave:it "does not draw samples with missing values"
                                (let* ((screen (cl-tty-kit:make-screen 10 4))
                                       (samples
                                        (%chart-test-samples
                                         '((0d0 nil) (1d0 nil))))
                                       (series
                                        (list
                                         (ntop.ui:make-chart-series :samples
                                                                    samples))))
                                  (ntop.ui:draw-time-chart screen
                                                           (%chart-test-rectangle
                                                            0
                                                            0
                                                            10
                                                            4)
                                                           (ntop.ui:make-chart-config
                                                            :x-start
                                                            0d0
                                                            :x-end
                                                            1d0
                                                            :y-min
                                                            0
                                                            :y-max
                                                            1)
                                                           series)
                                  (cl-weave:expect
                                   (search "⠀"
                                           (%chart-test-screen-text screen 4))
                                   :to-be
                                   nil)))
                   (cl-weave:it
                    "places a single Y label at the bottom of the plot"
                    (let ((screen (cl-tty-kit:make-screen 8 4)))
                      (ntop.ui::%draw-chart-y-labels screen
                                                     (%chart-test-rectangle 0
                                                                            0
                                                                            8
                                                                            4)
                                                     (%chart-test-rectangle 2
                                                                            0
                                                                            6
                                                                            4)
                                                     (ntop.ui:make-chart-config
                                                      :y-labels
                                                      '("only"))
                                                     4
                                                     nil)
                      (cl-weave:expect
                       (not
                        (null
                         (search "only" (cl-tty-kit:screen-row-string screen 3))))
                       :to-be
                       t)))
                   (cl-weave:it
                    "caps Y labels before placing the axis and plot"
                    (let* ((screen (cl-tty-kit:make-screen 6 4))
                           (config
                            (ntop.ui:make-chart-config
                             :x-start
                             0d0
                             :x-end
                             1d0
                             :y-min
                             0d0
                             :y-max
                             1d0
                             :y-labels
                             '("long" "0")
                             :dot-marker-p
                             t))
                           (series
                            (list
                             (ntop.ui:make-chart-series
                              :samples
                              (%chart-test-samples
                               '((0d0 1d0) (1d0 1d0)))))))
                      (ntop.ui:draw-time-chart
                       screen
                       (%chart-test-rectangle 0 0 6 4)
                       config
                       series)
                      (cl-weave:expect
                       (cl-tty-kit:screen-row-string screen 0)
                       :to-equal
                       " 0│•⠉•")
                      (cl-weave:expect
                       (cl-tty-kit:screen-row-string screen 3)
                       :to-equal
                       "lo│   ")))
                   (cl-weave:it
                    "matches BTM line rasterization and keeps empty plot widths safe"
                    (let ((screen (cl-tty-kit:make-screen 2 3)))
                      (ntop.ui:draw-time-chart screen
                                               (%chart-test-rectangle 0 0 2 3)
                                               (ntop.ui:make-chart-config
                                                :y-labels
                                                '("long label")
                                                :x-labels
                                                '("left" "right"))
                                               nil)
                      (cl-weave:expect
                       (length (%chart-test-screen-text screen 3))
                       :to-equal
                       9)
                      (let ((dots (make-array '(6 4) :initial-element 0))
                            (styles (make-array '(3 1) :initial-element nil)))
                        (ntop.ui::%draw-braille-line dots styles 0 0 4 2 :line)
                        (cl-weave:expect
                         (loop for y below 3
                               append (loop for x below 5
                                            when (= 1 (aref dots x y))
                                            collect (list x y)))
                         :to-equal
                         '((0 0) (1 0) (2 1) (3 1) (4 2)))
                        (ntop.ui::%set-chart-dot dots styles 0 0 :first)
                        (ntop.ui::%set-chart-dot dots styles 1 0 :first)
                        (ntop.ui::%set-chart-dot dots styles 0 1 :second)
                        (cl-weave:expect
                         (ntop.ui::%cell-braille-bits dots 0 0)
                         :to-equal
                         2)
                        (cl-weave:expect (aref styles 0 0)
                                         :to-equal
                                         :second))))
                   (cl-weave:it
                    "matches BTM high-slope tie rasterization"
                    (let ((dots (make-array '(4 8) :initial-element 0))
                          (styles (make-array '(2 2) :initial-element nil)))
                      (ntop.ui::%draw-braille-line dots styles 0 0 2 4 :line)
                      (cl-weave:expect
                       (loop for y below 5
                             append (loop for x below 3
                                          when (= 1 (aref dots x y))
                                            collect (list x y)))
                       :to-equal
                       '((0 0) (0 1) (1 2) (1 3) (2 4)))))
                   (cl-weave:it
                    "maps sparse CPU history into BTM's 60-second plot"
                    ;; BTM 0.14.9 percent.rs:30-35 sets a 60-second x window
                    ;; and 100.5% y bound; points.rs:46-56 anchors samples
                    ;; to the newest time; vendored/canvas.rs:82-120,148-163
                    ;; rounds and rasterizes dots into the 2x4 grid.
                    (let* ((screen (cl-tty-kit:make-screen 20 2))
                           (config
                             (ntop.ui:make-chart-config
                              :x-start -57d0
                              :x-end 3d0
                              :y-min 0d0
                              :y-max 100.5d0
                              :y-labels (list "  0%" "100%")
                              :legend-position nil))
                           (x-range (list -57d0 3d0))
                           (samples
                             (%chart-test-samples
                              (list (list 1d0 20d0)
                                    (list 2d0 32d0)
                                    (list 3d0 48d0))))
                           (series
                             (list (ntop.ui:make-chart-series
                                    :name "AVG"
                                    :samples samples))))
                      (cl-weave:expect
                       (multiple-value-list
                        (ntop.ui::%chart-point 1d0 20d0 config x-range 15 2))
                       :to-equal
                       (list 28 6))
                      (cl-weave:expect
                       (multiple-value-list
                        (ntop.ui::%chart-point 2d0 32d0 config x-range 15 2))
                       :to-equal
                       (list 29 5))
                      (cl-weave:expect
                       (multiple-value-list
                        (ntop.ui::%chart-point 3d0 48d0 config x-range 15 2))
                       :to-equal
                       (list 29 4))
                      (ntop.ui:draw-time-chart
                       screen
                       (%chart-test-rectangle 0 0 20 2)
                       config
                       series)
                      (cl-weave:expect
                       (cl-tty-kit:cell-char (cl-tty-kit:screen-cell screen 19 1))
                       :to-be
                       (code-char #x281c))
                      (cl-weave:expect
                       (cl-tty-kit:cell-char (cl-tty-kit:screen-cell screen 18 1))
                       :to-be
                       (code-char 32))
                      (let ((dots (make-array '(4 8) :initial-element 0))
                            (styles (make-array '(2 2) :initial-element nil)))
                        (ntop.ui::%draw-braille-line dots styles 0 0 2 4 :line)
                        (cl-weave:expect
                         (loop for y below 5
                               append (loop for x below 3
                                            when (= 1 (aref dots x y))
                                              collect (list x y)))
                         :to-equal
                         (list (list 0 0) (list 0 1) (list 1 2)
                               (list 1 3) (list 2 4))))))
                   (cl-weave:it-each
 ((20 2 ((19 0 #x2880) (19 1 #x280E)))
  (39 6 ((38 2 #x28B0) (38 3 #x2878) (37 4 #x2814)
          (38 4 #x2801) (37 5 #x2801)))
  (84 14 ((83 5 #x28B0) (83 6 #x2847)
           (82 7 #x28B8) (83 7 #x2870)
           (82 8 #x28A0) (83 8 #x2803)
           (81 9 #x2880) (82 9 #x2807) (83 9 #x280E)
           (80 10 #x2880) (81 10 #x280E) (82 10 #x280E)
           (80 11 #x2808) (81 11 #x280E)
           (80 12 #x2808))))
 "matches BTM 0.14.9 CPU fixture Braille cells"
 (width height expected-cells)
 ;; BTM uses a 60-second CPU window and a 100.5% ceiling
 ;; (src/components/time_series/percent.rs:30-37;
 ;; src/components/time_series.rs:108-112). The fixture's 1s, 2s,
 ;; and 3s timestamps therefore map to -2s, -1s, and 0s in that window.
 ;; CPU data is collected AVG, CPU0, CPU1 then drawn in reverse order
 ;; (src/collection/cpu/sysinfo.rs:10-36;
 ;; src/canvas/widgets/cpu_graph.rs:129-146). Canvas maps rounded points,
 ;; uses strict-positive Bresenham steps, and resets Braille cells when
 ;; series colors differ (src/canvas/components/time_series/vendored/canvas.rs:
 ;; 82-120,148-163; src/canvas/components/time_series/vendored/grid.rs:148-163).
 (let* ((screen (cl-tty-kit:make-screen width height))
        (config
          (ntop.ui:make-chart-config
           :x-start -57d0
           :x-end 3d0
           :y-min 0d0
           :y-max 100.5d0
           :theme-name "default"
           :legend-position nil))
        (cpu-colors (ntop.ui:presentation-theme-style "default" :cpu-colors))
        (series
          (list
           (ntop.ui:make-chart-series
            :name "CPU1"
            :style (second cpu-colors)
            :samples (%chart-test-samples '((1d0 16d0) (2d0 40d0) (3d0 62d0))))
           (ntop.ui:make-chart-series
            :name "CPU0"
            :style (first cpu-colors)
            :samples (%chart-test-samples '((1d0 12d0) (2d0 25d0) (3d0 35d0))))
           (ntop.ui:make-chart-series
            :name "AVG"
            :style (ntop.ui:presentation-theme-style "default" :avg-cpu)
            :samples (%chart-test-samples '((1d0 20d0) (2d0 32d0) (3d0 48d0)))))))
   (ntop.ui:draw-time-chart
    screen
    (%chart-test-rectangle 0 0 width height)
    config
    series)
   (cl-weave:expect
    (loop for y below height
          append (loop for x below width
                       for character =
                         (cl-tty-kit:cell-char
                          (cl-tty-kit:screen-cell screen x y))
                       unless (char= character #\Space)
                         collect (list x y (char-code character))))
    :to-equal
    expected-cells))))
