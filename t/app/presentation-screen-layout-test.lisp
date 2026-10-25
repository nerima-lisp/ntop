(in-package #:ntop.test)

(defun %wp12c-screen-field (screen row start width)
  (subseq (cl-tty-kit:screen-row-string screen row) start (+ start width)))

(defun %wp12c-screen-cell-char (screen x y)
  (cl-tty-kit:cell-char (cl-tty-kit:screen-cell screen x y)))

(defun %wp12c-pad-right (text width)
  (concatenate 'string text (make-string (- width (length text)) :initial-element #\Space)))

(defun %wp12c-golden-screen (columns rows golden)
  (let ((screen (cl-tty-kit:make-screen columns rows)))
    (with-open-file
        (stream (asdf:system-relative-pathname "ntop/test" (format nil "t/app/goldens/~A" golden))
                :direction
                :input
                :external-format
                :utf-8)
      (loop for row from 0 below rows
            for line = (read-line stream nil nil)
            while line
            do (cl-tty-kit:screen-write-string screen 0 row line)))
    screen))

(cl-weave:describe
 "FR-001 condition C fixture screen layout"
 (cl-weave:it-each
  ((40 12 "application-40x12.txt" 9 10 ((1 8 "PID(p)" "100") (10 9 "Name(n)" "alpha")
     (20 9 "CPU%(c)▼" "5.0%") (30 9 "Mem%(m)" "6.3%")))
   (80 24 "application-80x24.txt" 17 18 ((1 18 "PID(p)" "100") (20 19 "Name(n)" "alpha")
     (40 19 "CPU%(c)▼" "5.0%") (60 19 "Mem%(m)" "6.3%")))
   (160 48 "application-160x48.txt" 33 34 ((1 38 "PID(p)" "100") (40 39 "Name(n)" "alpha")
     (80 39 "CPU%(c)▼" "5.0%") (120 39 "Mem%(m)" "6.3%"))))
  "~D×~D process columns match the fixed golden's BTM layout"
  (width height golden header-row first-process-row fields)
  (let ((screen (%wp12c-golden-screen width height golden)))
    (cl-weave:expect (cl-tty-kit:screen-width screen) :to-equal width)
    (cl-weave:expect (cl-tty-kit:screen-height screen) :to-equal height)
    (loop for (x field-width heading value) in fields
          do (cl-weave:expect
              (%wp12c-screen-field screen header-row x field-width)
              :to-equal
              (%wp12c-pad-right heading field-width))
             (cl-weave:expect
              (%wp12c-screen-field screen first-process-row x field-width)
              :to-equal
              (%wp12c-pad-right value field-width)))
    (loop for field in fields
          for next-field in (rest fields)
          for x = (first field)
          for field-width = (second field)
          do (cl-weave:expect (+ x field-width 1)
                              :to-equal
                              (first next-field))
             (cl-weave:expect
              (%wp12c-screen-cell-char screen (+ x field-width) header-row)
              :to-equal
              #\Space))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 26 3 ((1 "100%") (2 "  0%")) nil nil 27)
  (80 24 "application-80x24.txt" 45 7 ((1 "100%") (4 "  0%")) 46 52 53)
  (160 48 "application-160x48.txt" 90 15 ((1 "100%") (12 "  0%")) 91 106 107))
 "~D×~D CPU graph and legend regions follow the BTM split"
 (width height golden graph-right graph-bottom y-labels legend-left
  legend-right memory-left)
 (let ((screen (%wp12c-golden-screen width height golden)))
   (cl-weave:expect (%wp12c-screen-field screen 0 8 14)
                    :to-equal
                    "0.25 0.50 0.75")
   (dolist (label y-labels)
     (destructuring-bind (row expected) label
       (cl-weave:expect (%wp12c-screen-field screen row 1 4)
                        :to-equal
                        expected)))
   (cl-weave:expect (%wp12c-screen-cell-char screen graph-right 0)
                    :to-equal
                    #\┐)
   (cl-weave:expect (%wp12c-screen-cell-char screen graph-right graph-bottom)
                    :to-equal
                    #\┘)
   (cl-weave:expect (%wp12c-screen-cell-char screen memory-left 0)
                    :to-equal
                    #\┌)
   (if legend-left
       (progn
         (cl-weave:expect (%wp12c-screen-cell-char screen legend-left 0)
                          :to-equal
                          #\┌)
         (cl-weave:expect (%wp12c-screen-cell-char screen legend-right 0)
                          :to-equal
                          #\┐)
         (cl-weave:expect (%wp12c-screen-cell-char screen legend-left graph-bottom)
                          :to-equal
                          #\└)
         (cl-weave:expect (%wp12c-screen-cell-char screen legend-right graph-bottom)
                          :to-equal
                          #\┘))
       (cl-weave:expect graph-right :to-equal 26))))
 (cl-weave:it-each
 ((80 24 47 5 (1 2 3 4 5 6) ("CPU  " "     " "All  " "AVG  " "0    " "1    "))
  (160 48 92 14 (1 2 3 4 5 6)
   ("CPU    Use    " "              " "All           "
    "AVG    48%    " "CPU0   35%    " "CPU1   62%    ")))
 "~D×~D fixture CPU rows follow BTM's table layout"
 (width height label-x label-width row-numbers expected-rows)
 ;; The 8-row CPU pane retains its header and both per-core rows after borders.
 (let ((screen (nth-value 1 (%wp12b-screen-render width height))))
   (cl-weave:expect
    (mapcar (lambda (row)
              (%wp12c-screen-field screen row label-x label-width))
            row-numbers)
    :to-equal
    expected-rows)))
 (cl-weave:it "places the visible network chart legend at the fixture's top right"
  (let ((screen
          (%wp12c-golden-screen 160 48 "application-160x48.txt")))
    (cl-weave:expect
     (mapcar (lambda (row)
               (%wp12c-screen-field screen row 33 30))
             '(17 18 19 20))
     :to-equal
     '("┌────────────────────────────┐"
       "│RX: 4.8Kb/s    All: 2.4KB   │"
       "│TX: 9.6Kb/s    All: 5.2KB   │"
       "└────────────────────────────┘"))))
 (cl-weave:it-each
  ((40 12 "application-40x12.txt" 4 5 ((5 " 24.")) nil nil)
   (80 24 "application-80x24.txt" 5 6 ((9 " 24.0") (10 " 16.0") (11 "  8.0") (12 "  0Kb")) 14 28)
   (160 48 "application-160x48.txt" 5 6
   ((17 " 24.0") (21 " 16.0")
    (25 "  8.0") (28 "  0Kb"))
   30 60))
  "~D×~D network axis labels keep the BTM width and cell positions"
  (width height golden label-width axis-x labels x-label-row now-x)
  (let ((screen (%wp12c-golden-screen width height golden)))
    (dolist (label labels)
      (destructuring-bind (row expected) label
        (cl-weave:expect (%wp12c-screen-field screen row 1 label-width)
                         :to-equal
                         expected)
        (cl-weave:expect (%wp12c-screen-cell-char screen axis-x row)
                         :to-equal
                         #\│)))
    (when x-label-row
      (cl-weave:expect (%wp12c-screen-field screen x-label-row 3 4)
                       :to-equal
                       " 60s")
      (cl-weave:expect (%wp12c-screen-field screen x-label-row now-x 3)
                       :to-equal
                       " 0s"))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 6 1 20 1 ((25 1 #x2880)))
  (80 24 "application-80x24.txt" 6 1 39 4
   ((44 2 #x28A0) (43 3 #x2880) (44 3 #x281C) (43 4 #x2801)))
  (160 48 "application-160x48.txt" 6 1 84 14
   ((89 5 #x28A0) (89 6 #x2847) (88 7 #x2878) (89 7 #x2870)
    (87 8 #x2880) (88 8 #x2870) (89 8 #x2801) (87 9 #x28A0)
    (88 9 #x2803) (89 9 #x2803) (86 10 #x2820) (87 10 #x2803)
    (88 10 #x2801) (86 11 #x2810) (87 11 #x2801))))
 "~D×~D fixture CPU plot cells match BTM's Braille raster"
 (screen-width screen-height golden plot-x plot-y plot-width plot-height expected-cells)
 ;; These cells use the same fixture values as the source-backed chart vector
 ;; in presentation-chart-drawing-test.lisp and BTM's 60-second CPU plot.
 ;; BTM's point rounding/Bresenham and 2×4 color-aware Braille packing are in
 ;; bottom-0.14.9/src/canvas/components/time_series/vendored/canvas.rs:82-120,148-163
 ;; and bottom-0.14.9/src/canvas/components/time_series/vendored/grid.rs:148-163.
 (let ((screen
         (%wp12c-golden-screen screen-width screen-height golden)))
   (cl-weave:expect
    (loop for y from plot-y below (+ plot-y plot-height)
          append (loop for x from plot-x below (+ plot-x plot-width)
                       for character = (%wp12c-screen-cell-char screen x y)
                       when (<= #x2800 (char-code character) #x28FF)
                         collect (list x y (char-code character))))
    :to-equal
    expected-cells)))
 (cl-weave:it-each
  ((40 12 "application-40x12.txt")
   (80 24 "application-80x24.txt")
   (160 48 "application-160x48.txt"))
  "~D×~D fixture output remains byte-equal to the fixed golden"
  (width height golden)
  (multiple-value-bind (areas screen output)
      (%wp12b-screen-render width height)
    (declare (ignore areas screen))
    (cl-weave:expect
     (%wp12b-screen-bytes
      (if (and (plusp (length output))
               (char= (char output (1- (length output))) #\Newline))
          (subseq output 0 (1- (length output)))
          output))
     :to-equal
     (%wp12b-screen-golden-bytes golden))))
 (cl-weave:it-each
 ((80 24 "application-80x24.txt" 47 5
   (1 2 3 4 5 6)
   ("CPU  " "     " "All  " "AVG  " "0    " "1    "))
  (160 48 "application-160x48.txt" 92 14 (1 2 3 4 5 6)
   ("CPU    Use    " "              " "All           "
    "AVG    48%    " "CPU0   35%    " "CPU1   62%    ")))
 "~D×~D fixed golden CPU rows follow BTM's table layout"
 (width height golden label-x label-width row-numbers expected-rows)
 ;; BTM table widths and rendered row offsets come from cpu_graph.rs and
 ;; data_table/column.rs, draw.rs in bottom-0.14.9.
 (let ((screen (%wp12c-golden-screen width height golden)))
   (cl-weave:expect
    (mapcar (lambda (row)
              (%wp12c-screen-field screen row label-x label-width))
            row-numbers)
    :to-equal
    expected-rows)))
 (cl-weave:it "renders the fixture memory legend only when BTM's width limit fits"
  (let ((screen-80 (nth-value 1 (%wp12b-screen-render 80 24)))
        (screen-160 (nth-value 1 (%wp12b-screen-render 160 48))))
    ;; BTM uses 3/4 of the chart area for the legend; the 26-cell label
    ;; cannot fit in the 18-cell allowance at width 80, but fits at width 160.
    ;; See mem_graph.rs:23-35,197-201 and vendored.rs:604-640.
    (cl-weave:expect
     (loop for row below 24
           thereis (search "RAM:" (cl-tty-kit:screen-row-string screen-80 row)))
     :to-equal
     nil)
    (cl-weave:expect
     (loop for row below 24
           thereis (search "SWP:" (cl-tty-kit:screen-row-string screen-80 row)))
     :to-equal
     nil)
    (cl-weave:expect (%wp12c-screen-field screen-160 2 133 26)
                     :to-equal
                     "│RAM: 62%   5.0GiB/8.0GiB│")
    (cl-weave:expect (%wp12c-screen-field screen-160 3 133 26)
                     :to-equal
                     "│SWP: 50%   1.0GiB/2.0GiB│")))
 (cl-weave:it-each
  ((40 12 "application-40x12.txt" 28 3 ((1 "100") (2 "  0")))
   (80 24 "application-80x24.txt" 54 4 ((1 "100%") (4 "  0%")))
   (160 48 "application-160x48.txt" 108 4 ((1 "100%") (12 "  0%"))))
  "~D×~D fixture Memory axis labels follow BTM chart geometry"
  (width height golden label-x label-width labels)
  (let ((screen (%wp12c-golden-screen width height golden)))
    (dolist (label labels)
      (destructuring-bind (row expected) label
        (cl-weave:expect (%wp12c-screen-field screen row label-x label-width)
                         :to-equal
                         expected)))))
 (cl-weave:it-each
  ((40 12 "application-40x12.txt" 28 1 11 1 nil)
   (80 24 "application-80x24.txt" 59 1 20 4 ((78 4 #x28C0)))
   (160 48 "application-160x48.txt" 113 1 46 14 ((157 12 #x28C0) (158 12 #x28C0))))
  "~D×~D fixture Memory plot cells match BTM's raster"
  (screen-width screen-height golden plot-x plot-y plot-width plot-height expected-cells)
  ;; BTM charts memory usage as a ratio inside a fixed trailing time window.
  ;; Fixture history byte values are tiny relative to the total-byte y bounds,
  ;; so their points quantize to the chart's bottom Braille row.
  ;; The 3/4-width top-right legend masks the upper-right plot cells.
  ;; BTM sources: collection/memory.rs:28-36, app/data/time_series.rs:123-149,
  ;; mem_graph.rs:197-201, time_series/base.rs:111-129,
  ;; vendored/canvas.rs:148-163, vendored/grid.rs:148-163.
  (let ((golden-screen (%wp12c-golden-screen screen-width screen-height golden))
        (fixture-screen (nth-value 1 (%wp12b-screen-render screen-width screen-height))))
    (cl-weave:expect
     (loop for y from plot-y below (+ plot-y plot-height)
           append (loop for x from plot-x below (+ plot-x plot-width)
                        for character = (%wp12c-screen-cell-char fixture-screen x y)
                        when (<= #x2800 (char-code character) #x28FF)
                          collect (list x y (char-code character))))
     :to-equal
     expected-cells)
    (cl-weave:expect
     (loop for y from plot-y below (+ plot-y plot-height)
           append (loop for x from plot-x below (+ plot-x plot-width)
                        for character = (%wp12c-screen-cell-char golden-screen x y)
                        when (<= #x2800 (char-code character) #x28FF)
                          collect (list x y (char-code character))))
     :to-equal
     expected-cells)))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 6 5 9 2 ((14 5 #x28A0) (14 6 #x2808)))
  (80 24 "application-80x24.txt" 7 9 24 4 ((30 10 #x2846) (30 11 #x2818) (30 12 #x2808)))
  (160 48 "application-160x48.txt" 7 17 56 12
   ((61 21 #x28B8) (62 22 #x2847) (62 23 #x28A3)
    (62 24 #x2808) (61 25 #x28A0) (62 26 #x2823))))
 "~D×~D fixture Network plot cells match BTM's 60-second Braille raster"
 (screen-width screen-height golden plot-x plot-y plot-width plot-height expected-cells)
 ;; BTM maps fixture t=2,3 points to relative seconds -1,0 inside [-60,0].
 ;; Source fixture rates are RX/TX 6400/16000 then 4800/9600 bits/s;
 ;; BTM's linear bound is 0..24000. The top-right legend masks the covered
 ;; 160x48 cells after RX then TX draw order and per-cell color reset.
 ;; BTM sources: network_graph.rs:78-83,384-441, vendored/points.rs:31-98,
 ;; vendored/canvas.rs:82-120,148-163, vendored/grid.rs:148-163.
 (let ((screen (%wp12c-golden-screen screen-width screen-height golden)))
   (cl-weave:expect
    (loop for y from plot-y below (+ plot-y plot-height)
          append (loop for x from plot-x below (+ plot-x plot-width)
                       for character = (%wp12c-screen-cell-char screen x y)
                       when (<= #x2800 (char-code character) #x28FF)
                         collect (list x y (char-code character))))
    :to-equal
    expected-cells)))
 )
