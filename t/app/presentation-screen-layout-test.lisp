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
 ((40 12 "application-40x12.txt" 9 10 ((21 8 "PID(p)" "100") (30 8 "Name(n)" "alpha")))
  (80 24 "application-80x24.txt" 18 19
   ((41 8 "PID(p)" "100") (50 9 "Name(n)" "alpha")
    (60 9 "CPU%(c)▼" "5.0%") (70 9 "Mem%(m)" "6.3%")))
  (160 48 "application-160x48.txt" 35 36
   ((81 18 "PID(p)" "100") (100 19 "Name(n)" "alpha")
    (120 19 "CPU%(c)▼" "5.0%") (140 19 "Mem%(m)" "6.3%"))))
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
 ((40 12 "application-40x12.txt" 33 3 ((1 "100%") (2 "  0%")) 34 39 0 4)
  (80 24 "application-80x24.txt" 67 6 ((1 "100%") (5 "  0%")) 68 79 0 7)
  (160 48 "application-160x48.txt" 135 13 ((1 "100%") (10 "  0%")) 136 159 0 14))
 "~D×~D CPU graph and legend regions follow the BTM split"
 (width height golden graph-right graph-bottom y-labels legend-left
  legend-right memory-left memory-top)
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
   (cl-weave:expect (%wp12c-screen-cell-char screen memory-left memory-top)
                    :to-equal
                    #\┌)
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
                    #\┘)))
 (cl-weave:it-each
 ((80 24 69 10 (1 2 3 4 5)
   ("CPU  Use  " "          " "All       " "AVG  48%  " "0    35%  "))
  (160 48 137 22 (1 2 3 4 5 6 7)
   ("CPU        Use        " "                      " "All                   "
    "AVG        48%        " "CPU0       35%        " "CPU1       62%        "
    "                      ")))
 "~D×~D fixture CPU rows follow BTM's table layout"
 (width height label-x label-width row-numbers expected-rows)
 (let ((screen (nth-value 1 (%wp12b-screen-render width height))))
   (cl-weave:expect
    (mapcar (lambda (row)
              (%wp12c-screen-field screen row label-x label-width))
            row-numbers)
    :to-equal
    expected-rows)))
 (cl-weave:it "places the visible network chart legend at the fixture's bottom right"
  (let ((screen
          (%wp12c-golden-screen 160 48 "application-160x48.txt")))
    (cl-weave:expect
     (mapcar (lambda (row)
               (%wp12c-screen-field screen row 49 30))
             (quote (35 36 37 38)))
     :to-equal
     '("┌────────────────────────────┐"
       "│RX: 4.8Kb/s    All: 2.4KB   │"
       "│TX: 9.6Kb/s    All: 5.2KB   │"
       "└────────────────────────────┘"))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 5 6 ((9 " 24.0") (10 " 16.0")) 8 4 nil nil)
  (80 24 "application-80x24.txt" 5 6
   ((18 " 24.0") (20 " 16.0") (21 "  8.0") (22 "  0Kb")) 17 7 nil nil)
  (160 48 "application-160x48.txt" 5 6
   ((35 " 24.0") (38 " 16.0") (41 "  8.0") (44 "  0Kb"))
   34 14 46 76))
 "~D×~D network axis labels keep the BTM width and cell positions"
 (width height golden label-width axis-x labels network-y network-height
  x-label-row now-x)
 (let ((screen (%wp12c-golden-screen width height golden)))
   (dolist (label labels)
     (destructuring-bind (row expected) label
       (cl-weave:expect (%wp12c-screen-field screen row 1 label-width)
                        :to-equal
                        expected)
       (cl-weave:expect (%wp12c-screen-cell-char screen axis-x row)
                        :to-equal
                        #\│)))
   (if x-label-row
       (progn
         (cl-weave:expect (%wp12c-screen-field screen x-label-row 3 4)
                          :to-equal
                          " 60s")
         (cl-weave:expect (%wp12c-screen-field screen x-label-row now-x 3)
                          :to-equal
                          " 0s"))
       (progn
         ;; BTM hides time labels below seven widget rows.
         (cl-weave:expect
          (loop for row from network-y below (+ network-y network-height)
                thereis (search "60s" (cl-tty-kit:screen-row-string screen row)))
          :to-equal
          nil)
         (cl-weave:expect
          (loop for row from network-y below (+ network-y network-height)
                thereis (search " 0s" (cl-tty-kit:screen-row-string screen row)))
          :to-equal
          nil)))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 6 1 27 2
   ((32 1 #x2880) (31 2 #x2820) (32 2 #x280A)))
  (80 24 "application-80x24.txt" 6 1 61 5
   ((66 2 #x2880) (65 3 #x2880) (66 3 #x2860) (64 4 #x2880)
    (65 4 #x2814) (66 4 #x2801) (64 5 #x2810) (65 5 #x2801)))
  (160 48 "application-160x48.txt" 6 1 129 10
   ((134 4 #x2880) (133 5 #x2880) (134 5 #x280E) (132 6 #x2880)
    (133 6 #x2880) (134 6 #x280E) (131 7 #x2880) (132 7 #x2880)
    (133 7 #x280E) (134 7 #x2814) (130 8 #x2860) (131 8 #x2812)
    (132 8 #x2801) (133 8 #x2801) (130 9 #x2814) (131 9 #x2801))))
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
 ((80 24 "application-80x24.txt" 69 10 (1 2 3 4 5)
   ("CPU  Use  " "          " "All       " "AVG  48%  " "0    35%  "))
  (160 48 "application-160x48.txt" 137 22 (1 2 3 4 5 6 7)
   ("CPU        Use        " "                      " "All                   "
    "AVG        48%        " "CPU0       35%        " "CPU1       62%        "
    "                      ")))
 "~D×~D fixed golden CPU rows follow BTM's table layout"
 (width height golden label-x label-width row-numbers expected-rows)
 (let ((screen (%wp12c-golden-screen width height golden)))
   (cl-weave:expect
    (mapcar (lambda (row)
              (%wp12c-screen-field screen row label-x label-width))
            row-numbers)
    :to-equal
    expected-rows)))
 (cl-weave:it "renders the fixture memory legend in both BTM default width layouts"
  (let ((screen-80 (nth-value 1 (%wp12b-screen-render 80 24)))
        (screen-160 (nth-value 1 (%wp12b-screen-render 160 48))))
    (cl-weave:expect
     (mapcar (lambda (row)
               (%wp12c-screen-field screen-80 row 19 26))
             '(8 9 10 11))
     :to-equal
     '("┌────────────────────────┐"
       "│RAM: 62%   5.0GiB/8.0GiB│"
       "│SWP: 50%   1.0GiB/2.0GiB│"
       "└────────────────────────┘"))
    (cl-weave:expect
     (mapcar (lambda (row)
               (%wp12c-screen-field screen-160 row 64 26))
             '(15 16 17 18))
     :to-equal
     '("┌────────────────────────┐"
       "│RAM: 62%   5.0GiB/8.0GiB│"
       "│SWP: 50%   1.0GiB/2.0GiB│"
       "└────────────────────────┘"))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 1 4 ((5 "100%") (6 "  0%")))
  (80 24 "application-80x24.txt" 1 4 ((8 "100%") (13 "  0%")))
  (160 48 "application-160x48.txt" 1 4 ((15 "100%") (30 "  0%"))))
 "~D×~D fixture Memory axis labels follow BTM chart geometry"
 (width height golden label-x label-width labels)
 (let ((screen (%wp12c-golden-screen width height golden)))
   (dolist (label labels)
     (destructuring-bind (row expected) label
       (cl-weave:expect (%wp12c-screen-field screen row label-x label-width)
                        :to-equal
                        expected)))))
 (cl-weave:it-each
 ((40 12 "application-40x12.txt" 6 5 16 2 ((21 6 #x28C0)))
  (80 24 "application-80x24.txt" 6 8 39 8
   ((43 13 #x28C0) (44 13 #x28C0)))
  (160 48 "application-160x48.txt" 6 15 84 18
   ((86 30 #x2880) (87 30 #x28C0) (88 30 #x28C0) (89 30 #x28C0))))
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
 ((40 12 "application-40x12.txt" 7 9 12 2
   ((18 9 #x28A0) (18 10 #x2808)))
  (80 24 "application-80x24.txt" 7 18 32 5
   ((38 19 #x2844) (38 20 #x28B1) (38 21 #x2884)))
  (160 48 "application-160x48.txt" 7 35 72 11
   ((78 39 #x2847) (78 40 #x28B1) (77 42 #x2810) (78 42 #x2884))))
 "~D×~D fixture Network plot cells match BTM's 60-second Braille raster"
 (screen-width screen-height golden plot-x plot-y plot-width plot-height expected-cells)
 ;; BTM maps fixture t=2,3 points to relative seconds -1,0 inside [-60,0].
 ;; Source fixture rates are RX/TX 6400/16000 then 4800/9600 bits/s;
 ;; BTM's linear bound is 0..24000. The lower Network pane legend masks cells
 ;; only where its frame overlaps the chart plot.
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
 (cl-weave:it
  "passes the global scroll-position setting to the Temperature table"
  ;; BTM 0.14.9: src/canvas/components/data_table/draw.rs:107-128.
  (let ((config (%wp12b-screen-config))
        (state (ntop.app.domain:make-app-state)))
    (setf
     (ntop.shared.application:application-config-show-table-scroll-position-p config)
     t)
    (multiple-value-bind (table-state table-config rows)
        (ntop.app.presentation::%screen-widget-inputs
         :temperature nil nil state config nil)
      (declare (ignore table-state rows))
      (cl-weave:expect
       (ntop.ui:table-config-show-scroll-position-p table-config)
       :to-be
       t))))
 )
