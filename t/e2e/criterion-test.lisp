(in-package #:ntop.e2e.test)

(defun criterion-test-write (grid row column sgr text)
  (ntop.e2e:parse-ansi
   (format nil
           "~c[~d;~dH~c[~am~a"
           #\Escape
           (1+ row)
           (1+ column)
           #\Escape
           sgr
           text)
   grid
   (lambda (value)
     value)
   (lambda (condition)
     (error condition))))

(defun criterion-test-grid (rows columns process-name
                            &optional (process-header-sgr "1;38;5;2;49")
                              (process-header-text "Count Name(n) CPU%(c)"))
  (let* ((grid (ntop.e2e:make-terminal-grid :rows rows :cols columns))
         (process-frame-top (- rows 4))
         (process-header-row (- rows 3))
         (process-data-row (- rows 2))
         (process-frame-bottom (1- rows)))
    (criterion-test-write grid 0 0 "38;5;7;49" "┌─")
    (criterion-test-write grid 0 3 "1;38;5;3;48;5;4" "CPU")
    (criterion-test-write grid 0 7 "38;5;8;49" "45.70 45.58 53.02")
    (criterion-test-write grid 0 (- columns 7) "38;2;1;2;3;48;2;4;5;6" "Memory")
    (criterion-test-write grid 0 (1- columns) "38;5;7;49" "┐")
    (loop for row from 1 below (1- rows)
          do (criterion-test-write grid row 0 "38;5;7;49" "│")
             (criterion-test-write grid row (1- columns) "38;5;7;49" "│"))
    (criterion-test-write grid (1- rows) 0 "38;5;7;49" "└")
    (criterion-test-write grid (1- rows) (1- columns) "38;5;7;49" "┘")
    (criterion-test-write grid process-frame-top 1 "38;5;7;49" "┌")
    (criterion-test-write grid process-frame-top 2 "38;5;8;49" "─ Processes ─")
    (criterion-test-write grid process-frame-top (1- columns) "38;5;7;49" "┐")
    (criterion-test-write grid process-header-row 1 "38;5;7;49" "│")
    (criterion-test-write grid
                          process-header-row
                          2
                          process-header-sgr
                          process-header-text)
    (criterion-test-write grid process-header-row (1- columns) "38;5;7;49" "│")
    (criterion-test-write grid process-data-row 1 "38;5;7;49" "│")
    (criterion-test-write grid process-data-row 2 "38;5;1;49" process-name)
    (criterion-test-write grid process-data-row (1- columns) "38;5;7;49" "│")
    (criterion-test-write grid process-frame-bottom 1 "38;5;7;49" "└")
    (criterion-test-write grid
                          process-frame-bottom
                          (1- columns)
                          "38;5;7;49"
                          "┘")
    grid))

(defun criterion-test-has-entry-p (projection entry)
  (not (null (find entry (aref projection 2) :test #'equalp))))

(defun criterion-test-hierarchy-grid (widget-left legend-offset title-sgr)
  (let* ((rows 18)
         (columns 60)
         (grid (ntop.e2e:make-terminal-grid :rows rows :cols columns))
         (widget-top 2)
         (widget-bottom (- rows 3))
         (widget-right (+ widget-left 53))
         (legend-top 7)
         (legend-bottom 10)
         (legend-left (+ widget-left legend-offset))
         (legend-right (+ legend-left 12)))
    (labels ((draw-frame (top left bottom right)
               (let ((sgr "38;5;7;49"))
                 (criterion-test-write grid top left sgr "┌")
                 (loop for column from (1+ left) below right
                       do (criterion-test-write grid top column sgr "─"))
                 (criterion-test-write grid top right sgr "┐")
                 (loop for row from (1+ top) below bottom
                       do (criterion-test-write grid row left sgr "│")
                          (criterion-test-write grid row right sgr "│"))
                 (criterion-test-write grid bottom left sgr "└")
                 (loop for column from (1+ left) below right
                       do (criterion-test-write grid bottom column sgr "─"))
                 (criterion-test-write grid bottom right sgr "┘"))))
      (draw-frame 0 0 (1- rows) (1- columns))
      (draw-frame widget-top widget-left widget-bottom widget-right)
      (criterion-test-write grid widget-top (+ widget-left 3) title-sgr "CPU")
      (draw-frame legend-top legend-left legend-bottom legend-right)
      (criterion-test-write grid legend-top (+ legend-left 3) "38;5;8;49" "Legend")
      grid)))

(cl-weave:describe "FR-001 condition A criterion projection"
                   (cl-weave:it "expands declarative vocabulary data"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e:define-criterion-vocabulary
                                    (:sample "value")))
                                 :to-equal
                                 '(defparameter ntop.e2e::*criterion-vocabulary*
                                    '((:sample "value")))))
                   (cl-weave:it
                    "preserves frame, title, header, and style while excluding data"
                    (let* ((grid
                            (criterion-test-grid 12 40 "SENTINEL_PROCESS_NAME"))
                           (projection (ntop.e2e:project-criterion-grid grid))
                           (entries (aref projection 2)))
                      (dolist
                          (expected
                           (list
                            #("border" 0
                                       0
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("border" 0
                                       39
                                       "┐"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("border" 1
                                       0
                                       "│"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("border" 1
                                       39
                                       "│"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("border" 11
                                       0
                                       "└"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("border" 11
                                       39
                                       "┘"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)
                            #("title" 0 3 "C" #("indexed" 3) #("indexed" 4) 1)
                            #("title" 0
                                      33
                                      "M"
                                      #("rgb" #(1 2 3))
                                      #("rgb" #(4 5 6))
                                      0)
                            #("process-header" 9
                                               2
                                               "C"
                                               #("indexed" 2)
                                               #("default" nil)
                                               1)))
                        (cl-weave:expect
                         (criterion-test-has-entry-p projection expected)
                         :to-be
                         t))
                      (cl-weave:expect
                       (search "SENTINEL_PROCESS_NAME"
                               (prin1-to-string entries))
                       :to-be
                       nil)
                      (cl-weave:expect
                       (search "45.70" (prin1-to-string entries))
                       :to-be
                       nil)))
                   (cl-weave:it "projects all 171 specified terminal dimensions"
                                (let ((dimensions
                                       (loop for
                                         columns
                                         from
                                         40
                                         to
                                         220
                                         by
                                         10
                                         append
                                         (loop for
                                           rows
                                           from
                                           12
                                           to
                                           60
                                           by
                                           6
                                           collect
                                           (list columns rows)))))
                                  (cl-weave:expect (length dimensions)
                                                   :to-be
                                                   171)
                                  (dolist (dimension dimensions)
                                    (destructuring-bind (columns rows) dimension
                                      (let* ((sentinel
                                              (format nil
                                                      "SENTINEL_~d_~d"
                                                      columns
                                                      rows))
                                             (projection
                                              (ntop.e2e:project-criterion-grid
                                               (criterion-test-grid rows
                                                                    columns
                                                                    sentinel)))
                                             (entries (aref projection 2)))
                                        (cl-weave:expect
                                         (list (aref projection 0)
                                               (aref projection 1))
                                         :to-equal
                                         (list columns rows))
                                        (cl-weave:expect
                                         (search sentinel
                                                 (prin1-to-string entries))
                                         :to-be
                                         nil)
                                        (cl-weave:expect
                                         (search "45.70"
                                                 (prin1-to-string entries))
                                         :to-be
                                         nil)))))))

(cl-weave:describe "FR-001 condition A process header boundary"
  (cl-weave:it
   "does not project dynamic process labels as a table header"
   (let* ((projection
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid 12 40 "Count Name(n) CPU%(c)")))
          (entries (aref projection 2)))
     (cl-weave:expect
      (loop for entry across entries
            thereis (and (string= (aref entry 0) "process-header")
                         (= (aref entry 1) 10)))
      :to-be
      nil)))
  (cl-weave:it
 "projects plain process headers and preserves their default attributes"
 (let ((projection
         (ntop.e2e:project-criterion-grid
          (criterion-test-grid 12 40 "live process" ""))))
   (cl-weave:expect
    (criterion-test-has-entry-p
     projection
     #("process-header" 9 2 "C" #("default" nil) #("default" nil) 0))
    :to-be
    t))))

(cl-weave:describe "FR-001 condition A frame hierarchy"
  (cl-weave:it-each
   ((:widget-outer-frame-move-diff
     4
     12
     "1;38;5;3;48;5;4"
     nil)
    (:fixed-title-attribute-change-diff
     3
     12
     "1;38;5;5;48;5;4"
     nil)
    (:inner-legend-only-move-equal
     3
     13
     "1;38;5;3;48;5;4"
     t))
   "checks condition A projection case ~A"
   (case-name widget-left legend-offset title-sgr expected)
   (declare (ignore case-name))
   (let* ((baseline
            (ntop.e2e:project-criterion-grid
             (criterion-test-hierarchy-grid
              3
              12
              "1;38;5;3;48;5;4")))
          (candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-hierarchy-grid
              widget-left
              legend-offset
              title-sgr))))
     (cl-weave:expect (equalp baseline candidate)
                      :to-be
                      expected)))
  (cl-weave:it
 "excludes CPU load averages and adjacent border fill while preserving frame geometry"
 (labels ((draw-panel (grid left right)
            (criterion-test-write grid 0 left "38;5;7;49" "┌")
            (loop for column from (1+ left) below right
                  do (criterion-test-write grid 0 column "38;5;7;49" "─"))
            (criterion-test-write grid 0 right "38;5;7;49" "┐")
            (loop for row from 1 below 11
                  do (criterion-test-write grid row left "38;5;7;49" "│")
                     (criterion-test-write grid row right "38;5;7;49" "│"))
            (criterion-test-write grid 11 left "38;5;7;49" "└")
            (loop for column from (1+ left) below right
                  do (criterion-test-write grid 11 column "38;5;7;49" "─"))
            (criterion-test-write grid 11 right "38;5;7;49" "┘"))
          (make-cpu-grid (load-averages cpu-right)
            (let* ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 50))
                   (memory-left (1+ cpu-right)))
              (draw-panel grid 0 cpu-right)
              (draw-panel grid memory-left 49)
              (criterion-test-write grid
                                    0
                                    2
                                    "1;38;5;3;49"
                                    (concatenate 'string "CPU ─ " load-averages))
              (criterion-test-write grid 0 (+ memory-left 2) "38;5;8;49" "Memory")
              grid)))
   (let* ((baseline
            (ntop.e2e:project-criterion-grid
             (make-cpu-grid "68.24 51.07 50.54" 32)))
          (unavailable
            (ntop.e2e:project-criterion-grid
             (make-cpu-grid "N/A N/A N/A" 32)))
          (no-values
            (ntop.e2e:project-criterion-grid
             (make-cpu-grid "" 32)))
          (shifted-frame
            (ntop.e2e:project-criterion-grid
             (make-cpu-grid "68.24 51.07 50.54" 31))))
     (cl-weave:expect
      (and (ntop.e2e::criterion-projections-equal-p baseline unavailable)
           (ntop.e2e::criterion-projections-equal-p baseline no-values))
      :to-be
      t)
     (cl-weave:expect
      (ntop.e2e::criterion-projections-equal-p baseline shifted-frame)
      :to-be
      nil)
     (dolist (projection (list baseline unavailable no-values))
       (dolist (expected
                (list #("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)
                      #("border" 0 32 "┐" #("indexed" 7) #("default" nil) 0)
                      #("border" 1 0 "│" #("indexed" 7) #("default" nil) 0)
                      #("border" 1 32 "│" #("indexed" 7) #("default" nil) 0)
                      #("border" 11 0 "└" #("indexed" 7) #("default" nil) 0)
                      #("border" 11 32 "┘" #("indexed" 7) #("default" nil) 0)
                      #("border" 0 6 "─" #("indexed" 3) #("default" nil) 1)))
         (cl-weave:expect (criterion-test-has-entry-p projection expected)
                          :to-be
                          t))
       (loop for column from 8 below 32
             do (cl-weave:expect
                 (loop for entry across (aref projection 2)
                       never (and (string= (aref entry 0) "border")
                                  (= (aref entry 1) 0)
                                  (= (aref entry 2) column)))
                 :to-be
                 t)))
     (cl-weave:expect
      (criterion-test-has-entry-p
       shifted-frame
       #("border" 0 31 "┐" #("indexed" 7) #("default" nil) 0))
      :to-be
      t))))
  (cl-weave:it
  "classifies selection movement in the input direction"
  (labels ((selection-grid (selected-index)
             (let ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 40))
                   (frames (list (list 0 3 0 11)
                                 (list 0 3 14 25)
                                 (list 4 7 0 11)
                                 (list 4 7 14 25)
                                 (list 8 11 0 11)
                                 (list 8 11 14 25))))
               (loop for frame in frames
                     for index from 0
                     do
                        (destructuring-bind (top bottom left right) frame
                          (let ((style (if (eql index selected-index)
                                           "38;5;12;49"
                                           "39;49")))
                            (criterion-test-write grid top left style "┌")
                            (criterion-test-write grid top right style "┐")
                            (criterion-test-write grid bottom left style "└")
                            (criterion-test-write grid bottom right style "┘"))))
               grid)))
    (let ((routes (ntop.e2e::route-catalog))
          (unmarked (selection-grid nil)))
      (dolist (case
                (list (list "key-8" 1 "Ctrl+Left"
                            "Move widget selection left" 1 0 2)
                      (list "key-8" 2 "Shift+Left"
                            "Move widget selection left" 1 0 2)
                      (list "key-8" 3 "H"
                            "Move widget selection left" 1 0 2)
                      (list "key-8" 4 "A"
                            "Move widget selection left" 1 0 2)
                      (list "key-9" 2 "Shift+Right"
                            "Move widget selection right" 0 1 2)
                      (list "key-10" 2 "Shift+Up"
                            "Move widget selection up" 4 2 5)
                      (list "key-10" 3 "K"
                            "Move widget selection up" 4 2 5)
                      (list "key-10" 4 "W"
                            "Move widget selection up" 4 2 5)
                      (list "key-11" 1 "Ctrl+Down"
                            "Move widget selection down" 0 2 1)
                      (list "key-11" 2 "Shift+Down"
                            "Move widget selection down" 0 2 1)
                      (list "key-11" 4 "S"
                            "Move widget selection down" 0 2 1)))
        (destructuring-bind (id subcase-index input action initial-index target-index
                                wrong-direction-index)
            case
          (let* ((route
                   (find id routes :key (function ntop.e2e::route-id)
                         :test (function string=)))
                 (subcases (ntop.e2e::%route-subcases-expanded route))
                 (subcase (nth (1- subcase-index) subcases)))
            (cl-weave:expect (ntop.e2e::route-action route) :to-equal action)
            (cl-weave:expect (getf subcase :input) :to-equal input)
            (cl-weave:expect (length subcases) :to-equal 4)
            (cl-weave:expect
             (ntop.e2e::%route-key-context-state
              route subcase
              (selection-grid initial-index)
              (selection-grid target-index)
              nil)
             :to-be t)
            (cl-weave:expect
             (ntop.e2e::%route-key-context-state
              route subcase
              (selection-grid initial-index)
              (selection-grid wrong-direction-index)
              nil)
             :to-be nil)
            (cl-weave:expect
             (ntop.e2e::%route-key-context-state
              route subcase unmarked unmarked nil)
             :to-be nil))))
      (let* ((route
               (find "key-9" routes :key (function ntop.e2e::route-id)
                     :test (function string=)))
             (subcase
               (nth 1 (ntop.e2e::%route-subcases-expanded route)))
             (reference-grid unmarked)
             (candidate-initial-grid (selection-grid 0))
             (candidate-grid (selection-grid 1))
             (reference-projection (ntop.e2e::project-criterion-grid reference-grid))
             (candidate-initial-projection
               (ntop.e2e::project-criterion-grid candidate-initial-grid))
             (candidate-projection (ntop.e2e::project-criterion-grid candidate-grid)))
        (multiple-value-bind (passed difference cause status)
            (ntop.e2e::%route-all-expected-actual
             route
             "ntop"
             subcase
             (list :initial-grid reference-grid
                   :grid reference-grid
                   :initial-projection reference-projection
                   :projection reference-projection)
             (list :initial-grid candidate-initial-grid
                   :grid candidate-grid
                   :initial-projection candidate-initial-projection
                   :projection candidate-projection))
          (cl-weave:expect passed :to-be nil)
          (cl-weave:expect (not (null difference)) :to-be t)
          (cl-weave:expect
           (search "condition-A reference and candidate differ" cause)
           :to-be 0)
          (cl-weave:expect status :to-equal nil))))))
  (cl-weave:it
 "requires semantic transitions for tree, navigation, sort, table, and search"
 (cl-weave:expect
  (list
   (ntop.e2e::%route-observer-tree-transition-p
    "Toggle process tree" (list 3 0 0) (list 3 0 0))
   (ntop.e2e::%route-observer-tree-transition-p
    "Toggle process tree" (list 3 0 0) (list 3 2 0))
   (ntop.e2e::%route-observer-navigation-index-p "Move to next row" 2 3 8 8)
   (ntop.e2e::%route-observer-navigation-index-p "Move to next row" 2 5 8 8)
   (ntop.e2e::%route-observer-sort-transition-p
    "Sort process by CPU"
    (list "Name(n)" (char "▲" 0))
    (list "CPU%(c)" (char "▼" 0)))
   (ntop.e2e::%route-observer-table-header-transition-p "Disk" "Disk")
   (ntop.e2e::%route-observer-table-header-transition-p "Disk" "Temp")
   (ntop.e2e::%route-observer-table-sort-transition-p
    "Disk(d)" (list "Mount(m)" (char "▲" 0))
    (list "Disk(d)" (char "▼" 0)))
   (ntop.e2e::%route-observer-search-result-rows-match-p
    "process" (list "1 - Process Table"))
   (ntop.e2e::%route-observer-search-result-rows-match-p
    "process" (list "1 - Network Table")))
  :to-equal
  (list nil t t nil t nil t t t nil))))
