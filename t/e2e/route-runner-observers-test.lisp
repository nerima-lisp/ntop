(in-package #:ntop.e2e.test)

(defun route-tree-test-grid (selected-name &optional (expanded-p t))
  (let* ((grid (ntop.e2e:make-terminal-grid :rows 16 :cols 64))
         (top 7)
         (header 8)
         (parent-row 9)
         (child-row 10)
         (other-row (if expanded-p 11 child-row))
         (bottom 12)
         (parent-name "ntop-t-parent")
         (child-name "ntop-t-child"))
    (criterion-test-write grid top 0 "38;5;7;49" "┌")
    (criterion-test-write grid top 2 "38;5;8;49" "─ Processes ─")
    (criterion-test-write grid top 63 "38;5;7;49" "┐")
    (criterion-test-write grid header 0 "38;5;7;49" "│")
    (criterion-test-write grid header 2 "1;38;5;2;49" "Count Name(n) CPU%(c)")
    (criterion-test-write grid header 63 "38;5;7;49" "│")
    (dolist (row (if expanded-p
                     (list parent-row child-row other-row)
                   (list parent-row other-row)))
      (criterion-test-write grid row 63 "38;5;7;49" "│"))
    (criterion-test-write grid parent-row 0
                          (if (string= selected-name parent-name)
                              "38;5;1;48;5;12"
                            "39;49")
                          (format nil "│  0 ~a" parent-name))
    (when expanded-p
      (criterion-test-write grid child-row 0
                            (if (string= selected-name child-name)
                                "38;5;1;48;5;12"
                              "39;49")
                            (format nil "│  0 └─ 1 ~a" child-name)))
    (criterion-test-write grid other-row 0 "39;49" "│  0 ntop-t-sibling")
    (criterion-test-write grid bottom 0 "38;5;7;49" "└")
    (criterion-test-write grid bottom 63 "38;5;7;49" "┘")
    grid))

(cl-weave:describe "route observer dispatch"
                   (cl-weave:it
                    "dispatches CPU frequency observations against powermetrics"
                    (let* ((route
                            (find "cpu-frequency"
                                  (ntop.e2e::route-catalog)
                                  :key
                                  (function ntop.e2e::route-id)
                                  :test
                                  (function string=)))
                           (subcase
                            (first (ntop.e2e::%route-subcases-expanded route)))
                           (grid (criterion-test-grid 12 40 "")))
                      (criterion-test-write grid 1 2 "0" "CPU MHz")
                      (criterion-test-write grid 2 5 "0" "1000")
                      (multiple-value-bind (passed actual cause)
                          (ntop.e2e::%route-observer-state-p route
                                                             "btm-self"
                                                             subcase
                                                             (list :grid
                                                                   grid
                                                                   :powermetrics-before-values
                                                                   (list
                                                                    (cons
                                                                     "P cluster"
                                                                     1000))
                                                                   :powermetrics-after-values
                                                                   (list
                                                                    (cons
                                                                     "P cluster"
                                                                     1000)))
                                                             :reference)
                        (cl-weave:expect passed :to-be t)
                        (cl-weave:expect actual
                                         :to-equal
                                         (list :ui-mhz
                                               (list 1000)
                                               :cluster-averages
                                               (list
                                                (cons "P cluster" 1000.0d0))))
                        (cl-weave:expect cause :to-equal nil))))
                   (cl-weave:it
 "requires unique CPU cluster matches while allowing several cores per cluster"
 (let* ((side
         (list :powermetrics-before-values
               (list (cons "P cluster" 1000) (cons "E cluster" 2000))
               :powermetrics-after-values
               (list (cons "P cluster" 1000) (cons "E cluster" 2000))))
        (grid (criterion-test-grid 12 40 ""))
        (ambiguous-grid (criterion-test-grid 12 40 "")))
   (criterion-test-write grid 1 2 "0" "CPU MHz")
   (criterion-test-write grid 2 5 "0" "1000")
   (criterion-test-write grid 3 5 "0" "1000")
   (criterion-test-write grid 4 5 "0" "2000")
   (criterion-test-write grid 5 5 "0" "2000")
   (cl-weave:expect
    (nth-value 0 (ntop.e2e::%route-observer-cpu-frequency-p side grid))
    :to-be
    t)
   (criterion-test-write ambiguous-grid 1 2 "0" "CPU MHz")
   (criterion-test-write ambiguous-grid 2 5 "0" "1000")
   (criterion-test-write ambiguous-grid 3 5 "0" "1050")
   (cl-weave:expect
    (nth-value
     0
     (ntop.e2e::%route-observer-cpu-frequency-p
      (list :powermetrics-before-values
            (list (cons "P cluster" 1000) (cons "E cluster" 1050))
            :powermetrics-after-values
            (list (cons "P cluster" 1000) (cons "E cluster" 1050)))
      ambiguous-grid))
    :to-be
    nil)))
                   (cl-weave:it
                    "dispatches semantic tree observations from route metadata"
                    (let* ((route
                            (find "key-47"
                                  (ntop.e2e::route-catalog)
                                  :key
                                  (function ntop.e2e::route-id)
                                  :test
                                  (function string=)))
                           (subcase
                            (first (ntop.e2e::%route-subcases-expanded route)))
                           (grid (criterion-test-grid 12 40 "1234 sshd")))
                      (multiple-value-bind (passed actual cause)
                          (ntop.e2e::%route-observer-state-p route
                                                             "btm-self"
                                                             subcase
                                                             (list :initial-grid
                                                                   grid
                                                                   :grid
                                                                   grid)
                                                             :reference)
                        (cl-weave:expect (getf subcase :probe) :to-equal :tree)
                        (cl-weave:expect passed :to-be nil)
                        (cl-weave:expect actual
                                         :to-equal
                                         (ntop.e2e::%route-observer-process-rows
                                          grid))
                        (cl-weave:expect cause
                                         :to-equal
                                         "tree action did not produce its expected transition")))))
(cl-weave:describe
 "process-tree observers"
 (cl-weave:it
  "detects collapsing a selected process-tree parent"
  (let* ((route
          (find "key-20" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase
          (find :tree (ntop.e2e::%route-subcases-expanded route)
                :key (lambda (case) (getf case :probe))
                :test (function eq)))
         (initial (route-tree-test-grid "ntop-t-parent"))
         (collapsed (route-tree-test-grid "ntop-t-parent" nil))
         (unchanged (route-tree-test-grid "ntop-t-parent")))
    (multiple-value-bind (passed actual cause)
        (ntop.e2e::%route-observer-state-p
         route "btm-self" subcase
         (list :initial-grid initial :grid collapsed) :reference)
      (cl-weave:expect passed :to-be t)
      (cl-weave:expect actual
                       :to-equal
                       (ntop.e2e::%route-observer-process-rows collapsed))
      (cl-weave:expect cause :to-equal nil))
    (cl-weave:expect
     (nth-value
      0
      (ntop.e2e::%route-observer-state-p
       route "btm-self" subcase
       (list :initial-grid initial :grid unchanged) :reference))
     :to-be nil)))
 (cl-weave:it
  "detects expanding a selected process-tree parent"
  (let* ((route
          (find "key-21" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase
          (find :tree (ntop.e2e::%route-subcases-expanded route)
                :key (lambda (case) (getf case :probe))
                :test (function eq)))
         (initial (route-tree-test-grid "ntop-t-parent" nil))
         (expanded (route-tree-test-grid "ntop-t-parent"))
         (unchanged (route-tree-test-grid "ntop-t-parent" nil)))
    (multiple-value-bind (passed actual cause)
        (ntop.e2e::%route-observer-state-p
         route "btm-self" subcase
         (list :initial-grid initial :grid expanded) :reference)
      (cl-weave:expect passed :to-be t)
      (cl-weave:expect actual
                       :to-equal
                       (ntop.e2e::%route-observer-process-rows expanded))
      (cl-weave:expect cause :to-equal nil))
    (cl-weave:expect
     (nth-value
      0
      (ntop.e2e::%route-observer-state-p
       route "btm-self" subcase
       (list :initial-grid initial :grid unchanged) :reference))
     :to-be nil)))
 (cl-weave:it
  "identifies either selected process row when two rows are present"
  (labels ((grid-with-selection (selected-index)
             (let* ((grid (ntop.e2e:make-terminal-grid :rows 16 :cols 64))
                    (blank-row 9)
                    (parent-row 10)
                    (child-row 11)
                    (bottom 12))
               (criterion-test-write grid 7 0 "38;5;7;49" "┌")
               (criterion-test-write grid 7 2 "38;5;8;49" "─ Processes ─")
               (criterion-test-write grid 7 63 "38;5;7;49" "┐")
               (criterion-test-write grid 8 0 "38;5;7;49" "│")
               (criterion-test-write grid 8 2 "1;38;5;2;49"
                                     "Count Name(n) CPU%(c)")
               (criterion-test-write grid 8 63 "38;5;7;49" "│")
               (dolist (row (list blank-row parent-row child-row))
                 (criterion-test-write grid row 0 "38;5;7;49" "│")
                 (criterion-test-write grid row 63 "38;5;7;49" "│"))
               (loop for row in (list parent-row child-row)
                     for index from 0
                     for style = (if (= index selected-index)
                                     "38;5;0;48;5;12"
                                   "39;49")
                     for name in (list "ntop-t-parent" "└─ 1 ntop-t-child")
                     do (criterion-test-write
                         grid row 1 style (format nil "  0 ~a" name)))
               (criterion-test-write grid bottom 0 "38;5;7;49" "└")
               (criterion-test-write grid bottom 63 "38;5;7;49" "┘")
               grid)))
    (dolist (expected-index (list 0 1))
      (let ((grid (grid-with-selection expected-index)))
        (cl-weave:expect
         (length (ntop.e2e::%route-observer-process-rows grid))
         :to-equal 2)
        (cl-weave:expect
         (multiple-value-list
          (ntop.e2e::%route-observer-process-selection-index grid))
         :to-equal
         (list expected-index 1)))))))
(cl-weave:describe
 "mouse observer transitions"
 (cl-weave:it
  "requires the expected frame or clicked table header to change"
  (labels ((find-route (id)
             (find id
                   (ntop.e2e::route-catalog)
                   :key (function ntop.e2e::route-id)
                   :test (function string=)))
           (observe (route subcase before after)
             (nth-value
              0
              (ntop.e2e::%route-observer-state-p
               route
               "btm-self"
               subcase
               (list :initial-grid before :grid after)
               :reference)))
           (frame-grid (selected-index)
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
                          (let ((style
                                  (if (eql index selected-index)
                                      "38;5;12;49"
                                      "39;49")))
                            (criterion-test-write grid top left style "┌")
                            (criterion-test-write grid top right style "┐")
                            (criterion-test-write grid bottom left style "└")
                            (criterion-test-write grid bottom right style "┘")
                            (when (= index 1)
                              (criterion-test-write
                               grid top (+ left 2) "0" "Memory")))))
               grid))
           (table-grid (title header)
             (let ((grid (criterion-test-grid 12 40 "")))
               (criterion-test-write
                grid 8 2 "0" (concatenate (quote string) title "            "))
               (criterion-test-write
                grid 9 2 "0" (concatenate (quote string) header
                                          "                        "))
               grid)))
    (loop for id in (list "mouse-71" "mouse-72")
          for route = (find-route id)
          for subcase = (first (ntop.e2e::%route-subcases-expanded route))
          for before = (frame-grid 0)
          for selected = (frame-grid 1)
          for wrong = (frame-grid 2)
          for unrelated = (frame-grid 0)
          do
             (criterion-test-write unrelated 10 30 "0" "changed")
             (cl-weave:expect (getf subcase :probe) :to-equal :mouse)
             (cl-weave:expect (observe route subcase before selected) :to-be t)
             (cl-weave:expect (observe route subcase before wrong) :to-be nil)
             (cl-weave:expect
              (ntop.e2e::%route-observer-grid-changed-p before unrelated)
              :to-be
              t)
             (cl-weave:expect
              (observe route subcase before unrelated)
              :to-be
              nil))
    (loop for context in (list "temperature table" "disk table")
          for route = (find-route "mouse-76")
          for subcase = (find context
                              (ntop.e2e::%route-subcases-expanded route)
                              :key (function (lambda (case) (getf case :context)))
                              :test (function string=))
          for title = (if (string= context "temperature table")
                          "Temperatures"
                          "Disks")
          for target = (if (string= context "temperature table")
                           "Sensor(s)"
                           "Disk(d)")
          for before = (table-grid
                        title (concatenate (quote string) target "▼"))
          for selected = (table-grid
                          title (concatenate (quote string) target "▲"))
          for unrelated = (table-grid
                           title (concatenate (quote string) target "▼"))
          do
             (criterion-test-write unrelated 10 30 "0" "changed")
             (cl-weave:expect (getf subcase :probe) :to-equal :mouse)
             (cl-weave:expect (observe route subcase before selected) :to-be t)
             (cl-weave:expect
              (ntop.e2e::%route-observer-grid-changed-p before unrelated)
              :to-be
              t)
             (cl-weave:expect
              (observe route subcase before unrelated)
              :to-be
              nil)))))
