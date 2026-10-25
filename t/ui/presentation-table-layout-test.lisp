(in-package #:ntop.test)

(defun %table-layout-summary (layout)
  (mapcar
   (lambda (column)
     (list (ntop.ui:table-column-layout-id column)
           (ntop.ui:table-column-layout-width column)))
   layout))

(defun %table-layout-columns (specifications)
  (mapcar
   (lambda (specification)
     (destructuring-bind (id kind width hidden-p header max-percent
                          &optional sortable-p)
         specification
       (ntop.ui:make-table-column :id
                                  id
                                  :header
                                  header
                                  :width-kind
                                  kind
                                  :width
                                  width
                                  :hidden-p
                                  hidden-p
                                  :max-percent
                                  max-percent
                                  :sortable-p
                                  sortable-p)))
   specifications))

(cl-weave:describe "table width remainder distribution"
                   (cl-weave:it-each
                    ((1 1) (2 1)
                     (3 2)
                     (4 3)
                     (5 4)
                     (6 5)
                     (7 6)
                     (8 7)
                     (9 8)
                     (10 9)
                     (11 10)
                     (12 11)
                     (13 12)
                     (14 13)
                     (15 14)
                     (16 15)
                     (17 16)
                     (18 17)
                     (19 18)
                     (20 19)
                     (21 20)
                     (22 21)
                     (23 22)
                     (24 23)
                     (25 24)
                     (26 25)
                     (27 26)
                     (28 27)
                     (29 28)
                     (30 29)
                     (31 30)
                     (32 31)
                     (33 32)
                     (34 33)
                     (35 34)
                     (36 35)
                     (37 36)
                     (38 37)
                     (39 38)
                     (40 39)
                     (41 40)
                     (42 41)
                     (43 42)
                     (44 43)
                     (45 44)
                     (46 45)
                     (47 46)
                     (48 47)
                     (49 48)
                     (50 49))
                    "width ~D allocates ~D cells"
                    (total-width expected-width)
                    (cl-weave:expect
                     (%table-layout-summary
                      (ntop.ui:compute-table-layout
                       (list
                        (ntop.ui:make-table-column :id
                                                   :only
                                                   :header
                                                   "X"
                                                   :width-kind
                                                   :hard
                                                   :width
                                                   1))
                       total-width))
                     :to-equal
                     (list (list :only expected-width))))
                   (cl-weave:it "expands declarative columns into a named column list"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.ui:define-table-columns probe-columns
                                                                 (:pid "PID"
                                                                       :hard
                                                                       :width
                                                                       4
                                                                       :sortable-p
                                                                       t)))
                                 :to-equal
                                 '(defparameter ntop.test::probe-columns
                                    (list
                                     (ntop.ui:make-table-column :id
                                                                :pid
                                                                :header
                                                                "PID"
                                                                :width-kind
                                                                :hard
                                                                :width
                                                                4
                                                                :sortable-p
                                                                t)))))
                   (cl-weave:it "measures BTM table text in UTF-8 bytes"
                                (cl-weave:expect (ntop.ui:measure-table-text "A界")
                                                 :to-be
                                                 4))
                   (cl-weave:it "does not truncate a wide character across the cell limit"
                                (cl-weave:expect (ntop.ui:truncate-table-text "A界B" 2)
                                                 :to-equal
                                                 "A"))
                   (cl-weave:it "keeps a wide character that exactly fits"
                                (cl-weave:expect (ntop.ui:truncate-table-text "A界B" 3)
                                                 :to-equal
                                                 "A界"))
                   (cl-weave:it
                    "measures a combining character by its UTF-8 bytes"
                    (cl-weave:expect (ntop.ui:measure-table-text "é")
                                     :to-be
                                     3))
                   (cl-weave:it "rejects negative text widths"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.ui:truncate-table-text "text" -1))
                                 :to-throw
                                 'error))
                   (cl-weave:it "rejects an unknown width kind"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.ui:make-table-column :id
                                                              :bad
                                                              :header
                                                              "Bad"
                                                              :width-kind
                                                              :elastic))
                                 :to-throw
                                 'error))
                   (cl-weave:it "requires a hard width"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.ui:make-table-column :id
                                                              :bad
                                                              :header
                                                              "Bad"
                                                              :width-kind
                                                              :hard))
                                 :to-throw
                                 'error))
                   (cl-weave:it "rejects a soft limit above one"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.ui:make-table-column :id
                                                              :bad
                                                              :header
                                                              "Bad"
                                                              :width-kind
                                                              :soft
                                                              :max-percent
                                                              2))
                                 :to-throw
                                 'error))
                   (cl-weave:it "accepts 100 table columns and rejects more"
                                (let ((column
                                       (ntop.ui:make-table-column :id
                                                                  :column
                                                                  :header
                                                                  "C"
                                                                  :width-kind
                                                                  :hard
                                                                  :width
                                                                  1)))
                                  (cl-weave:expect
                                   (length
                                    (ntop.ui:compute-table-layout
                                     (make-list 100 :initial-element column)
                                     200))
                                   :to-equal
                                   100)
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.ui:compute-table-layout
                                      (make-list 101 :initial-element column)
                                      202))
                                   :to-throw
                                   (quote error))))
                   (cl-weave:it "validates and measures column edge cases"
                                (let ((column
                                       (ntop.ui:make-table-column :id
                                                                  :value
                                                                  :header
                                                                  "A"
                                                                  :width-kind
                                                                  :soft
                                                                  :sortable-p
                                                                  t
                                                                  :value-function
                                                                  (lambda (row)
                                                                    (getf row :value)))))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.ui:make-table-column :id
                                                                :bad
                                                                :header
                                                                "Bad"
                                                                :width
                                                                1/2))
                                   :to-throw
                                   (quote error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.ui:make-table-column :id
                                                                :bad
                                                                :header
                                                                "Bad"
                                                                :width
                                                                -1))
                                   :to-throw
                                   (quote error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.ui:make-table-column :id
                                                                :bad
                                                                :header
                                                                "Bad"
                                                                :width-kind
                                                                :hard
                                                                :width
                                                                1
                                                                :max-percent
                                                                1/2))
                                   :to-throw
                                   (quote error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.ui:make-table-column :id
                                                                :bad
                                                                :header
                                                                "Bad"
                                                                :value-function
                                                                7))
                                   :to-throw
                                   (quote error))
                                  (cl-weave:expect
                                   (%table-layout-summary
                                    (ntop.ui:compute-table-layout (list column)
                                                                  10
                                                                  :rows
                                                                  (list
                                                                   (list :value "longer")
                                                                   (list :value "x")
                                                                   (list :value nil)
                                                                   (list :value 123))))
                                   :to-equal
                                   (list (list :value 9))))))

(cl-weave:describe
 "btm 0.14.9 source-derived table width vectors"
 ;; The literal vectors follow bottom/src/canvas/components/data_table/column.rs:172-255
 ;; and bottom/src/canvas/components/data_table/sortable.rs:191-197.
 (cl-weave:it-each
  ((5 :left-to-right
    ((:pid :follow-header nil nil "P" nil t)
     (:next :hard 2 nil "N" nil))
    ((:pid 2) (:next 2)))
   (8 :left-to-right
    ((:a :hard 2 nil "A" nil) (:b :hard 4 nil "B" nil)
     (:c :hard 3 nil "C" nil))
    ((:a 2) (:b 4)))
   (8 :right-to-left
    ((:a :hard 2 nil "A" nil) (:b :hard 4 nil "B" nil)
     (:c :hard 3 nil "C" nil))
    ((:a 4) (:b 3)))
   (12 :left-to-right
    ((:a :hard 2 nil "A" nil) (:b :hard 3 nil "B" nil)
     (:c :hard 2 nil "C" nil))
    ((:a 3) (:b 4) (:c 2)))
   (20 :left-to-right ((:soft :soft 12 nil "Name" 3/10))
    ((:soft 19)))
   (10 :left-to-right ((:soft :soft 12 nil "Name" nil))
    ((:soft 10)))
   (7 :left-to-right ((:soft :soft 6 nil "N" 1/2))
    ((:soft 6)))
   (3 :left-to-right ((:soft :soft 8 nil "Header" 1/10)) nil)
   (20 :left-to-right
    ((:a :soft 20 nil "A" 1/4)
     (:b :soft 20 nil "B" 1/4)
     (:c :soft 20 nil "C" 1/4))
    ((:a 6) (:b 6) (:c 5))))
  "~D cells ~S ~S -> ~S"
  (total-width direction specifications expected)
  (cl-weave:expect
   (%table-layout-summary
    (ntop.ui:compute-table-layout
     (%table-layout-columns specifications)
     total-width
     :right-to-left-p (eq direction :right-to-left)))
   :to-equal expected))
 (cl-weave:it "maps RTL widths to the original columns and their x positions"
  ;; BTM reverses the width vector, then zips it with the original columns.
  (let* ((columns
           (%table-layout-columns
            '((:a :hard 2 nil "A" nil)
              (:b :hard 4 nil "B" nil)
              (:c :hard 3 nil "C" nil))))
         (layout (ntop.ui:compute-table-layout columns 8 :right-to-left-p t)))
    (cl-weave:expect
     (mapcar (lambda (column)
               (list (ntop.ui:table-column-layout-id column)
                     (ntop.ui:table-column-layout-x column)
                     (ntop.ui:table-column-layout-width column)))
             layout)
     :to-equal '((:a 0 4) (:b 5 3))))))

(cl-weave:describe "btm 0.14.9 process table data width vector"
 (cl-weave:it "uses the widest row value in UTF-8 bytes"
  (let* ((columns (%table-layout-columns
                   (list (list :a :soft 0 nil "A" nil)
                         (list :b :soft 0 nil "B" nil))))
         (layout (ntop.ui:compute-table-layout
                  columns 7 :rows (list (list :a "界" :b "x")))))
    (cl-weave:expect
     (mapcar (lambda (column)
               (list (ntop.ui:table-column-layout-id column)
                     (ntop.ui:table-column-layout-x column)
                     (ntop.ui:table-column-layout-width column)))
             layout)
     :to-equal '((:a 0 4) (:b 5 1)))))
 (cl-weave:it "honors sortable FollowHeader sizing with longer row text"
  (let* ((columns
           (list (ntop.ui:make-table-column
                  :id :pid :header "P" :width-kind :follow-header :sortable-p t)
                 (ntop.ui:make-table-column
                  :id :next :header "N" :width-kind :hard :width 2)))
         (layout (ntop.ui:compute-table-layout
                  columns 5
                  :rows (list (list :pid "a much longer process identifier")))))
    (cl-weave:expect
     (mapcar #'ntop.ui:table-column-layout-x layout) :to-equal '(0 3))
    (cl-weave:expect
     (mapcar #'ntop.ui:table-column-layout-width layout) :to-equal '(2 2))))
 (cl-weave:it "redistributes capped widths across three columns in source order"
  (let* ((columns
           (list (ntop.ui:make-table-column
                  :id :a :header "A" :width-kind :soft
                  :width 20 :max-percent 1/4)
                 (ntop.ui:make-table-column
                  :id :b :header "B" :width-kind :soft
                  :width 20 :max-percent 1/4)
                 (ntop.ui:make-table-column
                  :id :c :header "C" :width-kind :soft
                  :width 20 :max-percent 1/4)))
         (layout (ntop.ui:compute-table-layout columns 20)))
    (cl-weave:expect
     (mapcar #'ntop.ui:table-column-layout-x layout) :to-equal '(0 7 14))
    (cl-weave:expect
     (mapcar #'ntop.ui:table-column-layout-width layout) :to-equal '(6 6 5)))))
(cl-weave:describe "table width drop and identity rules"
                   (cl-weave:it-each
                    ((8 ((:a :hard 5 nil "A" nil) (:b :hard 1 nil "B" nil))
                        ((:a 5) (:b 1)))
                     (7 ((:a :hard 2 nil "A" nil) (:b :hard 2 nil "B" nil))
                        ((:a 3) (:b 2)))
                     (8
                      ((:a :hard 2 nil "A" nil) (:b :hard 0 nil "B" nil)
                       (:c :hard 1 nil "C" nil))
                      ((:a 7)))
                     (8 ((:a :hard 0 nil "A" nil) (:b :hard 1 nil "B" nil)) nil)
                     (8 ((:hidden :hard 4 t "H" nil) (:a :hard 3 nil "A" nil)) ((:a 7)))
                     (8
                      ((:a :hard 4 nil "A" nil) (:hidden :hard 5 t "H" nil)
                       (:c :hard 2 nil "C" nil))
                      ((:a 4) (:c 2)))
                     (8
                      ((:a :hard 2 nil "A" nil) (:b :hard 5 nil "B" nil)
                       (:c :hard 1 nil "C" nil))
                      ((:a 2) (:b 5)))
                     (8
                      ((:a :hard 1 nil "A" nil) (:b :hard 1 nil "B" nil)
                       (:c :hard 1 nil "C" nil))
                      ((:a 2) (:b 2) (:c 1)))
                     (8 ((:a :hard 1 nil "A" nil) (:b :hard 1 nil "B" nil))
                        ((:a 3) (:b 3)))
                     (8
                      ((:a :hard 2 nil "A" nil) (:hidden :hard 4 t "H" nil)
                       (:c :hard 3 nil "C" nil))
                      ((:a 3) (:c 3))))
                    "~D cells columns ~S -> ~S"
                    (total-width specifications expected)
                    (cl-weave:expect
                     (%table-layout-summary
                      (ntop.ui:compute-table-layout (%table-layout-columns specifications)
                                                    total-width))
                     :to-equal
                     expected)))
(cl-weave:describe "CPU table legend"
                   (cl-weave:it-each
                    ((39 nil 0 0 0 39)
                     (40 nil 34 6 0 34)
                     (53 nil 46 7 0 46)
                     (100 nil 85 15 0 85)
                     (107 nil 91 16 0 91)
                     (40 t 0 6 6 34)
                     (100 t 0 15 15 85))
                    "width ~D left ~S legend ~D/~D graph ~D/~D"
                    (total-width left-p legend-x legend-width graph-x graph-width)
                    (multiple-value-bind (actual-legend-x actual-legend-width
                                                          actual-graph-x
                                                          actual-graph-width)
                        (ntop.ui:cpu-legend-region total-width :left-p left-p)
                      (cl-weave:expect
                       (list actual-legend-x
                             actual-legend-width
                             actual-graph-x
                             actual-graph-width)
                       :to-equal
                       (list legend-x legend-width graph-x graph-width))))
                   ;; bottom/src/canvas/widgets/cpu_graph.rs:24-53 sets a 15% legend
                   ;; region with a six-cell minimum.  Legend rows and columns
                   ;; follow bottom/src/widgets/cpu_graph.rs:118-125,146-164.
                   (cl-weave:it-each
                    ((:all 1 "All") (:average 1 "AVG")
                     (0 4 "0")
                     (12 4 "12")
                     (0 5 "CPU0")
                     (12 5 "CPU12"))
                    "entry ~S at width ~D -> ~S"
                    (kind width expected)
                    (cl-weave:expect (ntop.ui:cpu-legend-label kind width)
                                     :to-equal
                                     expected))
                   (cl-weave:it "rejects negative CPU core indices"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.ui:cpu-legend-label -1 5))
                                 :to-throw
                                 (quote error))))
