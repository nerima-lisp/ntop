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
     (destructuring-bind (id kind width hidden-p header max-percent) specification
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
                                  max-percent)))
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
                   (cl-weave:it "measures wide characters in terminal cells"
                                (cl-weave:expect (ntop.ui:measure-table-text "A界")
                                                 :to-be
                                                 3))
                   (cl-weave:it "does not truncate a wide character across the cell limit"
                                (cl-weave:expect (ntop.ui:truncate-table-text "A界B" 2)
                                                 :to-equal
                                                 "A"))
                   (cl-weave:it "keeps a wide character that exactly fits"
                                (cl-weave:expect (ntop.ui:truncate-table-text "A界B" 3)
                                                 :to-equal
                                                 "A界"))
                   (cl-weave:it
                    "measures a combining character by the library character widths"
                    (cl-weave:expect (ntop.ui:measure-table-text "é")
                                     :to-be
                                     (+ (cl-tty-kit:char-width #\e)
                                        (cl-tty-kit:char-width #\́))))
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
 ;; The literal vectors follow column.rs:172-259 and sortable.rs:191-197.
 (cl-weave:it-each
     ((8 :left-to-right
       ((:a :hard 2 nil "A" nil) (:b :hard 4 nil "B" nil)
        (:c :hard 3 nil "C" nil))
       ((:a 2) (:b 4)))
      (8 :right-to-left
       ((:a :hard 2 nil "A" nil) (:b :hard 4 nil "B" nil)
        (:c :hard 3 nil "C" nil))
       ((:b 4) (:c 3)))
      (12 :left-to-right
       ((:a :hard 2 nil "A" nil) (:b :hard 3 nil "B" nil)
        (:c :hard 2 nil "C" nil))
       ((:a 3) (:b 4) (:c 2)))
      (8 :left-to-right
       ((:a :hard 2 nil "A" nil) (:hidden :hard 6 t "H" nil)
        (:c :hard 3 nil "C" nil))
       ((:a 3) (:c 3)))
      (20 :left-to-right
       ((:soft :soft 12 nil "Name" 3/10))
       ((:soft 19)))
      (10 :left-to-right
       ((:soft :soft 12 nil "Name" nil))
       ((:soft 10)))
      (7 :left-to-right
       ((:soft :soft 6 nil "N" 1/2))
       ((:soft 6)))
      (3 :left-to-right
       ((:soft :soft 8 nil "Header" 1/10))
       nil))
     "~D cells ~S ~S -> ~S"
     (total-width direction specifications expected)
   (cl-weave:expect
    (%table-layout-summary
     (ntop.ui:compute-table-layout
      (%table-layout-columns specifications)
      total-width
      :right-to-left-p (eq direction :right-to-left)))
    :to-equal expected)))

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
                    ((39 nil 0 0 0 39) (40 nil 34 6 0 34)
                     (100 nil 85 15 0 85)
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
