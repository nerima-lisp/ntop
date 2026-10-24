(in-package #:ntop.e2e.test)

(cl-weave:describe "FR-001 condition A golden comparison"
  (cl-weave:it
   "expands comparison cases declaratively"
   (cl-weave:expect
    (macroexpand-1
     '(ntop.e2e::define-golden-comparison-cases (:same :left :right t)))
    :to-equal
    '(cl-weave:it-each
      ((:same :left :right t))
      "compares criterion-A projections for ~A"
      (ntop.e2e::case-name ntop.e2e::left ntop.e2e::right ntop.e2e::expected)
      (declare (ignore ntop.e2e::case-name))
      (cl-weave:expect
       (ntop.e2e::criterion-projections-equal-p
        ntop.e2e::left ntop.e2e::right)
       :to-be ntop.e2e::expected))))
  (ntop.e2e::define-golden-comparison-cases
    (:same
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     t)
    (:character
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 0 "┐" #("indexed" 7) #("default" nil) 0)))
     nil)
    (:foreground
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
     #(40 12 #(#("title" 0 1 "C" #("indexed" 4) #("default" nil) 1)))
     nil)
    (:background
     #(40 12 #(#("title" 0 1 "C" #("default" nil) #("indexed" 2) 1)))
     #(40 12 #(#("title" 0 1 "C" #("default" nil) #("indexed" 3) 1)))
     nil)
    (:bold
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 0)))
     nil)
    (:coordinates
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 1 "┌" #("indexed" 7) #("default" nil) 0)))
     nil)
    (:dimension #(40 12 #()) #(50 12 #()) nil)
    (:malformed-cell
     #(40 12 #(#("data" 0 0 "x" #("default" nil) #("default" nil) 0)))
     #(40 12 #(#("data" 0 0 "x" #("default" nil) #("default" nil) 0)))
     nil))
  (cl-weave:it
   "ignores header text, positions, and missing columns while comparing shared fields"
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "Count Name(n) CPU%(c)▼ Mem%(m) State")))
          (btm
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "PID      Name(n)                      CPU%(c)▼ Mem%(m)"))))
     (cl-weave:expect
      (ntop.e2e::criterion-projections-equal-p candidate btm)
      :to-be
      t)))
  (cl-weave:it-each
   ((:foreground "1;38;5;2;49" "1;38;5;3;49")
    (:background "1;38;5;2;48;5;3" "1;38;5;2;48;5;4")
    (:bold "1;38;5;2;49" "38;5;2;49"))
   "compares process header ~A attributes on shared columns"
   (attribute left-sgr right-sgr)
   (declare (ignore attribute))
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid 12 60 "live process" left-sgr)))
          (btm
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid 12 60 "live process" right-sgr))))
     (cl-weave:expect
      (ntop.e2e::criterion-projections-equal-p candidate btm)
      :to-be
      nil)))
  (cl-weave:it
   "compares the sort marker attribute as part of its logical column"
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "Count Name(n) CPU%(c)▼ Mem%(m)")))
          (btm (copy-seq candidate))
          (btm-cells (map 'vector #'copy-seq (aref candidate 2))))
     (setf (aref btm 2) btm-cells)
     (let ((marker
            (find-if (lambda (cell)
                       (and (string= (aref cell 0) "process-header")
                            (string= (aref cell 3) "▼")))
                     btm-cells)))
       (cl-weave:expect (not (null marker)) :to-be t)
       (setf (aref marker 4) #("indexed" 3))
       (cl-weave:expect
        (ntop.e2e::criterion-projections-equal-p candidate btm)
        :to-be
        nil)))))

(cl-weave:describe "FR-001 condition A golden reader"
                   (cl-weave:it
                    "reads the checked-in JSON golden with empty default colors"
                    (let* ((pathname
                            (asdf:system-relative-pathname "ntop/e2e"
                                                           "t/e2e/goldens/40-12.json"))
                           (projection
                            (ntop.e2e::%read-criterion-golden pathname 40 12))
                           (cells (aref projection 2)))
                      (cl-weave:expect
                       (list (aref projection 0) (aref projection 1))
                       :to-equal
                       (quote (40 12)))
                      (cl-weave:expect
                       (and (vectorp cells) (plusp (length cells)))
                       :to-be
                       t))))
