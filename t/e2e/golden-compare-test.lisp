(in-package #:ntop.e2e.test)

(cl-weave:describe "FR-001 condition A golden comparison"
                   (cl-weave:it "expands comparison cases declaratively"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e::define-golden-comparison-cases
                                    (:same :left :right t)))
                                 :to-equal
                                 '(cl-weave:it-each ((:same :left :right t))
                                                    "compares criterion-A projections for ~A"
                                                    (ntop.e2e::case-name
                                                     ntop.e2e::left
                                                     ntop.e2e::right
                                                     ntop.e2e::expected)
                                                    (declare (ignore
                                                              ntop.e2e::case-name))
                                                    (cl-weave:expect
                                                     (ntop.e2e::criterion-projections-equal-p
                                                      ntop.e2e::left
                                                      ntop.e2e::right)
                                                     :to-be
                                                     ntop.e2e::expected))))
                   (ntop.e2e::define-golden-comparison-cases
                    (:same
                     #(40 12
                          #(#("border" 0
                                       0
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     #(40 12
                          #(#("border" 0
                                       0
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     t)
                    (:character
                     #(40 12
                          #(#("border" 0
                                       0
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     #(40 12
                          #(#("border" 0
                                       0
                                       "┐"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     nil)
                    (:foreground
                     #(40 12
                          #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
                     #(40 12
                          #(#("title" 0 1 "C" #("indexed" 4) #("default" nil) 1)))
                     nil)
                    (:background
                     #(40 12
                          #(#("process-header" 10
                                               1
                                               "P"
                                               #("default" nil)
                                               #("indexed" 2)
                                               1)))
                     #(40 12
                          #(#("process-header" 10
                                               1
                                               "P"
                                               #("default" nil)
                                               #("indexed" 3)
                                               1)))
                     nil)
                    (:bold
                     #(40 12
                          #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
                     #(40 12
                          #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 0)))
                     nil)
                    (:coordinates
                     #(40 12
                          #(#("border" 0
                                       0
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     #(40 12
                          #(#("border" 0
                                       1
                                       "┌"
                                       #("indexed" 7)
                                       #("default" nil)
                                       0)))
                     nil)
                    (:dimension #(40 12 #()) #(50 12 #()) nil)
                    (:malformed-cell
                     #(40 12
                          #(#("data" 0
                                     0
                                     "x"
                                     #("default" nil)
                                     #("default" nil)
                                     0)))
                     #(40 12
                          #(#("data" 0
                                     0
                                     "x"
                                     #("default" nil)
                                     #("default" nil)
                                     0)))
                     nil)))

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
