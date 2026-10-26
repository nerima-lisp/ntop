(in-package #:ntop.e2e.test)

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
