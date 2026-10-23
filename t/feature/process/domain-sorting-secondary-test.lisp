(in-package #:ntop.test)

(cl-weave:describe "WP4 process sorting"
                   (cl-weave:it
                    "memory sorting follows the selected bytes or percentage mode"
                    (let* ((rows
                            (list
                             (ntop.process.domain:make-process-row :count
                                                                   1
                                                                   :pid
                                                                   1
                                                                   :memory-bytes
                                                                   100
                                                                   :memory-percent
                                                                   80)
                             (ntop.process.domain:make-process-row :count
                                                                   1
                                                                   :pid
                                                                   2
                                                                   :memory-bytes
                                                                   200
                                                                   :memory-percent
                                                                   20)))
                           (by-bytes
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :memory
                                                                            :memory-mode
                                                                            :bytes)))
                           (by-percent
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :memory
                                                                            :memory-mode
                                                                            :percent))))
                      (cl-weave:expect
                       (list (%wp4-sorted-pids by-bytes)
                             (%wp4-sorted-pids by-percent))
                       :to-equal
                       '((2 1) (1 2)))))
                   (cl-weave:it
                    "preserves equal-key input order in a stable sort"
                    (let* ((rows
                            (list (%wp4-sorting-row 4 :cpu-percent 1)
                                  (%wp4-sorting-row 1 :cpu-percent 1)
                                  (%wp4-sorting-row 3 :cpu-percent 2)
                                  (%wp4-sorting-row 2 :cpu-percent 1)))
                           (sorted
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :cpu-percent
                                                                            :order
                                                                            :ascending))))
                      (cl-weave:expect (%wp4-sorted-pids sorted)
                                       :to-equal
                                       '(4 1 2 3))))
                   (cl-weave:it "uses PID ascending as the stable key order"
                                (let* ((rows
                                        (list (%wp4-sorting-row 3 :pid nil)
                                              (%wp4-sorting-row 1 :pid nil)
                                              (%wp4-sorting-row 2 :pid nil)))
                                       (sorted
                                        (ntop.process.domain::sort-process-domain-rows
                                         rows
                                         (wp4-sort-state :key :pid))))
                                  (cl-weave:expect (%wp4-sorted-pids sorted)
                                                   :to-equal
                                                   '(1 2 3))))
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "WP4 stable sorting preserves the order of equal CPU values"
                    ((values
                      (cl-weave:gen-list (cl-weave:gen-integer :min 0 :max 4)
                                         :min-length
                                         1
                                         :max-length
                                         20)))
                    (let* ((rows
                            (loop for cpu in values
                                  for pid from 100
                                  collect (%wp4-sorting-row pid
                                                            :cpu-percent
                                                            cpu)))
                           (sorted
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :cpu-percent
                                                                            :order
                                                                            :ascending))))
                      (dolist (cpu (remove-duplicates values :test #'=))
                        (cl-weave:expect
                         (remove-if-not
                          (lambda (row)
                            (= (ntop.process.domain:process-row-cpu-percent row)
                               cpu))
                          sorted)
                         :to-satisfy
                         (lambda (actual)
                           (equal (%wp4-sorted-pids actual)
                                  (loop for value in values
                                        for pid from 100
                                        when (= value cpu)
                                          collect pid)))))))
                   (cl-weave:it-property
                    "WP4 ascending process keys are monotonically ordered"
                    ((values
                      (cl-weave:gen-list
                       (cl-weave:gen-integer :min -100 :max 100)
                       :min-length
                       1
                       :max-length
                       20)))
                    (let* ((rows
                            (loop for cpu in values
                                  for pid from 200
                                  collect (%wp4-sorting-row pid
                                                            :cpu-percent
                                                            cpu)))
                           (sorted
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :cpu-percent
                                                                            :order
                                                                            :ascending)))
                           (actual
                            (mapcar
                             #'ntop.process.domain:process-row-cpu-percent
                             sorted)))
                      (cl-weave:expect
                       (every #'<=
                              actual
                              (append (rest actual) (list most-positive-fixnum)))
                       :to-be
                       t)))
                   (cl-weave:it
                    "compares case-insensitive names and preserves case-folded ties"
                    (let* ((rows
                            (list (%wp4-sorting-row 1 :name "Zebra")
                                  (%wp4-sorting-row 2 :name "alpha")
                                  (%wp4-sorting-row 3 :name "Alpha")))
                           (sorted
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :name))))
                      (cl-weave:expect (%wp4-sorted-pids sorted)
                                       :to-equal
                                       (list 2 3 1))))
                   (cl-weave:it
                    "compares state strings case-sensitively in descending order"
                    (let* ((rows
                            (list (%wp4-sorting-row-for-column 1 :state "alpha")
                                  (%wp4-sorting-row-for-column 2 :state "Z")))
                           (sorted
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (wp4-sort-state
                                                                            :key
                                                                            :state
                                                                            :order
                                                                            :descending))))
                      (cl-weave:expect (%wp4-sorted-pids sorted)
                                       :to-equal
                                       (list 1 2))))
                   (cl-weave:it
                    "sorts GPU memory bytes and keeps missing values last"
                    (let* ((gpu-rows
                            (list
                             (%wp4-sorting-row-with-values 1
                                                           :gpu-memory
                                                           100
                                                           :bytes)
                             (%wp4-sorting-row-with-values 2
                                                           :gpu-memory
                                                           50
                                                           :bytes)))
                           (gpu-sorted
                            (ntop.process.domain::sort-process-domain-rows
                             gpu-rows
                             (wp4-sort-state :key
                                             :gpu-memory
                                             :memory-mode
                                             :bytes)))
                           (missing-rows
                            (list (%wp4-sorting-row 1 :cpu-percent 5)
                                  (%wp4-sorting-row 2 :cpu-percent nil)))
                           (missing-sorted
                            (ntop.process.domain::sort-process-domain-rows
                             missing-rows
                             (wp4-sort-state :key
                                             :cpu-percent
                                             :order
                                             :ascending))))
                      (cl-weave:expect
                       (list (%wp4-sorted-pids gpu-sorted)
                             (%wp4-sorted-pids missing-sorted))
                       :to-equal
                       (list (list 1 2) (list 2 1)))))
                   (cl-weave:it "rejects unsupported sort keys and memory modes"
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (wp4-sort-state :key :unknown)
                                                 :returned)
                                   (error ()
                                     :signaled))
                                 :to-be
                                 :signaled)
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (wp4-sort-state :memory-mode
                                                                 :unknown)
                                                 :returned)
                                   (error ()
                                     :signaled))
                                 :to-be
                                 :signaled))
                   (cl-weave:it
                    "returns an empty list when there are no rows to sort"
                    (cl-weave:expect
                     (ntop.process.domain::sort-process-domain-rows nil
                                                                    (wp4-sort-state))
                     :to-be
                     nil)))
