(in-package #:ntop.test)

(cl-weave:describe "WP4 process metrics"
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "WP4 process read rate follows fractional elapsed time"
                    ((values
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 1 :max 60))))
                    (destructuring-bind (previous increment elapsed) values
                      (let* ((before
                              (%wp4-metrics-record 81 :read-bytes previous))
                             (after
                              (%wp4-metrics-record 81
                                                   :read-bytes
                                                   (+ previous increment)))
                             (actual
                              (%wp4-read-bytes-per-second
                               (%wp4-metrics-sample 0 before)
                               (%wp4-metrics-sample elapsed after)
                               81)))
                        (cl-weave:expect actual :to-equal (/ increment elapsed)))))
                   (cl-weave:it-property
                    "WP4 reset read counters never produce negative rates"
                    ((values
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 1 :max 60))))
                    (destructuring-bind (previous current elapsed) values
                      (let ((actual
                             (%wp4-read-bytes-per-second
                              (%wp4-metrics-sample 0
                                                   (%wp4-metrics-record 91
                                                                        :read-bytes
                                                                        previous))
                              (%wp4-metrics-sample elapsed
                                                   (%wp4-metrics-record 91
                                                                        :read-bytes
                                                                        current))
                              91)))
                        (cl-weave:expect actual
                                         :to-satisfy
                                         (lambda (rate)
                                           (>= rate 0))))))
                   (cl-weave:it
                    "returns no current-usage percentage without system activity data"
                    (let* ((previous
                            (%wp4-metrics-sample 0
                                                 (%wp4-metrics-record 101
                                                                      :cpu-time-ns
                                                                      0)))
                           (current
                            (%wp4-metrics-sample 1
                                                 (%wp4-metrics-record 101
                                                                      :cpu-time-ns
                                                                      1000000000))))
                      (cl-weave:expect
                       (ntop.process.domain::process-cpu-percent previous
                                                                 current
                                                                 101
                                                                 4
                                                                 :current-usage-p
                                                                 t)
                       :to-be
                       nil)))
                   (cl-weave:it "rejects unsupported CPU accounting modes"
                                (let* ((previous
                                        (%wp4-metrics-sample 0
                                                             (%wp4-metrics-record
                                                              102
                                                              :cpu-time-ns
                                                              0)))
                                       (current
                                        (%wp4-metrics-sample 1
                                                             (%wp4-metrics-record
                                                              102
                                                              :cpu-time-ns
                                                              1000000000))))
                                  (cl-weave:expect
                                   (handler-case (progn
                                                   (ntop.process.domain::process-cpu-percent
                                                    previous
                                                    current
                                                    102
                                                    2
                                                    :cpu-mode
                                                    :unsupported)
                                                   :returned)
                                     (error ()
                                       :signaled))
                                   :to-be
                                   :signaled)))
                   (cl-weave:it
                    "leaves normalized CPU unavailable when capacity is zero"
                    (let* ((previous
                            (%wp4-metrics-sample 0
                                                 (%wp4-metrics-record 103
                                                                      :cpu-time-ns
                                                                      0)))
                           (current
                            (%wp4-metrics-sample 1
                                                 (%wp4-metrics-record 103
                                                                      :cpu-time-ns
                                                                      1000000000))))
                      (cl-weave:expect
                       (ntop.process.domain::process-cpu-percent previous
                                                                 current
                                                                 103
                                                                 0)
                       :to-be
                       nil)))
                   (cl-weave:it
                    "keeps zero-total memory and future start times unavailable or clamped"
                    (let* ((sample
                            (%wp4-metrics-sample 1
                                                 (%wp4-metrics-record 104
                                                                      :resident-memory-bytes
                                                                      10
                                                                      :gpu-memory-bytes
                                                                      4
                                                                      :start-time-unix-seconds
                                                                      100)))
                           (row
                            (apply
                             (function
                              ntop.process.domain::process-row-from-samples)
                             nil
                             sample
                             104
                             (list :memory-total-bytes
                                   0
                                   :gpu-memory-total-bytes
                                   0
                                   :current-time-unix-seconds
                                   90))))
                      (cl-weave:expect
                       (list
                        (ntop.process.domain:process-row-memory-percent row)
                        (ntop.process.domain:process-row-gpu-memory-percent row)
                        (ntop.process.domain:process-row-elapsed-seconds row))
                       :to-equal
                       (list nil nil 0)))))
