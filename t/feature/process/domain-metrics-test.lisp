(in-package #:ntop.test)

(cl-weave:describe "WP4 process metrics"
                   (cl-weave:it-each
                    ((:cpu-normal 0 1000000000 2 4 nil nil nil nil 12.5)
                     (:cpu-unnormalized 0 1000000000 2 4 t nil nil nil 50)
                     (:cpu-current-usage 0 1000000000 2 4 nil t 0.5 nil 25)
                     (:cpu-cgroup-capacity 0 1000000000 2 2 nil nil nil nil 25)
                     (:cpu-counter-reset 100 50 1 2 nil nil nil nil 0)
                     (:cpu-empty-system 0 1000000000 1 2 nil t 0 nil 0)
                     (:cpu-missing-capacity 0
                                            1000000000
                                            1
                                            nil
                                            nil
                                            nil
                                            nil
                                            nil
                                            nil)
                     (:cpu-unnormalized-without-capacity 0
                                                         1000000000
                                                         1
                                                         nil
                                                         t
                                                         nil
                                                         nil
                                                         nil
                                                         100)
                     (:cpu-ps-fallback-normalized nil nil 1 4 nil nil nil 80 20)
                     (:cpu-ps-fallback-unnormalized nil nil 1 4 t t 0.5 80 80)
                     (:cpu-ps-fallback-ignores-current-usage 0
                                                             nil
                                                             1
                                                             2
                                                             nil
                                                             t
                                                             0.25
                                                             40
                                                             20)
                     (:cpu-unavailable nil nil 1 4 nil nil nil nil nil))
                    "~A computes process CPU percentage from sample counters"
                    (case-name previous
                               current
                               elapsed
                               capacity
                               unnormalized-p
                               current-usage-p
                               active-fraction
                               fallback
                               expected)
                    (declare (ignorable case-name))
                    (let* ((previous-record
                            (%wp4-metrics-record 17 :cpu-time-ns previous))
                           (current-record
                            (%wp4-metrics-record 17
                                                 :cpu-time-ns
                                                 current
                                                 :ps-fallback-cpu-percent
                                                 fallback))
                           (previous-sample
                            (%wp4-metrics-sample 4 previous-record))
                           (current-sample
                            (%wp4-metrics-sample (+ 4 elapsed) current-record))
                           (actual
                            (ntop.process.domain::process-cpu-percent
                             previous-sample
                             current-sample
                             17
                             capacity
                             :unnormalized-p
                             unnormalized-p
                             :current-usage-p
                             current-usage-p
                             :active-system-cpu-fraction
                             active-fraction)))
                      (cl-weave:expect
                       (or (and (null actual) (null expected))
                           (and (numberp actual)
                                (numberp expected)
                                (= actual expected)))
                       :to-be
                       t)))
                   (cl-weave:it-each
                    ((:linux-normal 0
                                    1000000000
                                    2
                                    4
                                    nil
                                    nil
                                    0.5
                                    4000000000
                                    12.5)
                     (:linux-current-usage 0
                                           1000000000
                                           2
                                           4
                                           nil
                                           t
                                           0.5
                                           4000000000
                                           25)
                     (:linux-unnormalized 0
                                          1000000000
                                          2
                                          2
                                          t
                                          nil
                                          0.5
                                          4000000000
                                          25)
                     (:linux-unnormalized-current 0
                                                  1000000000
                                                  2
                                                  2
                                                  t
                                                  t
                                                  0.5
                                                  4000000000
                                                  50)
                     (:linux-no-active-fraction 0
                                                1000000000
                                                2
                                                4
                                                nil
                                                nil
                                                nil
                                                4000000000
                                                nil)
                     (:linux-no-active-delta 0 1000000000 2 4 nil t 0.5 nil nil)
                     (:linux-zero-active-delta 0 1000000000 2 4 nil t 0.5 0 nil)
                     (:linux-process-counter-reset 100
                                                   50
                                                   2
                                                   4
                                                   nil
                                                   t
                                                   0.5
                                                   4000000000
                                                   0))
                    "WP4 Linux CPU case ~A follows active system accounting"
                    (case-name previous
                               current
                               elapsed
                               capacity
                               unnormalized-p
                               current-usage-p
                               active-fraction
                               active-delta
                               expected)
                    (declare (ignorable case-name))
                    (let* ((previous-record
                            (%wp4-metrics-record 18 :cpu-time-ns previous))
                           (current-record
                            (%wp4-metrics-record 18 :cpu-time-ns current))
                           (actual
                            (ntop.process.domain::process-cpu-percent
                             (%wp4-metrics-sample 0 previous-record)
                             (%wp4-metrics-sample elapsed current-record)
                             18
                             capacity
                             :cpu-mode
                             :linux
                             :unnormalized-p
                             unnormalized-p
                             :current-usage-p
                             current-usage-p
                             :active-system-cpu-fraction
                             active-fraction
                             :linux-active-cpu-delta
                             active-delta)))
                      (cl-weave:expect
                       (or (and (null actual) (null expected))
                           (and (numberp actual)
                                (numberp expected)
                                (= actual expected)))
                       :to-be
                       t)))
                   (cl-weave:it-each
                    ((:io-fractional-seconds 0 700 0 1400 7/10 1000 2000)
                     (:io-half-second 10 260 20 120 0.5 500 200)
                     (:io-unchanged 5 5 11 11 0.25 0 0)
                     (:io-counter-reset 100 50 90 0 0.5 0 0)
                     (:io-missing-previous-read nil 100 0 200 1 nil 200)
                     (:io-missing-previous-write 0 100 nil 200 2 50 nil)
                     (:io-missing-current-read 0 nil 0 200 2 nil 100)
                     (:io-missing-current-write 0 100 0 nil 2 50 nil))
                    "~A uses real elapsed seconds for process I/O"
                    (case-name previous-read
                               current-read
                               previous-write
                               current-write
                               elapsed
                               expected-read
                               expected-write)
                    (declare (ignorable case-name))
                    (let* ((previous-record
                            (%wp4-metrics-record 23
                                                 :read-bytes
                                                 previous-read
                                                 :write-bytes
                                                 previous-write))
                           (current-record
                            (%wp4-metrics-record 23
                                                 :read-bytes
                                                 current-read
                                                 :write-bytes
                                                 current-write))
                           (previous-sample
                            (%wp4-metrics-sample 5 previous-record))
                           (current-sample
                            (%wp4-metrics-sample (+ 5 elapsed) current-record)))
                      (let ((rate
                             (%wp4-read-bytes-per-second previous-sample
                              current-sample
                              23)))
                        (cl-weave:expect
                         (or (and (null rate) (null expected-read))
                             (and (numberp rate)
                                  (numberp expected-read)
                                  (= rate expected-read)))
                         :to-be
                         t))
                      (let ((rate
                             (ntop.process.domain::process-write-bytes-per-second
                              previous-sample
                              current-sample
                              23)))
                        (cl-weave:expect
                         (or (and (null rate) (null expected-write))
                             (and (numberp rate)
                                  (numberp expected-write)
                                  (= rate expected-write)))
                         :to-be
                         t))))
                   (cl-weave:it
                    "returns unavailable rates when a process is absent from a sample"
                    (let ((previous-sample
                           (%wp4-metrics-sample 10
                                                (%wp4-metrics-record 31
                                                                     :read-bytes
                                                                     10)))
                          (current-sample
                           (%wp4-metrics-sample 11
                                                (%wp4-metrics-record 32
                                                                     :read-bytes
                                                                     20))))
                      (cl-weave:expect
                       (%wp4-read-bytes-per-second previous-sample
                        current-sample
                        31)
                       :to-be
                       nil)))
                   (cl-weave:it "rejects nonpositive elapsed sample intervals"
                                (let ((sample
                                       (%wp4-metrics-sample 10
                                                            (%wp4-metrics-record
                                                             41
                                                             :read-bytes
                                                             2))))
                                  (cl-weave:expect
                                   (handler-case (progn
                                                   (%wp4-read-bytes-per-second sample
                                                    sample
                                                    41)
                                                   :returned)
                                     (error ()
                                       :signaled))
                                   :to-be
                                   :signaled)))
                   (cl-weave:it
                    "builds every computed row field without mutating either sample"
                    (let* ((previous-record
                            (%wp4-metrics-record 51
                                                 :ppid
                                                 1
                                                 :cpu-time-ns
                                                 0
                                                 :read-bytes
                                                 100
                                                 :write-bytes
                                                 0))
                           (current-record
                            (%wp4-metrics-record 51
                                                 :ppid
                                                 7
                                                 :name
                                                 "worker"
                                                 :command
                                                 "worker --run"
                                                 :cpu-time-ns
                                                 1000000000
                                                 :resident-memory-bytes
                                                 40
                                                 :virtual-memory-bytes
                                                 800
                                                 :read-bytes
                                                 800
                                                 :write-bytes
                                                 1400
                                                 :uid
                                                 501
                                                 :user-name
                                                 "alice"
                                                 :state
                                                 "R"
                                                 :start-time-unix-seconds
                                                 900000
                                                 :priority
                                                 10
                                                 :nice
                                                 -5
                                                 :gpu-usage-percent
                                                 12.5
                                                 :gpu-memory-bytes
                                                 40))
                           (previous-sample
                            (%wp4-metrics-sample 200 previous-record))
                           (current-sample
                            (%wp4-metrics-sample 201 current-record))
                           (row
                            (ntop.process.domain::process-row-from-samples
                             previous-sample
                             current-sample
                             51
                             :cpu-capacity
                             2
                             :memory-total-bytes
                             100
                             :gpu-memory-total-bytes
                             200
                             :current-time-unix-seconds
                             900010))
                           (second-row
                            (ntop.process.domain::process-row-from-samples
                             previous-sample
                             current-sample
                             51
                             :cpu-capacity
                             2
                             :memory-total-bytes
                             100
                             :gpu-memory-total-bytes
                             200
                             :current-time-unix-seconds
                             900010)))
                      (cl-weave:expect
                       (list (ntop.process.domain:process-row-pid row)
                             (ntop.process.domain:process-row-ppid row)
                             (ntop.process.domain:process-row-name row)
                             (ntop.process.domain:process-row-command row)
                             (ntop.process.domain:process-row-cpu-percent row)
                             (ntop.process.domain:process-row-memory-percent
                              row)
                             (ntop.process.domain:process-row-memory-bytes row)
                             (ntop.process.domain:process-row-virtual-memory-bytes
                              row)
                             (ntop.process.domain:process-row-read-bytes-per-second
                              row)
                             (ntop.process.domain:process-row-write-bytes-per-second
                              row)
                             (ntop.process.domain:process-row-total-read-bytes
                              row)
                             (ntop.process.domain:process-row-total-write-bytes
                              row)
                             (ntop.process.domain:process-row-user-name row)
                             (ntop.process.domain:process-row-state row)
                             (ntop.process.domain:process-row-elapsed-seconds
                              row)
                             (ntop.process.domain:process-row-priority row)
                             (ntop.process.domain:process-row-nice row)
                             (ntop.process.domain:process-row-gpu-usage-percent
                              row)
                             (ntop.process.domain:process-row-gpu-memory-bytes
                              row)
                             (ntop.process.domain:process-row-gpu-memory-percent
                              row))
                       :to-equal
                       '(51 7
                            "worker"
                            "worker --run"
                            50
                            40
                            40
                            800
                            700
                            1400
                            800
                            1400
                            "alice"
                            "R"
                            10
                            10
                            -5
                            12.5
                            40
                            20))
                      (cl-weave:expect (eq row second-row) :to-be nil)
                      (cl-weave:expect
                       (list
                        (ntop.shared.domain:sample-record-sampled-at
                         previous-sample)
                        (ntop.shared.domain:sample-record-sampled-at
                         current-sample)
                        (ntop.process.domain:process-record-cpu-time-ns
                         previous-record)
                        (ntop.process.domain:process-record-cpu-time-ns
                         current-record)
                        (ntop.process.domain:process-record-read-bytes
                         previous-record)
                        (ntop.process.domain:process-record-read-bytes
                         current-record)
                        (ntop.process.domain:process-record-write-bytes
                         previous-record)
                        (ntop.process.domain:process-record-write-bytes
                         current-record))
                       :to-equal
                       '(200 201 0 1000000000 100 800 0 1400))))
                   (cl-weave:it
                    "leaves unavailable memory and runtime values absent"
                    (let* ((sample
                            (%wp4-metrics-sample 1
                                                 (%wp4-metrics-record 61
                                                                      :resident-memory-bytes
                                                                      nil)))
                           (row
                            (ntop.process.domain::process-row-from-samples nil
                                                                           sample
                                                                           61
                                                                           :memory-total-bytes
                                                                           0)))
                      (cl-weave:expect
                       (list
                        (ntop.process.domain:process-row-memory-percent row)
                        (ntop.process.domain:process-row-elapsed-seconds row)
                        (ntop.process.domain:process-row-read-bytes-per-second
                         row))
                       :to-equal
                       '(nil nil nil))))
                   (cl-weave:it
                    "returns no row when the current sample has no requested PID"
                    (cl-weave:expect
                     (ntop.process.domain::process-row-from-samples nil
                                                                    (%wp4-metrics-sample
                                                                     1)
                                                                    71)
                     :to-be
                     nil)))
