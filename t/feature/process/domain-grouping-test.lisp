(in-package #:ntop.test)

(defun %wp4-grouping-row (pid &key
                              (name "worker")
                              (command "worker --run")
                              cpu-percent
                              memory-bytes
                              memory-percent
                              read-rate
                              write-rate
                              total-read
                              total-write
                              gpu-percent
                              gpu-memory
                              gpu-memory-percent
                              elapsed-seconds)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :name
                                        name
                                        :command
                                        command
                                        :cpu-percent
                                        cpu-percent
                                        :memory-bytes
                                        memory-bytes
                                        :memory-percent
                                        memory-percent
                                        :read-bytes-per-second
                                        read-rate
                                        :write-bytes-per-second
                                        write-rate
                                        :total-read-bytes
                                        total-read
                                        :total-write-bytes
                                        total-write
                                        :user-name
                                        (format nil "user-~D" pid)
                                        :state
                                        "S"
                                        :elapsed-seconds
                                        elapsed-seconds
                                        :priority
                                        pid
                                        :nice
                                        (- pid)
                                        :gpu-usage-percent
                                        gpu-percent
                                        :gpu-memory-bytes
                                        gpu-memory
                                        :gpu-memory-percent
                                        gpu-memory-percent))

(defun %wp4-group-row-with-value (pid field value)
  (ecase field
    (:cpu (%wp4-grouping-row pid :cpu-percent value))
    (:memory-bytes (%wp4-grouping-row pid :memory-bytes value))
    (:memory-percent (%wp4-grouping-row pid :memory-percent value))
    (:read-rate (%wp4-grouping-row pid :read-rate value))
    (:write-rate (%wp4-grouping-row pid :write-rate value))
    (:total-read (%wp4-grouping-row pid :total-read value))
    (:total-write (%wp4-grouping-row pid :total-write value))
    (:gpu-percent (%wp4-grouping-row pid :gpu-percent value))
    (:gpu-memory (%wp4-grouping-row pid :gpu-memory value))
    (:gpu-memory-percent (%wp4-grouping-row pid :gpu-memory-percent value))
    (:runtime (%wp4-grouping-row pid :elapsed-seconds value))))

(cl-weave:describe "WP4 process grouping"
                   (cl-weave:it-each
                    ((:cpu 10 20 30) (:memory-bytes 100 200 300)
                     (:memory-percent 2.5 7.5 10)
                     (:read-rate 8 12 20)
                     (:write-rate 9 11 20)
                     (:total-read 40 60 100)
                     (:total-write 70 30 100)
                     (:gpu-percent 1 3 4)
                     (:gpu-memory 200 300 500)
                     (:gpu-memory-percent 20 30 50))
                    "WP4 grouped ~A sums both member values"
                    (field first-value second-value expected)
                    (let* ((rows
                            (list
                             (%wp4-group-row-with-value 3 field first-value)
                             (%wp4-group-row-with-value 1 field second-value)))
                           (group
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              rows
                              :name)))
                           (actual
                            (ecase field
                              (:cpu
                               (ntop.process.domain:process-row-cpu-percent
                                group))
                              (:memory-bytes
                               (ntop.process.domain:process-row-memory-bytes
                                group))
                              (:memory-percent
                               (ntop.process.domain:process-row-memory-percent
                                group))
                              (:read-rate
                               (ntop.process.domain:process-row-read-bytes-per-second
                                group))
                              (:write-rate
                               (ntop.process.domain:process-row-write-bytes-per-second
                                group))
                              (:total-read
                               (ntop.process.domain:process-row-total-read-bytes
                                group))
                              (:total-write
                               (ntop.process.domain:process-row-total-write-bytes
                                group))
                              (:gpu-percent
                               (ntop.process.domain:process-row-gpu-usage-percent
                                group))
                              (:gpu-memory
                               (ntop.process.domain:process-row-gpu-memory-bytes
                                group))
                              (:gpu-memory-percent
                               (ntop.process.domain:process-row-gpu-memory-percent
                                group)))))
                      (cl-weave:expect
                       (and (numberp actual)
                            (numberp expected)
                            (= actual expected))
                       :to-be
                       t)))
                   (cl-weave:it
                    "uses maximum runtime and minimum-PID representative values"
                    (let* ((rows
                            (list
                             (%wp4-grouping-row 9
                                                :name
                                                "worker"
                                                :command
                                                "worker --nine"
                                                :cpu-percent
                                                1
                                                :elapsed-seconds
                                                30)
                             (%wp4-grouping-row 2
                                                :name
                                                "worker"
                                                :command
                                                "worker --two"
                                                :cpu-percent
                                                3
                                                :elapsed-seconds
                                                45)))
                           (group
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              rows
                              :name))))
                      (cl-weave:expect
                       (list (ntop.process.domain:process-row-count group)
                             (ntop.process.domain:process-row-cpu-percent group)
                             (ntop.process.domain:process-row-elapsed-seconds
                              group)
                             (ntop.process.domain:process-row-pid group)
                             (ntop.process.domain:process-row-command group)
                             (ntop.process.domain:process-row-user-name group))
                       :to-equal
                       '(2 4 45 2 "worker --two" "user-2"))))
                   (cl-weave:it-each
                    ((:same-name :name "worker" "worker" "one" "two" 1)
                     (:different-name :name "worker" "worker2" "same" "same" 2)
                     (:case-sensitive-name :name
                                           "worker"
                                           "Worker"
                                           "same"
                                           "same"
                                           2)
                     (:same-command :command
                                    "one"
                                    "two"
                                    "worker --run"
                                    "worker --run"
                                    1)
                     (:different-command :command
                                         "one"
                                         "one"
                                         "worker --one"
                                         "worker --two"
                                         2)
                     (:same-command-different-name :command
                                                   "one"
                                                   "two"
                                                   "same"
                                                   "same"
                                                   1)
                     (:nil-name :name nil nil "one" "two" 1)
                     (:nil-command :command "one" "two" nil nil 1))
                    "WP4 grouping key case ~A keeps ~A groups"
                    (case-name key-kind
                               first-name
                               second-name
                               first-command
                               second-command
                               expected-count)
                    (let* ((rows
                            (list
                             (%wp4-grouping-row 5
                                                :name
                                                first-name
                                                :command
                                                first-command)
                             (%wp4-grouping-row 6
                                                :name
                                                second-name
                                                :command
                                                second-command)))
                           (groups
                            (ntop.process.domain::group-process-domain-rows rows
                                                                            key-kind)))
                      (cl-weave:expect (length groups) :to-be expected-count)))
                   (cl-weave:it
                    "keeps first-seen group ordering for equal sort keys"
                    (let* ((rows
                            (list
                             (%wp4-grouping-row 1 :name "beta" :cpu-percent 5)
                             (%wp4-grouping-row 2 :name "alpha" :cpu-percent 5)
                             (%wp4-grouping-row 3 :name "beta" :cpu-percent 9)))
                           (groups
                            (ntop.process.domain::group-process-domain-rows rows
                                                                            :name)))
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (ntop.process.domain:process-row-name row))
                        groups)
                       :to-equal
                       '("beta" "alpha"))))
                   (cl-weave:it
                    "keeps a grouped measurement unavailable when any member is missing"
                    (let* ((rows
                            (list
                             (%wp4-grouping-row 1 :cpu-percent 10 :read-rate 2)
                             (%wp4-grouping-row 2
                                                :cpu-percent
                                                nil
                                                :read-rate
                                                nil)))
                           (group
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              rows
                              :name))))
                      (cl-weave:expect
                       (list (ntop.process.domain:process-row-cpu-percent group)
                             (ntop.process.domain:process-row-read-bytes-per-second
                              group))
                       :to-equal
                       '(nil nil))))
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "WP4 grouping sum and count cover every same-name member"
                    ((values
                      (cl-weave:gen-list (cl-weave:gen-integer :min 0 :max 1000)
                                         :min-length
                                         1
                                         :max-length
                                         12)))
                    (let* ((rows
                            (loop for cpu in values
                                  for pid from 100
                                  collect (%wp4-grouping-row pid
                                                             :name
                                                             "same"
                                                             :cpu-percent
                                                             cpu)))
                           (group
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              rows
                              :name))))
                      (cl-weave:expect
                       (ntop.process.domain:process-row-cpu-percent group)
                       :to-equal
                       (reduce #'+ values))
                      (cl-weave:expect
                       (ntop.process.domain:process-row-count group)
                       :to-be
                       (length values))))
                   (cl-weave:it-property
                    "WP4 grouping chooses the minimum PID independent of input order"
                    ((values
                      (cl-weave:gen-list (cl-weave:gen-integer :min 0 :max 1000)
                                         :min-length
                                         1
                                         :max-length
                                         12)))
                    (let* ((rows
                            (loop for cpu in values
                                  for pid from 100
                                  collect (%wp4-grouping-row pid
                                                             :name
                                                             "same"
                                                             :cpu-percent
                                                             cpu)))
                           (group
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              (reverse rows)
                              :name))))
                      (cl-weave:expect
                       (ntop.process.domain:process-row-pid group)
                       :to-be
                       100)))
                   (cl-weave:it
                    "uses a non-null PID as the group representative"
                    (let* ((missing
                            (ntop.process.domain:make-process-row :count
                                                                  1
                                                                  :pid
                                                                  nil
                                                                  :name
                                                                  "worker"
                                                                  :user-name
                                                                  "unknown"))
                           (known
                            (ntop.process.domain:make-process-row :count
                                                                  1
                                                                  :pid
                                                                  5
                                                                  :name
                                                                  "worker"
                                                                  :user-name
                                                                  "known"))
                           (missing-first
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              (list missing known)
                              :name)))
                           (missing-last
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              (list known missing)
                              :name))))
                      (cl-weave:expect
                       (list (ntop.process.domain:process-row-pid missing-first)
                             (ntop.process.domain:process-row-pid missing-last)
                             (ntop.process.domain:process-row-user-name
                              missing-first)
                             (ntop.process.domain:process-row-user-name
                              missing-last))
                       :to-equal
                       (list 5 5 "known" "known"))))
                   (cl-weave:it
                    "counts a row with an unavailable count as one member"
                    (let ((group
                           (first
                            (ntop.process.domain::group-process-domain-rows
                             (list
                              (ntop.process.domain:make-process-row :pid
                                                                    1
                                                                    :name
                                                                    "worker")
                              (ntop.process.domain:make-process-row :pid
                                                                    2
                                                                    :name
                                                                    "worker"
                                                                    :count
                                                                    3))
                             :name))))
                      (cl-weave:expect
                       (ntop.process.domain:process-row-count group)
                       :to-be
                       4)))
                   (cl-weave:it
                    "returns no groups for empty input and rejects unsupported keys"
                    (cl-weave:expect
                     (ntop.process.domain::group-process-domain-rows nil :name)
                     :to-be
                     nil)
                    (cl-weave:expect
                     (handler-case (progn
                                     (ntop.process.domain::group-process-domain-rows
                                      (list (%wp4-grouping-row 1))
                                      :invalid)
                                     :returned)
                       (error ()
                         :signaled))
                     :to-be
                     :signaled))
                   (cl-weave:it
                    "keeps grouped runtime unavailable when a member has no runtime"
                    (let ((group
                           (first
                            (ntop.process.domain::group-process-domain-rows
                             (list (%wp4-grouping-row 1 :elapsed-seconds nil)
                                   (%wp4-grouping-row 2 :elapsed-seconds 10))
                             :name))))
                      (cl-weave:expect
                       (ntop.process.domain:process-row-elapsed-seconds group)
                       :to-be
                       nil))))
