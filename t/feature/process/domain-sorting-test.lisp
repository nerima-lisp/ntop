(in-package #:ntop.test)

(cl-weave:describe "WP4 process sorting"
                   (cl-weave:it-each
                    ((:cpu-descending :cpu-percent
                                      :percent
                                      :descending
                                      10
                                      5
                                      (1 2))
                     (:cpu-ascending :cpu-percent
                                     :percent
                                     :ascending
                                     10
                                     5
                                     (2 1))
                     (:memory-bytes-ascending :memory
                                              :bytes
                                              :ascending
                                              300
                                              100
                                              (2 1))
                     (:memory-percent-descending :memory
                                                 :percent
                                                 :descending
                                                 5
                                                 20
                                                 (2 1))
                     (:gpu-usage-descending :gpu-usage-percent
                                            :percent
                                            :descending
                                            10
                                            40
                                            (2 1))
                     (:gpu-memory-ascending :gpu-memory
                                            :percent
                                            :ascending
                                            100
                                            20
                                            (2 1))
                     (:name-ascending :name
                                      :percent
                                      :ascending
                                      "beta"
                                      "Alpha"
                                      (2 1))
                     (:name-descending :name
                                       :percent
                                       :descending
                                       "beta"
                                       "Alpha"
                                       (1 2))
                     (:command-ascending :command
                                         :percent
                                         :ascending
                                         "z"
                                         "A"
                                         (2 1))
                     (:command-descending :command
                                          :percent
                                          :descending
                                          "z"
                                          "A"
                                          (1 2))
                     (:missing-ascending :cpu-percent
                                         :percent
                                         :ascending
                                         nil
                                         5
                                         (1 2))
                     (:missing-descending :cpu-percent
                                          :percent
                                          :descending
                                          nil
                                          5
                                          (2 1)))
                    "WP4 sort key case ~A uses ~A order"
                    (case-name key
                               memory-mode
                               order
                               first-value
                               second-value
                               expected)
                    (declare (ignorable case-name))
                    (let* ((rows
                            (list
                             (%wp4-sorting-row-with-values 1
                                                           key
                                                           first-value
                                                           memory-mode)
                             (%wp4-sorting-row-with-values 2
                                                           key
                                                           second-value
                                                           memory-mode)))
                           (state
                            (ntop.process.domain::make-process-sort-state :key
                                                                          key
                                                                          :order
                                                                          order
                                                                          :memory-mode
                                                                          memory-mode)))
                      (cl-weave:expect
                       (%wp4-sorted-pids
                        (ntop.process.domain::sort-process-domain-rows rows
                                                                       state))
                       :to-equal
                       expected)))
                   (cl-weave:it-each
                    ((:virtual-memory-descending :virtual-memory-bytes
                                                 :descending
                                                 400
                                                 100
                                                 (1 2))
                     (:count-ascending :count :ascending 1 2 (1 2))
                     (:read-rate-descending :read-bytes-per-second
                                            :descending
                                            10
                                            5
                                            (1 2))
                     (:write-rate-descending :write-bytes-per-second
                                             :descending
                                             5
                                             15
                                             (2 1))
                     (:total-read-descending :total-read-bytes
                                             :descending
                                             10
                                             50
                                             (2 1))
                     (:total-write-descending :total-write-bytes
                                              :descending
                                              50
                                              10
                                              (1 2))
                     (:state-case-sensitive :state :ascending "Z" "a" (1 2))
                     (:user-case-sensitive :user :ascending "Zed" "alpha" (1 2))
                     (:user-nil-first :user :ascending nil "alpha" (1 2))
                     (:runtime-ascending :runtime-seconds
                                         :ascending
                                         20
                                         10
                                         (2 1))
                     (:priority-descending :priority :descending 10 20 (2 1))
                     (:nice-ascending :nice :ascending -5 5 (1 2)))
                    "WP4 column key case ~A sorts process values"
                    (case-name key order first-value second-value expected)
                    (declare (ignorable case-name))
                    (let* ((rows
                            (list
                             (%wp4-sorting-row-for-column 1 key first-value)
                             (%wp4-sorting-row-for-column 2 key second-value)))
                           (state
                            (ntop.process.domain::make-process-sort-state :key
                                                                          key
                                                                          :order
                                                                          order)))
                      (cl-weave:expect
                       (%wp4-sorted-pids
                        (ntop.process.domain::sort-process-domain-rows rows
                                                                       state))
                       :to-equal
                       expected)))
                   (cl-weave:it-each
                    ((#\c nil :cpu-percent) (#\m nil :memory)
                     (#\p nil :pid)
                     (#\n nil :name)
                     (#\n t :command)
                     (#\C nil :gpu-usage-percent)
                     (#\M nil :gpu-memory)
                     (#\x nil nil))
                    "WP4 key character ~A selects ~A"
                    (character command-p expected)
                    (cl-weave:expect
                     (ntop.process.domain::process-sort-key-from-character
                      character
                      :command-p
                      command-p)
                     :to-be
                     expected))
                   (cl-weave:it-each
                    ((:cpu-percent :descending) (:memory :descending)
                     (:virtual-memory-bytes :descending)
                     (:pid :ascending)
                     (:count :ascending)
                     (:name :ascending)
                     (:command :ascending)
                     (:read-bytes-per-second :descending)
                     (:write-bytes-per-second :descending)
                     (:total-read-bytes :descending)
                     (:total-write-bytes :descending)
                     (:state :ascending)
                     (:user :ascending)
                     (:runtime-seconds :ascending)
                     (:priority :descending)
                     (:nice :ascending)
                     (:gpu-usage-percent :descending)
                     (:gpu-memory :descending))
                    "WP4 default order for ~A is ~A"
                    (key expected-order)
                    (let ((state
                           (ntop.process.domain::make-process-sort-state :key
                                                                         key)))
                      (cl-weave:expect
                       (ntop.process.domain::process-sort-state-order state)
                       :to-be
                       expected-order)))
                   (cl-weave:it "defaults to CPU percentage descending"
                                (let ((state
                                       (ntop.process.domain::make-process-sort-state)))
                                  (cl-weave:expect
                                   (list
                                    (ntop.process.domain::process-sort-state-key
                                     state)
                                    (ntop.process.domain::process-sort-state-order
                                     state))
                                   :to-equal
                                   '(:cpu-percent :descending))))
                   (cl-weave:it
                    "repeated key selection reverses direction each time"
                    (let* ((initial
                            (ntop.process.domain::make-process-sort-state))
                           (ascending
                            (ntop.process.domain::process-sort-select-key
                             initial
                             :cpu-percent))
                           (descending
                            (ntop.process.domain::process-sort-select-key
                             ascending
                             :cpu-percent)))
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-sort-state-order initial)
                        (ntop.process.domain::process-sort-state-order
                         ascending)
                        (ntop.process.domain::process-sort-state-order
                         descending))
                       :to-equal
                       '(:descending :ascending :descending))))
                   (cl-weave:it
                    "selecting another key uses that key's default order"
                    (let* ((ascending-cpu
                            (ntop.process.domain::process-sort-select-key
                             (ntop.process.domain::make-process-sort-state)
                             :cpu-percent))
                           (pid-state
                            (ntop.process.domain::process-sort-select-key
                             ascending-cpu
                             :pid))
                           (gpu-state
                            (ntop.process.domain::process-sort-select-key
                             pid-state
                             :gpu-usage-percent)))
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-sort-state-key pid-state)
                        (ntop.process.domain::process-sort-state-order
                         pid-state)
                        (ntop.process.domain::process-sort-state-order
                         gpu-state))
                       :to-equal
                       '(:pid :ascending :descending))))
                   (cl-weave:it
                    "global inversion reverses the active order without changing the key"
                    (let* ((state
                            (ntop.process.domain::make-process-sort-state :key
                                                                          :name))
                           (inverted
                            (ntop.process.domain::process-sort-invert state))
                           (restored
                            (ntop.process.domain::process-sort-invert inverted)))
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-sort-state-key inverted)
                        (ntop.process.domain::process-sort-state-order inverted)
                        (ntop.process.domain::process-sort-state-order restored))
                       :to-equal
                       '(:name :descending :ascending))))
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next))))
