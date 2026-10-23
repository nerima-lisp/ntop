(in-package #:ntop.test)

(defun %wp4-sorting-row (pid key value)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :name
                                        (if (eq key :name)
                                            value
                                            (format nil "name-~D" pid))
                                        :command
                                        (if (eq key :command)
                                            value
                                            (format nil "command-~D" pid))
                                        :cpu-percent
                                        (if (eq key :cpu-percent)
                                            value
                                            nil)
                                        :memory-bytes
                                        (if (eq key :memory-bytes)
                                            value
                                            nil)
                                        :memory-percent
                                        (if (eq key :memory-percent)
                                            value
                                            nil)
                                        :gpu-usage-percent
                                        (if (eq key :gpu-usage-percent)
                                            value
                                            nil)
                                        :gpu-memory-bytes
                                        (if (eq key :gpu-memory-bytes)
                                            value
                                            nil)
                                        :gpu-memory-percent
                                        (if (eq key :gpu-memory-percent)
                                            value
                                            nil)))

(defun %wp4-sorting-row-with-values (pid key value memory-mode)
  (let ((field
         (if (eq key :memory)
             (if (eq memory-mode :bytes)
                 :memory-bytes
                 :memory-percent)
             (if (eq key :gpu-memory)
                 (if (eq memory-mode :bytes)
                     :gpu-memory-bytes
                     :gpu-memory-percent)
                 key))))
    (%wp4-sorting-row pid field value)))

(defun %wp4-sorted-pids (rows)
  (mapcar #'ntop.process.domain:process-row-pid rows))

(defun %wp4-sorting-row-for-column (pid key value)
  (ntop.process.domain:make-process-row :count
                                        (if (eq key :count)
                                            value
                                            1)
                                        :pid
                                        pid
                                        :name
                                        (if (eq key :name)
                                            value
                                            (format nil "name-~D" pid))
                                        :command
                                        (if (eq key :command)
                                            value
                                            (format nil "command-~D" pid))
                                        :virtual-memory-bytes
                                        (if (eq key :virtual-memory-bytes)
                                            value
                                            nil)
                                        :read-bytes-per-second
                                        (if (eq key :read-bytes-per-second)
                                            value
                                            nil)
                                        :write-bytes-per-second
                                        (if (eq key :write-bytes-per-second)
                                            value
                                            nil)
                                        :total-read-bytes
                                        (if (eq key :total-read-bytes)
                                            value
                                            nil)
                                        :total-write-bytes
                                        (if (eq key :total-write-bytes)
                                            value
                                            nil)
                                        :user-name
                                        (if (eq key :user)
                                            value
                                            nil)
                                        :state
                                        (if (eq key :state)
                                            value
                                            nil)
                                        :elapsed-seconds
                                        (if (eq key :runtime-seconds)
                                            value
                                            nil)
                                        :priority
                                        (if (eq key :priority)
                                            value
                                            nil)
                                        :nice
                                        (if (eq key :nice)
                                            value
                                            nil)))

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
                                                                           (ntop.process.domain::make-process-sort-state
                                                                            :key
                                                                            :memory
                                                                            :memory-mode
                                                                            :bytes)))
                           (by-percent
                            (ntop.process.domain::sort-process-domain-rows rows
                                                                           (ntop.process.domain::make-process-sort-state
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
                                                                           (ntop.process.domain::make-process-sort-state
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
                                         (ntop.process.domain::make-process-sort-state
                                          :key
                                          :pid))))
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
                                                                           (ntop.process.domain::make-process-sort-state
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
                                                                           (ntop.process.domain::make-process-sort-state
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
                                                                           (ntop.process.domain::make-process-sort-state
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
                                                                           (ntop.process.domain::make-process-sort-state
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
                             (ntop.process.domain::make-process-sort-state :key
                                                                           :gpu-memory
                                                                           :memory-mode
                                                                           :bytes)))
                           (missing-rows
                            (list (%wp4-sorting-row 1 :cpu-percent 5)
                                  (%wp4-sorting-row 2 :cpu-percent nil)))
                           (missing-sorted
                            (ntop.process.domain::sort-process-domain-rows
                             missing-rows
                             (ntop.process.domain::make-process-sort-state :key
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
                                                 (ntop.process.domain::make-process-sort-state
                                                  :key
                                                  :unknown)
                                                 :returned)
                                   (error ()
                                     :signaled))
                                 :to-be
                                 :signaled)
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (ntop.process.domain::make-process-sort-state
                                                  :memory-mode
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
                                                                    (ntop.process.domain::make-process-sort-state))
                     :to-be
                     nil)))
