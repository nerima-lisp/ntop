(in-package #:ntop.test)

(defun %f1-cpu-fixture-root (variant)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/feature/cpu/fixtures/~(~A~)/"
                                         variant)))

(defun %capture-f1-linux-cpu-source (root)
  (let ((value :unset)
        (reason :unset))
    (ntop.platform.linux:with-linux-root root
                                         (ntop.cpu.infrastructure::read-linux-cpu-source-sample
                                          (lambda (source)
                                            (setf value source))
                                          (lambda (unavailable-reason)
                                            (setf reason unavailable-reason))))
    (values value reason)))

(defun %f1-linux-core (source index)
  (find index
        (ntop.cpu.infrastructure::cpu-source-sample-core-values source)
        :key
        #'ntop.cpu.domain:cpu-core-value-core-index
        :test
        #'eql))

(cl-weave:describe "Linux CPU source fixtures"
                   (cl-weave:it
                    "reads per-core ticks, load averages, frequency, and cgroup v2"
                    (multiple-value-bind (source reason)
                        (%capture-f1-linux-cpu-source
                         (%f1-cpu-fixture-root :v2))
                      (let ((core-0 (%f1-linux-core source 0))
                            (core-1 (%f1-linux-core source 1))
                            (cgroup
                             (ntop.cpu.infrastructure::cpu-source-sample-cgroup-data
                              source)))
                        (cl-weave:expect reason :to-be :unset)
                        (cl-weave:expect
                         (length
                          (ntop.cpu.infrastructure::cpu-source-sample-core-values
                           source))
                         :to-be
                         2)
                        (cl-weave:expect
                         (ntop.cpu.domain:cpu-core-value-user-ticks core-0)
                         :to-be
                         100)
                        (cl-weave:expect
                         (ntop.cpu.domain:cpu-core-value-idle-ticks core-1)
                         :to-be
                         250)
                        (cl-weave:expect
                         (ntop.cpu.domain:cpu-core-value-frequency-mhz core-0)
                         :to-be
                         2400)
                        (cl-weave:expect
                         (ntop.cpu.domain:cpu-core-value-frequency-mhz core-1)
                         :to-be
                         2801/2)
                        (cl-weave:expect
                         (list
                          (ntop.cpu.infrastructure::cpu-source-sample-load-average-1
                           source)
                          (ntop.cpu.infrastructure::cpu-source-sample-load-average-5
                           source)
                          (ntop.cpu.infrastructure::cpu-source-sample-load-average-15
                           source))
                         :to-equal
                         '(5/4 1/2 1/4))
                        (cl-weave:expect (getf cgroup :cpu-usage-ns)
                                         :to-be
                                         250000000)
                        (cl-weave:expect (getf cgroup :cpu-quota-micros)
                                         :to-be
                                         50000)
                        (cl-weave:expect (getf cgroup :cpu-period-micros)
                                         :to-be
                                         100000)
                        (cl-weave:expect (getf cgroup :cpu-throttled-ns)
                                         :to-be
                                         20000000)
                        (cl-weave:expect (getf cgroup :cpu-throttled-periods)
                                         :to-be
                                         1)
                        (cl-weave:expect (getf cgroup :cpu-limit-cores)
                                         :to-be
                                         1/2))))
                   (cl-weave:it
                    "normalizes cgroup v1 CPU accounting to the sample units"
                    (multiple-value-bind (source reason)
                        (%capture-f1-linux-cpu-source
                         (%f1-cpu-fixture-root :v1))
                      (let ((cgroup
                             (ntop.cpu.infrastructure::cpu-source-sample-cgroup-data
                              source)))
                        (cl-weave:expect reason :to-be :unset)
                        (cl-weave:expect (getf cgroup :cpu-usage-ns)
                                         :to-be
                                         500000000)
                        (cl-weave:expect (getf cgroup :cpu-quota-micros)
                                         :to-be
                                         75000)
                        (cl-weave:expect (getf cgroup :cpu-period-micros)
                                         :to-be
                                         100000)
                        (cl-weave:expect (getf cgroup :cpu-throttled-ns)
                                         :to-be
                                         35000000)
                        (cl-weave:expect (getf cgroup :cpu-throttled-periods)
                                         :to-be
                                         2)
                        (cl-weave:expect (getf cgroup :cpu-limit-cores)
                                         :to-be
                                         3/4)
                        (cl-weave:expect
                         (ntop.cpu.domain:cpu-core-value-frequency-mhz
                          (%f1-linux-core source 0))
                         :to-be
                         nil))))
                   (cl-weave:it-each
                    ((:missing-stat :missing-stat :read-failed)
                     (:missing-loadavg :missing-loadavg :read-failed)
                     (:malformed-stat :malformed-stat :read-failed)
                     (:malformed-loadavg :malformed-loadavg :read-failed))
                    "reports ~A when the ~A input is absent or malformed"
                    (case-name fixture expected-reason)
                    (declare (ignore case-name))
                    (multiple-value-bind (source reason)
                        (%capture-f1-linux-cpu-source
                         (%f1-cpu-fixture-root fixture))
                      (cl-weave:expect source :to-be :unset)
                      (cl-weave:expect reason :to-be expected-reason)))
                   (cl-weave:it-each
                    ((:malformed-frequency :malformed-frequency)
                     (:malformed-cgroup :malformed-cgroup))
                    "keeps CPU ticks available when optional ~A data is invalid"
                    (case-name fixture)
                    (declare (ignore case-name))
                    (multiple-value-bind (source reason)
                        (%capture-f1-linux-cpu-source
                         (%f1-cpu-fixture-root fixture))
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect
                       (ntop.cpu.domain:cpu-core-value-user-ticks
                        (%f1-linux-core source 0))
                       :to-be
                       10)
                      (if (eq fixture :malformed-frequency)
                          (cl-weave:expect
                           (ntop.cpu.domain:cpu-core-value-frequency-mhz
                            (%f1-linux-core source 0))
                           :to-be
                           nil)
                          (cl-weave:expect
                           (ntop.cpu.infrastructure::cpu-source-sample-cgroup-data
                            source)
                           :to-be
                           nil)))))

(cl-weave:describe "Linux CPU branch cases"
                   (cl-weave:it-each
                    ((:not-a-string 7 nil) (:short-name "cpu" nil)
                     (:wrong-prefix "disk0" nil)
                     (:bad-suffix "cpuX" nil)
                     (:valid-core "cpu12" 12))
                    "maps core names in ~A"
                    (case-name name expected)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.cpu.infrastructure::%linux-cpu-core-index name)
                     :to-equal
                     expected))
                   (cl-weave:it
                    "rejects rows without a usable core index or tick set"
                    (cl-weave:expect
                     (ntop.cpu.infrastructure::%linux-cpu-core-from-row
                      (quote ("cpu0" 1 2)))
                     :to-be
                     nil)
                    (cl-weave:expect
                     (ntop.cpu.infrastructure::%linux-cpu-core-from-row
                      (quote ("cpuX" 1 2 3 4)))
                     :to-be
                     nil))
                   (cl-weave:it "rejects empty and partial per-core row sets"
                                (cl-weave:expect
                                 (ntop.cpu.infrastructure::%linux-cpu-core-values
                                  nil)
                                 :to-be
                                 nil)
                                (cl-weave:expect
                                 (ntop.cpu.infrastructure::%linux-cpu-core-values
                                  (quote (("cpu0" 1 2 3))))
                                 :to-be
                                 nil))
                   (cl-weave:it "preserves missing optional cgroup counters"
                                (let ((data
                                       (ntop.cpu.infrastructure::%linux-cgroup-source-data
                                        (list :version
                                              2
                                              :cpu-quota
                                              50000
                                              :cpu-period
                                              100000
                                              :cpu-usage
                                              nil
                                              :cpu-stat
                                              nil))))
                                  (cl-weave:expect (getf data :cpu-usage-ns)
                                                   :to-be
                                                   nil)
                                  (cl-weave:expect (getf data :cpu-throttled-ns) :to-be 0)
                                  (cl-weave:expect
                                   (getf data :cpu-throttled-periods)
                                   :to-be
                                   nil)
                                  (cl-weave:expect (getf data :cpu-limit-cores)
                                                   :to-equal
                                                   1/2)))
                   (cl-weave:it "keeps CPU usage when a cgroup has no quota"
                                (let ((data
                                       (ntop.cpu.infrastructure::%linux-cgroup-source-data
                                        (list :version
                                              1
                                              :cpu-quota
                                              nil
                                              :cpu-period
                                              nil
                                              :cpu-usage
                                              500000000
                                              :cpu-stat
                                              nil))))
                                  (cl-weave:expect (getf data :cpu-usage-ns)
                                                   :to-equal
                                                   500000000)
                                  (cl-weave:expect (getf data :cpu-quota-micros)
                                                   :to-be
                                                   nil)
                                  (cl-weave:expect (getf data :cpu-limit-cores)
                                                   :to-be
                                                   nil)))
                   (cl-weave:it "drops invalid cgroup counters and limits"
                                (cl-weave:expect
                                 (ntop.cpu.infrastructure::%linux-cgroup-source-data
                                  (list :version 1 :cpu-quota 0 :cpu-period -1 :cpu-usage -1 :cpu-stat nil))
                                 :to-be
                                 nil))
                   (cl-weave:it "looks up present and absent cgroup statistics"
                                (cl-weave:expect
                                 (ntop.cpu.infrastructure::%linux-cpu-stat-value
                                  (list :cpu-stat
                                        (quote (("nr_throttled" . 3))))
                                  "nr_throttled")
                                 :to-equal
                                 3)
                                (cl-weave:expect
                                 (ntop.cpu.infrastructure::%linux-cpu-stat-value
                                  (list :cpu-stat nil)
                                  "throttled_usec")
                                 :to-be
                                 nil)))
