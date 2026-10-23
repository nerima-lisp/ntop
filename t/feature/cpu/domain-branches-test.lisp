(in-package #:ntop.test)

(cl-weave:describe "CPU branch cases"
                   (cl-weave:it
                    "rejects missing or negative cumulative tick counters"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0 :user nil :nice 0 :system 0 :idle 0)
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      :mode
                      :darwin)
                     :to-be
                     nil)
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      (%cpu-domain-core 0 :user nil :nice 0 :system 0 :idle 0)
                      :mode
                      :darwin)
                     :to-be
                     nil)
                    (cl-weave:expect
                     (ntop.cpu.domain::%cpu-counter-delta -1 0)
                     :to-be
                     nil)
                    (cl-weave:expect
                     (ntop.cpu.domain::%cpu-counter-delta 0 -1)
                     :to-be
                     nil))
                   (cl-weave:it "rejects mismatched optional Linux tick slots"
                                (cl-weave:expect
                                 (ntop.cpu.domain:cpu-core-usage-percent
                                  (%cpu-domain-core 0
                                                    :user
                                                    0
                                                    :nice
                                                    0
                                                    :system
                                                    0
                                                    :idle
                                                    0)
                                  (%cpu-domain-core 0
                                                    :user
                                                    10
                                                    :nice
                                                    0
                                                    :system
                                                    0
                                                    :idle
                                                    0
                                                    :iowait
                                                    1)
                                  :mode
                                  :linux)
                                 :to-be
                                 nil))
                   (cl-weave:it
                    "returns zero for no tick interval and defaults to Linux for core usage"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      :mode
                      :darwin)
                     :to-equal
                     0)
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      (%cpu-domain-core 0 :user 90 :nice 0 :system 0 :idle 10))
                     :to-equal
                     90))
                   (cl-weave:it
                    "returns no All average without a match and defaults to Linux for matched cores"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-average-usage-percent
                      (list (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0))
                      (list (%cpu-domain-core nil :user 10 :nice 0 :system 0 :idle 0))
                      :mode
                      :darwin)
                     :to-be
                     nil)
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-average-usage-percent
                      (list (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0))
                      (list (%cpu-domain-core 0 :user 90 :nice 0 :system 0 :idle 10)))
                     :to-equal
                     90))
                   (cl-weave:it-each
                    ((:bad-previous :invalid-counter 0 1 50000 100000)
                     (:bad-current 0 :invalid-counter 1 50000 100000)
                     (:negative-current 0 -1 1 50000 100000)
                     (:negative-previous -1 0 1 50000 100000)
                     (:bad-time 0 1 :invalid-time 50000 100000)
                     (:zero-time 0 1 0 50000 100000)
                     (:bad-quota 0 1 1 :invalid-quota 100000)
                     (:zero-quota 0 1 1 0 100000)
                     (:bad-period 0 1 1 50000 :invalid-period)
                     (:zero-period 0 1 1 50000 0))
                    "returns no cgroup average for invalid ~A"
                    (case-name previous current elapsed quota period)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-cgroup-average-percent previous
                                                                 current
                                                                 elapsed
                                                                 quota
                                                                 period)
                     :to-be
                     nil))
                   (cl-weave:it
                    "accepts symbol states and rejects malformed state names"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-weighted-frequency-mhz
                      (list (cons (quote p0) 2))
                      (list 2400000))
                     :to-equal
                     2400)
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-weighted-frequency-mhz
                      (list (cons 42 1))
                      (list 1000000))
                     :to-be
                     nil))
                   (cl-weave:it-each
                    ((:non-list-states (quote #(("P0" . 1))) (quote (1000000)))
                     (:malformed-row (quote (42)) (quote (1000000)))
                     (:invalid-residency (quote (("P0" . :invalid)))
                                         (quote (1000000)))
                     (:empty-table (quote (("P0" . 1))) nil)
                     (:non-list-table (quote (("P0" . 1))) (quote #(1000000))))
                    "returns no weighted frequency for ~A"
                    (case-name states table)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-weighted-frequency-mhz states table)
                     :to-be
                     nil))
                   (cl-weave:it
                    "reports a source-reader failure through the unavailable callback"
                    (let ((reason nil)
                          (collector
                           (ntop.cpu.infrastructure:make-cpu-collector
                            :source-reader
                            (lambda (on-value on-unavailable)
                              (declare (ignore on-value))
                              (funcall on-unavailable :read-failed))
                            :clock
                            (make-instance '%cpu-deterministic-clock
                                           :times
                                           (quote (1d0))))))
                      (ntop.shared.application:collect-cpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (declare (ignore
                                                                              sample)))
                                                                  (lambda
                                                                      (unavailable-reason)
                                                                    (setf
                                                                     reason
                                                                     unavailable-reason)))
                      (cl-weave:expect reason :to-be :read-failed)))
                   (cl-weave:it
                    "rejects a source that is not a CPU source sample"
                    (let ((reason nil)
                          (collector
                           (ntop.cpu.infrastructure:make-cpu-collector
                            :source-reader
                            (lambda (on-value on-unavailable)
                              (declare (ignore on-unavailable))
                              (funcall on-value :invalid-source))
                            :clock
                            (make-instance '%cpu-deterministic-clock
                                           :times
                                           (quote (1d0))))))
                      (ntop.shared.application:collect-cpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (declare (ignore
                                                                              sample)))
                                                                  (lambda
                                                                      (unavailable-reason)
                                                                    (setf
                                                                     reason
                                                                     unavailable-reason)))
                      (cl-weave:expect reason :to-be :read-failed)))
                   (cl-weave:it "rejects a non-real monotonic timestamp"
                                (let ((reason nil)
                                      (collector
                                       (ntop.cpu.infrastructure:make-cpu-collector
                                        :source-reader
                                        (lambda (on-value on-unavailable)
                                          (declare (ignore on-unavailable))
                                          (funcall on-value
                                                   (ntop.cpu.infrastructure::make-cpu-source-sample
                                                    :core-values
                                                    nil)))
                                        :clock
                                        (make-instance '%cpu-deterministic-clock
                                                       :times
                                                       (quote (:invalid-time))))))
                                  (ntop.shared.application:collect-cpu-sample
                                   collector
                                   (lambda (sample)
                                     (declare (ignore sample)))
                                   (lambda (unavailable-reason)
                                     (setf reason unavailable-reason)))
                                  (cl-weave:expect reason :to-be :read-failed)))
                   (cl-weave:it "forwards a monotonic clock failure"
                                (let ((reason nil)
                                      (collector
                                       (ntop.cpu.infrastructure:make-cpu-collector
                                        :source-reader
                                        (lambda (on-value on-unavailable)
                                          (declare (ignore on-unavailable))
                                          (funcall on-value
                                                   (ntop.cpu.infrastructure::make-cpu-source-sample
                                                    :core-values
                                                    nil)))
                                        :clock
                                        (make-instance '%cpu-deterministic-clock
                                                       :times
                                                       (quote (nil))))))
                                  (ntop.shared.application:collect-cpu-sample
                                   collector
                                   (lambda (sample)
                                     (declare (ignore sample)))
                                   (lambda (unavailable-reason)
                                     (setf reason unavailable-reason)))
                                  (cl-weave:expect reason :to-be :read-failed)))
                   (cl-weave:it "rejects a non-function source reader"
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (ntop.cpu.infrastructure:make-cpu-collector
                                                  :source-reader
                                                  nil
                                                  :clock
                                                  (make-instance
                                                   '%cpu-deterministic-clock
                                                   :times
                                                   nil))
                                                 nil)
                                   (error ()
                                          t))
                                 :to-be
                                 t))
                   (cl-weave:it "requires a clock"
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (ntop.cpu.infrastructure:make-cpu-collector
                                                  :source-reader
                                                  (function identity))
                                                 nil)
                                   (error ()
                                          t))
                                 :to-be
                                 t)))

(cl-weave:describe
 "CPU collector cgroup integration"
 (cl-weave:it
  "derives cgroup AVG from successive source samples"
  (let* ((source-samples
          (list
           (ntop.cpu.infrastructure::make-cpu-source-sample
            :core-values
            (list (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0))
            :cgroup-data
            '(:cpu-usage-ns 0
              :cpu-quota-micros 50000
              :cpu-period-micros 100000))
           (ntop.cpu.infrastructure::make-cpu-source-sample
            :core-values
            (list (%cpu-domain-core 0 :user 90 :nice 0 :system 0 :idle 10))
            :cgroup-data
            '(:cpu-usage-ns 1000000000
              :cpu-quota-micros 50000
              :cpu-period-micros 100000))))
         (collector
          (ntop.cpu.infrastructure:make-cpu-collector
           :source-reader
           (lambda (on-value on-unavailable)
             (let ((source (pop source-samples)))
               (if source
                   (funcall on-value source)
                   (funcall on-unavailable :read-failed))))
           :clock
           (make-instance '%cpu-deterministic-clock :times '(0d0 2d0))))
         (baseline nil)
         (sample nil))
    (ntop.shared.application:collect-cpu-sample
     collector
     (lambda (current-sample) (setf baseline current-sample))
     (lambda (reason) (declare (ignore reason))))
    (ntop.shared.application:collect-cpu-sample
     collector
     (lambda (current-sample) (setf sample current-sample))
     (lambda (reason) (declare (ignore reason))))
    (cl-weave:expect
     (ntop.cpu.domain:cpu-sample-average-usage-percent baseline)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.cpu.domain:cpu-sample-cgroup-average-cpu-percent baseline)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.cpu.domain:cpu-sample-cgroup-average-cpu-percent sample)
     :to-equal
     100)
    (cl-weave:expect
     (ntop.cpu.domain:cpu-sample-average-usage-percent sample)
     :to-equal
     100))))
