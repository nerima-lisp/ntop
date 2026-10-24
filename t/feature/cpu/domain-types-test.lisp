(in-package #:ntop.test)

(cl-weave:describe "CPU domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              cpu-core-value
                              (core-index nil)))))
                      (cl-weave:expect (car expansion) :to-be 'defstruct)
                      (cl-weave:expect (second (second expansion))
                                       :to-equal
                                       '(:copier nil))))
                   (cl-weave:it "includes the shared record in a sample type"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.shared.domain:define-sample-type
                                          cpu-sample
                                          (average-usage-percent nil)))))
                                  (cl-weave:expect (car expansion)
                                                   :to-be
                                                   'defstruct)
                                  (cl-weave:expect (second (second expansion))
                                                   :to-equal
                                                   '(:include
                                                     ntop.shared.domain:sample-record))))
                   (cl-weave:it
                    "constructs a sample with core values and its timestamp"
                    (let* ((core
                            (ntop.cpu.domain:make-cpu-core-value :core-index
                                                                 0
                                                                 :user-ticks
                                                                 120
                                                                 :frequency-mhz
                                                                 1200.0))
                           (sample
                            (ntop.cpu.domain:make-cpu-sample :sampled-at
                                                             6.5
                                                             :core-values
                                                             (list core)
                                                             :load-average-1
                                                             1.25)))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       6.5)
                      (cl-weave:expect
                       (ntop.cpu.domain:cpu-sample-load-average-1 sample)
                       :to-be
                       1.25)
                      (cl-weave:expect
                       (ntop.cpu.domain:cpu-core-value-frequency-mhz
                        (first (ntop.cpu.domain:cpu-sample-core-values sample)))
                       :to-be
                       1200.0)))
                   (cl-weave:it-each ((nil nil) (0.0 0.0))
                                     "preserves ~S as an optional average"
                                     (value expected)
                                     (let ((sample
                                            (ntop.cpu.domain:make-cpu-sample
                                             :sampled-at
                                             0.0
                                             :average-usage-percent
                                             value)))
                                       (cl-weave:expect
                                        (ntop.cpu.domain:cpu-sample-average-usage-percent
                                         sample)
                                        :to-be
                                        expected)))
                   (cl-weave:it-each
                    ((ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-core-index
                      nil
                      :core-index
                      nil
                      nil
                      1
                      :invalid)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-user-ticks
                      nil
                      :user-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-nice-ticks
                      nil
                      :nice-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-system-ticks
                      nil
                      :system-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-idle-ticks
                      nil
                      :idle-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-iowait-ticks
                      nil
                      :iowait-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-irq-ticks
                      nil
                      :irq-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-softirq-ticks
                      nil
                      :softirq-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-steal-ticks
                      nil
                      :steal-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-guest-ticks
                      nil
                      :guest-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-guest-nice-ticks
                      nil
                      :guest-nice-ticks
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-usage-percent
                      nil
                      :usage-percent
                      nil
                      nil
                      12.5
                      :invalid)
                     (ntop.cpu.domain:make-cpu-core-value
                      ntop.cpu.domain:cpu-core-value-frequency-mhz
                      nil
                      :frequency-mhz
                      nil
                      nil
                      1200.0
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-core-values
                      (:sampled-at 0.0)
                      :core-values
                      nil
                      nil
                      (:core)
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-average-usage-percent
                      (:sampled-at 0.0)
                      :average-usage-percent
                      nil
                      nil
                      12.5
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-load-average-1
                      (:sampled-at 0.0)
                      :load-average-1
                      nil
                      nil
                      1.25
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-load-average-5
                      (:sampled-at 0.0)
                      :load-average-5
                      nil
                      nil
                      1.25
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-load-average-15
                      (:sampled-at 0.0)
                      :load-average-15
                      nil
                      nil
                      1.25
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-average-cpu-percent
                      (:sampled-at 0.0)
                      :cgroup-average-cpu-percent
                      nil
                      nil
                      12.5
                      :invalid)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-usage-ns
                      (:sampled-at 0.0)
                      :cgroup-cpu-usage-ns
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-quota-micros
                      (:sampled-at 0.0)
                      :cgroup-cpu-quota-micros
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-period-micros
                      (:sampled-at 0.0)
                      :cgroup-cpu-period-micros
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-throttled-ns
                      (:sampled-at 0.0)
                      :cgroup-cpu-throttled-ns
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-throttled-periods
                      (:sampled-at 0.0)
                      :cgroup-cpu-throttled-periods
                      nil
                      nil
                      100
                      -1)
                     (ntop.cpu.domain:make-cpu-sample
                      ntop.cpu.domain:cpu-sample-cgroup-cpu-limit-cores
                      (:sampled-at 0.0)
                      :cgroup-cpu-limit-cores
                      nil
                      nil
                      2.0
                      :invalid))
                    "validates typed CPU slot ~S"
                    (constructor accessor
                                 required-initargs
                                 initarg
                                 default-value
                                 nil-value
                                 accepted-value
                                 invalid-value)
                    (cl-weave:expect
                     (constructor-slot-outcomes constructor
                                                accessor
                                                required-initargs
                                                initarg
                                                nil-value
                                                accepted-value
                                                invalid-value)
                     :to-equal
                     (list default-value nil-value accepted-value :type-error))))
