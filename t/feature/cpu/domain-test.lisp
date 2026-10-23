(in-package #:ntop.test)
(defun %cpu-domain-core (index &key
                               user
                               nice
                               system
                               idle
                               iowait
                               irq
                               softirq
                               steal
                               guest
                               guest-nice)
  (ntop.cpu.domain:make-cpu-core-value :core-index
                                       index
                                       :user-ticks
                                       user
                                       :nice-ticks
                                       nice
                                       :system-ticks
                                       system
                                       :idle-ticks
                                       idle
                                       :iowait-ticks
                                       iowait
                                       :irq-ticks
                                       irq
                                       :softirq-ticks
                                       softirq
                                       :steal-ticks
                                       steal
                                       :guest-ticks
                                       guest
                                       :guest-nice-ticks
                                       guest-nice))

(defclass %cpu-deterministic-clock ()
  ((times :initarg :times :accessor %cpu-clock-times)))

(defmethod ntop.shared.domain:read-monotonic-time ((clock
                                                    %cpu-deterministic-clock)
                                                   on-time
                                                   on-unavailable)
  (let ((time (pop (%cpu-clock-times clock))))
    (cond
      ((eq time :invalid-time) (funcall on-time time))
      ((realp time) (funcall on-time time))
      (t (funcall on-unavailable :read-failed)))))

(cl-weave:describe "CPU domain metrics"
                   (cl-weave:it-each
                    ((:linux 0 80 0 20 80) (:darwin 0 90 0 10 90)
                     (:darwin 100 50 0 100 0))
                    "computes ~A usage from cumulative ticks"
                    (mode previous-user
                          current-user
                          previous-idle
                          current-idle
                          expected)
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0
                                        :user
                                        previous-user
                                        :idle
                                        previous-idle
                                        :nice
                                        0
                                        :system
                                        0)
                      (%cpu-domain-core 0
                                        :user
                                        current-user
                                        :idle
                                        current-idle
                                        :nice
                                        0
                                        :system
                                        0)
                      :mode
                      mode)
                     :to-equal
                     expected))
                   (cl-weave:it
                    "uses Linux guest ticks without counting them twice"
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
                                        0
                                        :iowait
                                        0
                                        :irq
                                        0
                                        :softirq
                                        0
                                        :steal
                                        0
                                        :guest
                                        0
                                        :guest-nice
                                        0)
                      (%cpu-domain-core 0
                                        :user
                                        50
                                        :nice
                                        0
                                        :system
                                        20
                                        :idle
                                        30
                                        :iowait
                                        5
                                        :irq
                                        2
                                        :softirq
                                        3
                                        :steal
                                        0
                                        :guest
                                        10
                                        :guest-nice
                                        0)
                      :mode
                      :linux)
                     :to-equal
                     (* 100 75/110)))
                   (cl-weave:it
                    "returns unavailable usage when a required tick is missing"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-core-usage-percent
                      (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                      (%cpu-domain-core 0 :user 10 :nice 1 :system 0 :idle nil)
                      :mode
                      :darwin)
                     :to-be
                     nil))
                   (cl-weave:it
                    "aggregates matched core deltas for the All average"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-average-usage-percent
                      (list
                       (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                       (%cpu-domain-core 1 :user 0 :nice 0 :system 0 :idle 0))
                      (list
                       (%cpu-domain-core 0 :user 90 :nice 0 :system 0 :idle 10)
                       (%cpu-domain-core 1 :user 20 :nice 0 :system 0 :idle 80))
                      :mode
                      :darwin)
                     :to-equal
                     55))
                   (cl-weave:it
                    "weights CPU average by each core's tick interval"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-average-usage-percent
                      (list
                       (%cpu-domain-core 0 :user 0 :nice 0 :system 0 :idle 0)
                       (%cpu-domain-core 1 :user 0 :nice 0 :system 0 :idle 0))
                      (list
                       (%cpu-domain-core 0 :user 90 :nice 0 :system 0 :idle 10)
                       (%cpu-domain-core 1
                                         :user
                                         100
                                         :nice
                                         0
                                         :system
                                         0
                                         :idle
                                         100))
                      :mode
                      :darwin)
                     :to-equal
                     (* 100 190/300)))
                   (cl-weave:it-each
                    ((:half-quota 0 500000000 2 50000 100000 50)
                     (:full-quota 0 1000000000 2 50000 100000 100)
                     (:over-quota 0 2000000000 2 50000 100000 100)
                     (:counter-reset 100 50 2 50000 100000 0)
                     (:no-quota 0 1000000000 2 nil 100000 nil))
                    "computes cgroup usage for ~A"
                    (case-name previous current elapsed quota period expected)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-cgroup-average-percent previous
                                                                 current
                                                                 elapsed
                                                                 quota
                                                                 period)
                     :to-equal
                     expected))
                   (cl-weave:it
                    "maps active frequency states after excluding DOWN and IDLE"
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-weighted-frequency-mhz
                      '(("DOWN" . 900) ("IDLE" . 800)
                        ("P0" . 3)
                        ("P1" . 1)
                        ("P2" . 0))
                      '(3000000 2000000 1000000))
                     :to-equal
                     2750))
                   (cl-weave:it-each
                    ((:empty nil '(1000000))
                     (:zero-residency '(("P0" . 0)) '(1000000))
                     (:missing-frequency '(("P0" . 1) ("P1" . 1)) '(1000000))
                     (:malformed-state '((42 . 1)) '(1000000))
                     (:negative-residency '(("P0" . -1)) '(1000000))
                     (:malformed-table '(("P0" . 1)) '(0)))
                    "returns unavailable frequency for ~A"
                    (case-name states table)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.cpu.domain:cpu-weighted-frequency-mhz states table)
                     :to-be
                     nil))
                   (cl-weave:it
                    "publishes the first CPU AVG after its baseline interval"
                    (let* ((source-samples
                            (list
                             (ntop.cpu.infrastructure::make-cpu-source-sample
                              :core-values
                              (list
                               (%cpu-domain-core 0
                                                 :user
                                                 0
                                                 :nice
                                                 0
                                                 :system
                                                 0
                                                 :idle
                                                 0)
                               (%cpu-domain-core 1
                                                 :user
                                                 0
                                                 :nice
                                                 0
                                                 :system
                                                 0
                                                 :idle
                                                 0))
                              :load-average-1
                              0
                              :load-average-5
                              0
                              :load-average-15
                              0)
                             (ntop.cpu.infrastructure::make-cpu-source-sample
                              :core-values
                              (list
                               (%cpu-domain-core 0
                                                 :user
                                                 180
                                                 :nice
                                                 0
                                                 :system
                                                 0
                                                 :idle
                                                 20)
                               (%cpu-domain-core 1
                                                 :user
                                                 180
                                                 :nice
                                                 0
                                                 :system
                                                 0
                                                 :idle
                                                 20))
                              :load-average-1
                              1
                              :load-average-5
                              1
                              :load-average-15
                              1)))
                           (collector
                            (ntop.cpu.infrastructure:make-cpu-collector
                             :source-reader
                             (lambda (on-value on-unavailable)
                               (let ((source (pop source-samples)))
                                 (if source
                                     (funcall on-value source)
                                     (funcall on-unavailable :read-failed))))
                             :clock
                             (make-instance '%cpu-deterministic-clock
                                            :times
                                            '(0d0 2d0))))
                           (first-sample nil)
                           (second-sample nil))
                      (ntop.shared.application:collect-cpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (setf first-sample sample))
                                                                  (lambda
                                                                      (reason)
                                                                    (declare (ignore
                                                                              reason))))
                      (ntop.shared.application:collect-cpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (setf second-sample sample))
                                                                  (lambda
                                                                      (reason)
                                                                    (declare (ignore
                                                                              reason))))
                      (cl-weave:expect
                       (ntop.cpu.domain:cpu-sample-average-usage-percent
                        first-sample)
                       :to-be
                       nil)
                      (cl-weave:expect
                       (>=
                        (ntop.cpu.domain:cpu-sample-average-usage-percent
                         second-sample)
                        90)
                       :to-be
                       t)
                      (cl-weave:expect
                       (mapcar #'ntop.cpu.domain:cpu-core-value-usage-percent
                               (ntop.cpu.domain:cpu-sample-core-values
                                second-sample))
                       :to-equal
                       '(90 90)))))

