(in-package #:ntop.cpu.domain)

(ntop.shared.domain:define-value-type cpu-core-value
  (core-index nil :type (or null integer))
  (user-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (nice-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (system-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (idle-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (iowait-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (irq-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (softirq-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (steal-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (guest-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (guest-nice-ticks nil :type (or null ntop.shared.domain:measurement-counter))
  (usage-percent nil :type ntop.shared.domain:measurement-value)
  (frequency-mhz nil :type ntop.shared.domain:measurement-value))

(ntop.shared.domain:define-sample-type cpu-sample
  (core-values nil :type list)
  (average-usage-percent nil :type ntop.shared.domain:measurement-value)
  (load-average-1 nil :type ntop.shared.domain:measurement-value)
  (load-average-5 nil :type ntop.shared.domain:measurement-value)
  (load-average-15 nil :type ntop.shared.domain:measurement-value)
  (cgroup-average-cpu-percent nil :type ntop.shared.domain:measurement-value)
  (cgroup-cpu-usage-ns nil
                       :type (or null ntop.shared.domain:measurement-counter))
  (cgroup-cpu-quota-micros nil
                           :type (or null ntop.shared.domain:measurement-counter))
  (cgroup-cpu-period-micros nil
                            :type (or null ntop.shared.domain:measurement-counter))
  (cgroup-cpu-throttled-ns nil
                           :type (or null ntop.shared.domain:measurement-counter))
  (cgroup-cpu-throttled-periods nil
                                :type (or null ntop.shared.domain:measurement-counter))
  (cgroup-cpu-limit-cores nil :type ntop.shared.domain:measurement-value))
