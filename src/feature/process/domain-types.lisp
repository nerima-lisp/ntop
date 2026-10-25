(in-package #:ntop.process.domain)

(ntop.shared.domain:define-value-type process-thread
  (thread-id nil :type (or null integer))
  (name nil :type (or null string))
  (cpu-time-ns nil :type (or null ntop.shared.domain:measurement-counter))
  (state nil :type (or null string))
  (priority nil :type (or null integer))
  (nice nil :type (or null integer)))

(ntop.shared.domain:define-value-type process-record
  (pid nil :type (or null integer))
  (ppid nil :type (or null integer))
  (name nil :type (or null string))
  (command nil :type (or null string))
  (cpu-time-ns nil :type (or null ntop.shared.domain:measurement-counter))
  (resident-memory-bytes nil
                         :type (or null ntop.shared.domain:measurement-counter))
  (virtual-memory-bytes nil
                        :type (or null ntop.shared.domain:measurement-counter))
  (read-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (write-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (uid nil :type (or null integer))
  (user-name nil :type (or null string))
  (state nil :type (or null string))
  (start-time-unix-seconds nil :type ntop.shared.domain:measurement-value)
  (priority nil :type (or null integer))
  (nice nil :type (or null integer))
  (kernel-thread-p :unknown :type (member t nil :unknown))
  (threads nil :type list)
  (gpu-usage-percent nil :type ntop.shared.domain:measurement-value)
  (gpu-memory-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (ps-fallback-cpu-percent nil :type ntop.shared.domain:measurement-value))

(ntop.shared.domain:define-value-type process-signal
  (operating-system nil :type (or null string))
  (name nil :type (or null string))
  (number nil :type (or null integer)))

(ntop.shared.domain:define-sample-type process-sample
  (processes nil :type list)
  (pid-index nil :type (or null hash-table))
  (signals nil :type list))

(ntop.shared.domain:define-value-type process-row
  (count nil :type (or null integer))
  (pid nil :type (or null integer))
  (ppid nil :type (or null integer))
  (name nil :type (or null string))
  (command nil :type (or null string))
  (cpu-percent nil :type ntop.shared.domain:measurement-value)
  (memory-percent nil :type ntop.shared.domain:measurement-value)
  (memory-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (virtual-memory-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (read-bytes-per-second nil :type ntop.shared.domain:measurement-value)
  (write-bytes-per-second nil :type ntop.shared.domain:measurement-value)
  (total-read-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (total-write-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (user-name nil :type (or null string))
  (state nil :type (or null string))
  (elapsed-seconds nil :type ntop.shared.domain:measurement-value)
  (priority nil :type (or null integer))
  (nice nil :type (or null integer))
  (gpu-usage-percent nil :type ntop.shared.domain:measurement-value)
  (gpu-memory-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (gpu-memory-percent nil :type ntop.shared.domain:measurement-value))


