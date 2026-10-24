(in-package #:ntop.gpu.domain)

(ntop.shared.domain:define-value-type gpu-device
  (name nil :type (or null string))
  (usage-percent nil :type ntop.shared.domain:measurement-value)
  (memory-used-bytes nil
                     :type (or null ntop.shared.domain:measurement-counter))
  (memory-total-bytes nil
                      :type (or null ntop.shared.domain:measurement-counter))
  (temperature-celsius nil :type ntop.shared.domain:measurement-value))

(ntop.shared.domain:define-sample-type gpu-sample
  (devices nil :type list))
