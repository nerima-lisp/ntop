(in-package #:ntop.disk.domain)

(ntop.shared.domain:define-value-type disk-mount
  (device-name nil :type (or null string))
  (mount-path nil :type (or null string))
  (filesystem-type nil :type (or null string))
  (used-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (free-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (total-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (io-key nil :type (or null string)))

(ntop.shared.domain:define-value-type disk-device
  (device-name nil :type (or null string))
  (read-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (write-bytes nil :type (or null ntop.shared.domain:measurement-counter)))

(ntop.shared.domain:define-sample-type disk-sample
  (mounts nil :type list)
  (devices nil :type list))
