(in-package #:ntop.network.domain)

(ntop.shared.domain:define-value-type network-interface
  (name nil :type (or null string))
  (rx-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (tx-bytes nil :type (or null ntop.shared.domain:measurement-counter))
  (rx-packets nil :type (or null ntop.shared.domain:measurement-counter))
  (tx-packets nil :type (or null ntop.shared.domain:measurement-counter)))

(ntop.shared.domain:define-sample-type network-sample
  (interfaces nil :type list))


