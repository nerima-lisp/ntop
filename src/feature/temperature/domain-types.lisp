(in-package #:ntop.temperature.domain)

(ntop.shared.domain:define-value-type temperature-sensor
  (name nil :type (or null string))
  (temperature-celsius nil :type ntop.shared.domain:measurement-value))

(ntop.shared.domain:define-sample-type temperature-sample
  (sensors nil :type list))


