(in-package #:ntop.battery.domain)

(ntop.shared.domain:define-value-type battery-device
  (name nil :type (or null string))
  (charge-percent nil :type ntop.shared.domain:measurement-value)
  (power-watts nil :type ntop.shared.domain:measurement-value)
  (state :unknown :type (member :charging :discharging :full :empty :unknown))
  (seconds-to-full nil :type ntop.shared.domain:measurement-value)
  (seconds-to-empty nil :type ntop.shared.domain:measurement-value)
  (health-percent nil :type ntop.shared.domain:measurement-value))

(ntop.shared.domain:define-sample-type battery-sample
  (devices nil :type list))
