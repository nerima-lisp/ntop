(in-package #:ntop.test)

(cl-weave:describe "battery domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type battery-device
                (charge-percent nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type battery-sample
                (devices nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a sample with a battery and timestamp"
    (let* ((device (ntop.battery.domain:make-battery-device
                    :name "Battery"
                    :charge-percent 75.0
                    :power-watts 12.5
                    :state :charging
                    :seconds-to-full 120
                    :health-percent 91.0))
           (sample (ntop.battery.domain:make-battery-sample
                    :sampled-at 30.0
                    :devices (list device))))
      (cl-weave:expect
       (ntop.shared.domain:sample-record-sampled-at sample)
       :to-be 30.0)
      (cl-weave:expect
       (ntop.battery.domain:battery-device-charge-percent
        (first (ntop.battery.domain:battery-sample-devices sample)))
       :to-be 75.0)
      (cl-weave:expect (ntop.battery.domain:battery-device-state device)
                       :to-be :charging)))
  (cl-weave:it-each
    ((:charging :charging) (:unknown :unknown))
    "preserves ~S as the battery state"
    (value expected)
    (let ((device (ntop.battery.domain:make-battery-device :state value)))
      (cl-weave:expect
       (ntop.battery.domain:battery-device-state device)
       :to-be expected))))
