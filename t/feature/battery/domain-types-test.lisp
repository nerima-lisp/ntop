(in-package #:ntop.test)

(cl-weave:describe "battery domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              battery-device
                              (charge-percent nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (let ((defstruct-form (second expansion))
                            (constructor-form (third expansion))
                            (struct-spec (second (second expansion))))
                        (cl-weave:expect (car defstruct-form)
                                         :to-be
                                         'defstruct)
                        (cl-weave:expect (second struct-spec)
                                         :to-equal
                                         '(:constructor make-battery-device-raw))
                        (cl-weave:expect (third struct-spec)
                                         :to-equal
                                         '(:copier nil))
                        (cl-weave:expect (third defstruct-form)
                                         :to-equal
                                         '(charge-percent nil :read-only t))
                        (cl-weave:expect (car constructor-form)
                                         :to-be
                                         'defun)
                        (cl-weave:expect (second constructor-form)
                                         :to-be
                                         'make-battery-device)
                        (cl-weave:expect (car (fourth constructor-form))
                                         :to-be
                                         'ntop.shared.domain::validate-typed-slots)
                        (cl-weave:expect (car (fifth constructor-form))
                                         :to-be
                                         'make-battery-device-raw))))
                   (cl-weave:it "includes the shared record in a sample type"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.shared.domain:define-sample-type
                                          battery-sample
                                          (devices nil)))))
                                  (cl-weave:expect (car expansion)
                                                   :to-be
                                                   'progn)
                                  (let ((defstruct-form (second expansion))
                                        (constructor-form (third expansion))
                                        (struct-spec (second (second expansion))))
                                    (cl-weave:expect (car defstruct-form)
                                                     :to-be
                                                     'defstruct)
                                    (cl-weave:expect (second struct-spec)
                                                     :to-equal
                                                     '(:include
                                                       ntop.shared.domain:sample-record))
                                    (cl-weave:expect (third struct-spec)
                                                     :to-equal
                                                     '(:constructor make-battery-sample-raw))
                                    (cl-weave:expect (fourth struct-spec)
                                                     :to-equal
                                                     '(:copier nil))
                                    (cl-weave:expect (third defstruct-form)
                                                     :to-equal
                                                     '(devices nil :read-only t))
                                    (cl-weave:expect (car constructor-form)
                                                     :to-be
                                                     'defun)
                                    (cl-weave:expect (second constructor-form)
                                                     :to-be
                                                     'make-battery-sample)
                                    (cl-weave:expect (car (fourth constructor-form))
                                                     :to-be
                                                     'ntop.shared.domain::validate-typed-slots)
                                    (cl-weave:expect (car (fifth constructor-form))
                                                     :to-be
                                                     'make-battery-sample-raw))))
                   (cl-weave:it
                    "constructs a sample with a battery and timestamp"
                    (let* ((device
                            (ntop.battery.domain:make-battery-device :name
                                                                     "Battery"
                                                                     :charge-percent
                                                                     75.0
                                                                     :power-watts
                                                                     12.5
                                                                     :state
                                                                     :charging
                                                                     :seconds-to-full
                                                                     120
                                                                     :health-percent
                                                                     91.0))
                           (sample
                            (ntop.battery.domain:make-battery-sample :sampled-at
                                                                     30.0
                                                                     :devices
                                                                     (list
                                                                      device))))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       30.0)
                      (cl-weave:expect
                       (ntop.battery.domain:battery-device-charge-percent
                        (first
                         (ntop.battery.domain:battery-sample-devices sample)))
                       :to-be
                       75.0)
                      (cl-weave:expect
                       (ntop.battery.domain:battery-device-state device)
                       :to-be
                       :charging)))
                   (cl-weave:it-each ((:charging :charging) (:unknown :unknown))
                                     "preserves ~S as the battery state"
                                     (value expected)
                                     (let ((device
                                            (ntop.battery.domain:make-battery-device
                                             :state
                                             value)))
                                       (cl-weave:expect
                                        (ntop.battery.domain:battery-device-state
                                         device)
                                        :to-be
                                        expected)))
                   (cl-weave:it-each
                    ((ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-name
                      nil
                      :name
                      nil
                      nil
                      "Battery"
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-charge-percent
                      nil
                      :charge-percent
                      nil
                      nil
                      75.0
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-power-watts
                      nil
                      :power-watts
                      nil
                      nil
                      12.5
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-state
                      nil
                      :state
                      :unknown
                      :type-error
                      :charging
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-seconds-to-full
                      nil
                      :seconds-to-full
                      nil
                      nil
                      120
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-seconds-to-empty
                      nil
                      :seconds-to-empty
                      nil
                      nil
                      60
                      :invalid)
                     (ntop.battery.domain:make-battery-device
                      ntop.battery.domain:battery-device-health-percent
                      nil
                      :health-percent
                      nil
                      nil
                      0.9
                      :invalid)
                     (ntop.battery.domain:make-battery-sample
                      ntop.battery.domain:battery-sample-devices
                      (:sampled-at 0.0)
                      :devices
                      nil
                      nil
                      (:device)
                      :invalid))
                    "validates typed battery slot ~S"
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
