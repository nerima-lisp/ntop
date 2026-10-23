(in-package #:ntop.test)

(defun %synthetic-darwin-battery-properties
    (&key (source-state "Battery Power")
          (current-capacity 75)
          (maximum-capacity 100)
          (charging-p :false)
          (external-connected-p :false)
          (fully-charged-p :false)
          (raw-max-capacity 4500)
          (design-capacity 5000)
          (amperage -2000)
          (voltage 12000)
          (time-remaining-minutes 90))
  (list :source-type "InternalBattery"
        :source-state source-state
        :current-capacity current-capacity
        :maximum-capacity maximum-capacity
        :charging-p charging-p
        :external-connected-p external-connected-p
        :fully-charged-p fully-charged-p
        :raw-max-capacity raw-max-capacity
        :design-capacity design-capacity
        :amperage amperage
        :voltage voltage
        :time-remaining-minutes time-remaining-minutes))

(defun %map-synthetic-darwin-batteries (properties)
  (let ((devices nil)
        (unavailable nil)
        (value-calls 0)
        (unavailable-calls 0))
    (ntop.battery.infrastructure::map-darwin-battery-properties
     properties
     (lambda (value)
       (incf value-calls)
       (setf devices value))
     (lambda (reason)
       (incf unavailable-calls)
       (setf unavailable reason)))
    (values devices unavailable value-calls unavailable-calls)))

(cl-weave:describe
 "Darwin battery property mapping"
 (cl-weave:it "maps charge, health, power, and remaining minutes"
   (multiple-value-bind (devices unavailable)
       (%map-synthetic-darwin-batteries
        (list (%synthetic-darwin-battery-properties)))
     (let ((device (first devices)))
       (cl-weave:expect unavailable :to-be-null)
       (cl-weave:expect
        (= (ntop.battery.domain:battery-device-charge-percent device) 75)
        :to-be t)
       (cl-weave:expect
        (= (ntop.battery.domain:battery-device-health-percent device) 90)
        :to-be t)
       (cl-weave:expect
        (= (ntop.battery.domain:battery-device-power-watts device) 24)
        :to-be t)
       (cl-weave:expect
        (ntop.battery.domain:battery-device-seconds-to-empty device)
        :to-be 5400)
       (cl-weave:expect
        (ntop.battery.domain:battery-device-seconds-to-full device)
        :to-be-null))))
 (cl-weave:it-each
  (((:source-state "Battery Power" :current-capacity 75
     :charging-p :false :external-connected-p :false
     :fully-charged-p :false :time-remaining-minutes 90)
    :discharging nil 5400)
   ((:source-state "AC Power" :current-capacity 60
     :charging-p :true :external-connected-p :true
     :fully-charged-p :false :time-remaining-minutes 35)
    :charging 2100 nil)
   ((:source-state "AC Power" :current-capacity 100
     :charging-p :false :external-connected-p :true
     :fully-charged-p :true :time-remaining-minutes 2147483647)
    :full nil nil)
   ((:source-state "Battery Power" :current-capacity 0
     :charging-p :false :external-connected-p :false
     :fully-charged-p :false :time-remaining-minutes 2147483647)
    :empty nil nil))
  "maps battery signals to ~S and the two remaining-time fields"
  (overrides expected-state expected-seconds-to-full expected-seconds-to-empty)
  (multiple-value-bind (devices unavailable)
      (%map-synthetic-darwin-batteries
       (list (apply #'%synthetic-darwin-battery-properties overrides)))
    (let ((device (first devices)))
      (cl-weave:expect unavailable :to-be-null)
      (cl-weave:expect (ntop.battery.domain:battery-device-state device)
                       :to-be expected-state)
      (if expected-seconds-to-full
          (cl-weave:expect
           (ntop.battery.domain:battery-device-seconds-to-full device)
           :to-be expected-seconds-to-full)
          (cl-weave:expect
           (ntop.battery.domain:battery-device-seconds-to-full device)
           :to-be-null))
      (if expected-seconds-to-empty
          (cl-weave:expect
           (ntop.battery.domain:battery-device-seconds-to-empty device)
           :to-be expected-seconds-to-empty)
          (cl-weave:expect
           (ntop.battery.domain:battery-device-seconds-to-empty device)
           :to-be-null)))))
 (cl-weave:it "uses generic names when more than one battery is present"
   (multiple-value-bind (devices unavailable)
       (%map-synthetic-darwin-batteries
        (list (%synthetic-darwin-battery-properties :current-capacity 50)
              (%synthetic-darwin-battery-properties :current-capacity 80)))
     (cl-weave:expect unavailable :to-be-null)
     (cl-weave:expect (mapcar #'ntop.battery.domain:battery-device-name devices)
                      :to-equal '("Battery 0" "Battery 1"))))
 (cl-weave:it "reports no battery sources unavailable exactly once"
   (multiple-value-bind
       (devices unavailable value-calls unavailable-calls)
       (%map-synthetic-darwin-batteries
        '((:source-type "AC Power" :source-state "AC Power")))
     (cl-weave:expect devices :to-be-null)
     (cl-weave:expect unavailable :to-be :not-present)
     (cl-weave:expect value-calls :to-be 0)
     (cl-weave:expect unavailable-calls :to-be 1)))
 (cl-weave:it "keeps battery state unknown when connection signals are missing"
  (multiple-value-bind
      (devices unavailable value-calls unavailable-calls)
      (%map-synthetic-darwin-batteries
       (list (list :source-type "InternalBattery"
                   :current-capacity 75
                   :maximum-capacity 100
                   :charging-p :false
                   :fully-charged-p :false)))
    (cl-weave:expect unavailable :to-be-null)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-state (first devices))
     :to-be :unknown)
    (cl-weave:expect value-calls :to-be 1)
    (cl-weave:expect unavailable-calls :to-be 0)))
 (cl-weave:it-each
  (((:current-capacity nil) "missing current capacity")
   ((:maximum-capacity nil) "missing maximum capacity")
   ((:maximum-capacity 0) "zero maximum capacity"))
  "reports ~A as a read failure"
  (overrides description)
  (multiple-value-bind
      (devices unavailable value-calls unavailable-calls)
      (%map-synthetic-darwin-batteries
       (list (apply #'%synthetic-darwin-battery-properties overrides)))
    (cl-weave:expect devices :to-be-null)
    (cl-weave:expect unavailable :to-be :read-failed)
    (cl-weave:expect value-calls :to-be 0)
    (cl-weave:expect unavailable-calls :to-be 1)))
 (cl-weave:it "requests only battery measurements from AppleSmartBattery"
   (cl-weave:expect
    (mapcar #'car ntop.platform.darwin::*apple-smart-battery-fields*)
    :to-equal
    '("ExternalConnected" "IsCharging" "FullyCharged"
      "AppleRawMaxCapacity" "DesignCapacity" "Amperage" "Voltage"
      "TimeRemaining")))
 (cl-weave:it-each
  ((:no-power-source-info :read-failed)
   (:no-power-source-list :read-failed)
   (:no-matching-dictionary :private-api-unavailable)
   ((:iokit-error #xe00002c1) :permission-denied)
   ((:iokit-error #xe00002e2) :permission-denied)
   ((:iokit-error 1) :read-failed)
   (:unexpected :read-failed))
  "maps Darwin read failures to ~S"
  (reason expected)
  (cl-weave:expect
   (ntop.battery.infrastructure::%darwin-battery-unavailable-reason reason)
   :to-be expected)))