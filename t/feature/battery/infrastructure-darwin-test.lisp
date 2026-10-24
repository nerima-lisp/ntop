(in-package #:ntop.test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames
         "helpers-darwin.lisp"
         (uiop:pathname-directory-pathname
          (asdf:component-pathname
           (asdf:find-component "ntop/test"
                                "t/feature/battery/infrastructure-darwin-test"))))))

(cl-weave:describe
 "Darwin battery property mapping"
 (cl-weave:it "maps charge, health, power, and remaining minutes"
  (multiple-value-bind (devices unavailable)
      (%map-synthetic-darwin-batteries
       (list (%synthetic-darwin-battery-properties)))
    (let ((device (first devices)))
      (cl-weave:expect
       (list unavailable
             (= (ntop.battery.domain:battery-device-charge-percent device) 75)
             (= (ntop.battery.domain:battery-device-health-percent device) 90)
             (= (ntop.battery.domain:battery-device-power-watts device) 24)
             (ntop.battery.domain:battery-device-seconds-to-empty device)
             (ntop.battery.domain:battery-device-seconds-to-full device))
       :to-equal
       '(nil t t t 5400 nil)))))
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
      (cl-weave:expect
       (list unavailable
             (ntop.battery.domain:battery-device-state device)
             (ntop.battery.domain:battery-device-seconds-to-full device)
             (ntop.battery.domain:battery-device-seconds-to-empty device))
       :to-equal
       (list nil expected-state
             expected-seconds-to-full expected-seconds-to-empty)))))
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
    (cl-weave:expect
     (list devices unavailable value-calls unavailable-calls)
     :to-equal '(nil :not-present 0 1))))
 (cl-weave:it "keeps battery state unknown when connection signals are missing"
  (multiple-value-bind
      (devices unavailable value-calls unavailable-calls)
      (%map-synthetic-darwin-batteries
       (list (list :source-type "InternalBattery"
                   :current-capacity 75
                   :maximum-capacity 100
                   :charging-p :false
                   :fully-charged-p :false)))
    (cl-weave:expect
     (list unavailable
           (ntop.battery.domain:battery-device-state (first devices))
           value-calls
           unavailable-calls)
     :to-equal '(nil :unknown 1 0))))
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
    (cl-weave:expect
     (list devices unavailable value-calls unavailable-calls)
     :to-equal '(nil :read-failed 0 1))))
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

(cl-weave:describe
 "Darwin battery fallback signals"
 (cl-weave:it-each
  (("AC Power" :connected)
   ("Battery Power" :disconnected))
  "uses the power status when the external connection property is absent"
  (status expected)
  (let ((connected
          (ntop.battery.infrastructure::%darwin-external-connected-p
           nil status)))
    (if (eq expected :connected)
        (cl-weave:expect connected :to-be t)
        (cl-weave:expect connected :to-be-null))))
 (cl-weave:it-each
  ((100 100 :full)
   (99 100 :not-full)
   (nil 100 :not-full)
   (100 nil :not-full))
  "infers full charge from numeric capacities when the property is absent"
  (current maximum expected)
  (let ((fully-charged
          (ntop.battery.infrastructure::%darwin-fully-charged-p
           nil current maximum)))
    (if (eq expected :full)
        (cl-weave:expect fully-charged :to-be t)
        (cl-weave:expect fully-charged :to-be-null))))
 (cl-weave:it "ignores a non-string battery source type"
   (cl-weave:expect
    (ntop.battery.infrastructure::%darwin-battery-source-p
     '(:source-type 7))
    :to-be-null))
 (cl-weave:it-each
  (((:other-iokit-error 1) :read-failed)
   ((:iokit-error "invalid-code") :read-failed))
  "rejects unrelated and nonnumeric IOKit errors"
  (reason expected)
  (cl-weave:expect
   (ntop.battery.infrastructure::%darwin-battery-unavailable-reason reason)
   :to-be expected)))
