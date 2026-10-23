(in-package #:ntop.test)

(defun %linux-battery-fixture-root (fixture)
  (asdf:system-relative-pathname
   "ntop"
   (format nil "t/feature/battery/fixtures/linux/~A/" fixture)))

(defun %read-linux-battery-fixture (fixture)
  (let ((result nil))
    (ntop.platform.linux:with-linux-root (%linux-battery-fixture-root fixture)
      (ntop.battery.infrastructure::read-linux-battery-devices
       (lambda (value)
         (setf result (list :value value)))
       (lambda (reason)
         (setf result (list :unavailable reason)))))
    result))

(cl-weave:describe
 "Linux battery infrastructure fixtures"
 (cl-weave:it "converts sysfs micro-units into a complete battery device"
   (let* ((result (%read-linux-battery-fixture "complete"))
          (devices (second result))
          (device (first devices)))
     (cl-weave:expect (first result) :to-be :value)
     (cl-weave:expect (length devices) :to-be 1)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-name device)
      :to-equal "Main Pack")
     (cl-weave:expect
      (ntop.battery.domain:battery-device-charge-percent device)
      :to-be 73)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-power-watts device)
      :to-be 72/5)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-state device)
      :to-be :charging)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-seconds-to-full device)
      :to-be 4050)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-seconds-to-empty device)
      :to-be nil)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-health-percent device)
      :to-be 80)))
 (cl-weave:it "uses capacity when raw current and maximum fields are absent"
   (let* ((result (%read-linux-battery-fixture "capacity-only"))
          (device (first (second result))))
     (cl-weave:expect (first result) :to-be :value)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-charge-percent device)
      :to-be 0)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-state device)
      :to-be :empty)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-power-watts device)
      :to-be nil)))
 (cl-weave:it "reports unavailable when no battery supplies are present"
   (cl-weave:expect (%read-linux-battery-fixture "no-batteries")
                    :to-equal
                    '(:unavailable :not-present)))
 (cl-weave:it "rejects a battery without a usable charge measure"
   (cl-weave:expect (%read-linux-battery-fixture "missing-charge")
                    :to-equal
                    '(:unavailable :read-failed)))
 (cl-weave:it "rejects malformed required capacity data"
   (cl-weave:expect (%read-linux-battery-fixture "invalid-charge")
                    :to-equal
                    '(:unavailable :read-failed)))
 (cl-weave:it "forwards a missing power-supply directory"
   (cl-weave:expect (%read-linux-battery-fixture "absent")
                    :to-equal
                    '(:unavailable :not-present)))
 (cl-weave:it "treats missing sysfs battery status as unknown"
   (let* ((result (%read-linux-battery-fixture "missing-status"))
          (device (first (second result))))
     (cl-weave:expect (first result) :to-be :value)
     (cl-weave:expect
      (ntop.battery.domain:battery-device-state device)
      :to-be :unknown)))
 (cl-weave:it "normalizes implausible Linux power rates"
   (dolist
       (case
        '(((:power-now-microwatts 100000001) 0)
          ((:power-now-microwatts 9) 0)
          ((:power-now-microwatts 10) 1/100000)
          ((:power-now-microwatts 100000000) 100)
          ((:power-now-microwatts 65535000000) 0)
          ((:power-now-microwatts 14400000) 72/5)
          ((:current-now-microamps 10100000
            :voltage-now-microvolts 10000000) 0)
          (nil nil)))
     (destructuring-bind (battery expected) case
       (cl-weave:expect
        (ntop.battery.infrastructure::%linux-battery-power-watts battery)
        :to-equal expected))))
 (cl-weave:it "limits Linux charging and discharging time estimates"
   (dolist
       (case
        '(("Charging" 0 9999999 89999991/2500 nil)
          ("Charging" 0 10000000 36000 nil)
          ("Charging" 0 10000001 nil nil)
          ("Discharging" 239999999 480000000 nil 2159999991/2500)
          ("Discharging" 240000000 480000000 nil 864000)
          ("Discharging" 240000001 480000000 nil nil)))
     (destructuring-bind
         (status energy-now energy-full expected-full expected-empty)
         case
       (multiple-value-bind (device reason)
           (ntop.battery.infrastructure::%linux-battery-device
            (list :name "Synthetic Pack"
                  :status status
                  :energy-now-microwatt-hours energy-now
                  :energy-full-microwatt-hours energy-full
                  :power-now-microwatts 1000000))
         (cl-weave:expect reason :to-be nil)
         (cl-weave:expect
          (ntop.battery.domain:battery-device-seconds-to-full device)
          :to-equal expected-full)
         (cl-weave:expect
          (ntop.battery.domain:battery-device-seconds-to-empty device)
          :to-equal expected-empty)))))
 (cl-weave:it "normalizes Linux unavailability reasons to the collector contract"
   (dolist
       (case
        '(("no-batteries" :not-present)
          ("absent" :not-present)
          ("missing-charge" :read-failed)
          ("invalid-charge" :read-failed)))
     (destructuring-bind (fixture reason) case
       (cl-weave:expect
        (%read-linux-battery-fixture fixture)
        :to-equal (list :unavailable reason))))
   (dolist
       (case
        '((:not-present :not-present)
          (:disabled :disabled)
          (:permission-denied :permission-denied)
          (:unsupported-platform :unsupported-platform)
          (:private-api-unavailable :private-api-unavailable)
          (:read-failed :read-failed)
          (:invalid-data :read-failed)
          (:unavailable :read-failed)
          (:root-unavailable :unsupported-platform)
          (:invalid-path :read-failed)
          (:too-large :read-failed)
          (:unexpected :read-failed)
          (nil :read-failed)
          ("unexpected" :read-failed)))
     (destructuring-bind (reason expected) case
       (cl-weave:expect
        (ntop.battery.infrastructure::%linux-battery-unavailable-reason reason)
        :to-be expected)))
   (let ((result nil))
     (let ((ntop.platform.linux:*linux-root* nil))
       (ntop.battery.infrastructure::read-linux-battery-devices
        (lambda (value) (setf result (list :value value)))
        (lambda (reason) (setf result (list :unavailable reason)))))
     (cl-weave:expect result :to-equal '(:unavailable :unsupported-platform))))
 (cl-weave:it-each
  (("discharging from charge counters"
    (:name "Synthetic Pack"
     :status "Discharging"
     :charge-now-microamp-hours 3200000
     :charge-full-microamp-hours 4000000
     :capacity-percent 41
     :charge-full-design-microamp-hours 5000000
     :voltage-now-microvolts 12000000
     :power-now-microwatts 8000000)
    80 80 :discharging nil 17280)
   ("charging from charge counters"
    (:name "Synthetic Pack"
     :status "Charging"
     :charge-now-microamp-hours 1500000
     :charge-full-microamp-hours 2000000
     :capacity-percent 45
     :charge-full-design-microamp-hours 2500000
     :voltage-now-microvolts 10000000
     :power-now-microwatts 5000000)
    75 80 :charging 3600 nil)
   ("time estimate without voltage"
    (:name "Synthetic Pack"
     :status "Discharging"
     :charge-now-microamp-hours 3200000
     :charge-full-microamp-hours 4000000
     :capacity-percent 10
     :charge-full-design-microamp-hours 4000000
     :power-now-microwatts 8000000)
    80 100 :discharging nil nil))
  "maps ~A using charge and voltage fallbacks"
  (case-name battery expected-charge expected-health expected-state
   expected-to-full expected-to-empty)
  (multiple-value-bind (device reason)
      (ntop.battery.infrastructure::%linux-battery-device battery)
    (cl-weave:expect reason :to-be nil)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-charge-percent device)
     :to-equal expected-charge)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-health-percent device)
     :to-equal expected-health)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-state device)
     :to-be expected-state)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-seconds-to-full device)
     :to-equal expected-to-full)
    (cl-weave:expect
     (ntop.battery.domain:battery-device-seconds-to-empty device)
     :to-equal expected-to-empty)))
)
