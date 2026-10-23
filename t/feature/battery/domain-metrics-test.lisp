(in-package #:ntop.test)

(defun expect-battery-measurement (actual expected)
  (cl-weave:expect
   (if (null expected)
       (null actual)
       (and (realp actual) (= actual expected)))
   :to-be t))

(cl-weave:describe "battery domain metrics"
  (cl-weave:it-each
    ((75 100 75)
     (1 3 100/3)
     (-5 100 0)
     (125 100 100)
     (nil 100 nil)
     (75 nil nil)
     (75 0 nil)
     (75 -1 nil))
    "bounds charge percent for current ~S and maximum ~S"
    (current maximum expected)
    (expect-battery-measurement
     (ntop.battery.domain:battery-charge-percent current maximum)
     expected))

  (cl-weave:it-each
    ((90 100 90)
     (1 3 100/3)
     (-5 100 0)
     (125 100 100)
     (nil 100 nil)
     (90 nil nil)
     (90 0 nil)
     (90 -1 nil))
    "bounds health percent for raw maximum ~S and design ~S"
    (raw-max design expected)
    (expect-battery-measurement
     (ntop.battery.domain:battery-health-percent raw-max design)
     expected))

  (cl-weave:it-each
    ((-2 12 24)
     (2 12 24)
     (1/2 4 2)
     (0 12 0)
     (nil 12 nil)
     (2 nil nil))
    "uses current magnitude for amperage ~S and voltage ~S"
    (amperage voltage expected)
    (expect-battery-measurement
     (ntop.battery.domain:battery-power-watts amperage voltage)
     expected))

  (cl-weave:it-each
    (("Charging" t nil 0 t :charging)
     ("discharging" t t 50 t :discharging)
     ("EMPTY" t nil 50 nil :empty)
     ("Full" nil nil 0 nil :full)
     ("Unknown" t nil 50 t :unknown)
     ("not charging" t t 50 t :unknown)
     (nil nil t 0 t :discharging)
     (nil t t 0 t :charging)
     (nil t nil 0 t :empty)
     (nil t nil 50 t :full)
     (nil t nil 50 nil :unknown)
     (nil t nil nil nil :unknown)
     (:charging nil nil nil nil :charging)
     (:discharging nil nil nil nil :discharging)
     (:empty nil nil nil nil :empty)
     (:full nil nil nil nil :full)
     (:unknown nil nil nil nil :unknown))
    "maps status and platform signals to ~S"
    (status external-connected-p charging-p current-capacity fully-charged-p expected)
    (cl-weave:expect
     (ntop.battery.domain:battery-state-from-signals
      status external-connected-p charging-p current-capacity fully-charged-p)
     :to-be expected))

  (cl-weave:it-each
    ((0 0)
     (1 60)
     (1/2 30)
     (15 900)
     (nil nil)
     ("15" nil)
     (2147483646 128849018760)
     (2147483647 nil)
     (-1 nil)
     (-1/2 nil))
    "converts available minutes ~S to seconds"
    (minutes expected)
    (expect-battery-measurement
     (ntop.battery.domain:battery-time-remaining-seconds minutes)
     expected))

  (cl-weave:it-each
    ((0 2 0)
     (1 2 1800)
     (3 2 5400)
     (1/2 1 1800)
     (nil 2 nil)
     (1 nil nil)
     (1 0 nil)
     (1 -2 nil)
     (-1 2 nil))
    "estimates time from ~S watt-hours at ~S watts"
    (remaining-energy-watt-hours power-watts expected)
    (expect-battery-measurement
     (ntop.battery.domain:battery-estimated-time-seconds
      remaining-energy-watt-hours power-watts)
     expected)))
