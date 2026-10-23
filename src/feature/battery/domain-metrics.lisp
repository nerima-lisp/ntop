(in-package #:ntop.battery.domain)

(defun battery-charge-percent (current maximum)
  (when (and (realp current) (realp maximum) (> maximum 0))
    (* 100 (min 1 (max 0 (/ current maximum))))))

(defun battery-health-percent (raw-max design)
  (when (and (realp raw-max) (realp design) (> design 0))
    (* 100 (min 1 (max 0 (/ raw-max design))))))

(defun battery-power-watts (amperage voltage)
  (when (and (realp amperage) (realp voltage))
    (* (abs amperage) voltage)))

(defun battery-state-from-signals (status external-connected-p
                                          charging-p
                                          current-capacity
                                          fully-charged-p)
  (cond
    ((stringp status)
     (cond
       ((string-equal status "charging") :charging)
       ((string-equal status "discharging") :discharging)
       ((string-equal status "empty") :empty)
       ((string-equal status "full") :full)
       (t :unknown)))
    ((and (symbolp status) status)
     (let ((name (symbol-name status)))
       (cond
         ((string-equal name "charging") :charging)
         ((string-equal name "discharging") :discharging)
         ((string-equal name "empty") :empty)
         ((string-equal name "full") :full)
         (t :unknown))))
    (t
     (cond
       ((not external-connected-p) :discharging)
       (charging-p :charging)
       ((and (realp current-capacity) (zerop current-capacity)) :empty)
       (fully-charged-p :full)
       (t :unknown)))))

(defun battery-time-remaining-seconds (time-remaining-minutes)
  (when
      (and (realp time-remaining-minutes)
           (>= time-remaining-minutes 0)
           (/= time-remaining-minutes 2147483647))
    (* time-remaining-minutes 60)))

(defun battery-estimated-time-seconds (remaining-energy-watt-hours power-watts)
  (when
      (and (realp remaining-energy-watt-hours)
           (>= remaining-energy-watt-hours 0)
           (realp power-watts)
           (> power-watts 0))
    (/ (* remaining-energy-watt-hours 3600) power-watts)))
