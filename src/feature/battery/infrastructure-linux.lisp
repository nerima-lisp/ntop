(in-package #:ntop.battery.infrastructure)

(defun %linux-battery-charge-inputs (battery)
  (let ((energy-now (getf battery :energy-now-microwatt-hours))
        (energy-full (getf battery :energy-full-microwatt-hours))
        (charge-now (getf battery :charge-now-microamp-hours))
        (charge-full (getf battery :charge-full-microamp-hours))
        (capacity (getf battery :capacity-percent)))
    (cond
      ((and (numberp energy-now) (numberp energy-full))
       (values energy-now energy-full))
      ((and (numberp charge-now) (numberp charge-full))
       (values charge-now charge-full))
      ((numberp capacity)
       (values capacity 100))
      (t
       (values nil nil)))))

(defun %linux-battery-health-percent (battery)
  (let ((energy-full (getf battery :energy-full-microwatt-hours))
        (energy-design (getf battery :energy-full-design-microwatt-hours))
        (charge-full (getf battery :charge-full-microamp-hours))
        (charge-design (getf battery :charge-full-design-microamp-hours)))
    (cond
      ((and (numberp energy-full) (numberp energy-design))
       (ntop.battery.domain:battery-health-percent energy-full energy-design))
      ((and (numberp charge-full) (numberp charge-design))
       (ntop.battery.domain:battery-health-percent charge-full charge-design))
      (t
       nil))))

(defun %linux-battery-power-watts (battery)
  (let ((current (getf battery :current-now-microamps))
        (voltage (getf battery :voltage-now-microvolts))
        (power (getf battery :power-now-microwatts)))
    (let ((watts
            (or
             (when (and (numberp current) (numberp voltage) (plusp voltage))
               (ntop.battery.domain:battery-power-watts (/ current 1000000)
                (/ voltage 1000000)))
             (when (numberp power)
               (/ power 1000000)))))
      (when (numberp watts)
        (if (or (= watts 65535)
                (> watts 100)
                (< watts (/ 1 100000)))
            0
            watts)))))

(defun %linux-battery-energy-watt-hours (battery)
  (let ((energy-now (getf battery :energy-now-microwatt-hours))
        (energy-full (getf battery :energy-full-microwatt-hours))
        (charge-now (getf battery :charge-now-microamp-hours))
        (charge-full (getf battery :charge-full-microamp-hours))
        (voltage (getf battery :voltage-now-microvolts)))
    (cond
      ((and (numberp energy-now) (numberp energy-full))
       (values (/ energy-now 1000000)
               (/ energy-full 1000000)))
      ((and (numberp charge-now)
            (numberp charge-full)
            (numberp voltage)
            (plusp voltage))
       (values (/ (* charge-now voltage) 1000000000000)
               (/ (* charge-full voltage) 1000000000000)))
      (t
       (values nil nil)))))

(defun %linux-battery-device (battery)
  (multiple-value-bind (current maximum)
      (%linux-battery-charge-inputs battery)
    (let* ((charge-percent
             (when (and (numberp current) (numberp maximum))
               (ntop.battery.domain:battery-charge-percent current maximum)))
           (status (getf battery :status)))
      (unless (numberp charge-percent)
        (return-from %linux-battery-device (values nil :invalid-data)))
      (let* ((fully-charged-p
               (or (and (stringp status) (string-equal status "Full"))
                   (>= charge-percent 100)))
             (state
               (ntop.battery.domain:battery-state-from-signals (or status :unknown)
                nil
                (and (stringp status) (string-equal status "Charging"))
                charge-percent
                fully-charged-p))
             (power-watts (%linux-battery-power-watts battery))
             (health-percent (%linux-battery-health-percent battery))
             (seconds-to-full nil)
             (seconds-to-empty nil))
        (multiple-value-bind (energy-now energy-full)
            (%linux-battery-energy-watt-hours battery)
          (when (and (eq state :charging) energy-now energy-full)
            (let ((estimate
                    (ntop.battery.domain:battery-estimated-time-seconds (- energy-full energy-now)
                     power-watts)))
              (when (and estimate (<= estimate 36000))
                (setf seconds-to-full estimate))))
          (when (and (eq state :discharging) energy-now)
            (let ((estimate
                    (ntop.battery.domain:battery-estimated-time-seconds energy-now
                     power-watts)))
              (when (and estimate (<= estimate 864000))
                (setf seconds-to-empty estimate)))))
        (values
         (ntop.battery.domain:make-battery-device
          :name (getf battery :name)
          :charge-percent charge-percent
          :power-watts power-watts
          :state state
          :seconds-to-full seconds-to-full
          :seconds-to-empty seconds-to-empty
          :health-percent health-percent)
         nil)))))

(defun %linux-battery-devices (batteries)
  (let ((devices nil)
        (failure nil))
    (dolist (battery batteries)
      (unless failure
        (multiple-value-bind (device reason)
            (%linux-battery-device battery)
          (if reason
              (setf failure reason)
              (push device devices)))))
    (if failure
        (values nil failure)
        (values (nreverse devices) nil))))

(defun %linux-battery-unavailable-reason (reason)
  (cond
   ((eq reason :root-unavailable) :unsupported-platform)
   ((typep reason 'ntop.shared.application:collector-unavailability-reason)
    reason)
   (t :read-failed)))

(defun read-linux-battery-devices (on-value on-unavailable)
  (ntop.platform.linux:read-linux-batteries
   (lambda (batteries)
     (if (null batteries)
         (funcall on-unavailable :not-present)
         (multiple-value-bind (devices reason)
             (%linux-battery-devices batteries)
           (if reason
               (funcall on-unavailable
                        (%linux-battery-unavailable-reason reason))
               (funcall on-value devices)))))
   (lambda (reason)
     (funcall on-unavailable
              (%linux-battery-unavailable-reason reason)))))

#+linux
(defmethod read-battery-devices
    ((collector battery-collector) on-value on-unavailable)
  (declare (ignore collector))
  (read-linux-battery-devices on-value on-unavailable))
