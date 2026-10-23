(in-package #:ntop.battery.infrastructure)

(defparameter *darwin-battery-source-types*
  '("InternalBattery" "Battery" "UPS"))

(defun %battery-property (properties name)
  (loop for (property value) on properties by #'cddr
        when (eq property name)
          return (values value t)
        finally (return (values nil nil))))

(defun %darwin-boolean (value)
  (or (eq value :true) (eq value t)))

(defun %darwin-battery-source-p (properties)
  (let ((type (getf properties :source-type)))
    (and (stringp type)
         (member type *darwin-battery-source-types* :test #'string-equal))))

(defun %darwin-external-connected-p (properties status)
  (multiple-value-bind (value found-p)
      (%battery-property properties :external-connected-p)
    (if found-p
        (%darwin-boolean value)
        (cond
          ((and (stringp status) (string-equal status "AC Power")) t)
          ((and (stringp status) (string-equal status "Battery Power")) nil)
          (t :unknown)))))

(defun %darwin-charging-p (properties)
  (multiple-value-bind (value found-p)
      (%battery-property properties :charging-p)
    (and found-p (%darwin-boolean value))))

(defun %darwin-fully-charged-p (properties current maximum)
  (multiple-value-bind (value found-p)
      (%battery-property properties :fully-charged-p)
    (if found-p
        (%darwin-boolean value)
        (and (numberp current)
             (numberp maximum)
             (>= current maximum)))))

(defun %darwin-battery-times (state time-remaining-minutes)
  (let ((seconds
          (when (integerp time-remaining-minutes)
            (ntop.battery.domain:battery-time-remaining-seconds
             time-remaining-minutes))))
    (values (when (eq state :charging) seconds)
            (when (eq state :discharging) seconds))))

(defun %battery-device-from-darwin-properties (properties index)
  (let* ((current (getf properties :current-capacity))
         (maximum (getf properties :maximum-capacity))
         (charge-percent
           (when (and (numberp current) (numberp maximum))
             (ntop.battery.domain:battery-charge-percent current maximum))))
    (unless (numberp charge-percent)
      (return-from %battery-device-from-darwin-properties
        (values nil :invalid-data)))
    (let* ((status (getf properties :source-state))
           (external-connected-p
             (%darwin-external-connected-p properties status))
           (charging-p (%darwin-charging-p properties))
           (fully-charged-p
             (%darwin-fully-charged-p properties current maximum))
           (state
             (ntop.battery.domain:battery-state-from-signals
              (cond
                (fully-charged-p "Full")
                (charging-p "Charging")
                ((zerop current) "Empty")
                (t nil))
              external-connected-p charging-p current fully-charged-p))
           (time-remaining-minutes (getf properties :time-remaining-minutes)))
      (multiple-value-bind (seconds-to-full seconds-to-empty)
          (%darwin-battery-times state time-remaining-minutes)
        (values
         (ntop.battery.domain:make-battery-device
          :name (format nil "Battery ~D" index)
          :charge-percent charge-percent
          :health-percent
          (let ((raw-max (getf properties :raw-max-capacity))
                (design (getf properties :design-capacity)))
            (when (and (numberp raw-max) (numberp design))
              (ntop.battery.domain:battery-health-percent raw-max design)))
          :power-watts
          (let ((amperage (getf properties :amperage))
                (voltage (getf properties :voltage)))
            (when (and (numberp amperage) (numberp voltage))
              (ntop.battery.domain:battery-power-watts
               (/ amperage 1000) (/ voltage 1000))))
          :state state
          :seconds-to-full seconds-to-full
          :seconds-to-empty seconds-to-empty)
         nil)))))

(defun map-darwin-battery-properties (properties on-value on-unavailable)
  (let ((index -1)
        (devices nil)
        (failure nil))
    (dolist (battery-properties properties)
      (unless failure
        (when (%darwin-battery-source-p battery-properties)
          (multiple-value-bind (device reason)
              (%battery-device-from-darwin-properties
               battery-properties (incf index))
            (if reason
                (setf failure reason)
                (push device devices))))))
    (cond
      (failure
       (funcall on-unavailable :read-failed))
      (devices
       (funcall on-value (nreverse devices)))
      (t
       (funcall on-unavailable :not-present)))))
(defun %darwin-battery-unavailable-reason (reason)
  (cond
    ((eq reason :no-matching-dictionary) :private-api-unavailable)
    ((and (consp reason)
          (eq (first reason) :iokit-error)
          (integerp (second reason))
          (member (logand (second reason) #xffffffff)
                  '(#xe00002c1 #xe00002e2)))
     :permission-denied)
    (t :read-failed)))

#+darwin
(defun read-darwin-battery-devices (on-value on-unavailable)
  (ntop.platform.darwin::call-with-power-source-properties
   (lambda (properties)
     (map-darwin-battery-properties properties on-value on-unavailable))
   (lambda (reason)
     (funcall on-unavailable
              (%darwin-battery-unavailable-reason reason)))))

#+darwin
(defmethod read-battery-devices
    ((collector battery-collector) on-value on-unavailable)
  (declare (ignore collector))
  (read-darwin-battery-devices on-value on-unavailable))
