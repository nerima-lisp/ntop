(in-package #:ntop.battery.presentation)

(ntop.ui:define-table-columns *battery-value-columns* (:value "" :soft))

(ntop.ui:define-table-columns *battery-charge-columns* (:charge "Charge" :soft))

(ntop.ui:define-table-columns *battery-metric-columns*
                              (:label "" :soft)
                              (:value "" :soft))

(defstruct
    (%battery-color-target
     (:constructor %make-battery-color-target
                   (target charge-x charge-y charge-role)))
  target
  charge-x
  charge-y
  charge-role)

(defmethod ntop.ui:draw-table-text ((target %battery-color-target) x
                                    y
                                    text
                                    role)
  (ntop.ui:draw-table-text (%battery-color-target-target target)
                           x
                           y
                           text
                           (if (and
                                (= x (%battery-color-target-charge-x target))
                                (= y (%battery-color-target-charge-y target)))
                               (%battery-color-target-charge-role target)
                               role)))

(defun %battery-table-config (columns &key
                                      (selectable-p nil)
                                      (focused-p t)
                                      (show-current-entry-when-unfocused-p nil))
  (ntop.ui:make-table-config :columns
                             columns
                             :selectable-p
                             selectable-p
                             :focused-p
                             focused-p
                             :show-current-entry-when-unfocused-p
                             show-current-entry-when-unfocused-p))

(defun %battery-draw-row (target x y width columns row)
  (when (plusp width)
    (ntop.ui:draw-table target
                        (ntop.ui:make-rectangle :x
                                                x
                                                :y
                                                y
                                                :width
                                                width
                                                :height
                                                1)
                        (ntop.ui:make-table-state)
                        (%battery-table-config columns)
                        (list row))))

(defun %battery-clamp-charge (charge)
  (min 100 (max 0 charge)))

(defun %battery-charge-color-role (charge)
  (if (realp charge)
      (let ((percentage (%battery-clamp-charge charge)))
        (cond
          ((< percentage 10) :low-battery)
          ((< percentage 50) :medium-battery)
          (t :high-battery)))
      :normal))

(defun %battery-charge-bar (charge width)
  (if (realp charge)
      (let* ((percentage (%battery-clamp-charge charge))
             (bar-width (max 0 (- width 6)))
             (filled
              (min bar-width (floor (+ (* bar-width (/ percentage 100)) 1/2))))
             (empty (- bar-width filled))
             (displayed-percentage (floor (+ percentage 1/2))))
        (format nil
                "[~A~A~3D%]"
                (make-string filled :initial-element #\|)
                (make-string empty :initial-element #\Space)
                displayed-percentage))
      (if (>= width 6)
          (format nil
                  "[~A N/A]"
                  (make-string (- width 6) :initial-element #\Space))
          "[N/A]")))

(defun %battery-rate-text (watts)
  (if (realp watts)
      (format nil "~,2FW" watts)
      "N/A"))

(defun %battery-health-text (health)
  (if (realp health)
      (format nil "~,2F%" health)
      "N/A"))

(defun %battery-state-text (state)
  (ecase state
    (:charging "Charging")
    (:discharging "Discharging")
    (:full "Full")
    (:empty "Empty")
    (:unknown "Unknown")))

(defun %battery-long-time (seconds)
  (multiple-value-bind (hours remainder) (floor seconds 3600)
    (multiple-value-bind (minutes seconds) (floor remainder 60)
      (if (plusp hours)
          (format nil
                  "~D ~A, ~D ~A, ~D ~A"
                  hours
                  (if (= hours 1)
                      "hour"
                      "hours")
                  minutes
                  (if (= minutes 1)
                      "minute"
                      "minutes")
                  seconds
                  (if (= seconds 1)
                      "second"
                      "seconds"))
          (format nil
                  "~D ~A, ~D ~A"
                  minutes
                  (if (= minutes 1)
                      "minute"
                      "minutes")
                  seconds
                  (if (= seconds 1)
                      "second"
                      "seconds"))))))

(defun %battery-short-time (seconds)
  (multiple-value-bind (hours remainder) (floor seconds 3600)
    (multiple-value-bind (minutes seconds) (floor remainder 60)
      (if (plusp hours)
          (format nil "~Dh ~Dm ~Ds" hours minutes seconds)
          (format nil "~Dm ~Ds" minutes seconds)))))

(defun %battery-time-row (device width)
  (let* ((state (ntop.battery.domain:battery-device-state device))
         (seconds
          (case state
            (:charging
             (ntop.battery.domain:battery-device-seconds-to-full device))
            (:discharging
             (ntop.battery.domain:battery-device-seconds-to-empty device))))
         (long-label
          (case state
            (:charging "Time to full")
            (:discharging "Time to empty")))
         (short-label
          (case state
            (:charging "To full")
            (:discharging "To empty"))))
    (when (and long-label (realp seconds) (not (minusp seconds)))
      (let* ((whole-seconds (floor seconds))
             (long-value (%battery-long-time whole-seconds))
             (short-value (%battery-short-time whole-seconds))
             (value-width (floor (max 0 (- width 2)) 2)))
        (if (>= value-width (length long-value))
            (list :label long-label :value long-value)
            (list :label short-label :value short-value))))))

(defun %battery-metric-rows (device width)
  (let ((time-row (%battery-time-row device width)))
    (append
     (list
      (list :label
            "Rate"
            :value
            (%battery-rate-text
             (ntop.battery.domain:battery-device-power-watts device)))
      (list :label
            "State"
            :value
            (%battery-state-text
             (ntop.battery.domain:battery-device-state device))))
     (when time-row
       (list time-row))
     (list
      (list :label
            "Health"
            :value
            (%battery-health-text
             (ntop.battery.domain:battery-device-health-percent device)))))))

(defun %battery-draw-charge (target rectangle charge)
  (let* ((x (ntop.ui:rectangle-x rectangle))
         (y (ntop.ui:rectangle-y rectangle))
         (width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle))
         (label-p (>= height 2))
         (bar-y
          (+ y
             (if label-p
                 1
                 0)))
         (bar (%battery-charge-bar charge width))
         (color-role (%battery-charge-color-role charge))
         (color-target (%make-battery-color-target target x bar-y color-role))
         (draw-rectangle
          (ntop.ui:make-rectangle :x
                                  x
                                  :y
                                  y
                                  :width
                                  width
                                  :height
                                  (if label-p
                                      2
                                      1))))
    (when (and (plusp width) (plusp height))
      (ntop.ui:draw-table color-target
                          draw-rectangle
                          (ntop.ui:make-table-state)
                          (%battery-table-config *battery-charge-columns*)
                          (list (list :charge bar))))))

(defun %battery-draw-metrics (target rectangle device start-y)
  (let* ((bottom
          (+ (ntop.ui:rectangle-y rectangle)
             (ntop.ui:rectangle-height rectangle)))
         (width (ntop.ui:rectangle-width rectangle))
         (rows (%battery-metric-rows device width)))
    (loop for row in rows
          for y from start-y
          while (< y bottom)
          do (%battery-draw-row target
                                (ntop.ui:rectangle-x rectangle)
                                y
                                width
                                *battery-metric-columns*
                                row))))

(defun %battery-draw-tab (target x y width label selected-p)
  (when (plusp width)
    (ntop.ui:draw-table target
                        (ntop.ui:make-rectangle :x
                                                x
                                                :y
                                                y
                                                :width
                                                width
                                                :height
                                                1)
                        (ntop.ui:make-table-state)
                        (ntop.ui:make-table-config :columns
                                                   *battery-value-columns*
                                                   :selectable-p
                                                   t
                                                   :focused-p
                                                   selected-p
                                                   :show-current-entry-when-unfocused-p
                                                   nil)
                        (list (list :value label)))))

(defun %battery-draw-tabs (target rectangle device-count selected-index)
  (let ((x (ntop.ui:rectangle-x rectangle))
        (y (ntop.ui:rectangle-y rectangle))
        (right (+ (ntop.ui:rectangle-x rectangle)
                  (ntop.ui:rectangle-width rectangle)))
        (start-index
         (if (<=
              (loop for index from 0 to selected-index
                    sum (+ (length (format nil "Battery ~D" index))
                           (if (< index selected-index) 3 0)))
              (ntop.ui:rectangle-width rectangle))
             0
             selected-index))
        (areas nil))
    (when (and (plusp (ntop.ui:rectangle-width rectangle))
               (plusp (ntop.ui:rectangle-height rectangle)))
      (loop for index from start-index below device-count
            while (< x right)
            for label = (format nil "Battery ~D" index)
            for visible-width = (min (length label) (- right x))
            do (%battery-draw-tab target
                                  x
                                  y
                                  visible-width
                                  label
                                  (= index selected-index))
               (push
                (ntop.ui:make-clickable-area
                 :rectangle
                 (ntop.ui:make-rectangle
                  :x x
                  :y y
                  :width visible-width
                  :height 1)
                 :action
                 :battery-tab
                 :value
                 index)
                areas)
               (incf x visible-width)
               (when (< visible-width (length label))
                 (return))
               (when (and (< index (1- device-count)) (< x right))
                 (let* ((separator " | ")
                        (separator-width (min (length separator) (- right x)))
                        (visible-separator
                          (if (= separator-width 1)
                              "|"
                              (subseq separator 0 separator-width))))
                   (%battery-draw-row target
                                      x
                                      y
                                      separator-width
                                      *battery-value-columns*
                                      (list :value visible-separator))
                   (incf x separator-width)))))
    (nreverse areas)))

(defun %battery-selected-index (state device-count)
  (let* ((requested
          (if (and (typep state 'ntop.ui:table-state)
                   (integerp (ntop.ui:table-state-selected-index state)))
              (ntop.ui:table-state-selected-index state)
              0))
         (selected
          (if (plusp device-count)
              (min (max 0 requested) (1- device-count))
              0)))
    (when (typep state 'ntop.ui:table-state)
      (setf (ntop.ui:table-state-selected-index state) selected))
    selected))

(defun %battery-device-list (sample)
  (if (typep sample 'ntop.battery.domain:battery-sample)
      (ntop.battery.domain:battery-sample-devices sample)
      nil))

(defun %battery-draw-unavailable (target rectangle)
  (when
      (and (plusp (ntop.ui:rectangle-width rectangle))
           (plusp (ntop.ui:rectangle-height rectangle)))
    (%battery-draw-row target
                       (ntop.ui:rectangle-x rectangle)
                       (ntop.ui:rectangle-y rectangle)
                       (ntop.ui:rectangle-width rectangle)
                       *battery-value-columns*
                       (list :value "No data found for this battery"))))

(defun %battery-render (target rectangle state sample)
  (let* ((devices (%battery-device-list sample))
         (device-count (length devices))
         (selected-index (%battery-selected-index state device-count)))
    (if (endp devices)
        (%battery-draw-unavailable target rectangle)
        (let* ((tabs-p (> device-count 1))
               (areas
                (if tabs-p
                    (%battery-draw-tabs target
                                        rectangle
                                        device-count
                                        selected-index)
                    nil))
               (content-y
                (+ (ntop.ui:rectangle-y rectangle)
                   (if tabs-p
                       1
                       0)))
               (content-height
                (max 0
                     (- (ntop.ui:rectangle-height rectangle)
                        (if tabs-p
                            1
                            0))))
               (content-rectangle
                (ntop.ui:make-rectangle :x
                                        (ntop.ui:rectangle-x rectangle)
                                        :y
                                        content-y
                                        :width
                                        (ntop.ui:rectangle-width rectangle)
                                        :height
                                        content-height))
               (device (nth selected-index devices)))
          (when (plusp content-height)
            (%battery-draw-charge target
                                  content-rectangle
                                  (ntop.battery.domain:battery-device-charge-percent
                                   device))
            (let* ((charge-height (min content-height 2))
                   (remaining-height (- content-height charge-height))
                   (metrics-y
                    (+ content-y
                       charge-height
                       (if (plusp remaining-height)
                           1
                           0))))
              (%battery-draw-metrics target content-rectangle device metrics-y)))
          areas))))

(ntop.ui:define-widget :battery
                       (target rectangle state config series)
                       (declare (ignore config))
                       (%battery-render target rectangle state series))
