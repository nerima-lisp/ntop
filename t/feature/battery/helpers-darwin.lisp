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
  "Build battery property fixtures for the Darwin mapper."
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
  "Capture value and unavailable callbacks from the battery mapper."
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
