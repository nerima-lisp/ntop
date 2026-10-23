(in-package #:ntop.ui)

(defun format-process-time (elapsed-duration)
  (check-type elapsed-duration ntop.shared.domain:quantity)
  (let* ((rule :process-time)
         (millisecond-unit (format-rule-value rule :elapsed-unit))
         (milliseconds-per-second
          (format-rule-value rule :milliseconds-per-second))
         (seconds-per-minute (format-rule-value rule :seconds-per-minute))
         (minutes-per-hour (format-rule-value rule :minutes-per-hour))
         (hours-per-day (format-rule-value rule :hours-per-day))
         (minute-precision (format-rule-value rule :minute-fraction-precision))
         (second-precision (format-rule-value rule :second-fraction-precision))
         (elapsed-milliseconds
          (floor
           (ntop.shared.domain:quantity-value
            (ntop.shared.domain:convert-quantity elapsed-duration
                                                 millisecond-unit))))
         (total-seconds (floor elapsed-milliseconds milliseconds-per-second))
         (milliseconds (mod elapsed-milliseconds milliseconds-per-second))
         (total-minutes (floor total-seconds seconds-per-minute))
         (total-hours (floor total-minutes minutes-per-hour))
         (total-days (floor total-hours hours-per-day)))
    (cond
      ((plusp total-days)
       (format nil
               "~Dd ~Dh ~Dm"
               total-days
               (mod total-hours hours-per-day)
               (mod total-minutes minutes-per-hour)))
      ((plusp total-hours)
       (format nil
               "~Dh ~Dm ~Ds"
               total-hours
               (mod total-minutes minutes-per-hour)
               (mod total-seconds seconds-per-minute)))
      ((plusp total-minutes)
       (format nil
               "~Dm ~As"
               total-minutes
               (format-fixed
                (+ (mod total-seconds seconds-per-minute)
                   (/ (floor milliseconds 10) 100d0))
                minute-precision)))
      (t
       (format nil
               "~As"
               (format-fixed
                (+ (mod total-seconds seconds-per-minute)
                   (/ milliseconds milliseconds-per-second))
                second-precision))))))
