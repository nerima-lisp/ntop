(in-package #:ntop.test)

(defun temperature-sensor-reading (sensor)
  (ntop.temperature.domain:temperature-sensor-temperature-celsius sensor))

(cl-weave:describe "temperature domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              temperature-sensor
                              (name nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (cl-weave:expect (car (second expansion))
                                       :to-be 'defstruct)
                      (cl-weave:expect
                       (second (second (second expansion)))
                       :to-equal
                       '(:constructor make-temperature-sensor-raw))
                      (cl-weave:expect
                       (third (second (second expansion)))
                       :to-equal
                       '(:copier nil))
                      (cl-weave:expect (car (third expansion)) :to-be 'defun)
                      (cl-weave:expect (second (third expansion))
                                       :to-be
                                       'make-temperature-sensor)
                      (cl-weave:expect (car (fourth (third expansion)))
                                       :to-be
                                       'ntop.shared.domain::validate-typed-slots)))
                   (cl-weave:it
                    "includes the shared record in a sample type"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-sample-type
                              temperature-sample
                              (sensors nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (cl-weave:expect (car (second expansion))
                                       :to-be 'defstruct)
                      (cl-weave:expect
                       (second (second (second expansion)))
                       :to-equal
                       '(:include ntop.shared.domain:sample-record))
                      (cl-weave:expect
                       (third (second (second expansion)))
                       :to-equal
                       '(:constructor make-temperature-sample-raw))
                      (cl-weave:expect
                       (fourth (second (second expansion)))
                       :to-equal
                       '(:copier nil))
                      (cl-weave:expect (car (third expansion)) :to-be 'defun)
                      (cl-weave:expect (second (third expansion))
                                       :to-be
                                       'make-temperature-sample)
                      (cl-weave:expect (car (fourth (third expansion)))
                                       :to-be
                                       'ntop.shared.domain::validate-typed-slots)))
                   (cl-weave:it
                    "constructs a sample with a sensor and timestamp"
                    (let* ((sensor
                            (ntop.temperature.domain:make-temperature-sensor
                             :name
                             "CPU"
                             :temperature-celsius
                             52.5))
                           (sample
                            (ntop.temperature.domain:make-temperature-sample
                             :sampled-at
                             27.0
                             :sensors
                             (list sensor))))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       27.0)
                      (cl-weave:expect
                       (ntop.temperature.domain:temperature-sensor-temperature-celsius
                        (first
                         (ntop.temperature.domain:temperature-sample-sensors
                          sample)))
                       :to-be
                       52.5)))
                   (cl-weave:it-each ((nil nil) (0.0 0.0))
                                     "preserves ~S as an optional sensor reading"
                                     (value expected)
                                     (let ((sensor
                                            (ntop.temperature.domain:make-temperature-sensor
                                             :temperature-celsius
                                             value)))
                                       (cl-weave:expect
                                        (temperature-sensor-reading sensor)
                                        :to-be
                                        expected))))

(cl-weave:it-each
 ((:name ntop.temperature.domain:make-temperature-sensor
         ntop.temperature.domain:temperature-sensor-name
         nil
         nil
         nil
         "CPU"
         :invalid)
  (:temperature-celsius ntop.temperature.domain:make-temperature-sensor
                        ntop.temperature.domain:temperature-sensor-temperature-celsius
                        nil
                        nil
                        nil
                        52.5
                        :invalid)
  (:sensors ntop.temperature.domain:make-temperature-sample
            ntop.temperature.domain:temperature-sample-sensors
            (:sampled-at 0.0)
            nil
            nil
            (:sensor)
            :invalid))
 "checks typed temperature slot ~S"
 (initarg constructor
          accessor
          required-initargs
          default-value
          nil-value
          accepted-value
          invalid-value)
 (cl-weave:expect
  (constructor-slot-outcomes constructor
                             accessor
                             required-initargs
                             initarg
                             nil-value
                             accepted-value
                             invalid-value)
  :to-equal
  (list default-value nil-value accepted-value :type-error)))
