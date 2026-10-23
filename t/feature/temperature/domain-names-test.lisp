(in-package #:ntop.test)

  (cl-weave:describe "temperature sensor labels"
    (cl-weave:it "keeps every row and suffixes repeated names in source order"
      (let* ((first-cpu
               (ntop.temperature.domain:make-temperature-sensor
                :name "CPU" :temperature-celsius 40))
             (lowercase-cpu
               (ntop.temperature.domain:make-temperature-sensor
                :name "cpu" :temperature-celsius 41))
             (unnamed
               (ntop.temperature.domain:make-temperature-sensor
                :temperature-celsius nil))
             (second-cpu
               (ntop.temperature.domain:make-temperature-sensor
                :name "CPU" :temperature-celsius 42))
             (sensors (list first-cpu lowercase-cpu unnamed second-cpu))
             (labels
               (ntop.temperature.domain:temperature-sensor-labels sensors)))
        (cl-weave:expect (length labels) :to-be 4)
        (cl-weave:expect
         (mapcar #'cdr labels)
         :to-equal
         '("CPU (1)" "cpu" nil "CPU (2)"))
        (cl-weave:expect
         (every #'eq sensors (mapcar #'car labels))
         :to-be t))))
