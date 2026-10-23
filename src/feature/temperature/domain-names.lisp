(in-package #:ntop.temperature.domain)

 (defun temperature-sensor-labels (sensors)
   (let ((name-counts (make-hash-table :test #'equal))
         (name-seen (make-hash-table :test #'equal)))
     (dolist (sensor sensors)
       (let ((name (temperature-sensor-name sensor)))
         (when name
           (setf (gethash name name-counts)
                 (1+ (gethash name name-counts 0))))))
     (loop for sensor in sensors
           for name = (temperature-sensor-name sensor)
           collect
           (cons sensor
                 (when name
                   (if (> (gethash name name-counts) 1)
                       (format nil "~A (~D)"
                               name
                               (incf (gethash name name-seen 0)))
                       name))))))
