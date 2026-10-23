(in-package #:ntop.platform.darwin)

(defparameter *darwin-ioaccel-statistic-fields*
  '((:usage-percent . "Device Utilization %")
    (:memory-used-bytes . "In use system memory")
    (:memory-total-bytes . "Alloc system memory")
    (:temperature-celsius . "Temperature")))

(defun decode-darwin-ioaccel-device-properties (properties)
  (let ((statistics (gethash "PerformanceStatistics" properties)))
    (unless (hash-table-p statistics)
      (error "IOAccelerator PerformanceStatistics is not a dictionary."))
    (append (list :name (gethash "IOClass" properties))
            (loop for (slot . key) in *darwin-ioaccel-statistic-fields*
                  append (list slot (gethash key statistics))))))

#+darwin
(defun darwin-read-gpu-device-data (on-value on-unavailable)
  (let ((rows nil)
        (failures nil))
    (handler-case
        (call-with-iokit-services
         "IOAccelerator"
         (lambda (service)
           (handler-case
               (iokit-registry-properties
                service
                (lambda (properties)
                  (push (decode-darwin-ioaccel-device-properties properties)
                        rows))
                (lambda (reason)
                  (push reason failures)))
             (error (condition)
               (push (list :ioaccel-property-error condition) failures))))
         (lambda () nil)
         (lambda (reason)
           (push reason failures)))
      (error (condition)
        (push (list :ioaccel-read-error condition) failures)))
    (cond
      (rows
       (funcall on-value (nreverse rows)))
      (failures
       (funcall on-unavailable (list :ioaccel-unavailable
                                     (nreverse failures))))
      (t
       (funcall on-unavailable :no-gpu-devices)))))
