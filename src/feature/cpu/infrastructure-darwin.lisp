(in-package #:ntop.cpu.infrastructure)

(defparameter *darwin-processor-cpu-load-layout*
  '(:entry-size 16 :user-offset 0 :system-offset 4 :idle-offset 8
    :nice-offset 12 :counter-width 4))

(defun decode-darwin-processor-cpu-load-info (octets processor-count)
  (check-type octets (vector (unsigned-byte 8)))
  (check-type processor-count (integer 0 *))
  (let* ((layout *darwin-processor-cpu-load-layout*)
         (entry-size (getf layout :entry-size))
         (counter-width (getf layout :counter-width)))
    (unless (= (length octets) (* processor-count entry-size))
      (error "Expected ~D CPU load octets, got ~D."
             (* processor-count entry-size) (length octets)))
    (loop for core-index below processor-count
          for start = (* core-index entry-size)
          collect
          (ntop.cpu.domain:make-cpu-core-value
           :core-index core-index
           :user-ticks
           (ntop.platform.darwin:decode-little-endian-unsigned
            octets :start (+ start (getf layout :user-offset))
            :end (+ start (getf layout :user-offset) counter-width))
           :system-ticks
           (ntop.platform.darwin:decode-little-endian-unsigned
            octets :start (+ start (getf layout :system-offset))
            :end (+ start (getf layout :system-offset) counter-width))
           :idle-ticks
           (ntop.platform.darwin:decode-little-endian-unsigned
            octets :start (+ start (getf layout :idle-offset))
            :end (+ start (getf layout :idle-offset) counter-width))
           :nice-ticks
           (ntop.platform.darwin:decode-little-endian-unsigned
            octets :start (+ start (getf layout :nice-offset))
            :end (+ start (getf layout :nice-offset) counter-width))
           :iowait-ticks nil :irq-ticks nil :softirq-ticks nil
           :steal-ticks nil :guest-ticks nil :guest-nice-ticks nil
           :frequency-mhz nil))))

(defun %darwin-cpu-core-frequencies (performance-state-data)
  (loop for (core-index state-residencies frequency-table-khz)
          in performance-state-data
        collect
        (cons core-index
              (ntop.cpu.domain:cpu-weighted-frequency-mhz
               state-residencies frequency-table-khz))))

(defun %darwin-cpu-core-with-frequency (core frequency-mhz)
  (ntop.cpu.domain:make-cpu-core-value
   :core-index (ntop.cpu.domain:cpu-core-value-core-index core)
   :user-ticks (ntop.cpu.domain:cpu-core-value-user-ticks core)
   :system-ticks (ntop.cpu.domain:cpu-core-value-system-ticks core)
   :nice-ticks (ntop.cpu.domain:cpu-core-value-nice-ticks core)
   :idle-ticks (ntop.cpu.domain:cpu-core-value-idle-ticks core)
   :iowait-ticks (ntop.cpu.domain:cpu-core-value-iowait-ticks core)
   :irq-ticks (ntop.cpu.domain:cpu-core-value-irq-ticks core)
   :softirq-ticks (ntop.cpu.domain:cpu-core-value-softirq-ticks core)
   :steal-ticks (ntop.cpu.domain:cpu-core-value-steal-ticks core)
   :guest-ticks (ntop.cpu.domain:cpu-core-value-guest-ticks core)
   :guest-nice-ticks (ntop.cpu.domain:cpu-core-value-guest-nice-ticks core)
   :frequency-mhz frequency-mhz))

(defun %make-darwin-cpu-source-sample
    (core-values load-averages read-performance-state-data)
  (let ((frequencies nil))
    (handler-case
        (funcall read-performance-state-data
                 (lambda (data)
                   (setf frequencies (%darwin-cpu-core-frequencies data)))
                 (lambda (reason) (declare (ignore reason))))
      (error () (setf frequencies nil)))
    (ntop.cpu.infrastructure::make-cpu-source-sample
     :core-values
     (mapcar (lambda (core)
               (%darwin-cpu-core-with-frequency
                core
                (cdr (assoc
                      (ntop.cpu.domain:cpu-core-value-core-index core)
                      frequencies))))
             core-values)
     :load-average-1 (first load-averages)
     :load-average-5 (second load-averages)
     :load-average-15 (third load-averages))))

#+darwin
(ntop.platform.darwin:define-darwin-ffi
  (:routine %host-processor-info "host_processor_info" sb-alien:int
    (host sb-alien:unsigned-int)
    (flavor sb-alien:int)
    (processor-count (* sb-alien:unsigned-int))
    (processor-info (* sb-alien:unsigned-long))
    (processor-info-count (* sb-alien:unsigned-int))))

#+darwin
(defun %copy-darwin-processor-info-octets (address size)
  (let ((octets (make-array size :element-type '(unsigned-byte 8)))
        (pointer (sb-sys:int-sap address)))
    (dotimes (index size octets)
      (setf (aref octets index) (sb-sys:sap-ref-8 pointer index)))))

#+darwin
(defun %read-darwin-processor-cpu-load-info (on-value on-unavailable)
  (multiple-value-bind (processor-count octets failure)
      (handler-case
          (ntop.platform.darwin:call-with-mach-host-port
           (lambda (host)
             (sb-alien:with-alien
                 ((processor-count sb-alien:unsigned-int 0)
                  (processor-info sb-alien:unsigned-long 0)
                  (processor-info-count sb-alien:unsigned-int 0))
               (let* ((status
                        (%host-processor-info
                         host 2 (sb-alien:addr processor-count)
                         (sb-alien:addr processor-info)
                         (sb-alien:addr processor-info-count)))
                      (size (* processor-info-count 4))
                      (octets nil)
                      (failure nil))
                 (unwind-protect
                      (cond
                        ((not (zerop status))
                         (setf failure
                               (list :host-processor-info-error status)))
                        ((or (zerop processor-count)
                             (/= processor-info-count
                                 (* processor-count 4)))
                         (setf failure
                               (list :host-processor-info-count-mismatch
                                     processor-count processor-info-count)))
                        ((zerop processor-info)
                         (setf failure
                               :host-processor-info-missing-buffer))
                        (t
                         (setf octets
                               (%copy-darwin-processor-info-octets
                                processor-info size))))
                   (unless (zerop processor-info)
                     (handler-case
                         (ntop.platform.darwin:mach-vm-deallocate
                          (sb-sys:int-sap processor-info) size
                          (lambda () nil)
                          (lambda (reason)
                            (setf failure
                                  (list :processor-info-deallocation-failed
                                        reason))))
                       (error (condition)
                         (setf failure
                               (list :processor-info-deallocation-failed
                                     condition))))))
                 (values processor-count octets failure))))
           (lambda (reason) (values nil nil reason)))
        (error (condition) (values nil nil condition)))
    (if failure
        (funcall on-unavailable :read-failed)
        (funcall on-value processor-count octets))))

#+darwin
(defun %darwin-load-average-values ()
  (let ((values nil))
    (handler-case
        (ntop.platform.darwin:mach-read-load-average
         (lambda (averages) (setf values averages))
         (lambda (reason) (declare (ignore reason))))
      (error () (setf values nil)))
    (loop for index below 3 collect (nth index values))))

#+darwin
(defun read-platform-cpu-source-sample (on-value on-unavailable)
  (%read-darwin-processor-cpu-load-info
   (lambda (processor-count octets)
     (multiple-value-bind (sample failure)
         (handler-case
             (values
              (%make-darwin-cpu-source-sample
               (decode-darwin-processor-cpu-load-info octets processor-count)
               (%darwin-load-average-values)
               (function
                ntop.platform.darwin::read-darwin-cpu-performance-state-data))
              nil)
           (error (condition) (values nil condition)))
       (if failure
           (funcall on-unavailable :read-failed)
           (funcall on-value sample))))
   (lambda (reason)
     (declare (ignore reason))
     (funcall on-unavailable :read-failed))))
