(in-package #:ntop.test)

(defun %darwin-cpu-fixture-u32 (value)
  (loop for shift from 0 below 32 by 8
        collect (ldb (byte 8 shift) value)))

(defun %darwin-cpu-fixture-bytes (core-counters)
  (coerce
   (loop for counters in core-counters
         append (loop for counter in counters
                      append (%darwin-cpu-fixture-u32 counter)))
   '(vector (unsigned-byte 8))))

(defun %darwin-core-frequency-table (core-index tables)
  (let ((key (cond ((<= 0 core-index 3) "voltage-states1-sram")
                   ((<= 4 core-index 9) "voltage-states5-sram")
                   ((<= 10 core-index 15) "voltage-states13-sram"))))
    (and key (cdr (assoc key tables :test #'string=)))))

(cl-weave:describe "Darwin CPU source layouts"
  (cl-weave:it "decodes synthetic host_processor_info counters"
    (let* ((octets (%darwin-cpu-fixture-bytes '((11 13 17 19)
                                                (23 29 31 37))))
           (cores
             (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info
              octets 2)))
      (cl-weave:expect
       (mapcar
        (lambda (core)
          (list (ntop.cpu.domain:cpu-core-value-core-index core)
                (ntop.cpu.domain:cpu-core-value-user-ticks core)
                (ntop.cpu.domain:cpu-core-value-system-ticks core)
                (ntop.cpu.domain:cpu-core-value-idle-ticks core)
                (ntop.cpu.domain:cpu-core-value-nice-ticks core)))
        cores)
       :to-equal '((0 11 13 17 19) (1 23 29 31 37)))))

  (cl-weave:it "decodes empty processor info and rejects invalid layouts"
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8)))
          (short (make-array 15 :element-type '(unsigned-byte 8)
                             :initial-element 0)))
      (cl-weave:expect
       (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info empty 0)
       :to-equal nil)
      (cl-weave:expect
       (lambda ()
         (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info
          "not octets" 0))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info empty -1))
       :to-throw 'error)
      (cl-weave:expect
       (lambda ()
         (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info short 1))
       :to-throw 'error)))

  (cl-weave:it "keeps CPU counters and load averages when IOReport is unavailable"
    (let* ((cores
             (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info
              (%darwin-cpu-fixture-bytes '((41 43 47 53))) 1))
           (sample
             (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
              cores '(1.25d0 0.75d0 0.5d0)
              (lambda (on-value on-unavailable)
                (declare (ignore on-value))
                (funcall on-unavailable :private-api-unavailable))))
           (core
             (first
              (ntop.cpu.infrastructure::cpu-source-sample-core-values sample))))
      (cl-weave:expect
       (list (ntop.cpu.domain:cpu-core-value-user-ticks core)
             (ntop.cpu.domain:cpu-core-value-system-ticks core)
             (ntop.cpu.domain:cpu-core-value-idle-ticks core)
             (ntop.cpu.domain:cpu-core-value-nice-ticks core)
             (ntop.cpu.domain:cpu-core-value-frequency-mhz core))
       :to-equal '(41 43 47 53 nil))
      (cl-weave:expect
       (list (ntop.cpu.infrastructure::cpu-source-sample-load-average-1 sample)
             (ntop.cpu.infrastructure::cpu-source-sample-load-average-5 sample)
             (ntop.cpu.infrastructure::cpu-source-sample-load-average-15 sample))
       :to-equal '(1.25d0 0.75d0 0.5d0))))

  (cl-weave:it "weights active IOReport states against their DVFS table"
    (let* ((cores
             (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info
              (%darwin-cpu-fixture-bytes '((41 43 47 53)
                                           (59 61 67 71))) 2))
           (residencies '( ("DOWN" . 90) ("IDLE" . 5)
                           ("E-core-state-0" . 25) ("E-core-state-1" . 75)))
           (sample
             (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
              cores '(1d0 2d0 3d0)
              (lambda (on-value on-unavailable)
                (declare (ignore on-unavailable))
                (funcall on-value
                         (list (list 0 residencies '(1000000 2000000))
                               (list 1 residencies nil))))))
           (sample-cores
             (ntop.cpu.infrastructure::cpu-source-sample-core-values sample)))
      (cl-weave:expect
       (mapcar #'ntop.cpu.domain:cpu-core-value-frequency-mhz sample-cores)
       :to-equal '(1750 nil))))

  (cl-weave:it-run-if (member :darwin *features*)
  "assembles fixed CPU samples within their DVFS table bounds"
  (let* ((core-counters
           (loop for index below 17
                 collect (list (+ index 1) (+ index 2)
                               (+ index 3) (+ index 4))))
         (octets (%darwin-cpu-fixture-bytes core-counters))
         (tables '(("voltage-states1-sram" . (1000000 2000000))
                   ("voltage-states5-sram" . (1500000 2500000))
                   ("voltage-states13-sram" . (2000000 3000000))))
         (residencies '(("DOWN" . 90) ("IDLE" . 5)
                        ("E-core-state-0" . 25)
                        ("E-core-state-1" . 75)))
         (performance-state-data
           (loop for core-index below 17
                 for table-key =
                   (cond ((<= 0 core-index 3) "voltage-states1-sram")
                         ((<= 4 core-index 9) "voltage-states5-sram")
                         ((<= 10 core-index 15) "voltage-states13-sram"))
                 collect
                 (list core-index residencies
                       (cdr (assoc table-key tables :test #'string=)))))
         (expected-frequencies
           (append (make-list 4 :initial-element 1750)
                   (make-list 6 :initial-element 2250)
                   (make-list 6 :initial-element 2750)
                   (list nil)))
         (sample nil)
         (failure nil))
    (cl-weave:with-mocked-functions
        (((symbol-function
           'ntop.cpu.infrastructure::%read-darwin-processor-cpu-load-info)
          (lambda (on-value on-unavailable)
            (declare (ignore on-unavailable))
            (funcall on-value (length core-counters) octets)))
         ((symbol-function
           'ntop.cpu.infrastructure::%darwin-load-average-values)
          (lambda () '(1d0 2d0 3d0)))
         ((symbol-function
           'ntop.platform.darwin::read-darwin-cpu-performance-state-data)
          (lambda (on-value on-unavailable)
            (declare (ignore on-unavailable))
            (funcall on-value performance-state-data))))
      (ntop.cpu.infrastructure::read-platform-cpu-source-sample
       (lambda (value) (setf sample value))
       (lambda (reason) (setf failure reason)))
      (let ((cores (and sample
                        (ntop.cpu.infrastructure::cpu-source-sample-core-values
                         sample))))
        (cl-weave:expect failure :to-be-null)
        (cl-weave:expect (and cores (= (length cores) 17)) :to-be t)
        (cl-weave:expect
         (mapcar
          (lambda (core)
            (list (ntop.cpu.domain:cpu-core-value-core-index core)
                  (ntop.cpu.domain:cpu-core-value-user-ticks core)
                  (ntop.cpu.domain:cpu-core-value-system-ticks core)
                  (ntop.cpu.domain:cpu-core-value-idle-ticks core)
                  (ntop.cpu.domain:cpu-core-value-nice-ticks core)
                  (ntop.cpu.domain:cpu-core-value-frequency-mhz core)))
          cores)
         :to-equal
         (loop for index below 17
               for counters in core-counters
               for frequency in expected-frequencies
               collect (append (list index) counters (list frequency))))
        (cl-weave:expect
         (list
          (ntop.cpu.infrastructure::cpu-source-sample-load-average-1 sample)
          (ntop.cpu.infrastructure::cpu-source-sample-load-average-5 sample)
          (ntop.cpu.infrastructure::cpu-source-sample-load-average-15 sample))
         :to-equal '(1d0 2d0 3d0))
        (cl-weave:expect
         (every
          (lambda (core)
            (let* ((index (ntop.cpu.domain:cpu-core-value-core-index core))
                   (frequency
                     (ntop.cpu.domain:cpu-core-value-frequency-mhz core))
                   (table (%darwin-core-frequency-table index tables)))
              (if (<= 0 index 15)
                  (and table
                       (realp frequency)
                       (<= (/ (apply #'min table) 1000d0)
                           frequency
                           (/ (apply #'max table) 1000d0)))
                  (null frequency))))
          cores)
         :to-be t))))))
(cl-weave:describe "Darwin CPU error paths"
  (cl-weave:it-run-if (member :darwin *features*)
    "reports processor info failures and releases returned memory"
    (let* ((octets (%darwin-cpu-fixture-bytes '((11 13 17 19))))
           (address 0)
           (processor-count 1)
           (processor-info-count 4)
           (status 0)
           (host-result :success)
           (deallocation-result :success)
           (deallocated nil)
           (value nil)
           (failure nil))
      (sb-sys:with-pinned-objects (octets)
        (setf address (sb-sys:sap-int (sb-sys:vector-sap octets)))
        (cl-weave:with-mocked-functions
          (((symbol-function 'ntop.platform.darwin:call-with-mach-host-port)
            (lambda (on-value on-error)
              (cond
                ((eq host-result :signal)
                 (error "Host port wrapper failed."))
                ((eq host-result :success)
                 (funcall on-value 41))
                (t
                 (funcall on-error host-result)))))
           ((symbol-function 'ntop.cpu.infrastructure::%host-processor-info)
            (lambda (host flavor processor-count-out processor-info-out
                         processor-info-count-out)
              (declare (ignore host flavor))
              (setf (sb-alien:deref processor-count-out) processor-count
                    (sb-alien:deref processor-info-out) address
                    (sb-alien:deref processor-info-count-out)
                    processor-info-count)
              status))
           ((symbol-function 'ntop.platform.darwin:mach-vm-deallocate)
            (lambda (memory size on-value on-error)
              (setf deallocated (list (sb-sys:sap-int memory) size))
              (case deallocation-result
                (:failure (funcall on-error :vm-failed))
                (:signal (error "VM deallocation wrapper failed."))
                (t (funcall on-value))))))
          (labels ((read-cpu ()
                     (setf value :unset failure :unset deallocated nil)
                     (ntop.cpu.infrastructure::%read-darwin-processor-cpu-load-info
                      (lambda (count bytes)
                        (setf value (list count (coerce bytes 'list))))
                      (lambda (reason)
                        (setf failure reason)))))
            (read-cpu)
            (cl-weave:expect value :to-equal (list 1 (coerce octets 'list)))
            (cl-weave:expect failure :to-be :unset)
            (cl-weave:expect deallocated :to-equal (list address 16))
            (setf status 7)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (cl-weave:expect deallocated :to-equal (list address 16))
            (setf status 0 processor-count 0 processor-info-count 0)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (cl-weave:expect deallocated :to-equal (list address 0))
            (setf processor-count 1 processor-info-count 3)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (setf processor-info-count 4 address 0)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (cl-weave:expect deallocated :to-be-null)
            (setf address (sb-sys:sap-int (sb-sys:vector-sap octets))
                  deallocation-result :failure)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (cl-weave:expect deallocated :to-equal (list address 16))
            (setf deallocation-result :signal)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (cl-weave:expect deallocated :to-equal (list address 16))
            (setf deallocation-result :success host-result :host-unavailable)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed)
            (setf host-result :signal)
            (read-cpu)
            (cl-weave:expect failure :to-be :read-failed))))))

  (cl-weave:it "keeps CPU and load data when performance-state data fails"
    (let* ((core
            (first
             (ntop.cpu.infrastructure::decode-darwin-processor-cpu-load-info
              (%darwin-cpu-fixture-bytes '((41 43 47 53))) 1)))
           (signal-sample
            (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
             (list core) '(1d0 2d0 3d0)
             (lambda (on-value on-unavailable)
               (declare (ignore on-value on-unavailable))
               (error "IOReport reader failed."))))
           (malformed-sample
            (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
             (list core) '(4d0 5d0 6d0)
             (lambda (on-value on-unavailable)
               (declare (ignore on-unavailable))
               (funcall on-value :malformed-data)))))
      (dolist (sample (list signal-sample malformed-sample))
        (cl-weave:expect
         (ntop.cpu.domain:cpu-core-value-frequency-mhz
          (first
           (ntop.cpu.infrastructure::cpu-source-sample-core-values sample)))
         :to-be-null))
      (cl-weave:expect
       (list
        (ntop.cpu.infrastructure::cpu-source-sample-load-average-1 malformed-sample)
        (ntop.cpu.infrastructure::cpu-source-sample-load-average-5 malformed-sample)
        (ntop.cpu.infrastructure::cpu-source-sample-load-average-15 malformed-sample))
       :to-equal '(4d0 5d0 6d0))))

  (cl-weave:it-run-if (member :darwin *features*)
  "converts load average and CPU read failures to fixed callbacks"
  (let ((load-mode :signal)
        (cpu-mode :malformed)
        (failure nil))
    (cl-weave:with-mocked-functions
      (((symbol-function 'ntop.platform.darwin:mach-read-load-average)
        (lambda (on-value on-unavailable)
          (case load-mode
              (:signal (error "getloadavg wrapper failed."))
              (:unavailable (funcall on-unavailable :no-load-data))
              (:partial (funcall on-value '(1d0)))
              (:success (funcall on-value '(1d0 2d0 3d0)))))))
      (cl-weave:expect
       (ntop.cpu.infrastructure::%darwin-load-average-values)
       :to-equal '(nil nil nil))
      (setf load-mode :unavailable)
      (cl-weave:expect
       (ntop.cpu.infrastructure::%darwin-load-average-values)
       :to-equal '(nil nil nil))
      (setf load-mode :partial)
      (progn
        (cl-weave:expect
         (ntop.cpu.infrastructure::%darwin-load-average-values)
         :to-equal '(1d0 nil nil))
        (setf load-mode :success)
        (cl-weave:expect
         (ntop.cpu.infrastructure::%darwin-load-average-values)
         :to-equal '(1d0 2d0 3d0))))
    (cl-weave:with-mocked-functions
      (((symbol-function
         'ntop.cpu.infrastructure::%read-darwin-processor-cpu-load-info)
        (lambda (on-value on-unavailable)
          (case cpu-mode
            (:unavailable (funcall on-unavailable :host-failure))
            (t (funcall on-value
                        1
                        (make-array 15
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))))))
      (labels ((read-cpu ()
                 (setf failure nil)
                 (ntop.cpu.infrastructure::read-platform-cpu-source-sample
                  (lambda (sample) (declare (ignore sample)))
                  (lambda (reason) (setf failure reason)))))
        (read-cpu)
        (cl-weave:expect failure :to-be :read-failed)
        (setf cpu-mode :unavailable)
        (read-cpu)
        (cl-weave:expect failure :to-be :read-failed)))));


  (cl-weave:it "keeps frequency unavailable without a DVFS table"
    (let* ((core (ntop.cpu.domain:make-cpu-core-value :core-index 16))
           (sample
             (ntop.cpu.infrastructure::%make-darwin-cpu-source-sample
              (list core) '(1d0 2d0 3d0)
              (lambda (on-value on-unavailable)
                (declare (ignore on-unavailable))
                (funcall on-value
                         '((16 (("P0" . 20)) nil)))))))
      (cl-weave:expect
       (ntop.cpu.domain:cpu-core-value-frequency-mhz
        (first
         (ntop.cpu.infrastructure::cpu-source-sample-core-values sample)))
       :to-be-null))))
