(in-package #:ntop.test)

(defstruct memory-test-clock seconds)

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock memory-test-clock) on-time on-unavailable)
  (declare (ignore on-unavailable))
  (funcall on-time (memory-test-clock-seconds clock)))

(defun %memory-linux-fixture-root (fixture)
  (asdf:system-relative-pathname
   "ntop"
   (format nil "t/feature/memory/fixtures/linux/~A/" fixture)))

(defun %capture-linux-memory-sample
    (fixture &key (enable-cache-memory-p nil) (sampled-at 12.5))
  (let ((result nil)
        (collector
          (ntop.memory.infrastructure::make-memory-collector
           :clock (make-memory-test-clock :seconds sampled-at)
           :enable-cache-memory-p enable-cache-memory-p)))
    (ntop.platform.linux:with-linux-root
     (%memory-linux-fixture-root fixture)
     (ntop.memory.infrastructure::%collect-linux-memory-sample
      collector sampled-at
      (lambda (sample) (setf result (list :sample sample)))
      (lambda (reason) (setf result (list :unavailable reason)))))
    result))

(defun %memory-sample-values (result)
  (let ((sample (second result)))
    (list (ntop.shared.domain:sample-record-sampled-at sample)
          (ntop.memory.domain:memory-sample-ram-used-bytes sample)
          (ntop.memory.domain:memory-sample-ram-total-bytes sample)
          (ntop.memory.domain:memory-sample-swap-used-bytes sample)
          (ntop.memory.domain:memory-sample-swap-total-bytes sample)
          (ntop.memory.domain:memory-sample-cache-used-bytes sample)
          (ntop.memory.domain:memory-sample-cache-total-bytes sample))))

(defun %make-memory-test-octets (size fields)
  (let ((octets (make-array size
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
    (dolist (field fields octets)
      (destructuring-bind (offset width value) field
        (dotimes (index width)
          (setf (aref octets (+ offset index))
                (ldb (byte 8 (* index 8)) value)))))))

(defun %darwin-memory-statistics-fixture ()
  (%make-memory-test-octets
   152
   '((0 4 90) (4 4 40) (8 4 80) (12 4 30)
     (88 4 20) (92 4 10) (128 4 15) (140 4 500))))

(defun %darwin-swapusage-fixture ()
  (%make-memory-test-octets 32 '((0 8 8192) (8 8 3072))))

(cl-weave:describe
 "Memory collector"
 (cl-weave:it "requires and stores a clock and cache setting"
              (let ((collector
                      (ntop.memory.infrastructure::make-memory-collector
                       :clock (make-memory-test-clock :seconds 3.5)
                       :enable-cache-memory-p t)))
                (cl-weave:expect
                 (ntop.memory.infrastructure::memory-collector-enable-cache-memory-p
                  collector)
                 :to-be
                 t)))
 (cl-weave:it-each
  ((:not-present :not-present)
   (:disabled :disabled)
   (:permission-denied :permission-denied)
   (:unsupported-platform :unsupported-platform)
   (:private-api-unavailable :private-api-unavailable)
   (:read-failed :read-failed)
   (:invalid-data :read-failed))
  "passes the shared unavailable reason ~A with one callback argument"
  (reason expected)
  (let ((calls nil))
    (funcall
     (ntop.memory.infrastructure::%memory-unavailable-callback
      (lambda (&rest arguments) (push arguments calls)))
     reason)
    (cl-weave:expect (nreverse calls) :to-equal (list (list expected)))))
 (cl-weave:it-each
  (("complete-v2" t
    (12.5 196608 524288 196608 262144 393216 1048576))
   ("complete-v1" t
    (12.5 655360 2097152 393216 524288 786432 2097152))
   ("no-cgroup" t
    (12.5 524288 1048576 196608 262144 393216 1048576)))
  "collects RAM, swap, and cache from the ~A fixture"
  (fixture enable-cache-memory-p expected)
  (let ((result (%capture-linux-memory-sample
                 fixture :enable-cache-memory-p enable-cache-memory-p)))
    (cl-weave:expect (first result) :to-be :sample)
    (cl-weave:expect (%memory-sample-values result) :to-equal expected)))
 (cl-weave:it "leaves cache values unavailable when cache collection is disabled"
              (let ((result (%capture-linux-memory-sample "no-cgroup")))
                (cl-weave:expect (first result) :to-be :sample)
                (cl-weave:expect (sixth (%memory-sample-values result))
                                 :to-be nil)
                (cl-weave:expect (seventh (%memory-sample-values result))
                                 :to-be nil)))
 (cl-weave:it "reports malformed proc meminfo through the unavailable callback"
              (cl-weave:expect (%capture-linux-memory-sample "malformed")
                               :to-equal '(:unavailable :invalid-data)))
 (cl-weave:it "uses current cgroup usage when inactive files do not reduce it"
              (let* ((meminfo
                       '(("MemTotal" 1024 "kB") ("MemAvailable" 256 "kB")
                         ("MemFree" 128 "kB") ("SwapTotal" 64 "kB")
                         ("SwapFree" 32 "kB")))
                     (missing-inactive
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 meminfo
                        '(:version 2 :memory-usage 400000 :memory-limit 2097152
                          :memory-stat nil)
                        nil))
                     (large-inactive
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 meminfo
                        '(:version 2 :memory-usage 400000 :memory-limit nil
                          :memory-stat (("inactive_file" . 500000)))
                        nil))
                     (invalid-usage
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 meminfo
                        '(:version 2 :memory-usage nil :memory-limit 0
                          :memory-stat nil)
                        nil)))
                (cl-weave:expect
                 (list (%memory-sample-values (list :sample missing-inactive))
                       (%memory-sample-values (list :sample large-inactive))
                       (%memory-sample-values (list :sample invalid-usage)))
                 :to-equal
                 '((1 400000 1048576 32768 65536 nil nil)
                   (1 400000 1048576 32768 65536 nil nil)
                   (1 786432 1048576 32768 65536 nil nil)))))
 (cl-weave:it "rejects invalid proc counters and leaves missing values unavailable"
              (let* ((invalid
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 '(("MemTotal" 1024 "kB") ("MemAvailable" 2048 "kB")
                            ("MemFree" 3072 "kB") ("SwapTotal" 64 "kB")
                            ("SwapFree" 128 "kB"))
                        nil t))
                     (missing
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 '(("MemTotal" 1024 "kB") ("SwapTotal" 64 "kB"))
                        nil t))
                     (bad-units
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 '(("MemTotal" "1024" "kB")
                            ("MemAvailable" 256 "KB")
                            ("SwapTotal" 64 "kB") ("SwapFree" 32 "kB"))
                        nil nil)))
                (cl-weave:expect
                 (list (ntop.memory.domain:memory-sample-ram-used-bytes invalid)
                       (ntop.memory.domain:memory-sample-swap-used-bytes invalid)
                       (ntop.memory.domain:memory-sample-cache-used-bytes invalid)
                       (ntop.memory.domain:memory-sample-cache-used-bytes missing)
                       (ntop.memory.domain:memory-sample-swap-used-bytes missing)
                       (ntop.memory.domain:memory-sample-ram-total-bytes bad-units)
                       (ntop.memory.domain:memory-sample-swap-used-bytes bad-units)
                       (ntop.memory.infrastructure::%linux-memory-sample-from-data
                        1 nil nil nil))
                 :to-equal
                 '(nil nil nil nil nil nil 32768 nil)))))

(cl-weave:describe
 "Darwin memory byte layouts"
 (cl-weave:it "decodes the selected HOST_VM_INFO64 page counts"
              (cl-weave:expect
               (ntop.memory.infrastructure::decode-darwin-vm-statistics64
                (%darwin-memory-statistics-fixture))
               :to-equal
               '(:free-count 90 :active-count 40 :inactive-count 80
                 :wire-count 30 :purgeable-count 20 :speculative-count 10
                 :compressor-page-count 15 :internal-page-count 500)))
 (cl-weave:it "saturates speculative pages above the free count"
              (cl-weave:expect
               (ntop.memory.infrastructure::%darwin-cache-used-bytes
                '(:active-count 40 :inactive-count 80 :free-count 5
                  :speculative-count 10)
                4096)
               :to-be 512000))
 (cl-weave:it "rejects invalid Darwin byte buffers"
              (cl-weave:expect
               (ntop.memory.infrastructure::decode-darwin-vm-statistics64
                (make-array 151 :element-type '(unsigned-byte 8)))
               :to-be nil)
              (cl-weave:expect
               (ntop.memory.infrastructure::decode-darwin-vm-statistics64
                (make-array 152 :initial-element 0))
               :to-be nil)
              (cl-weave:expect
               (ntop.memory.infrastructure::decode-darwin-swapusage
                (make-array 31 :element-type '(unsigned-byte 8)))
               :to-be nil))
 (cl-weave:it "decodes swap total and available byte counters"
              (cl-weave:expect
               (ntop.memory.infrastructure::decode-darwin-swapusage
                (%darwin-swapusage-fixture))
               :to-equal '(:total-bytes 8192 :available-bytes 3072)))
 (cl-weave:it "decodes Darwin sysctl integers and rejects other byte widths"
              (cl-weave:expect
               (ntop.memory.infrastructure::%decode-darwin-sysctl-integer
                (%make-memory-test-octets 4 '((0 4 #x12345678))))
               :to-be #x12345678)
              (cl-weave:expect
               (ntop.memory.infrastructure::%decode-darwin-sysctl-integer
                (%make-memory-test-octets 8 '((0 8 #x0102030405060708))))
               :to-be #x0102030405060708)
              (cl-weave:expect
               (ntop.memory.infrastructure::%decode-darwin-sysctl-integer
                (make-array 3 :element-type '(unsigned-byte 8)))
               :to-be nil)
              (cl-weave:expect
               (ntop.memory.infrastructure::%decode-darwin-sysctl-integer
                (make-array 4 :initial-element 0))
               :to-be nil))
 (cl-weave:it "builds Darwin memory values with the shared domain formulas"
              (let ((sample
                      (ntop.memory.infrastructure::%darwin-memory-sample-from-data
                       3.5 (%darwin-memory-statistics-fixture) 4096 8388608
                       (%darwin-swapusage-fixture) t)))
                (cl-weave:expect
                 (list (ntop.memory.domain:memory-sample-ram-used-bytes sample)
                       (ntop.memory.domain:memory-sample-swap-used-bytes sample)
                       (ntop.memory.domain:memory-sample-cache-used-bytes sample)
                       (ntop.memory.domain:memory-sample-cache-total-bytes sample))
                 :to-equal '(2150400 5120 532480 8388608))))
 (cl-weave:it "rejects invalid Darwin counters and swap bounds"
              (labels ((sample (statistics page-size memory-total swapusage cache-p)
                         (ntop.memory.infrastructure::%darwin-memory-sample-from-data
                          3.5 statistics page-size memory-total swapusage cache-p)))
                (let* ((statistics (%darwin-memory-statistics-fixture))
                       (swapusage (%darwin-swapusage-fixture))
                       (over-available
                         (%make-memory-test-octets 32 '((0 8 8192) (8 8 8193))))
                       (equal-available
                         (%make-memory-test-octets 32 '((0 8 8192) (8 8 8192))))
                       (bounded (sample statistics 4096 8388608 equal-available nil)))
                  (cl-weave:expect
                   (list (sample statistics 0 8388608 swapusage t)
                         (sample statistics -1 8388608 swapusage t)
                         (sample statistics "4096" 8388608 swapusage t)
                         (sample statistics 4096 nil swapusage t)
                         (sample statistics 4096 -1 swapusage t)
                         (sample (make-array 151 :element-type '(unsigned-byte 8))
                                 4096 8388608 swapusage t)
                         (sample statistics 4096 8388608
                                 (make-array 31 :element-type '(unsigned-byte 8)) t)
                         (sample statistics 4096 8388608 over-available t)
                         (list (ntop.memory.domain:memory-sample-swap-used-bytes bounded)
                               (ntop.memory.domain:memory-sample-cache-used-bytes bounded)
                               (ntop.memory.domain:memory-sample-cache-total-bytes bounded)))
                   :to-equal '(nil nil nil nil nil nil nil nil (0 nil nil)))))))

#+linux
(cl-weave:describe
 "Memory collector port"
 (cl-weave:it "stamps a sample with the shared monotonic clock value"
              (let ((result nil))
                (ntop.platform.linux:with-linux-root
                 (%memory-linux-fixture-root "no-cgroup")
                 (ntop.shared.application:collect-memory-sample
                  (ntop.memory.infrastructure::make-memory-collector
                   :clock (make-memory-test-clock :seconds 42.25))
                  (lambda (sample) (setf result (list :sample sample)))
                  (lambda (reason) (setf result (list :unavailable reason)))))
                (cl-weave:expect
                 (ntop.shared.domain:sample-record-sampled-at (second result))
                 :to-be
                 42.25)))
 (cl-weave:it "reports malformed proc data as a read failure"
              (let ((result nil))
                (ntop.platform.linux:with-linux-root
                 (%memory-linux-fixture-root "malformed")
                 (ntop.shared.application:collect-memory-sample
                  (ntop.memory.infrastructure::make-memory-collector
                   :clock (make-memory-test-clock :seconds 42.25))
                  (lambda (sample) (setf result (list :sample sample)))
                  (lambda (reason) (setf result (list :unavailable reason)))))
                (cl-weave:expect result :to-equal '(:unavailable :read-failed)))))
