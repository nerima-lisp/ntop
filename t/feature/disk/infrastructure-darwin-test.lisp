(in-package #:ntop.test)

(defun %make-darwin-disk-test-statfs (device-name mount-path
                                                  filesystem-type
                                                  block-size
                                                  blocks
                                                  free-blocks
                                                  available-blocks)
  (list :device-name
        device-name
        :mount-path
        mount-path
        :filesystem-type
        filesystem-type
        :block-size
        block-size
        :blocks
        blocks
        :free-blocks
        free-blocks
        :available-blocks
        available-blocks))

(cl-weave:describe "Darwin disk availability"
                   (cl-weave:it
                    "reports no device with the shared not-present reason"
                    (let ((value :unset)
                          (reason nil))
                      (ntop.disk.infrastructure::%read-darwin-disk-values
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value nil))
                       (lambda (mount-path on-value on-unavailable)
                         (declare (ignore mount-path on-value on-unavailable))
                         (error "Unexpected volume-space read."))
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value nil))
                       (lambda (result)
                         (setf value result))
                       (lambda (unavailable)
                         (setf reason unavailable)))
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be :not-present))))

(cl-weave:describe "Darwin disk value collection"
                   (cl-weave:it
                    "builds supported mounts and devices from injected readers"
                    (let ((value nil)
                          (failure nil)
                          (value-count 0)
                          (failure-count 0)
                          (volume-paths nil))
                      (ntop.disk.infrastructure::%read-darwin-disk-values
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value
                                  (list
                                   (%make-darwin-disk-test-statfs "/dev/disk1s1"
                                                                  "/"
                                                                  "apfs"
                                                                  4096
                                                                  100
                                                                  40
                                                                  10)
                                   (%make-darwin-disk-test-statfs "/dev/disk2"
                                                                  "/data"
                                                                  "ext4"
                                                                  1024
                                                                  50
                                                                  20
                                                                  15)
                                   (%make-darwin-disk-test-statfs "tmpfs"
                                                                  "/run"
                                                                  "tmpfs"
                                                                  4096
                                                                  1
                                                                  0
                                                                  0))))
                       (lambda (mount-path on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (push mount-path volume-paths)
                         (funcall on-value 360000))
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value
                                  '((:device-name "disk1"
                                                  :read-bytes
                                                  123
                                                  :write-bytes
                                                  456))))
                       (lambda (result)
                         (incf value-count)
                         (setf value result))
                       (lambda (reason)
                         (incf failure-count)
                         (setf failure reason)))
                      (cl-weave:expect value-count :to-be 1)
                      (cl-weave:expect failure-count :to-be 0)
                      (cl-weave:expect failure :to-be nil)
                      (cl-weave:expect (nreverse volume-paths) :to-equal '("/"))
                      (let* ((mounts (getf value :mounts))
                             (apfs-mount (first mounts))
                             (ext-mount (second mounts))
                             (device (first (getf value :devices))))
                        (cl-weave:expect (length mounts) :to-be 2)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-used-bytes apfs-mount)
                         :to-be
                         360000)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-free-bytes apfs-mount)
                         :to-be
                         40960)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-total-bytes apfs-mount)
                         :to-be
                         409600)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-io-key apfs-mount)
                         :to-equal
                         "disk1")
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-used-bytes ext-mount)
                         :to-be
                         35840)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-free-bytes ext-mount)
                         :to-be
                         15360)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-device-read-bytes device)
                         :to-be
                         123)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-device-write-bytes device)
                         :to-be
                         456))))
                   (cl-weave:it
                    "keeps APFS capacity when volume-used and IOKit are unavailable"
                    (let ((value nil)
                          (failure-count 0))
                      (ntop.disk.infrastructure::%read-darwin-disk-values
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value
                                  (list
                                   (%make-darwin-disk-test-statfs "/dev/disk4s1"
                                                                  "/"
                                                                  "apfs"
                                                                  4096
                                                                  200
                                                                  80
                                                                  50))))
                       (lambda (mount-path on-value on-unavailable)
                         (declare (ignore mount-path on-value))
                         (funcall on-unavailable :volume-used-unavailable))
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-value))
                         (funcall on-unavailable :iokit-unavailable))
                       (lambda (result)
                         (setf value result))
                       (lambda (reason)
                         (declare (ignore reason))
                         (incf failure-count)))
                      (let ((mount (first (getf value :mounts))))
                        (cl-weave:expect failure-count :to-be 0)
                        (cl-weave:expect (getf value :devices) :to-be nil)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-used-bytes mount)
                         :to-be
                         nil)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-free-bytes mount)
                         :to-be
                         204800)
                        (cl-weave:expect
                         (ntop.disk.domain:disk-mount-total-bytes mount)
                         :to-be
                         819200))))
                   (cl-weave:it
                    "propagates file-system failures without reading other sources"
                    (let ((value-count 0)
                          (failure nil)
                          (other-reader-count 0))
                      (ntop.disk.infrastructure::%read-darwin-disk-values
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-value))
                         (funcall on-unavailable :file-system-unavailable))
                       (lambda (mount-path on-value on-unavailable)
                         (declare (ignore mount-path on-value on-unavailable))
                         (incf other-reader-count))
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-value on-unavailable))
                         (incf other-reader-count))
                       (lambda (value)
                         (declare (ignore value))
                         (incf value-count))
                       (lambda (reason)
                         (setf failure reason)))
                      (cl-weave:expect value-count :to-be 0)
                      (cl-weave:expect other-reader-count :to-be 0)
                      (cl-weave:expect failure :to-be :read-failed)))
                   (cl-weave:it-run-if
                    (member :darwin *features*)
                    "skips block-storage services without BSD names"
                    (let
                     ((value nil) (failure nil)
                      (include-named-p t)
                      (properties (make-hash-table :test #'equal))
                      (statistics (make-hash-table :test #'equal)))
                     (setf (gethash "Statistics" properties) statistics
                           (gethash "Bytes (Read)" statistics) 4096
                           (gethash "Bytes (Write)" statistics) 8192)
                     (cl-weave:with-mocked-functions
                      (((symbol-function 'ntop.platform.darwin::%cf-string-create-with-c-string)
                        (lambda (allocator value encoding)
                         (declare (ignore allocator value encoding))
                         (sb-sys:int-sap 1)))
                       ((symbol-function
                         'ntop.platform.darwin::%io-registry-entry-search-cf-property)
                        (lambda (service plane key allocator options)
                         (declare (ignore service plane key allocator options))
                         (sb-sys:int-sap 0)))
                       ((symbol-function 'ntop.platform.darwin::%cf-release)
                        (lambda (reference)
                         (declare (ignore reference))
                         nil)))
                      (cl-weave:expect (ntop.platform.darwin::%darwin-iokit-device-name 17)
                                       :to-be-null))
                     (cl-weave:with-mocked-functions
                      (((symbol-function 'ntop.platform.darwin::call-with-iokit-services)
                        (lambda (service-class on-service on-complete on-error)
                         (declare (ignore service-class on-error))
                         (funcall on-service :unnamed)
                         (when include-named-p
                          (funcall on-service :named))
                         (funcall on-complete)))
                       ((symbol-function 'ntop.platform.darwin::%darwin-iokit-device-name)
                        (lambda (service)
                         (when (eq service :named)
                          "disk7")))
                       ((symbol-function 'ntop.platform.darwin::iokit-registry-properties)
                        (lambda (service on-value on-error)
                         (declare (ignore service on-error))
                         (funcall on-value properties))))
                      (ntop.platform.darwin:read-darwin-block-storage-stats
                       (lambda (devices)
                        (setf value devices))
                       (lambda (reason)
                        (setf failure reason)))
                      (cl-weave:expect value
                                       :to-equal
                                       '((:device-name "disk7" :read-bytes 4096 :write-bytes 8192)))
                      (cl-weave:expect failure :to-be-null)
                      (setf value nil
                            failure nil
                            include-named-p nil)
                      (ntop.platform.darwin:read-darwin-block-storage-stats
                       (lambda (devices)
                        (setf value devices))
                       (lambda (reason)
                        (setf failure reason)))
                      (cl-weave:expect value :to-be-null)
                      (cl-weave:expect failure :to-be :no-block-storage-statistics)))))


(cl-weave:describe "Darwin disk reader entry points"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "dispatches the collector through the Darwin reader adapters"
                                       (let ((value nil)
                                             (reason :unset)
                                             (volume-read-count 0))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin:read-darwin-file-system-stats)
                                            (lambda (on-value on-unavailable)
                                              (declare (ignore on-unavailable))
                                              (funcall on-value
                                                       (list
                                                        (%make-darwin-disk-test-statfs
                                                         "/dev/disk1s1"
                                                         "/"
                                                         "apfs"
                                                         4096
                                                         100
                                                         40
                                                         10)))))
                                           ((symbol-function
                                             'ntop.platform.darwin:read-darwin-volume-space-used)
                                            (lambda
                                                (mount-path on-value
                                                            on-unavailable)
                                              (declare (ignore mount-path
                                                               on-unavailable))
                                              (incf volume-read-count)
                                              (funcall on-value 300000)))
                                           ((symbol-function
                                             'ntop.platform.darwin:read-darwin-block-storage-stats)
                                            (lambda (on-value on-unavailable)
                                              (declare (ignore on-unavailable))
                                              (funcall on-value
                                                       '((:device-name "disk1"
                                                                       :read-bytes
                                                                       12
                                                                       :write-bytes
                                                                       34))))))
                                          (ntop.disk.infrastructure::read-disk-values
                                           (ntop.disk.infrastructure:make-disk-collector
                                            nil)
                                           (lambda (result)
                                             (setf value result))
                                           (lambda (unavailable)
                                             (setf reason unavailable))))
                                         (cl-weave:expect reason :to-be :unset)
                                         (cl-weave:expect volume-read-count
                                                          :to-be
                                                          1)
                                         (cl-weave:expect
                                          (ntop.disk.domain:disk-mount-used-bytes
                                           (first (getf value :mounts)))
                                          :to-be
                                          300000)
                                         (cl-weave:expect
                                          (ntop.disk.domain:disk-device-read-bytes
                                           (first (getf value :devices)))
                                          :to-be
                                          12))))

(cl-weave:describe "Darwin disk reader error classification"
                   (cl-weave:it-each
                    ((:no-block-storage-statistics :not-present)
                     (:permission-denied :permission-denied))
                    "classifies an empty inventory with ~A as ~A"
                    (reader-reason expected-reason)
                    (let ((value :unset)
                          (reason nil))
                      (ntop.disk.infrastructure::%read-darwin-disk-values
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-unavailable))
                         (funcall on-value nil))
                       (lambda (mount-path on-value on-unavailable)
                         (declare (ignore mount-path on-value on-unavailable))
                         (error "Unexpected volume-space read."))
                       (lambda (on-value on-unavailable)
                         (declare (ignore on-value))
                         (funcall on-unavailable reader-reason))
                       (lambda (result)
                         (setf value result))
                       (lambda (unavailable)
                         (setf reason unavailable)))
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be expected-reason))))
