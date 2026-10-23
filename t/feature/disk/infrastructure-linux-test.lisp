(in-package #:ntop.test)

(defun capture-linux-disk-values ()
  (let ((result nil)
        (reason nil))
    (ntop.disk.infrastructure::read-linux-disk-values
     (lambda (values)
       (setf result values))
     (lambda (unavailable)
       (setf reason unavailable)))
    (values result reason)))

(cl-weave:describe "Linux disk availability"
                   (cl-weave:it
                    "reports no device with the shared not-present reason"
                    (let ((result nil)
                          (reason nil))
                      (cl-weave:with-mocked-functions
                       (((symbol-function 'ntop.platform.linux:read-proc-mounts)
                         (lambda (on-value on-unavailable)
                           (declare (ignore on-unavailable))
                           (funcall on-value nil)))
                        ((symbol-function
                          'ntop.platform.linux:read-proc-diskstats)
                         (lambda (on-value on-unavailable)
                           (declare (ignore on-unavailable))
                           (funcall on-value nil))))
                       (ntop.disk.infrastructure::read-linux-disk-values
                        (lambda (values)
                          (setf result values))
                        (lambda (unavailable)
                          (setf reason unavailable))))
                      (cl-weave:expect result :to-be-null)
                      (cl-weave:expect reason :to-be :not-present))))

(cl-weave:describe "Linux disk reader error classification"
                   (cl-weave:it
                    "preserves diskstats failure when both inventories are empty"
                    (let ((value :unset)
                          (reason nil))
                      (ntop.platform.linux:with-linux-root
                       #p"t/feature/disk/fixtures/linux/mixed/"
                       (cl-weave:with-mocked-functions
                        (((symbol-function
                           'ntop.platform.linux:read-proc-mounts)
                          (lambda (on-value on-unavailable)
                            (declare (ignore on-unavailable))
                            (funcall on-value nil)))
                         ((symbol-function
                           'ntop.platform.linux:read-proc-diskstats)
                          (lambda (on-value on-unavailable)
                            (declare (ignore on-value))
                            (funcall on-unavailable :permission-denied))))
                        (ntop.disk.infrastructure::read-linux-disk-values
                         (lambda (result)
                           (setf value result))
                         (lambda (unavailable)
                           (setf reason unavailable))))
                       (cl-weave:expect value :to-be :unset)
                       (cl-weave:expect reason :to-be :permission-denied)))))

(cl-weave:describe "Linux disk values"
                   (cl-weave:it-each
                    (("mixed mount rows"
                      #p"t/feature/disk/fixtures/linux/mixed/"
                      2
                      2
                      nil
                      2
                      "/dev/sda\\p1"
                      "sda\\p1"
                      81920
                      12288
                      69632
                      2560
                      5120
                      "mnt/fail")
                     ("missing diskstats"
                      #p"t/feature/disk/fixtures/linux/missing-diskstats/"
                      1
                      0
                      nil
                      1
                      "/dev/loop0"
                      "loop0"
                      81920
                      12288
                      69632
                      nil
                      nil
                      nil)
                     ("unsupported filesystem"
                      #p"t/feature/disk/fixtures/linux/unsupported/"
                      0
                      1
                      nil
                      0
                      nil
                      "sda1"
                      nil
                      nil
                      nil
                      2560
                      5120
                      nil)
                     ("malformed mount row"
                      #p"t/feature/disk/fixtures/linux/malformed/"
                      0
                      1
                      nil
                      0
                      nil
                      "sda1"
                      nil
                      nil
                      nil
                      2560
                      5120
                      nil)
                     ("missing mounts"
                      #p"t/feature/disk/fixtures/linux/missing-mounts/"
                      0
                      0
                      :read-failed
                      0
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil
                      nil))
                    "assembles the ~A fixture"
                    (case-id fixture
                             expected-mount-count
                             expected-device-count
                             expected-reason
                             expected-statvfs-count
                             expected-mount-device
                             expected-io-key
                             expected-total
                             expected-free
                             expected-used
                             expected-read
                             expected-write
                             failed-path)
                    (let* ((statvfs-paths nil)
                           (ntop.platform.linux::*linux-statvfs-function*
                            (lambda (pathname)
                              (let ((path (namestring pathname)))
                                (push path statvfs-paths)
                                (unless
                                    (and failed-path (search failed-path path))
                                  '(:block-size 4096
                                                :fragment-size
                                                4096
                                                :blocks
                                                20
                                                :free-blocks
                                                5
                                                :available-blocks
                                                3
                                                :files
                                                1
                                                :free-files
                                                0
                                                :available-files
                                                0
                                                :name-max
                                                255))))))
                      (ntop.platform.linux:with-linux-root fixture
                                                           (multiple-value-bind (result
                                                                                 reason)
                                                               (capture-linux-disk-values)
                                                             (cl-weave:expect
                                                              reason
                                                              :to-be
                                                              expected-reason)
                                                             (cl-weave:expect
                                                              (length
                                                               statvfs-paths)
                                                              :to-be
                                                              expected-statvfs-count)
                                                             (if expected-reason
                                                                 (cl-weave:expect
                                                                  result
                                                                  :to-be
                                                                  nil)
                                                                 (let ((mounts
                                                                        (getf
                                                                         result
                                                                         :mounts))
                                                                       (devices
                                                                        (getf
                                                                         result
                                                                         :devices)))
                                                                   (cl-weave:expect
                                                                    (length
                                                                     mounts)
                                                                    :to-be
                                                                    expected-mount-count)
                                                                   (cl-weave:expect
                                                                    (length
                                                                     devices)
                                                                    :to-be
                                                                    expected-device-count)
                                                                   (when
                                                                       expected-mount-device
                                                                     (let ((mount
                                                                            (first
                                                                             mounts)))
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-mount-device-name
                                                                         mount)
                                                                        :to-equal
                                                                        expected-mount-device)
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-mount-filesystem-type
                                                                         mount)
                                                                        :to-equal
                                                                        "ext4")
                                                                       (progn
                                                                         (cl-weave:expect
                                                                          (ntop.disk.domain:disk-mount-io-key
                                                                           mount)
                                                                          :to-equal
                                                                          expected-io-key)
                                                                         (when
                                                                             failed-path
                                                                           (cl-weave:expect
                                                                            (position
                                                                             #\\
                                                                             (ntop.disk.domain:disk-mount-io-key
                                                                              mount))
                                                                            :to-be
                                                                            3)))
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-mount-total-bytes
                                                                         mount)
                                                                        :to-be
                                                                        expected-total)
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-mount-free-bytes
                                                                         mount)
                                                                        :to-be
                                                                        expected-free)
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-mount-used-bytes
                                                                         mount)
                                                                        :to-be
                                                                        expected-used))
                                                                     (when
                                                                         failed-path
                                                                       (let ((mount
                                                                              (find
                                                                               "/mnt/fail"
                                                                               mounts
                                                                               :key
                                                                               #'ntop.disk.domain:disk-mount-mount-path
                                                                               :test
                                                                               #'string=)))
                                                                         (cl-weave:expect
                                                                          mount
                                                                          :to-be-truthy)
                                                                         (when
                                                                             mount
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-device-name
                                                                             mount)
                                                                            :to-equal
                                                                            "/dev/sdb1")
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-mount-path
                                                                             mount)
                                                                            :to-equal
                                                                            "/mnt/fail")
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-filesystem-type
                                                                             mount)
                                                                            :to-equal
                                                                            "ext4")
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-io-key
                                                                             mount)
                                                                            :to-equal
                                                                            "sdb1")
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-total-bytes
                                                                             mount)
                                                                            :to-be
                                                                            nil)
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-free-bytes
                                                                             mount)
                                                                            :to-be
                                                                            nil)
                                                                           (cl-weave:expect
                                                                            (ntop.disk.domain:disk-mount-used-bytes
                                                                             mount)
                                                                            :to-be
                                                                            nil)))))
                                                                   (when
                                                                       expected-read
                                                                     (let ((device
                                                                            (first
                                                                             devices)))
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-device-device-name
                                                                         device)
                                                                        :to-equal
                                                                        expected-io-key)
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-device-read-bytes
                                                                         device)
                                                                        :to-be
                                                                        expected-read)
                                                                       (cl-weave:expect
                                                                        (ntop.disk.domain:disk-device-write-bytes
                                                                         device)
                                                                        :to-be
                                                                        expected-write))))))))))
