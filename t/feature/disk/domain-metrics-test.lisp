(in-package #:ntop.test)

(defun %f5-disk-mount (&key device mount filesystem used free total io-key)
  (ntop.disk.domain:make-disk-mount :device-name
                                    device
                                    :mount-path
                                    mount
                                    :filesystem-type
                                    filesystem
                                    :used-bytes
                                    used
                                    :free-bytes
                                    free
                                    :total-bytes
                                    total
                                    :io-key
                                    io-key))

(defun %f5-disk-device (&key name read write)
  (ntop.disk.domain:make-disk-device :device-name
                                     name
                                     :read-bytes
                                     read
                                     :write-bytes
                                     write))

(defun %f5-disk-sample (sampled-at &key mounts devices)
  (ntop.disk.domain:make-disk-sample :sampled-at
                                     sampled-at
                                     :mounts
                                     mounts
                                     :devices
                                     devices))

(cl-weave:describe "F5 disk metrics"
                   (cl-weave:it-each
                    ((:ext2 "ext2") (:ext3 "ext3")
                     (:ext4 "ext4")
                     (:msdos "msdos")
                     (:vfat "vfat")
                     (:ntfs3 "ntfs3")
                     (:ntfs "ntfs")
                     (:zfs "zfs")
                     (:hfs "hfs")
                     (:reiserfs "reiserfs")
                     (:reiser4 "reiser4")
                     (:exfat "exfat")
                     (:f2fs "f2fs")
                     (:hfsplus "hfsplus")
                     (:jfs "jfs")
                     (:btrfs "btrfs")
                     (:bcachefs "bcachefs")
                     (:minix "minix")
                     (:nilfs "nilfs")
                     (:nilfs2 "nilfs2")
                     (:xfs "xfs")
                     (:apfs "apfs")
                     (:fuseblk "fuseblk")
                     (:case-insensitive "NtFs3"))
                    "accepts supported filesystem type ~A"
                    (case-name filesystem-type)
                    (cl-weave:expect
                     (ntop.disk.domain:disk-filesystem-type-supported-p
                      filesystem-type)
                     :to-be
                     t))
                   (cl-weave:it-each
                    ((:proc "proc") (:tmpfs "tmpfs")
                     (:missing nil)
                     (:non-string :ext4))
                    "rejects unsupported filesystem type ~A"
                    (case-name filesystem-type)
                    (cl-weave:expect
                     (ntop.disk.domain:disk-filesystem-type-supported-p
                      filesystem-type)
                     :to-be
                     nil))
                   (cl-weave:it-each
                    ((:device-path "/dev/disk12s4" "disk12")
                     (:first-match "disk4-disk18" "disk4")
                     (:skip-unmatched "prefix-diskx-disk34s1" "disk34")
                     (:case-sensitive "DISK12s4" nil)
                     (:missing-digits "/dev/disk" nil)
                     (:other-device "/dev/nvme0n1" nil)
                     (:missing-device nil nil))
                    "extracts ASCII disk identifiers from ~A"
                    (case-name device-name expected)
                    (cl-weave:expect
                     (ntop.disk.domain:disk-io-key-from-device-name device-name)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((:mixed 3 1 75 25) (:no-used 0 10 0 100)
                     (:no-free 10 0 100 0)
                     (:fractional 1 2 100/3 200/3))
                    "computes used and free percentages for ~A bytes"
                    (case-name used free used-percent free-percent)
                    (cl-weave:expect
                     (list (ntop.disk.domain:disk-used-percent used free)
                           (ntop.disk.domain:disk-free-percent used free))
                     :to-equal
                     (list used-percent free-percent)))
                   (cl-weave:it-each
                    ((:missing-used nil 10) (:missing-free 10 nil)
                     (:empty-volume 0 0))
                    "returns unavailable percentages for ~A inputs"
                    (case-name used free)
                    (cl-weave:expect
                     (list (ntop.disk.domain:disk-used-percent used free)
                           (ntop.disk.domain:disk-free-percent used free))
                     :to-equal
                     '(nil nil)))
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "disk percentages follow the used/free byte ratio"
                    ((values
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 0 :max 1000000000)
                       (cl-weave:gen-integer :min 0 :max 1000000000))))
                    (destructuring-bind (used free) values
                      (let ((total (+ used free)))
                        (cl-weave:expect
                         (ntop.disk.domain:disk-used-percent used free)
                         :to-equal
                         (when (plusp total)
                           (* 100 (/ used total))))
                        (cl-weave:expect
                         (ntop.disk.domain:disk-free-percent used free)
                         :to-equal
                         (when (plusp total)
                           (* 100 (/ free total)))))))
                   (cl-weave:it
                    "builds supported mount rows with matched device rates"
                    (let* ((previous
                            (%f5-disk-sample 10
                                             :devices
                                             (list
                                              (%f5-disk-device :name
                                                               "disk1"
                                                               :read
                                                               1000
                                                               :write
                                                               2000)
                                              (%f5-disk-device :name
                                                               "disk2"
                                                               :read
                                                               100
                                                               :write
                                                               200))))
                           (current
                            (%f5-disk-sample 14
                                             :mounts
                                             (list
                                              (%f5-disk-mount :device
                                                              "/dev/disk1s1"
                                                              :mount
                                                              "/"
                                                              :filesystem
                                                              "EXT4"
                                                              :used
                                                              800
                                                              :free
                                                              200
                                                              :total
                                                              1100
                                                              :io-key
                                                              "disk1")
                                              (%f5-disk-mount :device
                                                              "/dev/disk2"
                                                              :mount
                                                              "/data"
                                                              :filesystem
                                                              "ntfs3"
                                                              :used
                                                              1
                                                              :free
                                                              3
                                                              :total
                                                              4
                                                              :io-key
                                                              "disk2")
                                              (%f5-disk-mount :device
                                                              "/dev/disk3s1"
                                                              :mount
                                                              "/first"
                                                              :filesystem
                                                              "exfat"
                                                              :free
                                                              5
                                                              :io-key
                                                              "disk3")
                                              (%f5-disk-mount :device
                                                              "/dev/disk4s1"
                                                              :mount
                                                              "/no-io"
                                                              :filesystem
                                                              "apfs")
                                              (%f5-disk-mount :device
                                                              "tmpfs"
                                                              :mount
                                                              "/run"
                                                              :filesystem
                                                              "tmpfs"))
                                             :devices
                                             (list
                                              (%f5-disk-device :name
                                                               "disk1"
                                                               :read
                                                               1800
                                                               :write
                                                               2600)
                                              (%f5-disk-device :name
                                                               "disk2"
                                                               :read
                                                               500)
                                              (%f5-disk-device :name
                                                               "disk3"
                                                               :read
                                                               5
                                                               :write
                                                               10))))
                           (rows
                            (ntop.disk.domain:disk-sample-rows previous current))
                           (rows-by-free
                            (ntop.disk.domain:disk-sample-rows previous
                                                               current
                                                               :sort-key
                                                               :free
                                                               :sort-order
                                                               :descending)))
                      (cl-weave:expect (length rows) :to-be 4)
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (getf row :disk))
                        rows)
                       :to-equal
                       '("/dev/disk1s1" "/dev/disk2"
                                        "/dev/disk3s1"
                                        "/dev/disk4s1"))
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (getf row :disk))
                        rows-by-free)
                       :to-equal
                       '("/dev/disk1s1" "/dev/disk3s1"
                                        "/dev/disk2"
                                        "/dev/disk4s1"))
                      (cl-weave:expect
                       (loop for key in '(:disk :mount
                                                :filesystem-type
                                                :used
                                                :free
                                                :total
                                                :used-percent
                                                :free-percent
                                                :read-rate
                                                :write-rate
                                                :io-key)
                             always (member key (first rows)))
                       :to-be
                       t)
                      (cl-weave:expect
                       (loop for key in '(:disk :mount
                                                :filesystem-type
                                                :used
                                                :free
                                                :total
                                                :used-percent
                                                :free-percent
                                                :read-rate
                                                :write-rate
                                                :io-key)
                             collect (getf (first rows) key))
                       :to-equal
                       '("/dev/disk1s1" "/"
                                        "EXT4"
                                        800
                                        200
                                        1100
                                        80
                                        20
                                        200
                                        150
                                        "disk1"))
                      (cl-weave:expect
                       (list (getf (second rows) :used-percent)
                             (getf (second rows) :free-percent)
                             (getf (second rows) :read-rate)
                             (getf (second rows) :write-rate))
                       :to-equal
                       '(25 75 100 nil))
                      (cl-weave:expect
                       (list (getf (third rows) :used-percent)
                             (getf (third rows) :free-percent)
                             (getf (third rows) :read-rate)
                             (getf (third rows) :write-rate))
                       :to-equal
                       '(nil nil 0 0))
                      (cl-weave:expect
                       (list (getf (fourth rows) :read-rate)
                             (getf (fourth rows) :write-rate)
                             (getf (fourth rows) :io-key))
                       :to-equal
                       '(nil nil nil))))
                   (cl-weave:it
                    "returns zero for reset counters and preserves missing current counters"
                    (let* ((previous
                            (%f5-disk-sample 20
                                             :devices
                                             (list
                                              (%f5-disk-device :name
                                                               "disk1"
                                                               :read
                                                               100
                                                               :write
                                                               100)
                                              (%f5-disk-device :name
                                                               "disk2"
                                                               :read
                                                               nil
                                                               :write
                                                               100))))
                           (current
                            (%f5-disk-sample 22
                                             :mounts
                                             (list
                                              (%f5-disk-mount :filesystem
                                                              "xfs"
                                                              :io-key
                                                              "disk1")
                                              (%f5-disk-mount :filesystem
                                                              "xfs"
                                                              :io-key
                                                              "disk2"))
                                             :devices
                                             (list
                                              (%f5-disk-device :name
                                                               "disk1"
                                                               :read
                                                               50
                                                               :write
                                                               160)
                                              (%f5-disk-device :name
                                                               "disk2"
                                                               :read
                                                               300
                                                               :write
                                                               nil))))
                           (rows
                            (ntop.disk.domain:disk-sample-rows previous current)))
                      (cl-weave:expect
                       (list (getf (first rows) :read-rate)
                             (getf (first rows) :write-rate)
                             (getf (second rows) :read-rate)
                             (getf (second rows) :write-rate))
                       :to-equal
                       '(0 30 0 nil)))))
