(in-package #:ntop.disk.domain)

(defparameter +disk-supported-filesystem-types+
  '("ext2" "ext3"
           "ext4"
           "msdos"
           "vfat"
           "ntfs3"
           "ntfs"
           "zfs"
           "hfs"
           "reiserfs"
           "reiser4"
           "exfat"
           "f2fs"
           "hfsplus"
           "jfs"
           "btrfs"
           "bcachefs"
           "minix"
           "nilfs"
           "nilfs2"
           "xfs"
           "apfs"
           "fuseblk"))

(defun disk-filesystem-type-supported-p (filesystem-type)
  (and (stringp filesystem-type)
       (not
        (null
         (member filesystem-type
                 +disk-supported-filesystem-types+
                 :test
                 #'string-equal)))))

(defun %ascii-digit-p (character)
  (and (char<= #\0 character) (char<= character #\9)))

(defun %disk-name-digit-end (device-name start)
  (loop with
        end = start
        while (and (< end (length device-name))
                   (%ascii-digit-p (char device-name end)))
        do (incf end)
        finally (return end)))

(defun disk-io-key-from-device-name (device-name)
  (when (stringp device-name)
    (loop for start below (max 0 (- (length device-name) 3))
          when (and (<= (+ start 4) (length device-name))
                    (string= "disk" device-name :start2 start :end2 (+ start 4)))
            do (let* ((digits-start (+ start 4))
                      (end (%disk-name-digit-end device-name digits-start)))
                 (when (< digits-start end)
                   (return (subseq device-name start end)))))))

(defun %disk-space-total (used-bytes free-bytes)
  (when
      (and (typep used-bytes 'ntop.shared.domain:measurement-counter)
           (typep free-bytes 'ntop.shared.domain:measurement-counter))
    (let ((total (+ used-bytes free-bytes)))
      (when (plusp total)
        total))))

(defun disk-used-percent (used-bytes free-bytes)
  (let ((total (%disk-space-total used-bytes free-bytes)))
    (when total
      (* 100 (/ used-bytes total)))))

(defun disk-free-percent (used-bytes free-bytes)
  (let ((total (%disk-space-total used-bytes free-bytes)))
    (when total
      (* 100 (/ free-bytes total)))))

(defun %disk-sample-device-for-key (sample io-key)
  (when (and sample io-key)
    (find io-key
          (disk-sample-devices sample)
          :key
          #'disk-device-device-name
          :test
          #'equal)))

(defun %disk-device-counter-rate (previous-sample current-sample
                                                  io-key
                                                  counter-accessor)
  (let* ((current-device (%disk-sample-device-for-key current-sample io-key))
         (current-counter
          (when current-device
            (funcall counter-accessor current-device))))
    (when current-counter
      (let* ((previous-device
              (%disk-sample-device-for-key previous-sample io-key))
             (previous-counter
              (when previous-device
                (funcall counter-accessor previous-device))))
        (if previous-counter
            (ntop.shared.domain:counter-rate-per-second
             previous-counter
             current-counter
             (-
              (ntop.shared.domain:sample-record-sampled-at
               current-sample)
              (ntop.shared.domain:sample-record-sampled-at
               previous-sample)))
            0)))))

(defun %disk-sample-row (previous-sample current-sample mount)
  (let ((io-key (disk-mount-io-key mount))
        (used-bytes (disk-mount-used-bytes mount))
        (free-bytes (disk-mount-free-bytes mount)))
    (list :disk
          (disk-mount-device-name mount)
          :mount
          (disk-mount-mount-path mount)
          :filesystem-type
          (disk-mount-filesystem-type mount)
          :used
          used-bytes
          :free
          free-bytes
          :total
          (disk-mount-total-bytes mount)
          :used-percent
          (disk-used-percent used-bytes free-bytes)
          :free-percent
          (disk-free-percent used-bytes free-bytes)
          :read-rate
          (%disk-device-counter-rate previous-sample
                                     current-sample
                                     io-key
                                     #'disk-device-read-bytes)
          :write-rate
          (%disk-device-counter-rate previous-sample
                                     current-sample
                                     io-key
                                     #'disk-device-write-bytes)
          :io-key
          io-key)))

(defun disk-sample-rows (previous-sample current-sample
                                         &key
                                         (sort-key :disk)
                                         (sort-order :ascending))
  (when current-sample
    (sort-disk-rows
     (loop for mount in (disk-sample-mounts current-sample)
           when (disk-filesystem-type-supported-p
                 (disk-mount-filesystem-type mount))
             collect (%disk-sample-row previous-sample current-sample mount))
     :sort-key
     sort-key
     :sort-order
     sort-order)))
