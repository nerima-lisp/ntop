(in-package #:ntop.disk.infrastructure)

(defun %darwin-disk-mount-from-statfs (statfs volume-used)
  (let* ((filesystem-type (getf statfs :filesystem-type))
         (device-name (getf statfs :device-name))
         (block-size (getf statfs :block-size))
         (free-bytes (* (getf statfs :available-blocks) block-size))
         (total-bytes (* (getf statfs :blocks) block-size))
         (used-bytes
          (if (string-equal filesystem-type "apfs")
              volume-used
              (- total-bytes free-bytes))))
    (ntop.disk.domain:make-disk-mount :device-name
                                      device-name
                                      :mount-path
                                      (getf statfs :mount-path)
                                      :filesystem-type
                                      filesystem-type
                                      :used-bytes
                                      used-bytes
                                      :free-bytes
                                      free-bytes
                                      :total-bytes
                                      total-bytes
                                      :io-key
                                      (ntop.disk.domain:disk-io-key-from-device-name
                                       device-name))))

(defun %collect-darwin-disk-mounts (file-systems read-volume-space-used
                                                 on-value)
  (labels ((read-next (remaining mounts)
             (if (null remaining)
                 (funcall on-value (nreverse mounts))
                 (let ((statfs (car remaining)))
                   (if (not
                        (ntop.disk.domain:disk-filesystem-type-supported-p
                         (getf statfs :filesystem-type)))
                       (read-next (cdr remaining) mounts)
                       (if (string-equal (getf statfs :filesystem-type) "apfs")
                           (funcall read-volume-space-used
                                    (getf statfs :mount-path)
                                    (lambda (used-bytes)
                                      (read-next (cdr remaining)
                                                 (cons
                                                  (%darwin-disk-mount-from-statfs
                                                   statfs
                                                   used-bytes)
                                                  mounts)))
                                    (lambda (reason)
                                      (declare (ignore reason))
                                      (read-next (cdr remaining)
                                                 (cons
                                                  (%darwin-disk-mount-from-statfs
                                                   statfs
                                                   nil)
                                                  mounts))))
                           (read-next (cdr remaining)
                                      (cons
                                       (%darwin-disk-mount-from-statfs statfs
                                                                       nil)
                                       mounts))))))))
    (read-next file-systems nil)))

(defun %make-darwin-disk-values (mounts device-rows)
  (list :mounts
        mounts
        :devices
        (mapcar
         (lambda (row)
           (apply #'ntop.disk.domain:make-disk-device row))
         device-rows)))

(defun %read-darwin-disk-values (read-file-system-stats read-volume-space-used
                                                        read-block-storage-stats
                                                        on-value
                                                        on-unavailable)
  (flet ((deliver-values (mounts device-rows)
           (let ((values (%make-darwin-disk-values mounts device-rows)))
             (if (and (null mounts) (null device-rows))
                 (funcall on-unavailable :not-present)
                 (funcall on-value values)))))
    (funcall read-file-system-stats
             (lambda (file-systems)
               (%collect-darwin-disk-mounts file-systems
                                            read-volume-space-used
                                            (lambda (mounts)
                                              (funcall read-block-storage-stats
                                                       (lambda (device-rows)
                                                         (deliver-values mounts
                                                                         device-rows))
                                                       (lambda (reason)
                                                         (if mounts
                                                             (deliver-values
                                                              mounts
                                                              nil)
                                                             (funcall
                                                              on-unavailable
                                                              (if (eq reason
                                                                      :no-block-storage-statistics)
                                                                  :not-present
                                                                  (ntop.disk.infrastructure::%disk-unavailable-reason
                                                                   reason)))))))))
             (lambda (reason)
               (funcall on-unavailable
                        (ntop.disk.infrastructure::%disk-unavailable-reason
                         reason))))))

#+darwin
(defun read-darwin-disk-values (on-value on-unavailable)
  (%read-darwin-disk-values
   #'ntop.platform.darwin:read-darwin-file-system-stats
   #'ntop.platform.darwin:read-darwin-volume-space-used
   #'ntop.platform.darwin:read-darwin-block-storage-stats
   on-value
   on-unavailable))

#+darwin
(defmethod read-disk-values ((collector disk-collector) on-value
                             on-unavailable)
  (declare (ignore collector))
  (read-darwin-disk-values on-value on-unavailable))
