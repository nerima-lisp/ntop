(in-package #:ntop.disk.infrastructure)

(defun linux-disk-mount-from-entry (entry)
  (let* ((filesystem-type (getf entry :type))
         (source-device (getf entry :device))
         (device-name
          (and source-device
               (ntop.platform.linux::decode-linux-mount-field source-device)))
         (mount-path (getf entry :mount-point))
         (statistics nil))
    (when (ntop.disk.domain:disk-filesystem-type-supported-p filesystem-type)
      (ntop.platform.linux:read-linux-statvfs mount-path
                                              (lambda (value)
                                                (setf statistics value))
                                              (lambda (reason)
                                                (declare (ignore reason))))
      (let* ((total-bytes (getf statistics :total-bytes))
             (free-bytes (getf statistics :available-bytes)))
        (ntop.disk.domain:make-disk-mount :device-name
                                          device-name
                                          :mount-path
                                          mount-path
                                          :filesystem-type
                                          filesystem-type
                                          :used-bytes
                                          (and total-bytes
                                               free-bytes
                                               (- total-bytes free-bytes))
                                          :free-bytes
                                          free-bytes
                                          :total-bytes
                                          total-bytes
                                          :io-key
                                          (and device-name
                                               (let ((separator
                                                      (position #\/
                                                                device-name
                                                                :from-end
                                                                t)))
                                                 (subseq device-name
                                                         (if separator
                                                             (1+ separator)
                                                             0)))))))))

(defun linux-disk-device-from-entry (entry)
  (ntop.disk.domain:make-disk-device :device-name
                                     (getf entry :name)
                                     :read-bytes
                                     (getf entry :read-bytes)
                                     :write-bytes
                                     (getf entry :written-bytes)))

(defun read-linux-disk-values (on-value on-unavailable)
  (flet ((read-failure-reason (reason)
           (let ((normalized-reason
                  (ntop.shared.application:normalize-collector-unavailability-reason reason)))
             (if (eq normalized-reason :not-present)
                 :read-failed
                 normalized-reason))))
    (ntop.platform.linux:read-proc-mounts
     (lambda (entries)
       (let ((mounts
              (loop for entry in entries
                    for mount = (linux-disk-mount-from-entry entry)
                    when mount
                      collect mount))
             (devices nil)
             (device-read-failure nil))
         (ntop.platform.linux:read-proc-diskstats
          (lambda (entries)
            (setf devices (mapcar #'linux-disk-device-from-entry entries)))
          (lambda (reason)
            (setf device-read-failure (read-failure-reason reason))))
         (cond
           ((or mounts devices)
            (funcall on-value (list :mounts mounts :devices devices)))
           (device-read-failure (funcall on-unavailable device-read-failure))
           (t (funcall on-unavailable :not-present)))))
     (lambda (reason)
       (funcall on-unavailable (read-failure-reason reason))))))

#+linux
(defmethod read-disk-values
    ((collector disk-collector) on-value on-unavailable)
  (declare (ignore collector))
  (read-linux-disk-values on-value on-unavailable))
