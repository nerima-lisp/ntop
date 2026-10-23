(in-package #:ntop.test)

(defun %write-disk-test-unsigned (octets offset width value)
  (dotimes (index width octets)
    (setf (aref octets (+ offset index)) (ldb (byte 8 (* index 8)) value))))

(defun %write-disk-test-string (octets offset width value)
  (loop for character across value
        for index from 0
        do (setf (aref octets (+ offset index)) (char-code character)))
  octets)

(defun %make-disk-test-octets (values)
  (make-array (length values)
              :element-type
              '(unsigned-byte 8)
              :initial-contents
              values))

(defun %make-statfs-entry (device-name mount-path
                                       filesystem-type
                                       block-size
                                       blocks
                                       free-blocks
                                       available-blocks)
  (let ((octets
         (make-array 2168 :element-type '(unsigned-byte 8) :initial-element 0)))
    (%write-disk-test-unsigned octets 0 4 block-size)
    (%write-disk-test-unsigned octets 8 8 blocks)
    (%write-disk-test-unsigned octets 16 8 free-blocks)
    (%write-disk-test-unsigned octets 24 8 available-blocks)
    (%write-disk-test-string octets 72 16 filesystem-type)
    (%write-disk-test-string octets 88 1024 mount-path)
    (%write-disk-test-string octets 1112 1024 device-name)
    octets))

(defun %make-volume-space-used-record (declared-size buffer-size used-bytes)
  (let ((octets
         (make-array buffer-size
                     :element-type
                     '(unsigned-byte 8)
                     :initial-element
                     0)))
    (when (>= buffer-size 4)
      (%write-disk-test-unsigned octets 0 4 declared-size))
    (when (>= buffer-size 12)
      (%write-disk-test-unsigned octets 4 8 used-bytes))
    octets))
