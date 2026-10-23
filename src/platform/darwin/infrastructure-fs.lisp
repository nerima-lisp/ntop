(in-package #:ntop.platform.darwin)

(defparameter *darwin-statfs-layout*
  '(:size 2168
          :block-size-offset
          0
          :blocks-offset
          8
          :free-blocks-offset
          16
          :available-blocks-offset
          24
          :filesystem-type-offset
          72
          :filesystem-type-size
          16
          :mount-path-offset
          88
          :mount-path-size
          1024
          :device-name-offset
          1112
          :device-name-size
          1024))

(defparameter *darwin-volume-space-used-layout*
  '(:minimum-size 12
                  :length-offset
                  0
                  :length-width
                  4
                  :value-offset
                  4
                  :value-width
                  8))

(defun %decode-darwin-statfs-entry (octets start)
  (let* ((layout *darwin-statfs-layout*)
         (block-size
          (decode-little-endian-unsigned octets
                                         :start
                                         (+ start
                                            (getf layout :block-size-offset))
                                         :end
                                         (+ start
                                            (getf layout :block-size-offset)
                                            4)))
         (blocks
          (decode-little-endian-unsigned octets
                                         :start
                                         (+ start (getf layout :blocks-offset))
                                         :end
                                         (+ start
                                            (getf layout :blocks-offset)
                                            8)))
         (free-blocks
          (decode-little-endian-unsigned octets
                                         :start
                                         (+ start
                                            (getf layout :free-blocks-offset))
                                         :end
                                         (+ start
                                            (getf layout :free-blocks-offset)
                                            8)))
         (available-blocks
          (decode-little-endian-unsigned octets
                                         :start
                                         (+ start
                                            (getf layout
                                                  :available-blocks-offset))
                                         :end
                                         (+ start
                                            (getf layout
                                                  :available-blocks-offset)
                                            8))))
    (when (zerop block-size)
      (error "A Darwin statfs block size must be positive."))
    (list :device-name
          (decode-c-string octets
                           :start
                           (+ start (getf layout :device-name-offset))
                           :end
                           (+ start
                              (getf layout :device-name-offset)
                              (getf layout :device-name-size)))
          :mount-path
          (decode-c-string octets
                           :start
                           (+ start (getf layout :mount-path-offset))
                           :end
                           (+ start
                              (getf layout :mount-path-offset)
                              (getf layout :mount-path-size)))
          :filesystem-type
          (decode-c-string octets
                           :start
                           (+ start (getf layout :filesystem-type-offset))
                           :end
                           (+ start
                              (getf layout :filesystem-type-offset)
                              (getf layout :filesystem-type-size)))
          :block-size
          block-size
          :blocks
          blocks
          :free-blocks
          free-blocks
          :available-blocks
          available-blocks)))

(defun decode-darwin-statfs-buffer (octets count)
  (check-type octets (vector (unsigned-byte 8)))
  (check-type count (integer 0 *))
  (let* ((entry-size (getf *darwin-statfs-layout* :size))
         (required-size (* count entry-size)))
    (when (> required-size (length octets))
      (error "Expected at least ~D statfs octets, got ~D."
             required-size
             (length octets)))
    (loop for index below
          count
          collect (%decode-darwin-statfs-entry octets (* index entry-size)))))

(defun decode-darwin-volume-space-used (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let* ((layout *darwin-volume-space-used-layout*)
         (minimum-size (getf layout :minimum-size))
         (record-size
          (decode-little-endian-unsigned octets
                                         :start
                                         (getf layout :length-offset)
                                         :end
                                         (+ (getf layout :length-offset)
                                            (getf layout :length-width)))))
    (unless (<= minimum-size record-size (length octets))
      (error "Malformed volume-space-used record length ~D for ~D octets."
             record-size
             (length octets)))
    (decode-little-endian-unsigned octets
                                   :start
                                   (getf layout :value-offset)
                                   :end
                                   (+ (getf layout :value-offset)
                                      (getf layout :value-width)))))

(defun %write-darwin-unsigned-octets (octets offset width value)
  (dotimes (index width octets)
    (setf (aref octets (+ offset index)) (ldb (byte 8 (* index 8)) value))))

(defparameter *darwin-volume-attrlist-layout*
  '(:size 24
          :bitmap-count-offset
          0
          :bitmap-count
          5
          :volume-attributes-offset
          8
          :volume-attributes
          #x80800000
          :buffer-size
          12))

(defun %make-darwin-volume-attrlist ()
  (let* ((layout *darwin-volume-attrlist-layout*)
         (octets
          (make-array (getf layout :size)
                      :element-type
                      '(unsigned-byte 8)
                      :initial-element
                      0)))
    (%write-darwin-unsigned-octets octets
                                   (getf layout :bitmap-count-offset)
                                   2
                                   (getf layout :bitmap-count))
    (%write-darwin-unsigned-octets octets
                                   (getf layout :volume-attributes-offset)
                                   4
                                   (getf layout :volume-attributes))
    octets))

#+darwin
(define-darwin-ffi
  (:routine %getfsstat "getfsstat" sb-alien:int
    (buffer sb-alien:system-area-pointer)
    (buffer-size sb-alien:long)
    (flags sb-alien:int))
  (:routine %getattrlist "getattrlist" sb-alien:int
    (path sb-alien:c-string)
    (attributes sb-alien:system-area-pointer)
    (buffer sb-alien:system-area-pointer)
    (buffer-size sb-alien:unsigned-long)
    (options sb-alien:unsigned-int))
  (:routine %io-registry-entry-search-cf-property
    "IORegistryEntrySearchCFProperty" sb-alien:system-area-pointer
    (entry sb-alien:unsigned-int)
    (plane sb-alien:c-string)
    (key sb-alien:system-area-pointer)
    (allocator sb-alien:system-area-pointer)
    (options sb-alien:unsigned-int)))

(defconstant +darwin-mnt-nowait+
  2)

#+darwin
(defun read-darwin-file-system-stats (on-value on-unavailable)
  (let* ((layout *darwin-statfs-layout*)
         (entry-size (getf layout :size))
         (null-pointer (sb-sys:int-sap 0))
         (count (%getfsstat null-pointer 0 +darwin-mnt-nowait+)))
    (cond
      ((minusp count)
       (funcall on-unavailable
                (list :getfsstat-error count (sb-alien::get-errno))))
      ((zerop count)
       (funcall on-value nil))
      ((> (* count entry-size) array-total-size-limit)
       (funcall on-unavailable :statfs-buffer-too-large))
      (t
       (let ((octets (make-array (* count entry-size)
                                 :element-type '(unsigned-byte 8)))
             (copied nil)
             (failure nil))
         (sb-sys:with-pinned-objects (octets)
           (setf copied
                 (%getfsstat (sb-sys:vector-sap octets)
                             (length octets)
                             +darwin-mnt-nowait+))
           (when (minusp copied)
             (setf failure
                   (list :getfsstat-error copied (sb-alien::get-errno)))))
         (when (and (null failure) (> copied count))
           (setf failure (list :statfs-count-exceeded-buffer count copied)))
         (when (null failure)
           (let ((current-count
                   (%getfsstat null-pointer 0 +darwin-mnt-nowait+)))
             (cond
               ((minusp current-count)
                (setf failure
                      (list :getfsstat-error
                            current-count (sb-alien::get-errno))))
               ((> current-count copied)
                (setf failure
                      (list :statfs-count-changed copied current-count))))))
         (let ((file-systems nil))
           (unless failure
             (handler-case
                 (setf file-systems
                       (decode-darwin-statfs-buffer octets copied))
               (error (condition)
                 (setf failure (list :invalid-statfs-buffer condition)))))
         (if failure
             (funcall on-unavailable failure)
             (funcall on-value file-systems))))))))

#+darwin
(defun read-darwin-volume-space-used (mount-path on-value on-unavailable)
  (check-type mount-path string)
  (let* ((attrlist (%make-darwin-volume-attrlist))
         (buffer-size (getf *darwin-volume-attrlist-layout* :buffer-size))
         (buffer (make-array buffer-size
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (failure nil)
         (used-bytes nil))
    (sb-sys:with-pinned-objects (attrlist buffer)
      (let ((status
              (%getattrlist mount-path
                            (sb-sys:vector-sap attrlist)
                            (sb-sys:vector-sap buffer)
                            buffer-size
                            0)))
        (unless (zerop status)
          (setf failure
                (list :getattrlist-error status (sb-alien::get-errno))))))
    (unless failure
      (handler-case
          (setf used-bytes (decode-darwin-volume-space-used buffer))
        (error (condition)
          (setf failure (list :invalid-volume-space-used condition)))))
    (if failure
        (funcall on-unavailable failure)
        (funcall on-value used-bytes))))

#+darwin
(defun %darwin-iokit-device-name (service)
  (let ((device-name nil))
    (with-owned-cf-reference
        (key (%cf-string-create-with-c-string
              (sb-sys:int-sap 0) "BSD Name" #x08000100)
             #'%cf-release)
      (with-owned-cf-reference
          (reference (%io-registry-entry-search-cf-property
                      service "IOService" key (sb-sys:int-sap 0) 1)
                     #'%cf-release)
        (let ((value (cf-value-to-lisp reference)))
          (unless (and (stringp value) (plusp (length value)))
            (error "IOKit returned an invalid BSD Name."))
          (setf device-name value))
        nil)
      nil)
    device-name))

#+darwin
(defun %darwin-iokit-device-statistics (service)
  (let ((device-name (%darwin-iokit-device-name service)))
    (when device-name
      (let ((properties nil)
            (failure nil))
        (iokit-registry-properties
         service
         (lambda (value) (setf properties value))
         (lambda (reason) (setf failure reason)))
        (when failure
          (error "Could not read IOKit properties: ~S." failure))
        (unless (hash-table-p properties)
          (error "IOKit properties are not a dictionary."))
        (let ((statistics (gethash "Statistics" properties)))
          (unless (hash-table-p statistics)
            (error "IOKit statistics are not a dictionary."))
          (multiple-value-bind (read-bytes read-present-p)
              (gethash "Bytes (Read)" statistics)
            (multiple-value-bind (write-bytes write-present-p)
                (gethash "Bytes (Write)" statistics)
              (unless (and read-present-p write-present-p
                           (integerp read-bytes) (not (minusp read-bytes))
                           (integerp write-bytes) (not (minusp write-bytes)))
                (error "IOKit returned invalid block-storage counters."))
              (list :device-name device-name
                    :read-bytes read-bytes
                    :write-bytes write-bytes))))))))

#+darwin
(defun read-darwin-block-storage-stats (on-value on-unavailable)
  (let ((devices nil)
        (failure nil))
    (handler-case
        (call-with-iokit-services
         "IOBlockStorageDriver"
         (lambda (service)
           (unless failure
             (let ((device (%darwin-iokit-device-statistics service)))
               (when device
                 (push device devices)))))
         (lambda () nil)
         (lambda (reason) (setf failure reason)))
      (error (condition)
        (setf failure (list :iokit-read-error condition))))
    (cond
      (failure
       (funcall on-unavailable failure))
      ((null devices)
       (funcall on-unavailable :no-block-storage-statistics))
      (t
       (funcall on-value (nreverse devices))))))
