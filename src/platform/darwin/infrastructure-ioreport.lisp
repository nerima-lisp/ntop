(in-package #:ntop.platform.darwin)

(defparameter *pmgr-dvfs-entry-layout*
  '(:size 8 :frequency-offset 0 :frequency-width 4))

(defparameter *ioreport-pmgr-dvfs-table-keys*
  '("voltage-states1-sram" "voltage-states5-sram"
    "voltage-states13-sram"))

(defun decode-pmgr-dvfs-table (octets)
  (check-type octets (vector (unsigned-byte 8)))
  (let* ((layout *pmgr-dvfs-entry-layout*)
         (entry-size (getf layout :size))
         (frequency-offset (getf layout :frequency-offset))
         (frequency-width (getf layout :frequency-width)))
    (unless (zerop (mod (length octets) entry-size))
      (error "DVFS table size ~D is not a multiple of ~D."
             (length octets) entry-size))
    (loop for offset from 0 below (length octets) by entry-size
          collect
          (decode-little-endian-unsigned
           octets
           :start (+ offset frequency-offset)
           :end (+ offset frequency-offset frequency-width)))))

(defun ioreport-cpu-channel-metadata (name)
  (when (and (stringp name)
             (= (length name) 7)
             (loop for index from 4 below 7
                   always (digit-char-p (char name index))))
    (let ((number (parse-integer name :start 4)))
      (cond
        ((and (string= name "ECPU" :end1 4 :end2 4)
              (<= 0 number 30)
              (zerop (mod number 10)))
         (values (floor number 10) "voltage-states1-sram"))
        ((and (string= name "PCPU" :end1 4 :end2 4)
              (<= 0 number 50)
              (zerop (mod number 10)))
         (values (+ 4 (floor number 10)) "voltage-states5-sram"))
        ((and (string= name "PCPU" :end1 4 :end2 4)
              (<= 100 number 150)
              (zerop (mod number 10)))
         (values (+ 10 (floor (- number 100) 10))
                 "voltage-states13-sram"))))))

#+darwin
(define-darwin-ffi
  (:routine %io-registry-entry-get-name "IORegistryEntryGetName" sb-alien:int
    (entry sb-alien:unsigned-int)
    (name sb-alien:system-area-pointer))
  (:routine %ioreport-copy-channels "IOReportCopyChannelsInGroup"
    sb-alien:system-area-pointer
    (group sb-alien:system-area-pointer)
    (subgroup sb-alien:system-area-pointer)
    (flags sb-alien:unsigned-long)
    (unit sb-alien:unsigned-long)
    (channel sb-alien:unsigned-long))
  (:routine %ioreport-create-subscription "IOReportCreateSubscription"
    sb-alien:system-area-pointer
    (reporter sb-alien:system-area-pointer)
    (channels sb-alien:system-area-pointer)
    (subscribed-channels (* sb-alien:system-area-pointer))
    (identifier sb-alien:unsigned-long)
    (options sb-alien:system-area-pointer))
  (:routine %ioreport-create-samples "IOReportCreateSamples"
    sb-alien:system-area-pointer
    (subscription sb-alien:system-area-pointer)
    (channels sb-alien:system-area-pointer)
    (options sb-alien:system-area-pointer))
  (:routine %ioreport-create-samples-delta "IOReportCreateSamplesDelta"
    sb-alien:system-area-pointer
    (previous sb-alien:system-area-pointer)
    (current sb-alien:system-area-pointer)
    (options sb-alien:system-area-pointer))
  (:routine %ioreport-channel-name "IOReportChannelGetChannelName"
    sb-alien:system-area-pointer
    (channel sb-alien:system-area-pointer))
  (:routine %ioreport-channel-subgroup "IOReportChannelGetSubGroup"
    sb-alien:system-area-pointer
    (channel sb-alien:system-area-pointer))
  (:routine %ioreport-state-count "IOReportStateGetCount" sb-alien:int
    (channel sb-alien:system-area-pointer))
  (:routine %ioreport-state-name "IOReportStateGetNameForIndex"
    sb-alien:system-area-pointer
    (channel sb-alien:system-area-pointer)
    (index sb-alien:int))
  (:routine %ioreport-state-residency "IOReportStateGetResidency"
    sb-alien:long-long
    (channel sb-alien:system-area-pointer)
    (index sb-alien:int))
  (:routine %cf-dictionary-create-mutable-copy "CFDictionaryCreateMutableCopy"
    sb-alien:system-area-pointer
    (allocator sb-alien:system-area-pointer)
    (capacity sb-alien:long)
    (dictionary sb-alien:system-area-pointer)))

(defun %call-with-owned-cf-reference (reference function)
  (call-with-owned-cf-reference
   reference #'ntop.platform.darwin::%cf-release function
   (lambda () (error "IOReport returned a null CoreFoundation reference."))))

#+darwin
(defun %ioreport-service-name (service)
  (let ((octets (make-array 128 :element-type '(unsigned-byte 8))))
    (sb-sys:with-pinned-objects (octets)
      (let ((status (%io-registry-entry-get-name
                     service (sb-sys:vector-sap octets))))
        (unless (zerop status)
          (error "IORegistryEntryGetName failed with code ~D." status))))
    (decode-c-string octets)))

(defun %decode-pmgr-dvfs-tables (properties)
  (loop for key in *ioreport-pmgr-dvfs-table-keys*
        collect
        (cons
         key
         (cond
           ((and properties (not (hash-table-p properties))) :invalid)
           ((null properties) nil)
           (t
            (multiple-value-bind (octets present-p) (gethash key properties)
              (cond
                ((not present-p) nil)
                ((not (typep octets '(vector (unsigned-byte 8)))) :invalid)
                (t
                 (handler-case
                     (let ((frequencies
                             (decode-pmgr-dvfs-table octets)))
                       (if (and frequencies
                                (every (lambda (frequency)
                                         (and (integerp frequency)
                                              (plusp frequency)))
                                       frequencies))
                           frequencies
                           :invalid))
                   (error () :invalid))))))))))

#+darwin
(defun %read-pmgr-dvfs-tables ()
  (let ((properties nil)
        (found-p nil))
    (call-with-iokit-services
     "AppleARMIODevice"
     (lambda (service)
       (when (and (not found-p)
                  (string= (%ioreport-service-name service) "pmgr"))
         (setf found-p t)
         (iokit-registry-properties
          service
          (lambda (value) (setf properties value))
          (lambda (reason)
            (unless properties
              (error "IOKit registry properties read failed: ~S." reason))))))
     (lambda () nil)
     (lambda (reason)
       (error "IOKit service enumeration failed: ~S." reason)))
    (if found-p
        (%decode-pmgr-dvfs-tables properties)
        (%decode-pmgr-dvfs-tables nil))))

#+darwin
(defun %ioreport-dictionary-value (dictionary key)
  (%call-with-owned-cf-reference
   (ntop.platform.darwin::%cf-string-create-with-c-string
    (sb-sys:int-sap 0) key #x08000100)
   (lambda (key-reference)
     (ntop.platform.darwin::%cf-dictionary-get-value
      dictionary key-reference))))

#+darwin
(defun %ioreport-state-residencies (channel)
  (let ((count (%ioreport-state-count channel)))
    (when (minusp count)
      (error "IOReport returned a negative state count."))
    (loop for index below count
          for name = (cf-value-to-lisp (%ioreport-state-name channel index))
          for residency = (%ioreport-state-residency channel index)
          unless (and (stringp name) (integerp residency) (not (minusp residency)))
            do (error "IOReport returned invalid CPU state data.")
          collect (cons name residency))))

#+darwin
(defun %ioreport-cpu-channel-data (delta tables)
  (let ((channels (%ioreport-dictionary-value delta "IOReportChannels")))
    (unless (cf-null-reference-p channels)
      (loop for index below
            (ntop.platform.darwin::%cf-array-get-count channels)
            for channel =
              (ntop.platform.darwin::%cf-array-get-value-at-index channels index)
            for subgroup =
              (cf-value-to-lisp (%ioreport-channel-subgroup channel))
            when (string= subgroup "CPU Core Performance States")
              append
              (multiple-value-bind (core-index table-key)
                  (ioreport-cpu-channel-metadata
                   (cf-value-to-lisp (%ioreport-channel-name channel)))
                (when core-index
                  (list
                   (list core-index
                         (%ioreport-state-residencies channel)
                         (cdr (assoc table-key tables :test #'string=))))))))))

#+darwin
(defun %ioreport-sleep (seconds)
  (sleep seconds))

#+darwin
(defun %read-ioreport-cpu-performance-state-data ()
  (let ((tables (%read-pmgr-dvfs-tables))
        (group nil)
        (subgroup nil))
    (unwind-protect
         (progn
           (setf group
                 (ntop.platform.darwin::%cf-string-create-with-c-string
                  (sb-sys:int-sap 0) "CPU Stats" #x08000100))
           (setf subgroup
                 (ntop.platform.darwin::%cf-string-create-with-c-string
                  (sb-sys:int-sap 0) "CPU Core Performance States" #x08000100))
           (%call-with-owned-cf-reference
            (%ioreport-copy-channels group subgroup 0 0 0)
            (lambda (channels)
              (%call-with-owned-cf-reference
               (%cf-dictionary-create-mutable-copy
                (sb-sys:int-sap 0) 0 channels)
               (lambda (mutable-channels)
                 (sb-alien:with-alien
                     ((subscribed-channels sb-alien:system-area-pointer))
                   (setf subscribed-channels (sb-sys:int-sap 0))
                   (let ((subscription nil))
                     (unwind-protect
                          (progn
                            (setf subscription
                                  (%ioreport-create-subscription
                                   (sb-sys:int-sap 0) mutable-channels
                                   (sb-alien:addr subscribed-channels) 0
                                   (sb-sys:int-sap 0)))
                            (%call-with-owned-cf-reference
                             subscription
                             (lambda (subscription)
                               (%call-with-owned-cf-reference
                                (%ioreport-create-samples
                                 subscription subscribed-channels
                                 (sb-sys:int-sap 0))
                                (lambda (previous)
                                  (%ioreport-sleep 0.05d0)
                                  (%call-with-owned-cf-reference
                                   (%ioreport-create-samples
                                    subscription subscribed-channels
                                    (sb-sys:int-sap 0))
                                   (lambda (current)
                                     (%call-with-owned-cf-reference
                                      (%ioreport-create-samples-delta
                                       previous current (sb-sys:int-sap 0))
                                      (lambda (delta)
                                        (%ioreport-cpu-channel-data
                                         delta tables))))))))))
                       (unless (cf-null-reference-p subscribed-channels)
                         (ntop.platform.darwin::%cf-release
                          subscribed-channels))))))))))
      (unless (cf-null-reference-p group)
        (ntop.platform.darwin::%cf-release group))
      (unless (cf-null-reference-p subgroup)
        (ntop.platform.darwin::%cf-release subgroup)))))

#+darwin
(defun read-darwin-cpu-performance-state-data (on-value on-unavailable)
  (multiple-value-bind (data failure)
      (handler-case
          (progn
            (sb-alien:load-shared-object "/usr/lib/libIOReport.dylib"
                                         :dont-save t)
            (values (%read-ioreport-cpu-performance-state-data) nil))
        (error (condition) (values nil condition)))
    (cond
      (failure (funcall on-unavailable :private-api-unavailable))
      ((some (lambda (row) (eq (third row) :invalid)) data)
       (funcall on-unavailable :read-failed))
      ((notany #'third data) (funcall on-unavailable :not-present))
      (t (funcall on-value data)))))
