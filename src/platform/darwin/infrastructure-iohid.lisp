(in-package #:ntop.platform.darwin)

(defstruct
    (darwin-iohid-service-handle
     (:constructor %make-darwin-iohid-service-handle
         (pointer identity &optional client-pointer))
     (:copier nil))
  pointer
  (identity nil :read-only t)
  client-pointer)

(defun %darwin-iohid-product-name (value)
  (and (stringp value) (plusp (length value)) value))

(defun %darwin-iohid-service-record (name handle)
  (when (%darwin-iohid-product-name name)
    (list (darwin-iohid-service-handle-identity handle) name handle)))

#+darwin
(define-darwin-ffi
  (:routine %cf-retain "CFRetain" sb-alien:system-area-pointer
    (object sb-alien:system-area-pointer))
  (:routine %cf-number-create "CFNumberCreate" sb-alien:system-area-pointer
    (allocator sb-alien:system-area-pointer)
    (number-type sb-alien:long)
    (value sb-alien:system-area-pointer))
  (:routine %cf-dictionary-create "CFDictionaryCreate"
    sb-alien:system-area-pointer
    (allocator sb-alien:system-area-pointer)
    (keys sb-alien:system-area-pointer)
    (values sb-alien:system-area-pointer)
    (count sb-alien:long)
    (key-callbacks sb-alien:system-area-pointer)
    (value-callbacks sb-alien:system-area-pointer))
  (:routine %iohid-event-system-client-create
    "IOHIDEventSystemClientCreate" sb-alien:system-area-pointer
    (allocator sb-alien:system-area-pointer))
  (:routine %iohid-event-system-client-set-matching
    "IOHIDEventSystemClientSetMatching" sb-alien:void
    (client sb-alien:system-area-pointer)
    (matching sb-alien:system-area-pointer))
  (:routine %iohid-event-system-client-copy-services
    "IOHIDEventSystemClientCopyServices" sb-alien:system-area-pointer
    (client sb-alien:system-area-pointer))
  (:routine %iohid-service-client-copy-property
    "IOHIDServiceClientCopyProperty" sb-alien:system-area-pointer
    (service sb-alien:system-area-pointer)
    (key sb-alien:system-area-pointer))
  (:routine %iohid-service-client-copy-event "IOHIDServiceClientCopyEvent"
    sb-alien:system-area-pointer
    (service sb-alien:system-area-pointer)
    (event-type sb-alien:long-long)
    (options sb-alien:int)
    (timestamp sb-alien:long-long))
  (:routine %iohid-event-get-float-value "IOHIDEventGetFloatValue"
    sb-alien:double
    (event sb-alien:system-area-pointer)
    (field sb-alien:int)))

#+darwin
(defun %make-retained-darwin-iohid-service-handle
    (pointer identity client)
  (let ((retained-client (%cf-retain client)))
    (when (cf-null-reference-p retained-client)
      (%cf-release pointer)
      (error "CFRetain failed for the IOHID event system client."))
    (let ((handle
            (%make-darwin-iohid-service-handle
             pointer identity retained-client)))
      (handler-case
          (sb-ext:finalize handle
            (let ((retained-service pointer)
                  (retained-client retained-client))
              (lambda ()
                (ignore-errors (%cf-release retained-service))
                (ignore-errors (%cf-release retained-client))))
            :dont-save t)
        (error (condition)
          (%cf-release pointer)
          (%cf-release retained-client)
          (error condition)))
      handle)))

#+darwin
(defun %release-darwin-iohid-service-handle (handle)
  (let ((service-pointer
          (darwin-iohid-service-handle-pointer handle))
        (client-pointer
          (darwin-iohid-service-handle-client-pointer handle)))
    (when (or service-pointer client-pointer)
      (setf (darwin-iohid-service-handle-pointer handle) nil
            (darwin-iohid-service-handle-client-pointer handle) nil)
      (sb-ext:cancel-finalization handle)
      (when service-pointer
        (%cf-release service-pointer))
      (when client-pointer
        (%cf-release client-pointer)))))

#+darwin
(defun %make-iohid-number (value)
  (sb-alien:with-alien ((number sb-alien:int value))
    (let ((reference
            (%cf-number-create
             (sb-sys:int-sap 0) 3
             (sb-alien:alien-sap (sb-alien:addr number)))))
      (when (cf-null-reference-p reference)
        (error "CFNumberCreate failed for IOHID matching data."))
      reference)))

#+darwin
(defun %make-iohid-matching-dictionary ()
  (let ((usage-page-key
          (%cf-string-create-with-c-string
           (sb-sys:int-sap 0) "PrimaryUsagePage" #x08000100))
        (usage-key
          (%cf-string-create-with-c-string
           (sb-sys:int-sap 0) "PrimaryUsage" #x08000100))
        (usage-page-value nil)
        (usage-value nil)
        (dictionary nil))
    (unwind-protect
         (progn
           (when (or (cf-null-reference-p usage-page-key)
                     (cf-null-reference-p usage-key))
             (error "Could not create IOHID matching keys."))
           (setf usage-page-value (%make-iohid-number #xff00)
                 usage-value (%make-iohid-number 5))
           (let ((keys (sb-alien:make-alien sb-alien:system-area-pointer 2))
                 (values (sb-alien:make-alien sb-alien:system-area-pointer 2)))
             (unwind-protect
                  (progn
                    (setf (sb-alien:deref keys 0) usage-page-key
                          (sb-alien:deref keys 1) usage-key
                          (sb-alien:deref values 0) usage-page-value
                          (sb-alien:deref values 1) usage-value
                          dictionary
                          (%cf-dictionary-create
                           (sb-sys:int-sap 0)
                           (sb-alien:alien-sap keys)
                           (sb-alien:alien-sap values)
                           2
                           (sb-sys:foreign-symbol-sap
                            "kCFTypeDictionaryKeyCallBacks" t)
                           (sb-sys:foreign-symbol-sap
                            "kCFTypeDictionaryValueCallBacks" t)))
                    (when (cf-null-reference-p dictionary)
                      (error "CFDictionaryCreate failed for IOHID matching."))
                    dictionary)
               (sb-alien:free-alien keys)
               (sb-alien:free-alien values))))
      (dolist (reference
               (list usage-page-key usage-key usage-page-value usage-value))
        (unless (cf-null-reference-p reference)
          (%cf-release reference))))))

#+darwin
(defun %iohid-service-product-name (service product-key)
  (let ((property
          (%iohid-service-client-copy-property service product-key)))
    (call-with-owned-cf-reference
     property #'%cf-release
     (lambda (reference)
       (%darwin-iohid-product-name (cf-value-to-lisp reference)))
     (lambda () nil))))

#+darwin
(defun %copy-named-iohid-service (service product-key client on-retained)
  (let ((name (%iohid-service-product-name service product-key)))
    (when name
      (let ((pointer (%cf-retain service)))
        (when (cf-null-reference-p pointer)
          (error "CFRetain failed for an IOHID service."))
        (let ((handle
                (%make-retained-darwin-iohid-service-handle
                 pointer (sb-sys:sap-int pointer) client)))
          (funcall on-retained handle)
          (%darwin-iohid-service-record name handle))))))

#+darwin
(defun %copy-named-iohid-services (services client on-retained)
  (let ((count (%cf-array-get-count services)))
    (when (minusp count)
      (error "CFArrayGetCount returned a negative IOHID service count."))
    (let ((product-key
            (%cf-string-create-with-c-string
             (sb-sys:int-sap 0) "Product" #x08000100)))
      (call-with-owned-cf-reference
       product-key #'%cf-release
       (lambda (key)
         (loop for index below count
               for service = (%cf-array-get-value-at-index services index)
               for record = (%copy-named-iohid-service
                             service key client on-retained)
               when record collect record))
       (lambda ()
         (error "Could not create the IOHID Product property key."))))))

#+darwin
(defun %enumerate-darwin-iohid-services ()
  (let ((client
          (%iohid-event-system-client-create (sb-sys:int-sap 0)))
        (retained-handles nil)
        (completed nil))
    (when (cf-null-reference-p client)
      (error "IOHIDEventSystemClientCreate returned null."))
    (unwind-protect
         (let ((records
                 (call-with-owned-cf-reference
                  (%make-iohid-matching-dictionary)
                  #'%cf-release
                  (lambda (matching)
                    (%iohid-event-system-client-set-matching client matching)
                    (call-with-owned-cf-reference
                     (%iohid-event-system-client-copy-services client)
                     #'%cf-release
                     (lambda (services)
                       (%copy-named-iohid-services
                        services
                        client
                        (lambda (handle)
                          (push handle retained-handles))))
                     (lambda ()
                       (error "IOHIDEventSystemClientCopyServices returned null."))))
                  (lambda ()
                    (error "Could not create IOHID matching data.")))))
           (setf completed t)
           records)
      (%cf-release client)
      (unless completed
        (mapc #'%release-darwin-iohid-service-handle retained-handles)))))

#+darwin
(defun %read-darwin-iohid-service-temperature (handle)
  (check-type handle darwin-iohid-service-handle)
  (let ((pointer (darwin-iohid-service-handle-pointer handle)))
    (unless pointer
      (error "The IOHID service handle has already been released."))
    (let ((event
            (%iohid-service-client-copy-event pointer 15 0 0)))
      (call-with-owned-cf-reference
       event #'%cf-release
       (lambda (reference)
         (let ((value
                 (%iohid-event-get-float-value reference (ash 15 16))))
           (and (realp value) (= value value) value)))
       (lambda () nil)))))

#+darwin
(defun %require-darwin-iohid-symbols
    (&optional (symbol-lookup #'sb-sys:foreign-symbol-sap))
  (dolist (name
           '("CFRetain" "CFNumberCreate" "CFDictionaryCreate"
             "IOHIDEventSystemClientCreate"
             "IOHIDEventSystemClientSetMatching"
             "IOHIDEventSystemClientCopyServices"
             "IOHIDServiceClientCopyProperty"
             "IOHIDServiceClientCopyEvent" "IOHIDEventGetFloatValue"))
    (unless (funcall symbol-lookup name nil)
      (error "The Darwin IOHID API symbol ~A is unavailable." name))))

(defun load-darwin-iohid-api ()
  #+darwin
  (progn
    (sb-alien:load-shared-object
     "/System/Library/Frameworks/IOKit.framework/IOKit" :dont-save t)
    (%require-darwin-iohid-symbols)
    (list :enumerate-services #'%enumerate-darwin-iohid-services
          :read-temperature #'%read-darwin-iohid-service-temperature))
  #-darwin
  (error "The Darwin IOHID API is unavailable on this platform."))
