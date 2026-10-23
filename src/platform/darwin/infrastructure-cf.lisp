(in-package #:ntop.platform.darwin)

(defparameter *cf-type-id-routines*
  '((:boolean . %cf-boolean-type-id) (:dictionary . %cf-dictionary-type-id)
    (:string . %cf-string-type-id)
    (:number . %cf-number-type-id)
    (:array . %cf-array-type-id)
    (:data . %cf-data-type-id)))

(defparameter *cf-object-readers*
  '((:boolean . %cf-boolean-tree) (:dictionary . %cf-dictionary-tree-reader)
    (:string . %cf-string-tree)
    (:number . %cf-number-tree)
    (:array . %cf-array-object-tree)
    (:data . %cf-data-tree)))

(defparameter *cf-tree-converters*
  '((:boolean . %cf-tree-boolean) (:dictionary . %cf-tree-dictionary)
    (:string . %cf-tree-value)
    (:number . %cf-tree-value)
    (:array . %cf-tree-array)
    (:data . %cf-tree-data)))

(defun cf-tree-to-lisp (tree)
  (destructuring-bind (kind value) tree
    (let ((converter (cdr (assoc kind *cf-tree-converters*))))
      (unless converter
        (error "Unsupported CoreFoundation tree kind ~S." kind))
      (funcall (symbol-function converter) value))))

(defun %cf-tree-value (value)
  value)

(defun %cf-tree-boolean (value)
  (if value
      :true
      :false))

(defun %cf-tree-array (values)
  (mapcar #'cf-tree-to-lisp values))

(declaim (ftype function %cf-object-tree))

(defun %cf-tree-dictionary (entries)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (setf (gethash (cf-tree-to-lisp (car entry)) table) (cf-tree-to-lisp
                                                           (cdr entry))))))

(defun %cf-tree-data (octets)
  (copy-seq octets))

(defun cf-null-reference-p (reference)
  (or (null reference) (zerop (sb-sys:sap-int reference))))

(defun call-with-owned-cf-reference (reference release on-value on-null)
  (if (cf-null-reference-p reference)
      (funcall on-null)
      (unwind-protect (funcall on-value reference)
        (funcall release reference))))

(defmacro with-owned-cf-reference ((reference expression release) on-value
                                   on-null)
  `(call-with-owned-cf-reference ,expression
                                 ,release
                                 (lambda (,reference)
                                   ,on-value)
                                 (lambda ()
                                   ,on-null)))
#+darwin
(define-darwin-ffi
  (:routine %cf-release "CFRelease" sb-alien:void
    (object sb-alien:system-area-pointer))
  (:routine %cf-get-type-id "CFGetTypeID" sb-alien:unsigned-long
    (object sb-alien:system-area-pointer))
  (:routine %cf-boolean-type-id "CFBooleanGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-dictionary-type-id "CFDictionaryGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-string-type-id "CFStringGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-number-type-id "CFNumberGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-array-type-id "CFArrayGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-data-type-id "CFDataGetTypeID" sb-alien:unsigned-long)
  (:routine %cf-string-create-with-c-string "CFStringCreateWithCString"
    sb-alien:system-area-pointer
    (allocator sb-alien:system-area-pointer)
    (string sb-alien:c-string)
    (encoding sb-alien:unsigned-int))
  (:routine %cf-string-get-length "CFStringGetLength" sb-alien:long
    (string sb-alien:system-area-pointer))
  (:routine %cf-string-get-maximum-size-for-encoding
    "CFStringGetMaximumSizeForEncoding" sb-alien:long
    (length sb-alien:long)
    (encoding sb-alien:unsigned-int))
  (:routine %cf-string-get-c-string "CFStringGetCString" sb-alien:unsigned-char
    (string sb-alien:system-area-pointer)
    (buffer sb-alien:system-area-pointer)
    (buffer-size sb-alien:long)
    (encoding sb-alien:unsigned-int))
  (:routine %cf-boolean-get-value "CFBooleanGetValue" sb-alien:unsigned-char
    (boolean sb-alien:system-area-pointer))
  (:routine %cf-number-is-float-type "CFNumberIsFloatType" sb-alien:unsigned-char
    (number sb-alien:system-area-pointer))
  (:routine %cf-number-get-value "CFNumberGetValue" sb-alien:unsigned-char
    (number sb-alien:system-area-pointer)
    (type sb-alien:long)
    (value sb-alien:system-area-pointer))
  (:routine %cf-array-get-count "CFArrayGetCount" sb-alien:long
    (array sb-alien:system-area-pointer))
  (:routine %cf-array-get-value-at-index "CFArrayGetValueAtIndex"
    sb-alien:system-area-pointer
    (array sb-alien:system-area-pointer)
    (index sb-alien:long))
  (:routine %cf-dictionary-get-value "CFDictionaryGetValue"
    sb-alien:system-area-pointer
    (dictionary sb-alien:system-area-pointer)
    (key sb-alien:system-area-pointer))
  (:routine %cf-dictionary-get-count "CFDictionaryGetCount" sb-alien:long
    (dictionary sb-alien:system-area-pointer))
  (:routine %cf-dictionary-get-keys-and-values "CFDictionaryGetKeysAndValues"
    sb-alien:void
    (dictionary sb-alien:system-area-pointer)
    (keys sb-alien:system-area-pointer)
    (values sb-alien:system-area-pointer))
  (:routine %cf-data-get-length "CFDataGetLength" sb-alien:long
    (data sb-alien:system-area-pointer))
  (:routine %cf-data-get-byte-ptr "CFDataGetBytePtr"
    sb-alien:system-area-pointer
    (data sb-alien:system-area-pointer))
  (:routine %io-service-matching "IOServiceMatching" sb-alien:system-area-pointer
    (service-class sb-alien:c-string))
  (:routine %io-service-get-matching-services "IOServiceGetMatchingServices"
    sb-alien:int
    (master-port sb-alien:unsigned-int)
    (matching sb-alien:system-area-pointer)
    (iterator (* sb-alien:unsigned-int)))
  (:routine %io-iterator-next "IOIteratorNext" sb-alien:unsigned-int
    (iterator sb-alien:unsigned-int))
  (:routine %io-registry-entry-create-cf-properties
    "IORegistryEntryCreateCFProperties" sb-alien:int
    (entry sb-alien:unsigned-int)
    (properties (* sb-alien:system-area-pointer))
    (allocator sb-alien:system-area-pointer)
    (options sb-alien:unsigned-int))
  (:routine %io-object-release "IOObjectRelease" sb-alien:int
    (object sb-alien:unsigned-int)))

#+darwin
(progn
  (sb-alien:load-shared-object
   "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation"
   :dont-save t)
  (sb-alien:load-shared-object
   "/System/Library/Frameworks/IOKit.framework/IOKit"
   :dont-save t))
#+darwin
(defun %cf-object-kind (reference)
  (let ((type-id (%cf-get-type-id reference)))
    (or (loop for (kind . routine) in *cf-type-id-routines*
              when (= type-id (funcall (symbol-function routine)))
                return kind)
        (error "Unsupported CoreFoundation type ID ~D." type-id))))

#+darwin
(defun %cf-string-value (reference)
  (let* ((length (%cf-string-get-length reference))
         (maximum (%cf-string-get-maximum-size-for-encoding
                   length #x08000100)))
    (when (minusp maximum)
      (error "CoreFoundation could not size the string buffer."))
    (let ((octets (make-array (1+ maximum)
                              :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (octets)
        (unless (plusp (%cf-string-get-c-string
                        reference (sb-sys:vector-sap octets) (length octets)
                        #x08000100))
          (error "CFStringGetCString failed.")))
      (decode-c-string octets))))

#+darwin
(defun %cf-number-value (reference)
  (if (plusp (%cf-number-is-float-type reference))
      (sb-alien:with-alien ((value sb-alien:double 0d0))
        (unless (plusp (%cf-number-get-value
                        reference 13
                        (sb-alien:alien-sap (sb-alien:addr value))))
          (error "CFNumberGetValue could not read a double."))
        value)
      (sb-alien:with-alien ((value sb-alien:long-long 0))
        (unless (plusp (%cf-number-get-value
                        reference 4
                        (sb-alien:alien-sap (sb-alien:addr value))))
          (error "CFNumberGetValue could not read a signed integer."))
        value)))

#+darwin
(defun %cf-array-tree (reference)
  (let ((count (%cf-array-get-count reference)))
    (when (minusp count)
      (error "CFArrayGetCount returned a negative count."))
    (loop for index below count
          collect (%cf-object-tree
                   (%cf-array-get-value-at-index reference index)))))
#+darwin
(defun %cf-dictionary-tree (reference)
  (let ((count (%cf-dictionary-get-count reference)))
    (when (minusp count)
      (error "CFDictionaryGetCount returned a negative count."))
    (if (zerop count)
        '()
        (let ((keys (sb-alien:make-alien sb-alien:system-area-pointer count))
              (values (sb-alien:make-alien sb-alien:system-area-pointer count)))
          (unwind-protect
               (progn
                 (%cf-dictionary-get-keys-and-values
                  reference (sb-alien:alien-sap keys)
                  (sb-alien:alien-sap values))
                 (loop for index below count
                       collect
                       (cons (%cf-object-tree (sb-alien:deref keys index))
                             (%cf-object-tree (sb-alien:deref values index)))))
            (sb-alien:free-alien keys)
            (sb-alien:free-alien values))))))

#+darwin
(defun %cf-data-tree (reference)
  (let* ((length (%cf-data-get-length reference))
         (pointer (%cf-data-get-byte-ptr reference)))
    (when (minusp length)
      (error "CFDataGetLength returned a negative length."))
    (when (and (plusp length) (cf-null-reference-p pointer))
      (error "CFDataGetBytePtr returned null for non-empty data."))
    (list :data
          (let ((octets (make-array length :element-type '(unsigned-byte 8))))
            (dotimes (index length octets)
              (setf (aref octets index) (sb-sys:sap-ref-8 pointer index)))))))
#+darwin
(defun %cf-boolean-tree (reference)
  (list :boolean (plusp (%cf-boolean-get-value reference))))

#+darwin
(defun %cf-dictionary-tree-reader (reference)
  (list :dictionary (%cf-dictionary-tree reference)))

#+darwin
(defun %cf-string-tree (reference)
  (list :string (%cf-string-value reference)))

#+darwin
(defun %cf-number-tree (reference)
  (list :number (%cf-number-value reference)))

#+darwin
(defun %cf-array-object-tree (reference)
  (list :array (%cf-array-tree reference)))

#+darwin
(defun %cf-object-tree (reference)
  (let* ((kind (%cf-object-kind reference))
         (reader (cdr (assoc kind *cf-object-readers*))))
    (unless reader
      (error "No CoreFoundation reader for ~S." kind))
    (funcall (symbol-function reader) reference)))

#+darwin
(defun cf-value-to-lisp (reference)
  (cf-tree-to-lisp (%cf-object-tree reference)))
#+darwin
(defun cf-dictionary-get-value (dictionary key on-value on-missing)
  (with-owned-cf-reference
      (key-reference
       (%cf-string-create-with-c-string (sb-sys:int-sap 0) key #x08000100)
       #'%cf-release)
    (let ((value (%cf-dictionary-get-value dictionary key-reference)))
      (if (cf-null-reference-p value)
          (funcall on-missing)
          (funcall on-value (cf-value-to-lisp value))))
    (funcall on-missing)))
#+darwin
(defun %release-iokit-object (object)
  (let ((status (%io-object-release object)))
    (unless (zerop status)
      (error "IOObjectRelease failed with code ~D." status))))

#+darwin
(defun call-with-iokit-services (service-class on-service on-complete on-error)
  (let ((matching (%io-service-matching service-class)))
    (if (cf-null-reference-p matching)
        (funcall on-error :no-matching-dictionary)
        (sb-alien:with-alien ((iterator sb-alien:unsigned-int 0))
          (let ((status (%io-service-get-matching-services
                         0 matching (sb-alien:addr iterator))))
            (if (not (zerop status))
                (progn
                  (unless (zerop iterator)
                    (%release-iokit-object iterator))
                  (funcall on-error (list :iokit-error status)))
                (unwind-protect
                     (progn
                       (loop for service = (%io-iterator-next iterator)
                             until (zerop service)
                             do (unwind-protect
                                    (funcall on-service service)
                                  (%release-iokit-object service)))
                       (funcall on-complete))
                  (unless (zerop iterator)
                    (%release-iokit-object iterator)))))))))
#+darwin
(defun iokit-registry-properties (entry on-value on-error)
  (sb-alien:with-alien ((properties sb-alien:system-area-pointer))
    (setf properties (sb-sys:int-sap 0))
    (let ((status (%io-registry-entry-create-cf-properties
                   entry (sb-alien:addr properties)
                   (sb-sys:int-sap 0) 0)))
      (if (not (zerop status))
          (funcall on-error (list :iokit-error status))
          (with-owned-cf-reference
              (reference properties #'%cf-release)
            (funcall on-value (cf-value-to-lisp reference))
            (funcall on-error :null-properties))))))
