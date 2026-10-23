(in-package #:ntop.platform.darwin)

(defparameter *iops-power-source-fields*
  '(("Type" . :source-type) ("Power Source State" . :source-state)
    ("Current Capacity" . :current-capacity)
    ("Max Capacity" . :maximum-capacity)
    ("Is Charging" . :charging-p)))

(defparameter *apple-smart-battery-fields*
  '(("ExternalConnected" . :external-connected-p) ("IsCharging" . :charging-p)
    ("FullyCharged" . :fully-charged-p)
    ("AppleRawMaxCapacity" . :raw-max-capacity)
    ("DesignCapacity" . :design-capacity)
    ("Amperage" . :amperage)
    ("Voltage" . :voltage)
    ("TimeRemaining" . :time-remaining-minutes)))

#+darwin
(define-darwin-ffi
  (:routine %iops-copy-power-sources-info "IOPSCopyPowerSourcesInfo"
    sb-alien:system-area-pointer)
  (:routine %iops-copy-power-sources-list "IOPSCopyPowerSourcesList"
    sb-alien:system-area-pointer
    (source-info sb-alien:system-area-pointer))
  (:routine %iops-get-power-source-description "IOPSGetPowerSourceDescription"
    sb-alien:system-area-pointer
    (source-info sb-alien:system-area-pointer)
    (source sb-alien:system-area-pointer))
  (:routine %io-registry-entry-create-cf-property
    "IORegistryEntryCreateCFProperty" sb-alien:system-area-pointer
    (entry sb-alien:unsigned-int)
    (key sb-alien:system-area-pointer)
    (allocator sb-alien:system-area-pointer)
    (options sb-alien:unsigned-int)))

#+darwin
(defun %read-selected-cf-fields (dictionary fields)
  (loop for (native-name . property-name) in fields
        append
        (let ((found-p nil)
              (value nil))
          (cf-dictionary-get-value
           dictionary native-name
           (lambda (property)
             (setf found-p t
                   value property))
           (lambda () nil))
          (when found-p
            (list property-name value)))))

#+darwin
(defun %read-iops-power-source-fields (source-info sources)
  (let ((count (%cf-array-get-count sources)))
    (when (minusp count)
      (error "IOPSCopyPowerSourcesList returned a negative count."))
    (loop for index below count
          for source = (%cf-array-get-value-at-index sources index)
          for description = (%iops-get-power-source-description
                             source-info source)
          collect
          (if (cf-null-reference-p description)
              (error "IOPSGetPowerSourceDescription returned null.")
              (%read-selected-cf-fields
               description *iops-power-source-fields*)))))

#+darwin
(defun iokit-registry-property (entry name on-value on-missing)
  (with-owned-cf-reference
      (key-reference
       (%cf-string-create-with-c-string (sb-sys:int-sap 0) name #x08000100)
       #'%cf-release)
    (call-with-owned-cf-reference
     (%io-registry-entry-create-cf-property
      entry key-reference (sb-sys:int-sap 0) 0)
     #'%cf-release
     (lambda (property-reference)
       (funcall on-value (cf-value-to-lisp property-reference)))
     on-missing)
    (funcall on-missing)))

#+darwin
(defun %read-selected-iokit-fields (entry fields)
  (loop for (native-name . property-name) in fields
        append
        (let ((found-p nil)
              (value nil))
          (iokit-registry-property
           entry native-name
           (lambda (property)
             (setf found-p t
                   value property))
           (lambda () nil))
          (when found-p
            (list property-name value)))))

#+darwin
(defun call-with-apple-smart-battery-properties (on-value on-error)
  (let ((properties nil))
    (call-with-iokit-services
     "AppleSmartBattery"
     (lambda (service)
       (push (%read-selected-iokit-fields
              service *apple-smart-battery-fields*)
             properties))
     (lambda ()
       (funcall on-value (nreverse properties)))
     (lambda (reason)
       (if (eq reason :no-matching-dictionary)
           (funcall on-value nil)
           (funcall on-error reason))))))

#+darwin
(defun %apple-smart-battery-source-p (properties)
  (let ((type (getf properties :source-type)))
    (and (stringp type)
         (member type '("InternalBattery" "Battery")
                 :test #'string-equal))))

#+darwin
(defun %merge-apple-smart-battery-fields (sources battery-fields)
  (let ((remaining battery-fields))
    (loop for source in sources
          collect
          (if (and remaining (%apple-smart-battery-source-p source))
              (append (pop remaining) source)
              source))))

#+darwin
(defun call-with-power-source-properties (on-value on-unavailable)
  (with-owned-cf-reference
      (source-info (%iops-copy-power-sources-info) #'%cf-release)
    (with-owned-cf-reference
        (sources (%iops-copy-power-sources-list source-info) #'%cf-release)
      (let ((source-fields
              (%read-iops-power-source-fields source-info sources)))
        (if source-fields
            (call-with-apple-smart-battery-properties
             (lambda (battery-fields)
               (funcall on-value
                        (%merge-apple-smart-battery-fields
                         source-fields battery-fields)))
             on-unavailable)
            (funcall on-value nil)))
      (funcall on-unavailable :no-power-source-list))
    (funcall on-unavailable :no-power-source-info)))
