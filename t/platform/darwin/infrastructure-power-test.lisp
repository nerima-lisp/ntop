(in-package #:ntop.test)
(cl-weave:describe
 "Darwin battery boundary"
 (cl-weave:it "routes null CoreFoundation references to missing only"
   (let ((value-calls 0)
         (missing-calls 0))
     (ntop.platform.darwin::call-with-owned-cf-reference
      nil
      (lambda (reference)
        (declare (ignore reference)))
      (lambda (reference)
        (declare (ignore reference))
        (incf value-calls))
      (lambda ()
        (incf missing-calls)))
     (cl-weave:expect value-calls :to-be 0)
     (cl-weave:expect missing-calls :to-be 1)))
 (cl-weave:it "routes non-null CoreFoundation references to value and release"
   (let ((value-calls 0)
         (release-calls 0)
         (missing-calls 0))
     (ntop.platform.darwin::call-with-owned-cf-reference
      (sb-sys:int-sap 1)
      (lambda (reference)
        (declare (ignore reference))
        (incf release-calls))
      (lambda (reference)
        (declare (ignore reference))
        (incf value-calls))
      (lambda ()
        (incf missing-calls)))
     (cl-weave:expect value-calls :to-be 1)
     (cl-weave:expect release-calls :to-be 1)
     (cl-weave:expect missing-calls :to-be 0)))
 (cl-weave:it-run-if
  (member :darwin *features*)
  "maps explicit native battery state without reading host state"
  (let ((devices nil)
        (unavailable nil))
    (cl-weave:with-mocked-functions
      (((symbol-function
         'ntop.platform.darwin::call-with-power-source-properties)
        (lambda (on-value on-unavailable)
          (declare (ignore on-unavailable))
          (funcall
           on-value
           (list
            (list :source-type "InternalBattery"
                  :source-state "AC Power"
                  :current-capacity 75
                  :maximum-capacity 100
                  :external-connected-p :true
                  :charging-p :false
                  :fully-charged-p :false))))))
      (ntop.battery.infrastructure::read-darwin-battery-devices
       (lambda (value) (setf devices value))
       (lambda (reason) (setf unavailable reason))))
    (cl-weave:expect
     (list unavailable
           (ntop.battery.domain:battery-device-charge-percent (first devices))
           (ntop.battery.domain:battery-device-state (first devices)))
     :to-equal
     (list nil 75 :unknown))))) (cl-weave:describe
 "Mocked Darwin battery power reads"
 (cl-weave:it-run-if
 (member :darwin *features*)
 "routes null and present IOKit property references once"
 (let ((key-reference (sb-sys:int-sap 0))
       (property-reference (sb-sys:int-sap 0))
       (property-reads 0)
       (conversions 0)
       (releases 0)
       (value nil)
       (value-calls 0)
       (missing-calls 0))
   (cl-weave:with-mocked-functions
     (((symbol-function
        'ntop.platform.darwin::%cf-string-create-with-c-string)
       (lambda (allocator name encoding)
         (declare (ignore allocator name encoding))
         key-reference))
      ((symbol-function
        'ntop.platform.darwin::%io-registry-entry-create-cf-property)
       (lambda (entry key allocator options)
         (declare (ignore entry key allocator options))
         (incf property-reads)
         property-reference))
      ((symbol-function 'ntop.platform.darwin::cf-value-to-lisp)
       (lambda (reference)
         (declare (ignore reference))
         (incf conversions)
         :fixture-value))
      ((symbol-function 'ntop.platform.darwin::%cf-release)
       (lambda (reference)
         (declare (ignore reference))
         (incf releases))))
     (ntop.platform.darwin::iokit-registry-property
      (sb-sys:int-sap 9) "FixtureProperty"
      (lambda (property)
        (incf value-calls)
        (setf value property))
      (lambda () (incf missing-calls)))
     (setf key-reference (sb-sys:int-sap 1))
     (ntop.platform.darwin::iokit-registry-property
      (sb-sys:int-sap 9) "FixtureProperty"
      (lambda (property)
        (incf value-calls)
        (setf value property))
      (lambda () (incf missing-calls)))
     (setf property-reference (sb-sys:int-sap 2))
     (ntop.platform.darwin::iokit-registry-property
      (sb-sys:int-sap 9) "FixtureProperty"
      (lambda (property)
        (incf value-calls)
        (setf value property))
      (lambda () (incf missing-calls)))
     (cl-weave:expect
      (list value value-calls missing-calls property-reads conversions releases)
      :to-equal
      '(:fixture-value 1 2 2 1 3)))))
 (cl-weave:it-run-if
  (member :darwin *features*)
  "reads selected IOPS fields and rejects invalid source descriptions"
  (let ((array-count 1)
        (description (sb-sys:int-sap 20))
        (properties
          '(("Type" . "InternalBattery")
            ("Current Capacity" . 75)
            ("Max Capacity" . 100))))
    (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::%cf-array-get-count)
       (lambda (array)
         (declare (ignore array))
         array-count))
      ((symbol-function
        'ntop.platform.darwin::%cf-array-get-value-at-index)
       (lambda (array index)
         (declare (ignore array index))
         (sb-sys:int-sap 11)))
      ((symbol-function
        'ntop.platform.darwin::%iops-get-power-source-description)
       (lambda (source-info source)
         (declare (ignore source-info source))
         description))
      ((symbol-function 'ntop.platform.darwin::cf-dictionary-get-value)
       (lambda (dictionary key on-value on-missing)
         (declare (ignore dictionary))
         (let ((entry (assoc key properties :test #'string=)))
           (if entry
               (funcall on-value (cdr entry))
               (funcall on-missing))))))
     (cl-weave:expect
      (ntop.platform.darwin::%read-iops-power-source-fields
       (sb-sys:int-sap 10) (sb-sys:int-sap 12))
      :to-equal
      '((:source-type "InternalBattery"
         :current-capacity 75
         :maximum-capacity 100)))
     (setf array-count -1)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%read-iops-power-source-fields
         (sb-sys:int-sap 10) (sb-sys:int-sap 12)))
      :to-throw
      'error)
     (setf array-count 1
           description (sb-sys:int-sap 0))
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%read-iops-power-source-fields
         (sb-sys:int-sap 10) (sb-sys:int-sap 12)))
      :to-throw
      'error)))) (cl-weave:it-run-if
 (member :darwin *features*)
 "handles AppleSmartBattery service success, absence, and errors"
 (let ((mode :success)
       (service-class nil)
       (property-values
         '(("ExternalConnected" . :true)
           ("Amperage" . -1200)))
       (value nil)
       (unavailable nil)
       (value-calls 0)
       (unavailable-calls 0)
       (observations nil))
   (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin::call-with-iokit-services)
       (lambda (class on-service on-complete on-error)
         (setf service-class class)
         (case mode
           (:success
            (funcall on-service (sb-sys:int-sap 7))
            (funcall on-complete))
           (:no-match
            (funcall on-error :no-matching-dictionary))
           (t
            (funcall on-error :mocked-iokit-error)))))
      ((symbol-function 'ntop.platform.darwin::iokit-registry-property)
       (lambda (service name on-value on-missing)
         (declare (ignore service))
         (let ((entry (assoc name property-values :test #'string=)))
           (if entry
               (funcall on-value (cdr entry))
               (funcall on-missing))))))
     (ntop.platform.darwin::call-with-apple-smart-battery-properties
      (lambda (properties)
        (incf value-calls)
        (setf value properties))
      (lambda (reason)
        (incf unavailable-calls)
        (setf unavailable reason)))
     (push (list service-class value value-calls unavailable-calls)
           observations)
     (setf mode :no-match
           value :unset
           unavailable nil
           value-calls 0
           unavailable-calls 0)
     (ntop.platform.darwin::call-with-apple-smart-battery-properties
      (lambda (properties)
        (incf value-calls)
        (setf value properties))
      (lambda (reason)
        (incf unavailable-calls)
        (setf unavailable reason)))
     (push (list value value-calls unavailable-calls) observations)
     (setf mode :error
           value nil
           unavailable nil
           value-calls 0
           unavailable-calls 0)
     (ntop.platform.darwin::call-with-apple-smart-battery-properties
      (lambda (properties)
        (incf value-calls)
        (setf value properties))
      (lambda (reason)
        (incf unavailable-calls)
        (setf unavailable reason)))
     (push (list value unavailable value-calls unavailable-calls)
           observations))
   (cl-weave:expect
    (nreverse observations)
    :to-equal
    '(("AppleSmartBattery" ((:external-connected-p :true :amperage -1200)) 1 0)
      (nil 1 0)
      (nil :mocked-iokit-error 0 1))))) (cl-weave:it-run-if
 (member :darwin *features*)
 "routes IOPS failures and merges battery properties"
 (let ((source-info (sb-sys:int-sap 0))
       (sources (sb-sys:int-sap 0))
       (array-count 0)
       (source-properties
         '(:source-type "InternalBattery"
           :source-state "Battery Power"
           :current-capacity 75
           :maximum-capacity 100))
       (battery-properties '((:amperage -1200 :voltage 12000)))
       (battery-error nil)
       (apple-calls 0)
       (release-calls 0)
       (observations nil))
   (labels
       ((read-source ()
          "Capture the callback result and owned-reference count."
          (let ((value nil)
                (unavailable nil)
                (value-calls 0)
                (unavailable-calls 0))
            (ntop.platform.darwin::call-with-power-source-properties
             (lambda (properties)
               (incf value-calls)
               (setf value properties))
             (lambda (reason)
               (incf unavailable-calls)
               (setf unavailable reason)))
            (list
             (when value
               (mapcar
                (lambda (properties)
                  (list (getf properties :source-type)
                        (getf properties :source-state)
                        (getf properties :current-capacity)
                        (getf properties :maximum-capacity)
                        (getf properties :amperage)
                        (getf properties :voltage)))
                value))
             unavailable value-calls unavailable-calls apple-calls release-calls))))
     (cl-weave:with-mocked-functions
       (((symbol-function
          'ntop.platform.darwin::%iops-copy-power-sources-info)
         (lambda () source-info))
        ((symbol-function
          'ntop.platform.darwin::%iops-copy-power-sources-list)
         (lambda (info)
           (declare (ignore info))
           sources))
        ((symbol-function 'ntop.platform.darwin::%cf-array-get-count)
         (lambda (array)
           (declare (ignore array))
           array-count))
        ((symbol-function
          'ntop.platform.darwin::%cf-array-get-value-at-index)
         (lambda (array index)
           (declare (ignore array index))
           (sb-sys:int-sap 11)))
        ((symbol-function
          'ntop.platform.darwin::%iops-get-power-source-description)
         (lambda (info source)
           (declare (ignore info source))
           (sb-sys:int-sap 12)))
        ((symbol-function
          'ntop.platform.darwin::%read-selected-cf-fields)
         (lambda (description fields)
           (declare (ignore description fields))
           source-properties))
        ((symbol-function 'ntop.platform.darwin::%cf-release)
         (lambda (reference)
           (declare (ignore reference))
           (incf release-calls)))
        ((symbol-function
          'ntop.platform.darwin::call-with-apple-smart-battery-properties)
         (lambda (on-value on-error)
           (incf apple-calls)
           (if battery-error
               (funcall on-error battery-error)
               (funcall on-value battery-properties)))))
       (push (read-source) observations)
       (setf source-info (sb-sys:int-sap 1)
             sources (sb-sys:int-sap 0))
       (push (read-source) observations)
       (setf sources (sb-sys:int-sap 2)
             array-count 0)
       (push (read-source) observations)
       (setf array-count 1)
       (push (read-source) observations)
       (setf battery-error :mocked-iokit-error)
       (push (read-source) observations)))
   (cl-weave:expect
    (nreverse observations)
    :to-equal
    '((nil :no-power-source-info 0 1 0 0)
      (nil :no-power-source-list 0 1 0 1)
      (nil nil 1 0 0 3)
      ((("InternalBattery" "Battery Power" 75 100 -1200 12000)) nil 1 0 1 5)
      (nil :mocked-iokit-error 0 1 2 7))))))

(cl-weave:describe
 "Darwin selected IOKit properties and battery merging"
 (cl-weave:it-run-if
  (member :darwin *features*)
  "omits missing selected IOKit fields"
  (cl-weave:with-mocked-functions
   (((symbol-function 'ntop.platform.darwin::iokit-registry-property)
     (lambda (entry name on-value on-missing)
       (declare (ignore entry))
       (if (string= name "Present")
           (funcall on-value :present-value)
         (funcall on-missing)))))
   (cl-weave:expect
    (ntop.platform.darwin::%read-selected-iokit-fields
     (sb-sys:int-sap 1)
     '(("Present" . :present)
       ("Missing" . :missing)))
    :to-equal
    '(:present :present-value))))
 (cl-weave:it-run-if
 (member :darwin *features*)
 "merges Apple battery fields into the first matching source only"
 (let* ((source-types
          (mapcar
           (lambda (source)
             (not (null (ntop.platform.darwin::%apple-smart-battery-source-p source))))
           '((:source-type 7)
             (:source-type "UPS")
             (:source-type "InternalBattery")
             (:source-type "Battery"))))
        (merged
          (ntop.platform.darwin::%merge-apple-smart-battery-fields
           '((:source-type "UPS" :name "ups")
             (:source-type "InternalBattery" :name "internal")
             (:source-type "Battery" :name "external"))
           '((:amperage -1200))))
        (fields
          (mapcar
           (lambda (source)
             (list (getf source :source-type)
                   (getf source :name)
                   (getf source :amperage)))
           merged)))
   (cl-weave:expect source-types :to-equal '(nil nil t t))
   (cl-weave:expect
    fields
    :to-equal
    '(("UPS" "ups" nil)
      ("InternalBattery" "internal" -1200)
      ("Battery" "external" nil)))))
 )
