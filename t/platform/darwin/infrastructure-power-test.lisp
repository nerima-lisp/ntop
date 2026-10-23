(in-package #:ntop.test)

(defconstant +battery-boundary-command-timeout-seconds+
  5)
(defvar *battery-boundary-delta-class* nil)

(defconstant +battery-boundary-output-limit+
  8192)

(defun %percent-values-from-output (output)
  (let ((cursor 0)
        (values nil))
    (loop for end = (position #\% output :start cursor)
          while end
          do (let ((start end))
               (loop while (and (plusp start)
                                (digit-char-p (char output (1- start))))
                     do (decf start))
               (when (and (< start end)
                          (or (zerop start)
                              (not (char= (char output (1- start)) #\.))))
                 (let ((value (parse-integer output :start start :end end)))
                   (when (<= 0 value 100)
                     (push value values))))
               (setf cursor (1+ end)))) (nreverse values))) (defun %pmset-battery-reading ()
  (let* ((result
           (host-kit:run-program
            "pmset" '("-g" "batt")
            :timeout +battery-boundary-command-timeout-seconds+
            :max-output-characters +battery-boundary-output-limit+))
         (output (host-kit:process-result-stdout result)))
    (unless (zerop (host-kit:process-result-exit-code result))
      (error "pmset failed while reading battery status."))
    (when (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
      (error "pmset output exceeded its capture limit."))
    (let ((charge
            (or (first (%percent-values-from-output output))
                (error "pmset reported no battery charge percentage.")))
          (status
            (cond
              ((search "not charging" output :test #'char-equal) :unknown)
              ((search "discharging" output :test #'char-equal) :discharging)
              ((search "charging" output :test #'char-equal) :charging)
              ((search "charged" output :test #'char-equal) :full)
              (t nil))))
      (unless status
        (error "pmset reported no supported battery status."))
      (values charge status)))) (cl-weave:describe
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
  "brackets the native charge and status read with pmset"
  (let ((devices nil)
        (unavailable nil))
    (multiple-value-bind (before before-state) (%pmset-battery-reading)
      (ntop.battery.infrastructure::read-darwin-battery-devices
       (lambda (value)
         (setf devices value))
       (lambda (reason)
         (setf unavailable reason)))
      (multiple-value-bind (after after-state) (%pmset-battery-reading)
        (let* ((device (first devices))
               (charge
                 (and device
                      (ntop.battery.domain:battery-device-charge-percent device)))
               (state
                 (and device
                      (ntop.battery.domain:battery-device-state device))))
          (setf *battery-boundary-delta-class*
                (if (and (= before after) (eq before-state after-state))
                    :stable
                    :changed))
          (cl-weave:expect unavailable :to-be-null)
          (cl-weave:expect (numberp charge) :to-be t)
          (cl-weave:expect
           (<= (min before after) (round charge) (max before after))
           :to-be
           t)
          (cl-weave:expect
           (not (null (member state (list before-state after-state) :test #'eq)))
           :to-be
           t))))))) (cl-weave:describe
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
      (lambda ()
        (incf missing-calls)))
     (setf key-reference (sb-sys:int-sap 1))
     (ntop.platform.darwin::iokit-registry-property
      (sb-sys:int-sap 9) "FixtureProperty"
      (lambda (property)
        (incf value-calls)
        (setf value property))
      (lambda ()
        (incf missing-calls)))
     (setf property-reference (sb-sys:int-sap 2))
     (ntop.platform.darwin::iokit-registry-property
      (sb-sys:int-sap 9) "FixtureProperty"
      (lambda (property)
        (incf value-calls)
        (setf value property))
      (lambda ()
        (incf missing-calls)))
     (cl-weave:expect value :to-be :fixture-value)
     (cl-weave:expect value-calls :to-be 1)
     (cl-weave:expect missing-calls :to-be 2)
     (cl-weave:expect property-reads :to-be 2)
     (cl-weave:expect conversions :to-be 1)
     (cl-weave:expect releases :to-be 3))))
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
      'error)))
 (cl-weave:it-run-if
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
        (unavailable-calls 0))
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
     (cl-weave:expect service-class :to-be "AppleSmartBattery")
     (cl-weave:expect
      value
      :to-equal
      '((:external-connected-p :true) (:amperage -1200)))
     (cl-weave:expect value-calls :to-be 1)
     (cl-weave:expect unavailable-calls :to-be 0)
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
     (cl-weave:expect value :to-be-null)
     (cl-weave:expect value-calls :to-be 1)
     (cl-weave:expect unavailable-calls :to-be 0)
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
     (cl-weave:expect value-calls :to-be 0)
     (cl-weave:expect unavailable :to-be :mocked-iokit-error)
     (cl-weave:expect unavailable-calls :to-be 1))))
 (cl-weave:it-run-if
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
        (battery-properties
          '((:amperage -1200 :voltage 12000)))
        (battery-error nil)
        (apple-calls 0)
        (release-calls 0))
    (labels
        ((read-source ()
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
             (values value unavailable value-calls unavailable-calls))))
      (cl-weave:with-mocked-functions
       (((symbol-function
          'ntop.platform.darwin::%iops-copy-power-sources-info)
         (lambda ()
           source-info))
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
       (multiple-value-bind
           (value unavailable value-calls unavailable-calls)
           (read-source)
         (cl-weave:expect value :to-be-null)
         (cl-weave:expect unavailable :to-be :no-power-source-info)
         (cl-weave:expect value-calls :to-be 0)
         (cl-weave:expect unavailable-calls :to-be 1)
         (cl-weave:expect release-calls :to-be 0))
       (setf source-info (sb-sys:int-sap 1)
             sources (sb-sys:int-sap 0))
       (multiple-value-bind
           (value unavailable value-calls unavailable-calls)
           (read-source)
         (cl-weave:expect value :to-be-null)
         (cl-weave:expect unavailable :to-be :no-power-source-list)
         (cl-weave:expect value-calls :to-be 0)
         (cl-weave:expect unavailable-calls :to-be 1)
         (cl-weave:expect release-calls :to-be 1))
       (setf sources (sb-sys:int-sap 2)
             array-count 0)
       (multiple-value-bind
           (value unavailable value-calls unavailable-calls)
           (read-source)
         (cl-weave:expect value :to-be-null)
         (cl-weave:expect unavailable :to-be-null)
         (cl-weave:expect value-calls :to-be 1)
         (cl-weave:expect unavailable-calls :to-be 0)
         (cl-weave:expect apple-calls :to-be 0)
         (cl-weave:expect release-calls :to-be 3))
       (setf array-count 1)
       (multiple-value-bind
           (value unavailable value-calls unavailable-calls)
           (read-source)
         (cl-weave:expect unavailable :to-be-null)
         (cl-weave:expect
          value
          :to-equal
          '((:source-type "InternalBattery"
             :source-state "Battery Power"
             :current-capacity 75
             :maximum-capacity 100
             :amperage -1200
             :voltage 12000)))
         (cl-weave:expect value-calls :to-be 1)
         (cl-weave:expect unavailable-calls :to-be 0)
         (cl-weave:expect apple-calls :to-be 1)
         (cl-weave:expect release-calls :to-be 5))
       (setf battery-error :mocked-iokit-error)
       (multiple-value-bind
           (value unavailable value-calls unavailable-calls)
           (read-source)
         (cl-weave:expect value :to-be-null)
         (cl-weave:expect unavailable :to-be :mocked-iokit-error)
         (cl-weave:expect value-calls :to-be 0)
         (cl-weave:expect unavailable-calls :to-be 1)
         (cl-weave:expect apple-calls :to-be 2)
         (cl-weave:expect release-calls :to-be 7))))))))