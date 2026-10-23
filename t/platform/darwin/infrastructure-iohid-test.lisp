(in-package #:ntop.test)
(defmacro with-iohid-mocked-functions (bindings &body body)
  `(let ((locked-packages
           (remove-if-not
            (function sb-ext:package-locked-p)
            (list (find-package "SB-EXT")
                  (find-package "SB-SYS")
                  (find-package "SB-ALIEN")))))
     (unwind-protect
          (progn
            (dolist (package locked-packages)
              (sb-ext:unlock-package package))
            (cl-weave:with-mocked-functions ,bindings ,@body))
       (dolist (package locked-packages)
         (sb-ext:lock-package package)))))

(cl-weave:describe "Darwin IOHID property and service records"
                   (cl-weave:it-each
                    ((nil nil) ("" nil) (17 nil) ("Sensor" "Sensor"))
                    "accepts Product value ~S as ~S"
                    (value expected)
                    (cl-weave:expect
                     (ntop.platform.darwin::%darwin-iohid-product-name value)
                     :to-equal
                     expected))
                   (cl-weave:it
                    "keeps every named service with a distinct identity"
                    (let* ((first-handle
                            (ntop.platform.darwin::%make-darwin-iohid-service-handle
                             nil
                             101))
                           (second-handle
                            (ntop.platform.darwin::%make-darwin-iohid-service-handle
                             nil
                             102))
                           (records
                            (remove nil
                                    (list
                                     (ntop.platform.darwin::%darwin-iohid-service-record
                                      "Sensor"
                                      first-handle)
                                     (ntop.platform.darwin::%darwin-iohid-service-record
                                      "Sensor"
                                      second-handle)
                                     (ntop.platform.darwin::%darwin-iohid-service-record
                                      nil
                                      (ntop.platform.darwin::%make-darwin-iohid-service-handle
                                       nil
                                       103))))))
                      (cl-weave:expect (length records) :to-be 2)
                      (cl-weave:expect (mapcar #'second records)
                                       :to-equal
                                       '("Sensor" "Sensor"))
                      (cl-weave:expect (mapcar #'first records)
                                       :to-equal
                                       '(101 102)))))
(cl-weave:describe "Darwin IOHID resource and error paths"
  (cl-weave:it-run-if
   (member :darwin *features*)
   "retains service handles, releases both references once, and cleans up failures"
   (let ((service (sb-sys:int-sap 3))
         (client (sb-sys:int-sap 4))
         (retained-client (sb-sys:int-sap 5))
         (retain-result (sb-sys:int-sap 5))
         (fail-finalize nil)
         (released nil)
         (finalized nil)
         (cancelled nil))
     (with-iohid-mocked-functions (((symbol-function 'ntop.platform.darwin::%cf-retain)
        (lambda (object)
          (declare (ignore object))
          retain-result))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (object)
          (push object released)))
       ((symbol-function 'sb-ext:finalize)
        (lambda (object function &key dont-save)
          (declare (ignore function dont-save))
          (when fail-finalize (error "finalizer setup failed"))
          (setf finalized object)
          object))
       ((symbol-function 'sb-ext:cancel-finalization)
        (lambda (object)
          (push object cancelled))))
      (let ((handle
              (ntop.platform.darwin::%make-retained-darwin-iohid-service-handle
               service 30 client)))
        (cl-weave:expect (sb-sys:sap-int (ntop.platform.darwin::darwin-iohid-service-handle-client-pointer handle)) :to-be (sb-sys:sap-int retained-client))
        (cl-weave:expect finalized :to-be handle)
        (ntop.platform.darwin::%release-darwin-iohid-service-handle handle)
        (ntop.platform.darwin::%release-darwin-iohid-service-handle handle)
        (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list retained-client service)))
        (cl-weave:expect cancelled :to-equal (list handle))
        (setf retain-result nil
              released nil)
        (cl-weave:expect
         (lambda ()
           (ntop.platform.darwin::%make-retained-darwin-iohid-service-handle
            service 31 client))
         :to-throw
         'error)
        (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list service)))
        (setf retain-result retained-client
              fail-finalize t
              released nil)
        (cl-weave:expect
         (lambda ()
           (ntop.platform.darwin::%make-retained-darwin-iohid-service-handle
            service 32 client))
         :to-throw
         'error)
        (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list retained-client service)))))))
  (cl-weave:it-run-if
   (member :darwin *features*)
   "creates and releases matching dictionary values on success and failure"
   (let ((keys (list (sb-sys:int-sap 10) (sb-sys:int-sap 11)))
         (numbers (list (sb-sys:int-sap 12) (sb-sys:int-sap 13)))
         (dictionary (sb-sys:int-sap 14))
         (released nil)
         (number-fails nil)
         (calls 0))
     (with-iohid-mocked-functions (((symbol-function
        'ntop.platform.darwin::%cf-string-create-with-c-string)
        (lambda (allocator name encoding)
          (declare (ignore allocator name encoding))
          (pop keys)))
       ((symbol-function 'ntop.platform.darwin::%make-iohid-number)
        (lambda (value)
          (declare (ignore value))
          (incf calls)
          (when number-fails (error "number creation failed"))
          (pop numbers)))
       ((symbol-function 'ntop.platform.darwin::%cf-dictionary-create)
        (lambda (allocator key-array value-array count key-callbacks
                 value-callbacks)
          (declare (ignore allocator key-array value-array key-callbacks
                           value-callbacks))
          (cl-weave:expect count :to-be 2)
          dictionary))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (reference)
          (push reference released)))
       ((symbol-function 'sb-sys:foreign-symbol-sap)
        (lambda (name &optional errorp)
          (declare (ignore name errorp))
          (sb-sys:int-sap 15))))
      (cl-weave:expect
       (ntop.platform.darwin::%make-iohid-matching-dictionary)
       :to-be
       dictionary)
      (cl-weave:expect (length released) :to-be 4)
      (setf keys (list (sb-sys:int-sap 20) nil)
            released nil
            calls 0)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%make-iohid-matching-dictionary))
       :to-throw
       'error)
      (cl-weave:expect calls :to-be 0)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list (sb-sys:int-sap 20))))
      (setf keys (list (sb-sys:int-sap 21) (sb-sys:int-sap 22))
            released nil
            number-fails t)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%make-iohid-matching-dictionary))
       :to-throw
       'error)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list (sb-sys:int-sap 22) (sb-sys:int-sap 21))))
      (setf keys (list (sb-sys:int-sap 23) (sb-sys:int-sap 24))
            numbers (list (sb-sys:int-sap 25) (sb-sys:int-sap 26))
            dictionary nil
            number-fails nil
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%make-iohid-matching-dictionary))
       :to-throw
       'error)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list (sb-sys:int-sap 26) (sb-sys:int-sap 25)
                             (sb-sys:int-sap 24) (sb-sys:int-sap 23)))))))
  (cl-weave:it-run-if
 (member :darwin *features*)
 "reads Product values and copies only named services"
 (let* ((client (sb-sys:int-sap 44))
        (services (list (sb-sys:int-sap 40)
                        (sb-sys:int-sap 41)
                        (sb-sys:int-sap 42)
                        (sb-sys:int-sap 43)))
        (properties (list (sb-sys:int-sap 55)
                          nil
                          (sb-sys:int-sap 56)
                          (sb-sys:int-sap 57)))
        (products (list "Sensor" nil "Fan" 17))
        (service-references (list (sb-sys:int-sap 70)
                                  (sb-sys:int-sap 71)
                                  (sb-sys:int-sap 72)
                                  (sb-sys:int-sap 73)))
        (client-reference (sb-sys:int-sap 74))
        (product-key (sb-sys:int-sap 50))
        (array-count 4)
        (fail-service-index nil)
        (released nil)
        (retained nil)
        (finalized nil)
        (cancelled nil))
   (with-iohid-mocked-functions
       (((symbol-function
          'ntop.platform.darwin::%cf-array-get-count)
         (lambda (array)
           (declare (ignore array))
           array-count))
        ((symbol-function
          'ntop.platform.darwin::%cf-string-create-with-c-string)
         (lambda (allocator name encoding)
           (declare (ignore allocator name encoding))
           product-key))
        ((symbol-function
          'ntop.platform.darwin::%cf-array-get-value-at-index)
         (lambda (array index)
           (declare (ignore array))
           (nth index services)))
        ((symbol-function
          'ntop.platform.darwin::%iohid-service-client-copy-property)
         (lambda (service key)
           (declare (ignore key))
           (nth (position service services :test #'eq) properties)))
        ((symbol-function 'ntop.platform.darwin::cf-value-to-lisp)
         (lambda (reference)
           (nth (position reference properties :test #'eq) products)))
        ((symbol-function 'ntop.platform.darwin::%cf-retain)
         (lambda (object)
           (if (eq object client)
               client-reference
               (let ((index (position object services :test #'eq)))
                 (unless (eql index fail-service-index)
                   (nth index service-references))))))
        ((symbol-function 'ntop.platform.darwin::%cf-release)
         (lambda (reference)
           (push reference released)))
        ((symbol-function 'sb-ext:finalize)
         (lambda (object function &key dont-save)
           (declare (ignore function dont-save))
           (push object finalized)
           object))
        ((symbol-function 'sb-ext:cancel-finalization)
         (lambda (object)
           (push object cancelled))))
     (let ((records
             (ntop.platform.darwin::%copy-named-iohid-services
              (sb-sys:int-sap 51)
              client
              (lambda (handle)
                (push handle retained)))))
       (cl-weave:expect (mapcar #'second records) :to-equal '("Sensor" "Fan"))
       (cl-weave:expect (mapcar #'first records) :to-equal '(70 72))
       (cl-weave:expect (length retained) :to-be 2)
       (cl-weave:expect (length finalized) :to-be 2)
       (cl-weave:expect
        (every
         (lambda (record)
           (= (sb-sys:sap-int
               (ntop.platform.darwin::darwin-iohid-service-handle-client-pointer
                (third record)))
              (sb-sys:sap-int client-reference)))
         records)
        :to-be
        t)
       (dolist (record records)
         (ntop.platform.darwin::%release-darwin-iohid-service-handle
          (third record)))
       (cl-weave:expect (length cancelled) :to-be 2)
       (cl-weave:expect (length released) :to-be 8))
     (setf fail-service-index 0)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%copy-named-iohid-service
         (first services)
         product-key
         client
         #'identity))
      :to-throw
      'error)
     (setf array-count -1)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%copy-named-iohid-services
         (sb-sys:int-sap 51)
         client
         #'identity))
      :to-throw
      'error)
     (setf array-count 4
           product-key nil)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%copy-named-iohid-services
         (sb-sys:int-sap 51)
         client
         #'identity))
      :to-throw
      'error))))
  (cl-weave:it-run-if
   (member :darwin *features*)
   "cleans enumeration resources when service lookup fails"
   (let ((client (sb-sys:int-sap 60))
         (matching (sb-sys:int-sap 61))
         (services (sb-sys:int-sap 62))
         (handle
           (ntop.platform.darwin::%make-darwin-iohid-service-handle
            (sb-sys:int-sap 63) 63))
         (client-result (sb-sys:int-sap 60))
         (matching-result (sb-sys:int-sap 61))
         (services-result (sb-sys:int-sap 62))
         (copy-fails nil)
         (released nil)
         (released-handles nil))
     (with-iohid-mocked-functions (((symbol-function
        'ntop.platform.darwin::%iohid-event-system-client-create)
        (lambda (allocator)
          (declare (ignore allocator))
          client-result))
       ((symbol-function 'ntop.platform.darwin::%make-iohid-matching-dictionary)
        (lambda () matching-result))
       ((symbol-function
        'ntop.platform.darwin::%iohid-event-system-client-set-matching)
        (lambda (actual-client actual-matching)
          (declare (ignore actual-client actual-matching))))
       ((symbol-function
        'ntop.platform.darwin::%iohid-event-system-client-copy-services)
        (lambda (actual-client)
          (declare (ignore actual-client))
          services-result))
       ((symbol-function 'ntop.platform.darwin::%copy-named-iohid-services)
        (lambda (actual-services actual-client on-retained)
          (declare (ignore actual-services actual-client))
          (funcall on-retained handle)
          (when copy-fails (error "service enumeration failed"))
          '((63 "Sensor" :handle))))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (reference)
          (push reference released)))
       ((symbol-function
        'ntop.platform.darwin::%release-darwin-iohid-service-handle)
        (lambda (actual-handle)
          (push actual-handle released-handles))))
      (cl-weave:expect
       (ntop.platform.darwin::%enumerate-darwin-iohid-services)
       :to-equal
       '((63 "Sensor" :handle)))
      (cl-weave:expect (mapcar (function sb-sys:sap-int) (reverse released)) :to-equal (mapcar (function sb-sys:sap-int) (list services matching client)))
      (setf client-result nil
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%enumerate-darwin-iohid-services))
       :to-throw
       'error)
      (cl-weave:expect released :to-be-null)
      (setf client-result client
            matching-result nil
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%enumerate-darwin-iohid-services))
       :to-throw
       'error)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) released) :to-equal (mapcar (function sb-sys:sap-int) (list client)))
      (setf matching-result matching
            services-result nil
            released nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%enumerate-darwin-iohid-services))
       :to-throw
       'error)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) (reverse released)) :to-equal (mapcar (function sb-sys:sap-int) (list matching client)))
      (setf services-result services
            released nil
            copy-fails t)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%enumerate-darwin-iohid-services))
       :to-throw
       'error)
      (cl-weave:expect (mapcar (function sb-sys:sap-int) (reverse released)) :to-equal (mapcar (function sb-sys:sap-int) (list services matching client)))
      (cl-weave:expect released-handles :to-equal (list handle)))))
  (cl-weave:it-run-if
   (member :darwin *features*)
   "returns finite event temperatures and skips invalid values"
   (let ((event nil)
         (value 42.5d0)
         (released nil)
         (handle
           (ntop.platform.darwin::%make-darwin-iohid-service-handle
            (sb-sys:int-sap 70) 70)))
     (with-iohid-mocked-functions (((symbol-function
        'ntop.platform.darwin::%iohid-service-client-copy-event)
        (lambda (service type options timestamp)
          (declare (ignore service type options timestamp))
          event))
       ((symbol-function 'ntop.platform.darwin::%iohid-event-get-float-value)
        (lambda (actual-event field)
          (declare (ignore actual-event field))
          value))
       ((symbol-function 'ntop.platform.darwin::%cf-release)
        (lambda (reference)
          (push reference released))))
      (cl-weave:expect
       (ntop.platform.darwin::%read-darwin-iohid-service-temperature handle)
       :to-be-null)
      (setf event (sb-sys:int-sap 71))
      (cl-weave:expect
       (ntop.platform.darwin::%read-darwin-iohid-service-temperature handle)
       :to-be
       value)
      (setf value (sb-kernel:make-double-float #x7ff80000 0))
      (cl-weave:expect
       (ntop.platform.darwin::%read-darwin-iohid-service-temperature handle)
       :to-be-null)
      (cl-weave:expect (length released) :to-be 2)
      (setf
       (ntop.platform.darwin::darwin-iohid-service-handle-pointer handle)
       nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%read-darwin-iohid-service-temperature handle))
       :to-throw
       'error)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%read-darwin-iohid-service-temperature
          :not-a-handle))
       :to-throw
       'type-error))))
  (cl-weave:it-run-if
 (member :darwin *features*)
 "requires every IOHID symbol and reports the missing name"
 (let ((looked-up nil)
       (missing nil))
   (labels ((lookup (name errorp)
              (declare (ignore errorp))
              (push name looked-up)
              (unless (equal name missing)
                (sb-sys:int-sap 80))))
     (ntop.platform.darwin::%require-darwin-iohid-symbols #'lookup)
     (cl-weave:expect (length looked-up) :to-be 9)
     (setf missing "IOHIDServiceClientCopyEvent"
           looked-up nil)
     (cl-weave:expect
      (lambda ()
        (ntop.platform.darwin::%require-darwin-iohid-symbols #'lookup))
      :to-throw
      'error)
     (cl-weave:expect (length looked-up) :to-be 8)
     (cl-weave:expect (first looked-up) :to-equal missing)))))
(cl-weave:describe "Darwin IOHID API setup"
  (cl-weave:it-run-if
   (member :darwin *features*)
   "passes number type and value to CFNumberCreate"
   (let ((reference (sb-sys:int-sap 8))
         (arguments nil))
     (with-iohid-mocked-functions (((symbol-function 'ntop.platform.darwin::%cf-number-create)
        (lambda (allocator type value)
          (setf arguments
                (list (sb-sys:sap-int allocator)
                      type
                      (sb-sys:sap-ref-32 value 0)))
          reference)))
      (cl-weave:expect
       (ntop.platform.darwin::%make-iohid-number #xff00)
       :to-be
       reference)
      (cl-weave:expect arguments :to-equal '(0 3 #xff00))
      (setf reference nil)
      (cl-weave:expect
       (lambda ()
         (ntop.platform.darwin::%make-iohid-number 5))
       :to-throw
       'error))))
  (cl-weave:it-run-if
   (member :darwin *features*)
   "loads the framework before exposing the two IOHID operations"
   (let ((loaded nil)
         (required 0))
     (with-iohid-mocked-functions (((symbol-function 'sb-alien:load-shared-object)
        (lambda (path &key dont-save)
          (setf loaded (list path dont-save))))
       ((symbol-function
        'ntop.platform.darwin::%require-darwin-iohid-symbols)
        (lambda ()
          (incf required))))
      (let ((api (ntop.platform.darwin:load-darwin-iohid-api)))
        (cl-weave:expect
         loaded
         :to-equal
         (list "/System/Library/Frameworks/IOKit.framework/IOKit" t))
        (cl-weave:expect required :to-be 1)
        (cl-weave:expect
         (getf api :enumerate-services)
         :to-equal
         (symbol-function
          'ntop.platform.darwin::%enumerate-darwin-iohid-services))
        (cl-weave:expect
         (getf api :read-temperature)
         :to-equal
         (symbol-function
          'ntop.platform.darwin::%read-darwin-iohid-service-temperature)))))))

