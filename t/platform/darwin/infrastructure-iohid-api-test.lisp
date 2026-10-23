(in-package #:ntop.test)

(cl-weave:describe
  "Darwin IOHID API setup"
  (cl-weave:it-run-if
    (member :darwin *features*)
    "passes number type and value to CFNumberCreate"
    (let ((reference (sb-sys:int-sap 8))
          (arguments nil))
      (with-iohid-mocked-functions
       (((symbol-function 'ntop.platform.darwin::%cf-number-create)
         (lambda (allocator type value)
           (setf arguments (list (sb-sys:sap-int allocator)
                                 type
                                 (sb-sys:sap-ref-32 value 0)))
           reference)))
       (cl-weave:expect (ntop.platform.darwin::%make-iohid-number #xff00)
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
      (with-iohid-mocked-functions
       (((symbol-function 'sb-alien:load-shared-object)
         (lambda (path &key dont-save)
           (setf loaded (list path dont-save))))
        ((symbol-function 'ntop.platform.darwin::%require-darwin-iohid-symbols)
         (lambda ()
           (incf required))))
       (let ((api (ntop.platform.darwin:load-darwin-iohid-api)))
         (cl-weave:expect loaded
                          :to-equal
                          (list
                           "/System/Library/Frameworks/IOKit.framework/IOKit"
                           t))
         (cl-weave:expect required :to-be 1)
         (cl-weave:expect (getf api :enumerate-services)
                          :to-equal
                          (symbol-function
                           'ntop.platform.darwin::%enumerate-darwin-iohid-services))
         (cl-weave:expect (getf api :read-temperature)
                          :to-equal
                          (symbol-function
                           'ntop.platform.darwin::%read-darwin-iohid-service-temperature)))))))
