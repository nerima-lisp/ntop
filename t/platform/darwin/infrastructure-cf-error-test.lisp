(in-package #:ntop.test)

(cl-weave:describe "CoreFoundation and IOKit error paths"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "rejects unsupported types and malformed CF values"
                                       (cl-weave:with-mocked-functions
                                        (((symbol-function
                                           'ntop.platform.darwin::%cf-get-type-id)
                                          (lambda (object)
                                            (declare (ignore object))
                                            99))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-boolean-type-id)
                                          (lambda ()
                                            1))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-dictionary-type-id)
                                          (lambda ()
                                            2))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-string-type-id)
                                          (lambda ()
                                            3))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-number-type-id)
                                          (lambda ()
                                            4))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-array-type-id)
                                          (lambda ()
                                            5))
                                         ((symbol-function
                                           'ntop.platform.darwin::%cf-data-type-id)
                                          (lambda ()
                                            6)))
                                        (cl-weave:expect
                                         (lambda ()
                                           (ntop.platform.darwin::%cf-object-kind
                                            (sb-sys:int-sap 1)))
                                         :to-throw
                                         'error))
                                       (let ((maximum -1)
                                             (cstring-result 0)
                                             (float-number 1)
                                             (data-length -1))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%cf-string-get-length)
                                            (lambda (object)
                                              (declare (ignore object))
                                              1))
                                           ((symbol-function
                                             (find-symbol
                                              "%CF-STRING-GET-MAXIMUM-SIZE-FOR-ENCODING"
                                              "NTOP.PLATFORM.DARWIN"))
                                            (lambda (size encoding)
                                              (declare (ignore size encoding))
                                              maximum))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-string-get-c-string)
                                            (lambda
                                                (object buffer size encoding)
                                              (declare (ignore object
                                                               buffer
                                                               size
                                                               encoding))
                                              cstring-result))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-number-is-float-type)
                                            (lambda (object)
                                              (declare (ignore object))
                                              float-number))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-number-get-value)
                                            (lambda (object type value)
                                              (declare (ignore object
                                                               type
                                                               value))
                                              0))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-array-get-count)
                                            (lambda (array)
                                              (declare (ignore array))
                                              -1))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-dictionary-get-count)
                                            (lambda (dictionary)
                                              (declare (ignore dictionary))
                                              -1))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-data-get-length)
                                            (lambda (data)
                                              (declare (ignore data))
                                              data-length))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-data-get-byte-ptr)
                                            (lambda (data)
                                              (declare (ignore data))
                                              (sb-sys:int-sap 0)))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-object-kind)
                                            (lambda (object)
                                              (declare (ignore object))
                                              :string)))
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-string-value
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (setf maximum 8)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-string-value
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-number-value
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (setf float-number 0)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-number-value
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-array-tree
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-dictionary-tree
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-data-tree
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (setf data-length 1)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%cf-data-tree
                                              (sb-sys:int-sap 1)))
                                           :to-throw
                                           'error)
                                          (let ((*cf-object-readers* nil))
                                            (cl-weave:expect
                                             (lambda ()
                                               (ntop.platform.darwin::%cf-object-tree
                                                (sb-sys:int-sap 1)))
                                             :to-throw
                                             'error)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "uses the missing continuation when key creation fails"
                                       (let ((value nil)
                                             (missing nil)
                                             (releases nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%cf-string-create-with-c-string)
                                            (lambda (allocator key encoding)
                                              (declare (ignore allocator
                                                               key
                                                               encoding))
                                              (sb-sys:int-sap 0)))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-release)
                                            (lambda (reference)
                                              (push reference releases))))
                                          (ntop.platform.darwin:cf-dictionary-get-value
                                           (sb-sys:int-sap 1)
                                           "key"
                                           (lambda (found)
                                             (setf value found))
                                           (lambda ()
                                             (setf missing :missing)))
                                          (cl-weave:expect value :to-be-null)
                                          (cl-weave:expect missing
                                                           :to-be
                                                           :missing)
                                          (cl-weave:expect releases :to-be-null))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "releases IOKit services and iterators on each exit path"
                                       (let ((matching (sb-sys:int-sap 1))
                                             (status 0)
                                             (iterator 13)
                                             (next-services '(41 0))
                                             (release-status 0)
                                             (released nil)
                                             (services nil)
                                             (complete nil)
                                             (failure nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%io-service-matching)
                                            (lambda (service-class)
                                              (declare (ignore service-class))
                                              matching))
                                           ((symbol-function
                                             (find-symbol
                                              "%IO-SERVICE-GET-MATCHING-SERVICES"
                                              "NTOP.PLATFORM.DARWIN"))
                                            (lambda
                                                (port dictionary
                                                      iterator-pointer)
                                              (declare (ignore port dictionary))
                                              (setf (sb-alien:deref
                                                     iterator-pointer) iterator)
                                              status))
                                           ((symbol-function
                                             'ntop.platform.darwin::%io-iterator-next)
                                            (lambda (current-iterator)
                                              (declare (ignore current-iterator))
                                              (pop next-services)))
                                           ((symbol-function
                                             'ntop.platform.darwin::%io-object-release)
                                            (lambda (object)
                                              (push object released)
                                              release-status)))
                                          (ntop.platform.darwin:call-with-iokit-services
                                           "fixture"
                                           (lambda (service)
                                             (push service services))
                                           (lambda ()
                                             (setf complete t))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect services
                                                           :to-equal
                                                           '(41))
                                          (cl-weave:expect released
                                                           :to-equal
                                                           '(13 41))
                                          (cl-weave:expect complete :to-be t)
                                          (setf status 9
                                                iterator 17
                                                released nil
                                                failure nil)
                                          (ntop.platform.darwin:call-with-iokit-services
                                           "fixture"
                                           (lambda (service)
                                             (declare (ignore service)))
                                           (lambda ()
                                             (setf complete :unexpected))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect failure
                                                           :to-equal
                                                           '(:iokit-error 9))
                                          (cl-weave:expect released
                                                           :to-equal
                                                           '(17))
                                          (setf matching (sb-sys:int-sap 0)
                                                failure nil)
                                          (ntop.platform.darwin:call-with-iokit-services
                                           "fixture"
                                           (lambda (service)
                                             (declare (ignore service)))
                                           (lambda ()
                                             (setf complete :unexpected))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect failure
                                                           :to-be
                                                           :no-matching-dictionary)
                                          (setf matching (sb-sys:int-sap 1)
                                                status 0
                                                iterator 19
                                                next-services '(51 0)
                                                released nil)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin:call-with-iokit-services
                                              "fixture"
                                              (lambda (service)
                                                (declare (ignore service))
                                                (error
                                                 "service callback failed"))
                                              (lambda ()
                                                (setf complete :unexpected))
                                              (lambda (reason)
                                                (setf failure reason))))
                                           :to-throw
                                           "service callback failed")
                                          (cl-weave:expect released
                                                           :to-equal
                                                           '(19 51))
                                          (setf release-status 8)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%release-iokit-object
                                              22))
                                           :to-throw
                                           'error))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "reports IOKit property errors and null property references"
                                       (let ((status 0)
                                             (properties (sb-sys:int-sap 0))
                                             (value nil)
                                             (failure nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             (find-symbol
                                              "%IO-REGISTRY-ENTRY-CREATE-CF-PROPERTIES"
                                              "NTOP.PLATFORM.DARWIN"))
                                            (lambda
                                                (entry output allocator options)
                                              (declare (ignore entry
                                                               allocator
                                                               options))
                                              (setf (sb-alien:deref output) properties)
                                              status)))
                                          (setf status 5)
                                          (ntop.platform.darwin:iokit-registry-properties
                                           7
                                           (lambda (found)
                                             (setf value found))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect failure
                                                           :to-equal
                                                           '(:iokit-error 5))
                                          (setf status 0
                                                failure nil)
                                          (ntop.platform.darwin:iokit-registry-properties
                                           7
                                           (lambda (found)
                                             (setf value found))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect value :to-be-null)
                                          (cl-weave:expect failure
                                                           :to-be
                                                           :null-properties)))))
