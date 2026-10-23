(in-package #:ntop.test)

(cl-weave:describe "Darwin IOKit block-storage readers"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "decodes an IOKit BSD name and releases both CoreFoundation references"
                                       (let ((release-count 0))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%cf-string-create-with-c-string)
                                            (lambda (allocator value encoding)
                                              (declare (ignore allocator
                                                               value
                                                               encoding))
                                              (sb-sys:int-sap 1)))
                                           ((symbol-function
                                             'ntop.platform.darwin::%io-registry-entry-search-cf-property)
                                            (lambda
                                                (service plane
                                                         key
                                                         allocator
                                                         options)
                                              (declare (ignore service
                                                               plane
                                                               key
                                                               allocator
                                                               options))
                                              (sb-sys:int-sap 2)))
                                           ((symbol-function
                                             'ntop.platform.darwin:cf-value-to-lisp)
                                            (lambda (reference)
                                              (declare (ignore reference))
                                              "disk9"))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-release)
                                            (lambda (reference)
                                              (declare (ignore reference))
                                              (incf release-count))))
                                          (cl-weave:expect
                                           (ntop.platform.darwin::%darwin-iokit-device-name
                                            17)
                                           :to-equal
                                           "disk9"))
                                         (cl-weave:expect release-count
                                                          :to-be
                                                          2)))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "rejects empty or non-string IOKit BSD names"
                                       (dolist (name '(nil ""))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%cf-string-create-with-c-string)
                                            (lambda (allocator value encoding)
                                              (declare (ignore allocator
                                                               value
                                                               encoding))
                                              (sb-sys:int-sap 1)))
                                           ((symbol-function
                                             'ntop.platform.darwin::%io-registry-entry-search-cf-property)
                                            (lambda
                                                (service plane
                                                         key
                                                         allocator
                                                         options)
                                              (declare (ignore service
                                                               plane
                                                               key
                                                               allocator
                                                               options))
                                              (sb-sys:int-sap 2)))
                                           ((symbol-function
                                             'ntop.platform.darwin:cf-value-to-lisp)
                                            (lambda (reference)
                                              (declare (ignore reference))
                                              name))
                                           ((symbol-function
                                             'ntop.platform.darwin::%cf-release)
                                            (lambda (reference)
                                              (declare (ignore reference))
                                              nil)))
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin::%darwin-iokit-device-name
                                              17))
                                           :to-throw
                                           'error))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "reads validated byte counters from IOKit properties"
                                       (let ((properties
                                              (make-hash-table :test #'equal))
                                             (statistics
                                              (make-hash-table :test #'equal)))
                                         (setf (gethash "Statistics" properties) statistics
                                               (gethash "Bytes (Read)"
                                                        statistics) 1234
                                               (gethash "Bytes (Write)"
                                                        statistics) 5678)
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%darwin-iokit-device-name)
                                            (lambda (service)
                                              (declare (ignore service))
                                              "disk9"))
                                           ((symbol-function
                                             'ntop.platform.darwin:iokit-registry-properties)
                                            (lambda (service on-value on-error)
                                              (declare (ignore service on-error))
                                              (funcall on-value properties))))
                                          (cl-weave:expect
                                           (ntop.platform.darwin::%darwin-iokit-device-statistics
                                            17)
                                           :to-equal
                                           '(:device-name "disk9"
                                                          :read-bytes
                                                          1234
                                                          :write-bytes
                                                          5678)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "rejects failed and malformed IOKit statistics dictionaries"
                                       (dolist
                                           (scenario
                                            '(:reader-failed
                                              :properties-not-dictionary
                                              :statistics-not-dictionary
                                              :missing-counter
                                              :negative-counter))
                                         (let ((properties
                                                (make-hash-table :test #'equal))
                                               (reader-value nil))
                                           (setf reader-value properties)
                                           (case scenario
                                             (:properties-not-dictionary
                                              (setf reader-value "invalid"))
                                             ((:statistics-not-dictionary
                                               :missing-counter
                                               :negative-counter)
                                              (let ((dictionary
                                                     (make-hash-table :test
                                                                      #'equal)))
                                                (setf (gethash "Statistics"
                                                               properties) dictionary)
                                                (case scenario
                                                  (:statistics-not-dictionary
                                                   (setf (gethash "Statistics"
                                                                  properties) nil))
                                                  (:missing-counter
                                                   (setf (gethash "Bytes (Read)"
                                                                  dictionary) 1))
                                                  (:negative-counter
                                                   (setf (gethash "Bytes (Read)"
                                                                  dictionary) -1
                                                         (gethash
                                                          "Bytes (Write)"
                                                          dictionary) 2))))))
                                           (cl-weave:with-mocked-functions
                                            (((symbol-function
                                               'ntop.platform.darwin::%darwin-iokit-device-name)
                                              (lambda (service)
                                                (declare (ignore service))
                                                "disk9"))
                                             ((symbol-function
                                               'ntop.platform.darwin:iokit-registry-properties)
                                              (lambda
                                                  (service on-value on-error)
                                                (declare (ignore service))
                                                (if (eq scenario :reader-failed)
                                                    (funcall on-error
                                                             :permission-denied)
                                                    (funcall on-value
                                                             reader-value)))))
                                            (cl-weave:expect
                                             (lambda ()
                                               (ntop.platform.darwin::%darwin-iokit-device-statistics
                                                17))
                                             :to-throw
                                             'error)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "forwards IOKit enumeration failures"
                                       (let ((value nil)
                                             (reason nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::call-with-iokit-services)
                                            (lambda
                                                (service-class on-service
                                                               on-complete
                                                               on-error)
                                              (declare (ignore service-class
                                                               on-service
                                                               on-complete))
                                              (funcall on-error
                                                       :permission-denied))))
                                          (ntop.platform.darwin:read-darwin-block-storage-stats
                                           (lambda (devices)
                                             (setf value devices))
                                           (lambda (unavailable)
                                             (setf reason unavailable))))
                                         (cl-weave:expect value :to-be-null)
                                         (cl-weave:expect reason
                                                          :to-be
                                                          :permission-denied))))

(cl-weave:describe "Darwin IOKit enumeration error handling"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "converts an enumeration exception into an unavailable result"
                                       (let ((value nil)
                                             (reason nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::call-with-iokit-services)
                                            (lambda
                                                (service-class on-service
                                                               on-complete
                                                               on-error)
                                              (declare (ignore service-class
                                                               on-service
                                                               on-complete
                                                               on-error))
                                              (error "IOKit enumeration failed"))))
                                          (ntop.platform.darwin:read-darwin-block-storage-stats
                                           (lambda (devices)
                                             (setf value devices))
                                           (lambda (unavailable)
                                             (setf reason unavailable))))
                                         (cl-weave:expect value :to-be-null)
                                         (cl-weave:expect (first reason)
                                                          :to-be
                                                          :iokit-read-error)
                                         (cl-weave:expect
                                          (typep (second reason) 'error)
                                          :to-be
                                          t))))
