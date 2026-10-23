(in-package #:ntop.test)

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
