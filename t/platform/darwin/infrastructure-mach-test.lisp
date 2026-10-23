(in-package #:ntop.test)

(cl-weave:describe "Darwin Mach boundary"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "releases a host port and reports failures"
                                       (let ((port 41)
                                             (status 0)
                                             (released nil)
                                             (value nil)
                                             (failure nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%mach-host-self)
                                            (lambda ()
                                              port))
                                           ((symbol-function
                                             'ntop.platform.darwin::%mach-port-deallocate)
                                            (lambda (task name)
                                              (declare (ignore task))
                                              (setf released name)
                                              status)))
                                          (ntop.platform.darwin:call-with-mach-host-port
                                           (lambda (found)
                                             (setf value found))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect value :to-be 41)
                                          (cl-weave:expect released :to-be 41)
                                          (setf port 0
                                                released nil)
                                          (ntop.platform.darwin:call-with-mach-host-port
                                           (lambda (found)
                                             (setf value found))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect failure
                                                           :to-be
                                                           :invalid-host-port)
                                          (cl-weave:expect released :to-be-null)
                                          (setf port 41
                                                status 8)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin:call-with-mach-host-port
                                              (lambda (found)
                                                (setf value found))
                                              (lambda (reason)
                                                (setf failure reason))))
                                           :to-throw
                                           'error)
                                          (cl-weave:expect released :to-be 41)
                                          (setf status 0
                                                released nil)
                                          (cl-weave:expect
                                           (lambda ()
                                             (ntop.platform.darwin:call-with-mach-host-port
                                              (lambda (found)
                                                (declare (ignore found))
                                                (error "callback failed"))
                                              (lambda (reason)
                                                (declare (ignore reason)))))
                                           :to-throw
                                           "callback failed")
                                          (cl-weave:expect released :to-be 41))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "decodes Mach timebase success and error continuations"
                                       (let ((status 0)
                                             (numerator nil)
                                             (denominator nil)
                                             (failure nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%mach-timebase-info)
                                            (lambda (info)
                                              (when (zerop status)
                                                (setf (sb-sys:sap-ref-32 info 0) 125)
                                                (setf (sb-sys:sap-ref-32 info 4) 3))
                                              status)))
                                          (ntop.platform.darwin:mach-timebase-info
                                           (lambda (n d)
                                             (setf numerator n
                                                   denominator d))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect numerator :to-be 125)
                                          (cl-weave:expect denominator :to-be 3)
                                          (setf status 7)
                                          (ntop.platform.darwin:mach-timebase-info
                                           (lambda (n d)
                                             (setf numerator n
                                                   denominator d))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect failure
                                                           :to-equal
                                                           '(:mach-error 7)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "passes VM deallocation results through continuations"
                                       (let ((status 0)
                                             (address-and-size nil)
                                             (outcome nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%vm-deallocate)
                                            (lambda (task address size)
                                              (declare (ignore task))
                                              (setf address-and-size (list
                                                                      address
                                                                      size))
                                              status)))
                                          (ntop.platform.darwin:mach-vm-deallocate
                                           (sb-sys:int-sap #x1234)
                                           16
                                           (lambda ()
                                             (setf outcome :released))
                                           (lambda (reason)
                                             (setf outcome reason)))
                                          (cl-weave:expect address-and-size
                                                           :to-equal
                                                           '(4660 16))
                                          (cl-weave:expect outcome
                                                           :to-be
                                                           :released)
                                          (setf status 5)
                                          (ntop.platform.darwin:mach-vm-deallocate
                                           (sb-sys:int-sap #x1234)
                                           16
                                           (lambda ()
                                             (setf outcome :released))
                                           (lambda (reason)
                                             (setf outcome reason)))
                                          (cl-weave:expect outcome
                                                           :to-equal
                                                           '(:mach-error 5)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "reads three load averages and reports a failed call"
                                       (let ((result 3)
                                             (values nil)
                                             (failure nil))
                                         (cl-weave:with-mocked-functions
                                          (((symbol-function
                                             'ntop.platform.darwin::%getloadavg)
                                            (lambda (samples count)
                                              (declare (ignore count))
                                              (when (plusp result)
                                                (setf (sb-sys:sap-ref-double
                                                       samples
                                                       0) 1.25d0)
                                                (setf (sb-sys:sap-ref-double
                                                       samples
                                                       8) 2.5d0)
                                                (setf (sb-sys:sap-ref-double
                                                       samples
                                                       16) 3.75d0))
                                              result)))
                                          (ntop.platform.darwin:mach-read-load-average
                                           (lambda (sample)
                                             (setf values sample))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect
                                           (equalp values
                                                   '(1.25d0 2.5d0 3.75d0))
                                           :to-be
                                           t)
                                          (setf result -1)
                                          (ntop.platform.darwin:mach-read-load-average
                                           (lambda (sample)
                                             (setf values sample))
                                           (lambda (reason)
                                             (setf failure reason)))
                                          (cl-weave:expect (first failure)
                                                           :to-be
                                                           :errno)
                                          (cl-weave:expect
                                           (integerp (second failure))
                                           :to-be
                                           t))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "returns a libc error message"
                                       (cl-weave:expect
                                        (let ((message
                                               (ntop.platform.darwin:darwin-error-string
                                                22)))
                                          (and (stringp message)
                                               (plusp (length message))))
                                        :to-be
                                        t)))
