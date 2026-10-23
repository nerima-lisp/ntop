(in-package #:ntop.test)

(cl-weave:describe
 "Darwin filesystem readers"
 (cl-weave:it-run-if (member :darwin *features*)
                     "covers getfsstat success and failure responses"
                     (let ((entry
                            (%make-statfs-entry "/dev/disk9s1"
                                                "/"
                                                "apfs"
                                                4096
                                                1000
                                                300
                                                250)))
                      (flet ((capture (responses octets)
                              (let ((calls 0)
                                    (value :unseen)
                                    (reason :unseen))
                               (cl-weave:with-mocked-functions
                                (((symbol-function
                                   'ntop.platform.darwin::%getfsstat)
                                  (lambda (buffer buffer-size flags)
                                   (declare (ignore buffer-size flags))
                                   (incf calls)
                                   (let ((response (pop responses)))
                                    (when
                                      (and (= calls 2)
                                           octets
                                           (not (zerop (sb-sys:sap-int buffer))))
                                     (dotimes (index (length octets))
                                      (setf (sb-sys:sap-ref-8 buffer index) (aref
                                                                             octets
                                                                             index))))
                                    response))))
                                (ntop.platform.darwin:read-darwin-file-system-stats
                                 (lambda (rows)
                                  (setf value rows))
                                 (lambda (unavailable)
                                  (setf reason unavailable))))
                               (values value reason calls))))
                       (multiple-value-bind (value reason calls)
                         (capture '(0) nil)
                        (cl-weave:expect value :to-be-null)
                        (cl-weave:expect reason :to-be :unseen)
                        (cl-weave:expect calls :to-be 1))
                       (multiple-value-bind (value reason calls)
                         (capture '(-1) nil)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason) :to-be :getfsstat-error)
                        (cl-weave:expect calls :to-be 1))
                       (multiple-value-bind (value reason calls)
                         (capture
                          (list
                           (1+
                            (floor array-total-size-limit
                                   (getf
                                    ntop.platform.darwin::*darwin-statfs-layout*
                                    :size))))
                          nil)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect reason :to-be :statfs-buffer-too-large)
                        (cl-weave:expect calls :to-be 1))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 -1) entry)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason) :to-be :getfsstat-error)
                        (cl-weave:expect calls :to-be 2))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 2) entry)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason)
                                         :to-be
                                         :statfs-count-exceeded-buffer)
                        (cl-weave:expect calls :to-be 2))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 1 -1) entry)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason) :to-be :getfsstat-error)
                        (cl-weave:expect calls :to-be 3))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 1 2) entry)
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason)
                                         :to-be
                                         :statfs-count-changed)
                        (cl-weave:expect calls :to-be 3))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 1 1)
                                  (make-array 2168
                                              :element-type
                                              '(unsigned-byte 8)
                                              :initial-element
                                              0))
                        (cl-weave:expect value :to-be :unseen)
                        (cl-weave:expect (first reason)
                                         :to-be
                                         :invalid-statfs-buffer)
                        (cl-weave:expect calls :to-be 3))
                       (multiple-value-bind (value reason calls)
                         (capture '(1 1 1) entry)
                        (cl-weave:expect value
                                         :to-equal
                                         (list
                                          (first
                                           (ntop.platform.darwin:decode-darwin-statfs-buffer
                                            entry
                                            1))))
                        (cl-weave:expect reason :to-be :unseen)
                        (cl-weave:expect calls :to-be 3)))))
 (cl-weave:it-run-if (member :darwin *features*)
                     "covers getattrlist success, errors, and invalid input"
                     (flet ((capture (status octets mount-path)
                             (let ((value :unseen)
                                   (reason :unseen))
                              (cl-weave:with-mocked-functions
                               (((symbol-function
                                  'ntop.platform.darwin::%getattrlist)
                                 (lambda
                                   (path attributes buffer buffer-size options)
                                  (declare (ignore path
                                                   attributes
                                                   buffer-size
                                                   options))
                                  (when (and (zerop status) octets)
                                   (dotimes (index (length octets))
                                    (setf (sb-sys:sap-ref-8 buffer index) (aref
                                                                           octets
                                                                           index))))
                                  status)))
                               (ntop.platform.darwin:read-darwin-volume-space-used
                                mount-path
                                (lambda (used-bytes)
                                 (setf value used-bytes))
                                (lambda (unavailable)
                                 (setf reason unavailable))))
                              (values value reason))))
                      (multiple-value-bind (value reason)
                        (capture 0
                                 (%make-volume-space-used-record 12 12 42)
                                 "/")
                       (cl-weave:expect value :to-be 42)
                       (cl-weave:expect reason :to-be :unseen))
                      (multiple-value-bind (value reason)
                        (capture 1
                                 (%make-volume-space-used-record 12 12 42)
                                 "/")
                       (cl-weave:expect value :to-be :unseen)
                       (cl-weave:expect (first reason)
                                        :to-be
                                        :getattrlist-error))
                      (multiple-value-bind (value reason)
                        (capture 0
                                 (%make-volume-space-used-record 11 12 42)
                                 "/")
                       (cl-weave:expect value :to-be :unseen)
                       (cl-weave:expect (first reason)
                                        :to-be
                                        :invalid-volume-space-used))
                      (cl-weave:expect
                       (lambda ()
                        (ntop.platform.darwin:read-darwin-volume-space-used nil
                                                                            (lambda
                                                                              (value)
                                                                             (declare (ignore
                                                                                       value)))
                                                                            (lambda
                                                                              (reason)
                                                                             (declare (ignore
                                                                                       reason)))))
                       :to-throw
                       'error))))

