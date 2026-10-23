(in-package #:ntop.test)

(defun %write-disk-test-unsigned (octets offset width value)
  (dotimes (index width octets)
    (setf (aref octets (+ offset index)) (ldb (byte 8 (* index 8)) value))))

(defun %write-disk-test-string (octets offset width value)
  (loop for character across value
        for index from 0
        do (setf (aref octets (+ offset index)) (char-code character)))
  octets)

(defun %make-disk-test-octets (values)
  (make-array (length values)
              :element-type
              '(unsigned-byte 8)
              :initial-contents
              values))

(defun %make-statfs-entry (device-name mount-path
                                       filesystem-type
                                       block-size
                                       blocks
                                       free-blocks
                                       available-blocks)
  (let ((octets
         (make-array 2168 :element-type '(unsigned-byte 8) :initial-element 0)))
    (%write-disk-test-unsigned octets 0 4 block-size)
    (%write-disk-test-unsigned octets 8 8 blocks)
    (%write-disk-test-unsigned octets 16 8 free-blocks)
    (%write-disk-test-unsigned octets 24 8 available-blocks)
    (%write-disk-test-string octets 72 16 filesystem-type)
    (%write-disk-test-string octets 88 1024 mount-path)
    (%write-disk-test-string octets 1112 1024 device-name)
    octets))

(defun %make-volume-space-used-record (declared-size buffer-size used-bytes)
  (let ((octets
         (make-array buffer-size
                     :element-type
                     '(unsigned-byte 8)
                     :initial-element
                     0)))
    (when (>= buffer-size 4)
      (%write-disk-test-unsigned octets 0 4 declared-size))
    (when (>= buffer-size 12)
      (%write-disk-test-unsigned octets 4 8 used-bytes))
    octets))

(cl-weave:describe "Darwin statfs decoder"
                   (cl-weave:it-each
                    (("/dev/disk9s1" "/"
                                     "apfs"
                                     4096
                                     1000
                                     300
                                     250
                                     (:device-name "/dev/disk9s1"
                                                   :mount-path
                                                   "/"
                                                   :filesystem-type
                                                   "apfs"
                                                   :block-size
                                                   4096
                                                   :blocks
                                                   1000
                                                   :free-blocks
                                                   300
                                                   :available-blocks
                                                   250)))
                    "decodes statfs fields from ~A"
                    (device mount
                            filesystem
                            block-size
                            blocks
                            free
                            available
                            expected)
                    (let* ((entry
                            (%make-statfs-entry device
                                                mount
                                                filesystem
                                                block-size
                                                blocks
                                                free
                                                available))
                           (row
                            (first
                             (ntop.platform.darwin:decode-darwin-statfs-buffer
                              entry
                              1))))
                      (cl-weave:expect row :to-equal expected)))
                   (cl-weave:it
                    "decodes multiple entries and permits trailing bytes"
                    (let* ((first-entry
                            (%make-statfs-entry "/dev/disk1"
                                                "/"
                                                "apfs"
                                                4096
                                                10
                                                4
                                                3))
                           (second-entry
                            (%make-statfs-entry "/dev/disk2"
                                                "/data"
                                                "hfs"
                                                1024
                                                20
                                                8
                                                6))
                           (octets
                            (concatenate '(vector (unsigned-byte 8))
                                         first-entry
                                         second-entry
                                         #(9 8))))
                      (cl-weave:expect
                       (mapcar
                        (lambda (row)
                          (getf row :mount-path))
                        (ntop.platform.darwin:decode-darwin-statfs-buffer octets
                                                                          2))
                       :to-equal
                       '("/" "/data")))
                    (cl-weave:it "returns no rows for a zero count"
                                 (cl-weave:expect
                                  (ntop.platform.darwin:decode-darwin-statfs-buffer
                                   (%make-disk-test-octets nil)
                                   0)
                                  :to-equal
                                  nil))
                    (cl-weave:it-each ((nil 1) ((1 2 3) 1))
                                      "rejects short statfs input ~S with count ~D"
                                      (octet-values count)
                                      (cl-weave:expect
                                       (lambda ()
                                         (ntop.platform.darwin:decode-darwin-statfs-buffer
                                          (%make-disk-test-octets octet-values)
                                          count))
                                       :to-throw
                                       'error))
                    (cl-weave:it "rejects a negative statfs count"
                                 (cl-weave:expect
                                  (lambda ()
                                    (ntop.platform.darwin:decode-darwin-statfs-buffer
                                     (%make-disk-test-octets nil)
                                     -1))
                                  :to-throw
                                  'error))
                    (cl-weave:it "rejects a zero statfs block size"
                                 (let ((entry
                                        (%make-statfs-entry "/dev/disk1"
                                                            "/"
                                                            "apfs"
                                                            0
                                                            10
                                                            4
                                                            3)))
                                   (cl-weave:expect
                                    (lambda ()
                                      (ntop.platform.darwin:decode-darwin-statfs-buffer
                                       entry
                                       1))
                                    :to-throw
                                    'error))))
                   (cl-weave:describe "Darwin volume-space-used decoder"
                                      (cl-weave:it-each
                                       ((0 0) (1 1) (1234567890 1234567890))
                                       "decodes ~D used bytes"
                                       (used-bytes expected)
                                       (cl-weave:expect
                                        (ntop.platform.darwin:decode-darwin-volume-space-used
                                         (%make-volume-space-used-record 12
                                                                         12
                                                                         used-bytes))
                                        :to-be
                                        expected))
                                      (cl-weave:it-property
                                       "preserves unsigned 64-bit used byte counts"
                                       ((used-bytes
                                         (cl-weave:gen-integer :min
                                                               0
                                                               :max
                                                               18446744073709551615)))
                                       (cl-weave:expect
                                        (ntop.platform.darwin:decode-darwin-volume-space-used
                                         (%make-volume-space-used-record 12
                                                                         12
                                                                         used-bytes))
                                        :to-be
                                        used-bytes))
                                      (cl-weave:it-each
                                       ((11 12) (13 12) (12 11))
                                       "rejects record length ~D in a ~D byte buffer"
                                       (declared-size buffer-size)
                                       (cl-weave:expect
                                        (lambda ()
                                          (ntop.platform.darwin:decode-darwin-volume-space-used
                                           (%make-volume-space-used-record
                                            declared-size
                                            buffer-size
                                            42)))
                                        :to-throw
                                        'error))
                                      (cl-weave:it
                                       "rejects a buffer shorter than the length word"
                                       (cl-weave:expect
                                        (lambda ()
                                          (ntop.platform.darwin:decode-darwin-volume-space-used
                                           (%make-disk-test-octets nil)))
                                        :to-throw
                                        'error))))

(cl-weave:describe "Darwin filesystem readers"
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
                                                       (lambda
                                                           (buffer buffer-size
                                                                   flags)
                                                         (declare (ignore
                                                                   buffer-size
                                                                   flags))
                                                         (incf calls)
                                                         (let ((response
                                                                (pop responses)))
                                                           (when
                                                               (and (= calls 2)
                                                                    octets
                                                                    (not
                                                                     (zerop
                                                                      (sb-sys:sap-int
                                                                       buffer))))
                                                             (dotimes
                                                                 (index
                                                                  (length
                                                                   octets))
                                                               (setf (sb-sys:sap-ref-8
                                                                      buffer
                                                                      index) (aref
                                                                              octets
                                                                              index))))
                                                           response))))
                                                     (ntop.platform.darwin:read-darwin-file-system-stats
                                                      (lambda (rows)
                                                        (setf value rows))
                                                      (lambda (unavailable)
                                                        (setf reason unavailable))))
                                                    (values value reason calls))))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(0) nil)
                                             (cl-weave:expect value :to-be-null)
                                             (cl-weave:expect reason
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect calls :to-be 1))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(-1) nil)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :getfsstat-error)
                                             (cl-weave:expect calls :to-be 1))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture
                                                (list
                                                 (1+
                                                  (floor array-total-size-limit
                                                         (getf
                                                          ntop.platform.darwin::*darwin-statfs-layout*
                                                          :size))))
                                                nil)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect reason
                                                              :to-be
                                                              :statfs-buffer-too-large)
                                             (cl-weave:expect calls :to-be 1))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 -1) entry)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :getfsstat-error)
                                             (cl-weave:expect calls :to-be 2))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 2) entry)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :statfs-count-exceeded-buffer)
                                             (cl-weave:expect calls :to-be 2))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 1 -1) entry)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :getfsstat-error)
                                             (cl-weave:expect calls :to-be 3))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 1 2) entry)
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :statfs-count-changed)
                                             (cl-weave:expect calls :to-be 3))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 1 1)
                                                        (make-array 2168
                                                                    :element-type
                                                                    '(unsigned-byte
                                                                      8)
                                                                    :initial-element
                                                                    0))
                                             (cl-weave:expect value
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect (first reason)
                                                              :to-be
                                                              :invalid-statfs-buffer)
                                             (cl-weave:expect calls :to-be 3))
                                           (multiple-value-bind (value reason
                                                                       calls)
                                               (capture '(1 1 1) entry)
                                             (cl-weave:expect value
                                                              :to-equal
                                                              (list
                                                               (first
                                                                (ntop.platform.darwin:decode-darwin-statfs-buffer
                                                                 entry
                                                                 1))))
                                             (cl-weave:expect reason
                                                              :to-be
                                                              :unseen)
                                             (cl-weave:expect calls :to-be 3)))))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "covers getattrlist success, errors, and invalid input"
                                       (flet ((capture (status octets
                                                               mount-path)
                                                (let ((value :unseen)
                                                      (reason :unseen))
                                                  (cl-weave:with-mocked-functions
                                                   (((symbol-function
                                                      'ntop.platform.darwin::%getattrlist)
                                                     (lambda
                                                         (path attributes
                                                               buffer
                                                               buffer-size
                                                               options)
                                                       (declare (ignore path
                                                                        attributes
                                                                        buffer-size
                                                                        options))
                                                       (when
                                                           (and (zerop status)
                                                                octets)
                                                         (dotimes
                                                             (index
                                                              (length octets))
                                                           (setf (sb-sys:sap-ref-8
                                                                  buffer
                                                                  index) (aref
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
                                                      (%make-volume-space-used-record
                                                       12
                                                       12
                                                       42)
                                                      "/")
                                           (cl-weave:expect value :to-be 42)
                                           (cl-weave:expect reason
                                                            :to-be
                                                            :unseen))
                                         (multiple-value-bind (value reason)
                                             (capture 1
                                                      (%make-volume-space-used-record
                                                       12
                                                       12
                                                       42)
                                                      "/")
                                           (cl-weave:expect value
                                                            :to-be
                                                            :unseen)
                                           (cl-weave:expect (first reason)
                                                            :to-be
                                                            :getattrlist-error))
                                         (multiple-value-bind (value reason)
                                             (capture 0
                                                      (%make-volume-space-used-record
                                                       11
                                                       12
                                                       42)
                                                      "/")
                                           (cl-weave:expect value
                                                            :to-be
                                                            :unseen)
                                           (cl-weave:expect (first reason)
                                                            :to-be
                                                            :invalid-volume-space-used))
                                         (cl-weave:expect
                                          (lambda ()
                                            (ntop.platform.darwin:read-darwin-volume-space-used
                                             nil
                                             (lambda (value)
                                               (declare (ignore value)))
                                             (lambda (reason)
                                               (declare (ignore reason)))))
                                          :to-throw
                                          'error))))

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
