(in-package #:ntop.test)

; Darwin MIB path for hw.ncpu: CTL_HW (6), HW_NCPU (3).
(defparameter *hw-ncpu-mib*
  #(6 3))

(defun %sysctl-command-count ()
  (let ((result
         (host-kit:run-program "sysctl"
                               (list "-n" "hw.ncpu")
                               :timeout
                               5
                               :max-output-characters
                               64)))
    (unless
        (and (= (host-kit:process-result-exit-code result) 0)
             (not (host-kit:process-result-stdout-truncated-p result))
             (not (host-kit:process-result-stderr-truncated-p result)))
      (error "sysctl command failed or exceeded its output limit."))
    (parse-integer
     (string-trim (list #\Space #\Tab #\Newline #\Return)
                  (host-kit:process-result-stdout result)))))

(defun %decode-mib-unsigned (octets)
  (ntop.platform.darwin:decode-little-endian-unsigned octets))

(cl-weave:describe "Darwin sysctl boundary"
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "matches hw.ncpu before and after the FFI read"
                                       (let* ((before (%sysctl-command-count))
                                              (processor-count nil))
                                         (ntop.platform.darwin::sysctl-read-integer-by-name
                                          "hw.ncpu"
                                          4
                                          nil
                                          (lambda (value)
                                            (setf processor-count value))
                                          (lambda (reason)
                                            (error "sysctl failed: ~S." reason)))
                                         (let ((after (%sysctl-command-count)))
                                           (cl-weave:expect
                                            (= before processor-count after)
                                            :to-be
                                            t)))))

(progn
  (cl-weave:describe "Darwin decoder failures"
                     (cl-weave:it
                      "rejects invalid ranges, timebase sizes, zero fields, and zero denominators"
                      (cl-weave:expect
                       (lambda ()
                         (ntop.platform.darwin:decode-c-string
                          (%make-octets '(65 66))
                          :start
                          2
                          :end
                          1))
                       :to-throw
                       'error)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.platform.darwin:decode-mach-timebase
                          (%make-octets '(1 0 0 0 1 0 0 0 0 0 0 0))))
                       :to-throw
                       'error)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.platform.darwin:decode-mach-timebase
                          (%make-octets '(0 0 0 0 1 0 0 0))))
                       :to-throw
                       'error)
                      (cl-weave:expect
                       (lambda ()
                         (ntop.platform.darwin:mach-ticks-to-nanoseconds 1 1 0))
                       :to-throw
                       'error)))
  (cl-weave:describe "Darwin syscall edges"
                     (cl-weave:it-run-if (member :darwin *features*)
                                         "reads hw.ncpu through the numeric MIB"
                                         (let ((mib-value nil)
                                               (reason nil))
                                           (ntop.platform.darwin:sysctl-read-mib
                                            *hw-ncpu-mib*
                                            (lambda (octets)
                                              (setf mib-value (%decode-mib-unsigned
                                                               octets)))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect reason :to-be-null)
                                           (cl-weave:expect mib-value
                                                            :to-be
                                                            (%sysctl-command-count))))
                     (cl-weave:it-run-if (member :darwin *features*)
                                         "rejects empty and out-of-range MIBs"
                                         (cl-weave:expect
                                          (lambda ()
                                            (ntop.platform.darwin:sysctl-read-mib
                                             nil
                                             (lambda (value)
                                               (declare (ignore value)))
                                             (lambda (reason)
                                               (declare (ignore reason)))))
                                          :to-throw
                                          'error)
                                         (cl-weave:expect
                                          (lambda ()
                                            (ntop.platform.darwin:sysctl-read-mib
                                             #(6 2147483648)
                                             (lambda (value)
                                               (declare (ignore value)))
                                             (lambda (reason)
                                               (declare (ignore reason)))))
                                          :to-throw
                                          'error))
                     (cl-weave:it-run-if (member :darwin *features*)
                                         "uses both sysctl continuations and detects buffer growth"
                                         (let ((calls 0))
                                           (cl-weave:with-continuation-values
                                            (outcome next)
                                            (ntop.platform.darwin::%read-sysctl
                                             (lambda (value length)
                                               (incf calls)
                                               (if (= calls 1)
                                                   (setf (sb-alien:deref length) 3)
                                                   (progn
                                                     (setf (sb-alien:deref
                                                            length) 3)
                                                     (dotimes (index 3)
                                                       (setf (sb-sys:sap-ref-8
                                                              value
                                                              index) (aref
                                                                      #(7 8 9)
                                                                      index)))))
                                               0)
                                             (lambda (octets)
                                               (next :value octets))
                                             (lambda (reason)
                                               (next :error reason)))
                                            (cl-weave:expect
                                             (equalp outcome
                                                     (quote (:value #(7 8 9))))
                                             :to-be
                                             t))
                                           (cl-weave:expect calls :to-be 2))
                                         (let ((reason nil))
                                           (ntop.platform.darwin::%read-sysctl
                                            (lambda (value length)
                                              (declare (ignore value length))
                                              7)
                                            (lambda (octets)
                                              (declare (ignore octets)))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect (first reason)
                                                            :to-be
                                                            :sysctl-error)
                                           (cl-weave:expect (second reason)
                                                            :to-be
                                                            7))
                                         (let ((calls 0)
                                               (reason nil))
                                           (ntop.platform.darwin::%read-sysctl
                                            (lambda (value length)
                                              (declare (ignore value))
                                              (incf calls)
                                              (if (= calls 1)
                                                  (progn
                                                    (setf (sb-alien:deref
                                                           length) 2)
                                                    0)
                                                  9))
                                            (lambda (octets)
                                              (declare (ignore octets)))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect (first reason)
                                                            :to-be
                                                            :sysctl-error)
                                           (cl-weave:expect (second reason)
                                                            :to-be
                                                            9))
                                         (let ((calls 0)
                                               (reason nil))
                                           (ntop.platform.darwin::%read-sysctl
                                            (lambda (value length)
                                              (declare (ignore value))
                                              (incf calls)
                                              (setf (sb-alien:deref length) (if (=
                                                                                 calls
                                                                                 1)
                                                                                1
                                                                                2))
                                              0)
                                            (lambda (octets)
                                              (declare (ignore octets)))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect reason
                                                            :to-equal
                                                            '(:sysctl-buffer-grew
                                                              1
                                                              2))))
                     (cl-weave:it-run-if (member :darwin *features*)
                                         "returns exact buffers and slices short results"
                                         (let* ((buffer (make-array 3
                                                                    :element-type '(unsigned-byte 8)
                                                                    :initial-contents '(7 8 9)))
                                                (result
                                                  (ntop.platform.darwin::%sysctl-result-octets
                                                   buffer
                                                   3)))
                                           (cl-weave:expect (eq result buffer) :to-be t))
                                         (let* ((buffer (make-array 3
                                                                    :element-type '(unsigned-byte 8)
                                                                    :initial-contents '(7 8 9)))
                                                (result
                                                  (ntop.platform.darwin::%sysctl-result-octets
                                                   buffer
                                                   2)))
                                           (cl-weave:expect (eq result buffer) :to-be nil)
                                           (cl-weave:expect (length result) :to-be 2)
                                           (cl-weave:expect (equalp result #(7 8)) :to-be t))
                                         (let* ((buffer (make-array 1
                                                                    :element-type '(unsigned-byte 8)
                                                                    :initial-contents '(0)))
                                                (result
                                                  (ntop.platform.darwin::%sysctl-result-octets
                                                   buffer
                                                   0)))
                                           (cl-weave:expect (length result) :to-be 0)
                                           (cl-weave:expect (equalp result #()) :to-be t)))
                     (cl-weave:it-run-if (member :darwin *features*)
                                         "reports width mismatches and reads sysctl strings"
                                         (let ((reason nil)
                                               (signed-value nil)
                                               (string-value nil))
                                           (ntop.platform.darwin:sysctl-read-integer-by-name
                                            "hw.ncpu"
                                            8
                                            nil
                                            (lambda (value)
                                              (setf signed-value value))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect reason
                                                            :to-equal
                                                            '(:sysctl-width-mismatch
                                                              8
                                                              4))
                                           (ntop.platform.darwin:sysctl-read-integer-by-name
                                            "hw.ncpu"
                                            4
                                            t
                                            (lambda (value)
                                              (setf signed-value value))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (cl-weave:expect signed-value
                                                            :to-be
                                                            (%sysctl-command-count))
                                           (ntop.platform.darwin:sysctl-read-string-by-name
                                            "hw.model"
                                            (lambda (value)
                                              (setf string-value value))
                                            (lambda (failure)
                                              (setf reason failure)))
                                           (let* ((command
                                                   (host-kit:run-program
                                                    "sysctl"
                                                    (list "-n" "hw.model")
                                                    :timeout
                                                    5
                                                    :max-output-characters
                                                    128))
                                                  (expected
                                                   (string-trim
                                                    '(#\Space #\Tab
                                                              #\Newline
                                                              #\Return)
                                                    (host-kit:process-result-stdout
                                                     command))))
                                             (cl-weave:expect
                                              (host-kit:process-result-exit-code
                                               command)
                                              :to-be
                                              0)
                                             (cl-weave:expect
                                              (host-kit:process-result-stdout-truncated-p
                                               command)
                                              :to-be
                                              nil)
                                             (cl-weave:expect
                                              (host-kit:process-result-stderr-truncated-p
                                               command)
                                              :to-be
                                              nil)
                                             (cl-weave:expect string-value
                                                              :to-equal
                                                              expected))))))
