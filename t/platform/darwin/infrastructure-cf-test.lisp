(in-package #:ntop.test)

(defmacro define-cf-tree-cases (cases &body body)
  `(cl-weave:describe "CoreFoundation pure tree conversion"
                      (cl-weave:it-each ,cases
                                        "~S -> ~S"
                                        (tree expected)
                                        ,@body)))

(define-cf-tree-cases
 (((:string "kernel") "kernel") ((:number -42) -42)
  ((:number 1.5d0) 1.5d0)
  ((:boolean t) :true)
  ((:boolean nil) :false)
  ((:array ((:number 1) (:string "two"))) (1 "two")))
 (cl-weave:expect (ntop.platform.darwin::cf-tree-to-lisp tree)
                  :to-equal
                  expected))

(cl-weave:describe "CoreFoundation tree fixtures"
                   (cl-weave:it
                    "converts nested dictionary trees into hash tables"
                    (let ((properties
                           (ntop.platform.darwin::cf-tree-to-lisp
                            '(:dictionary
                              (((:string "name") . (:string "fixture"))
                               ((:string "enabled") . (:boolean t)))))))
                      (cl-weave:expect (gethash "name" properties)
                                       :to-be
                                       "fixture")
                      (cl-weave:expect (gethash "enabled" properties)
                                       :to-be
                                       :true)))
                   (cl-weave:it "copies data tree octets"
                                (let* ((source
                                        (make-array 3
                                                    :element-type
                                                    '(unsigned-byte 8)
                                                    :initial-contents
                                                    '(1 2 255)))
                                       (copy
                                        (ntop.platform.darwin::cf-tree-to-lisp
                                         (list :data source))))
                                  (setf (aref source 0) 9)
                                  (cl-weave:expect (equalp copy #(1 2 255))
                                                   :to-be
                                                   t)))
                   (cl-weave:it "rejects unsupported tree kinds"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.platform.darwin::cf-tree-to-lisp
                                    '(:date 0)))
                                 :to-throw
                                 'error))
                   (cl-weave:it-property
                    "array conversion preserves generated values"
                    ((values
                      (cl-weave:gen-list
                       (cl-weave:gen-integer :min -1000 :max 1000)
                       :min-length
                       0
                       :max-length
                       16)))
                    (cl-weave:expect
                     (ntop.platform.darwin::cf-tree-to-lisp
                      (list :array
                            (mapcar
                             (lambda (value)
                               (list :number value))
                             values)))
                     :to-equal
                     values))
                   (cl-weave:it-property
                    "data conversion preserves generated octets"
                    ((octets
                      (cl-weave:gen-vector
                       (cl-weave:gen-integer :min 0 :max 255)
                       :min-length
                       0
                       :max-length
                       16)))
                    (cl-weave:expect
                     (equalp
                      (ntop.platform.darwin::cf-tree-to-lisp
                       (list :data octets))
                      octets)
                     :to-be
                     t))
                   (cl-weave:it "expands table driven CF tests"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.test::define-cf-tree-cases
                                          (((:string "fixture") "fixture"))
                                          (cl-weave:expect
                                           (ntop.platform.darwin::cf-tree-to-lisp
                                            tree)
                                           :to-equal
                                           expected)))))
                                  (cl-weave:expect (first expansion)
                                                   :to-be
                                                   'cl-weave:describe)
                                  (cl-weave:expect (first (third expansion))
                                                   :to-be
                                                   'cl-weave:it-each))))
(defvar *released-cf-references*
  nil)

(defun %record-cf-release (reference)
  (push (sb-sys:sap-int reference) *released-cf-references*))

(cl-weave:describe "owned CoreFoundation references"
                   (cl-weave:before-each
                    (setf *released-cf-references* nil))
                   (cl-weave:after-each
                    (setf *released-cf-references* nil))
                   (cl-weave:it
                    "releases a non-null owned reference after success"
                    (let ((value nil))
                      (ntop.platform.darwin::call-with-owned-cf-reference
                       (sb-sys:int-sap 41)
                       #'%record-cf-release
                       (lambda (reference)
                         (setf value (sb-sys:sap-int reference)))
                       (lambda ()
                         (setf value :missing)))
                      (cl-weave:expect value :to-be 41)
                      (cl-weave:expect *released-cf-references* :to-equal '(41))))
                   (cl-weave:it
                    "releases an owned reference when its continuation signals"
                    (cl-weave:expect
                     (lambda ()
                       (ntop.platform.darwin::call-with-owned-cf-reference
                        (sb-sys:int-sap 42)
                        #'%record-cf-release
                        (lambda (reference)
                          (declare (ignore reference))
                          (error "conversion failed"))
                        (lambda ()
                          :missing)))
                     :to-throw
                     "conversion failed")
                    (cl-weave:expect *released-cf-references* :to-equal '(42)))
                   (cl-weave:it "does not release a null reference"
                                (let ((value nil))
                                  (ntop.platform.darwin::call-with-owned-cf-reference
                                   (sb-sys:int-sap 0)
                                   #'%record-cf-release
                                   (lambda (reference)
                                     (setf value (sb-sys:sap-int reference)))
                                   (lambda ()
                                     (setf value :missing)))
                                  (cl-weave:expect value :to-be :missing)
                                  (cl-weave:expect *released-cf-references*
                                                   :to-be-null)))
                   (cl-weave:it
                    "passes owned and missing results through continuations"
                    (let ((runner
                           (fdefinition
                            (find-symbol
                             "CALL-WITH-OWNED-CF-REFERENCE"
                             "NTOP.PLATFORM.DARWIN"))))
                      (cl-weave:with-continuation-values
                       (values next)
                       (funcall
                        runner
                        (sb-sys:int-sap 7)
                        (function
                         %record-cf-release)
                        (lambda (reference)
                          (funcall
                           next
                           :value
                           (sb-sys:sap-int reference)))
                        (lambda ()
                          (funcall next :missing)))
                       (cl-weave:expect
                        values
                        :to-equal
                        (quote (:value 7))))))
                   (cl-weave:it
                    "expands the ownership macro into the CPS cleanup call"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.platform.darwin::with-owned-cf-reference
                              (reference ntop.test::make-reference
                                         #'ntop.test::release-reference)
                              (ntop.test::consume reference)
                              (ntop.test::missing)))))
                      (cl-weave:expect expansion
                                       :to-equal
                                       '(ntop.platform.darwin::call-with-owned-cf-reference
                                         ntop.test::make-reference
                                         #'ntop.test::release-reference
                                         (lambda (reference)
                                           (ntop.test::consume reference))
                                         (lambda ()
                                           (ntop.test::missing)))))))
(defun %ioreg-model-value (output)
  (let* ((marker "\"model\" = <\"")
         (value-start (search marker output)))
    (when value-start
      (let* ((start (+ value-start (length marker)))
             (end (search "\">" output :start2 start)))
        (when end
          (subseq output start end))))))

(defun %cf-model-string (value)
  (cond
    ((stringp value) value)
    ((typep value '(vector (unsigned-byte 8)))
     (ntop.platform.darwin::decode-c-string value))
    (t nil)))

(defun %read-ioreg-model ()
  (let ((result
         (host-kit:run-program
          "/usr/sbin/ioreg"
          (list "-rd1" "-l" "-c" "IOPlatformExpertDevice")
          :timeout 5
          :max-output-characters 65536)))
    (unless (and
             (= (host-kit:process-result-exit-code result) 0)
             (not (host-kit:process-result-stdout-truncated-p result))
             (not (host-kit:process-result-stderr-truncated-p result)))
      (error "ioreg command failed or exceeded its output limit."))
    (%ioreg-model-value (host-kit:process-result-stdout result))))

(cl-weave:describe "Darwin CoreFoundation boundary"
                   (cl-weave:it-run-if
                    (member :darwin *features*)
                    "matches the device model around the read"
                    (let* ((before (%read-ioreg-model))
                           (models nil))
                      (ntop.platform.darwin::call-with-iokit-services
                       "IOPlatformExpertDevice"
                       (lambda (service)
                         (ntop.platform.darwin::iokit-registry-properties
                          service
                          (lambda (properties)
                            (multiple-value-bind (value foundp)
                                (gethash "model" properties)
                              (when foundp
                                (push (%cf-model-string value) models))))
                          (lambda (reason)
                            (error "IOKit property read failed: ~S." reason))))
                       (lambda () nil)
                       (lambda (reason)
                         (error "IOKit enumeration failed: ~S." reason)))
                      (let ((after (%read-ioreg-model)))
                        (cl-weave:expect
                         (and (= (length models) 1)
                              (equal (first models) before)
                              (equal (first models) after))
                         :to-be
                         t)))))
