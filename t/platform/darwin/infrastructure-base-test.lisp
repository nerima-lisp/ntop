(in-package #:ntop.test)

(defmacro define-case-table (suite cases format-control bindings &body body)
  `(cl-weave:describe ,suite
                      (cl-weave:it-each ,cases ,format-control ,bindings ,@body)))

(defun %make-octets (values)
  (make-array (length values)
              :element-type
              (quote (unsigned-byte 8))
              :initial-contents
              values))

(define-case-table "little-endian unsigned"
                   ((() 0) ((52 18) 4660) ((255 255 255 255) 4294967295))
                   "~S -> ~D"
                   (octets expected)
                   (cl-weave:expect
                    (ntop.platform.darwin::decode-little-endian-unsigned
                     (%make-octets octets))
                    :to-be
                    expected))

(define-case-table "little-endian signed"
                   ((() 0) ((255) -1) ((0 128) -32768) ((255 127) 32767))
                   "~S -> ~D"
                   (octets expected)
                   (cl-weave:expect
                    (ntop.platform.darwin::decode-little-endian-signed
                     (%make-octets octets))
                    :to-be
                    expected))

(define-case-table "C strings"
                   ((() "") ((65 66 0 67) "AB") ((195 169 0) "é"))
                   "~S -> ~S"
                   (octets expected)
                   (cl-weave:expect
                    (ntop.platform.darwin::decode-c-string
                     (%make-octets octets))
                    :to-equal
                    expected))

(define-case-table "Mach timebase layout"
                   (((1 0 0 0 1 0 0 0) 1 1) ((125 0 0 0 3 0 0 0) 125 3)
                    ((232 3 0 0 1 0 0 0) 1000 1))
                   "~S -> ~D/~D"
                   (octets expected-numerator expected-denominator)
                   (multiple-value-bind (numerator denominator)
                       (ntop.platform.darwin::decode-mach-timebase
                        (%make-octets octets))
                     (cl-weave:expect numerator :to-be expected-numerator)
                     (cl-weave:expect denominator :to-be expected-denominator)))

(define-case-table "Mach tick conversion"
                   ((0 125 3 0) (1 125 3 41) (100 125 3 4166) (3 1 2 1))
                   "~D ticks at ~D/~D -> ~D ns"
                   (ticks numerator denominator expected)
                   (cl-weave:expect
                    (ntop.platform.darwin::mach-ticks-to-nanoseconds ticks
                                                                     numerator
                                                                     denominator)
                    :to-be
                    expected))

(defun %encode-unsigned-octets (value width)
  (let ((octets (make-array width :element-type '(unsigned-byte 8))))
    (dotimes (index width octets)
      (setf (aref octets index) (ldb (byte 8 (* index 8)) value)))))

(cl-weave:describe "Darwin base pure behavior"
                   (cl-weave:it-property "unsigned little-endian round trips"
                                         ((value
                                           (cl-weave:gen-integer :min
                                                                 0
                                                                 :max
                                                                 18446744073709551615)))
                                         (cl-weave:expect
                                          (ntop.platform.darwin::decode-little-endian-unsigned
                                           (%encode-unsigned-octets value 8))
                                          :to-be
                                          value))
                   (cl-weave:it-property "signed little-endian round trips"
                                         ((value
                                           (cl-weave:gen-integer :min
                                                                 -9223372036854775808
                                                                 :max
                                                                 9223372036854775807)))
                                         (let ((encoded
                                                (if (minusp value)
                                                    (+ value (ash 1 64))
                                                    value)))
                                           (cl-weave:expect
                                            (ntop.platform.darwin::decode-little-endian-signed
                                             (%encode-unsigned-octets encoded 8))
                                            :to-be
                                            value)))
                   (cl-weave:it-property "UTF-8 C strings round trip"
                                         ((value
                                           (cl-weave:gen-string :min-length
                                                                0
                                                                :max-length
                                                                32
                                                                :alphabet
                                                                "abc")))
                                         (let* ((encoded
                                                 (sb-ext:string-to-octets value
                                                                          :external-format
                                                                          :utf-8))
                                                (terminated
                                                 (concatenate
                                                  '(vector (unsigned-byte 8))
                                                  encoded
                                                  #(0 88))))
                                           (cl-weave:expect
                                            (ntop.platform.darwin::decode-c-string
                                             terminated)
                                            :to-equal
                                            value)))
                   (cl-weave:it "rejects a zero Mach timebase field"
                                (cl-weave:expect
                                 (lambda ()
                                   (ntop.platform.darwin::decode-mach-timebase
                                    (%make-octets (quote (1 0 0 0 0 0 0 0)))))
                                 :to-throw
                                 'error))
                   (cl-weave:it "expands fixed FFI routines and variables"
                                (let* ((form
                                        '(ntop.platform.darwin::define-darwin-ffi
                                          (:routine ntop.test::probe
                                                    "probe"
                                                    sb-alien:int
                                                    (value sb-alien:int))
                                          (:variable ntop.test::task-self
                                                     "mach_task_self_"
                                                     sb-alien:unsigned-int)))
                                       (expansion (macroexpand-1 form)))
                                  (cl-weave:expect expansion
                                                   :to-equal
                                                   '(progn
                                                      (sb-alien:define-alien-routine
                                                       ("probe"
                                                        ntop.test::probe)
                                                       sb-alien:int
                                                       (value sb-alien:int))
                                                      (sb-alien:define-alien-variable
                                                       ("mach_task_self_"
                                                        ntop.test::task-self)
                                                       sb-alien:unsigned-int)))))
                   (cl-weave:it "expands table tests into cl-weave cases"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.test::define-case-table "fixture"
                                                                       ((#(1) 1))
                                                                       "~S -> ~D"
                                                                       (octets
                                                                        expected)
                                                                       (cl-weave:expect
                                                                        octets
                                                                        :to-be
                                                                        expected)))))
                                  (cl-weave:expect (first expansion)
                                                   :to-be
                                                   'cl-weave:describe)
                                  (cl-weave:expect (first (third expansion))
                                                   :to-be
                                                   'cl-weave:it-each)))
                   (cl-weave:it "rejects invalid byte ranges"
                                (let ((decoder
                                       (fdefinition
                                        (find-symbol
                                         "DECODE-LITTLE-ENDIAN-UNSIGNED"
                                         "NTOP.PLATFORM.DARWIN"))))
                                  (cl-weave:expect
                                   (lambda ()
                                     (funcall decoder
                                              (%make-octets (quote (1 2)))
                                              :start
                                              2
                                              :end
                                              1))
                                   :to-throw
                                   (quote error)))))

