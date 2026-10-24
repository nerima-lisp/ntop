(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load (merge-pathnames
         "helpers-infrastructure-base.lisp"
         (uiop:pathname-directory-pathname
          (asdf:component-pathname
           (asdf:find-component "ntop/test"
                                "t/platform/darwin/infrastructure-base-test"))))))

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
                   (cl-weave:it
                    "expands optional FFI declarations into arity-specific routines and dispatcher"
                    (let* ((fixed-expansion
                            (macroexpand-1
                             '(ntop.platform.darwin::define-darwin-ffi
                               (:routine ntop.test::probe
                                         "probe"
                                         sb-alien:int
                                         (value sb-alien:int))
                               (:variable ntop.test::task-self
                                          "mach_task_self_"
                                          sb-alien:unsigned-int))))
                           (optional-expansion
                            (macroexpand-1
                             '(ntop.platform.darwin::define-darwin-ffi
                               (:routine ntop.test::variadic-probe
                                         "variadic_probe"
                                         sb-alien:int
                                         (descriptor sb-alien:int)
                                         &optional
                                         (argument sb-alien:int))))))
                      (cl-weave:expect
                       (list fixed-expansion
                             (list (first optional-expansion)
                                   (length optional-expansion)))
                       :to-equal
                       (list
                        '(progn
                          (sb-alien:define-alien-routine ("probe" ntop.test::probe)
                            sb-alien:int
                            (value sb-alien:int))
                          (sb-alien:define-alien-variable
                           ("mach_task_self_" ntop.test::task-self)
                           sb-alien:unsigned-int))
                        '(progn 2)))
                      (let* ((routine-expansion (second optional-expansion))
                             (one-argument-routine (second routine-expansion))
                             (two-argument-routine (third routine-expansion))
                             (dispatcher (fourth routine-expansion))
                             (one-argument-name (second (second one-argument-routine)))
                             (two-argument-name (second (second two-argument-routine)))
                             (lambda-list (third dispatcher))
                             (optional-binding (third lambda-list))
                             (dispatch-body (fourth dispatcher))
                             (supplied-p (third optional-binding)))
                        (cl-weave:expect
                         (list
                          (list (first routine-expansion)
                                (length routine-expansion))
                          (list
                           (list (first one-argument-routine)
                                 (first (second one-argument-routine))
                                 (length one-argument-routine))
                           (list (first two-argument-routine)
                                 (first (second two-argument-routine))
                                 (length two-argument-routine)))
                         :to-equal
                         (list
                          '(progn 4)
                          '((sb-alien:define-alien-routine "variadic_probe" 4)
                            (sb-alien:define-alien-routine "variadic_probe" 5)))))
                        (cl-weave:expect
                         (list
                          (first dispatcher)
                          (second dispatcher)
                          (first lambda-list)
                          (second lambda-list)
                          (first optional-binding)
                          (second optional-binding)
                          (not (eq one-argument-name two-argument-name))
                          (first dispatch-body)
                          (second dispatch-body)
                          (first (third dispatch-body))
                          (rest (third dispatch-body))
                          (first (fourth dispatch-body))
                          (rest (fourth dispatch-body)))
                         :to-equal
                         (list
                          'defun
                          'ntop.test::variadic-probe
                          'ntop.test::descriptor
                          '&optional
                          'ntop.test::argument
                          nil
                          t
                          'if
                          supplied-p
                          two-argument-name
                          '(ntop.test::descriptor ntop.test::argument)
                          one-argument-name
                          '(ntop.test::descriptor))))))
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
(cl-weave:describe
 "Darwin FFI declaration validation"
 (cl-weave:it
  "rejects a variadic declaration without optional arguments"
  (cl-weave:expect
   (lambda ()
     (macroexpand-1
      (quote
       (ntop.platform.darwin::define-darwin-ffi
        (:routine ntop.test::probe
                  "probe"
                  sb-alien:int
                  &optional)))))
   :to-throw
   (quote error)))
 (cl-weave:it
  "rejects a malformed optional argument"
  (cl-weave:expect
   (lambda ()
     (macroexpand-1
      (quote
       (ntop.platform.darwin::define-darwin-ffi
        (:routine ntop.test::probe
                  "probe"
                  sb-alien:int
                  (descriptor sb-alien:int)
                  &optional
                  (argument))))))
   :to-throw
   (quote error))))


