(in-package #:ntop.e2e.test)

(defmacro define-ps-duration-cases (&body cases)
  `(cl-weave:it-each ,cases
                     "parses ps CPU duration ~s"
                     (input expected)
                     (cl-weave:expect (ntop.e2e::%parse-ps-cpu-duration input)
                                      :to-satisfy
                                      (lambda (actual)
                                        (= actual expected)))))

(defmacro define-n6-reply-cases (&body cases)
  `(cl-weave:it-each ,cases
                     "parses DECRQM status from od output ~s"
                     (text expected)
                     (cl-weave:expect
                      (let ((actual (ntop.e2e::%n6-reply-status text)))
                        (if expected
                            (eql actual expected)
                            (null actual)))
                      :to-be
                      t)))

(cl-weave:describe "WP0-6 measurement helpers"
                   (cl-weave:it
                    "expands measurement macros into data and test forms"
                    (let* ((duration-expansion
                            (macroexpand-1
                             '(define-ps-duration-cases ("0:00.00" 0d0))))
                           (key-cycle-macro
                            (find-symbol "DEFINE-MEASUREMENT-KEY-CYCLE"
                                         "NTOP.E2E"))
                           (key-cycle-expansion
                            (macroexpand-1
                             (list key-cycle-macro
                                   (list #\? #x3f)
                                   (list #\Escape #x1b))))
                           (status-table-macro
                            (find-symbol "DEFINE-N6-REPLY-STATUS-TABLE"
                                         "NTOP.E2E"))
                           (status-table-expansion
                            (macroexpand-1
                             (list status-table-macro
                                   (list 0 #x30)
                                   (list 1 #x31)
                                   (list 2 #x32)
                                   (list 3 #x33)
                                   (list 4 #x34)))))
                      (cl-weave:expect
                       (and
                        (eq (first duration-expansion)
                            (find-symbol "IT-EACH" "CL-WEAVE"))
                        (equal key-cycle-expansion
                               '(defparameter ntop.e2e::*measurement-key-cycle*
                                  (vector
                                   (list :key
                                         #\?
                                         :octets
                                         (make-array 1
                                                     :element-type
                                                     (quote (unsigned-byte 8))
                                                     :initial-element
                                                     63))
                                   (list :key
                                         #\Escape
                                         :octets
                                         (make-array 1
                                                     :element-type
                                                     (quote (unsigned-byte 8))
                                                     :initial-element
                                                     27)))))
                        (equal status-table-expansion
                               '(defparameter ntop.e2e::*n6-reply-status-table*
                                  (quote
                                   ((48 . 0) (49 . 1)
                                    (50 . 2)
                                    (51 . 3)
                                    (52 . 4))))))
                       :to-be
                       t)))
                   (cl-weave:it-each
                    (("n1" "%RUN-N1" t) ("n2" "%RUN-N2" t)
                     ("n3" "%RUN-N3" nil)
                     ("n4" "%RUN-N4" nil)
                     ("n5" "%RUN-N5" nil)
                     ("n6" "%RUN-N6" nil))
                    "expands and maps ~a measurement metadata"
                    (name procedure-name duration-p)
                    (let* ((procedure-symbol
                            (find-symbol procedure-name "NTOP.E2E"))
                           (macro-symbol
                            (find-symbol "DEFINE-MEASUREMENT-PROCEDURES"
                                         "NTOP.E2E"))
                           (expansion
                            (macroexpand-1
                             (list macro-symbol
                                   (list name procedure-symbol duration-p))))
                           (definition
                            (find name
                                  ntop.e2e::*measurement-procedures*
                                  :key
                                  (lambda (entry)
                                    (getf entry :name))
                                  :test
                                  #'string=))
                           (window-valid-p
                            (handler-case (progn
                                            (ntop.e2e::%validate-duration
                                             definition
                                             (if duration-p
                                                 1
                                                 0)
                                             (if duration-p
                                                 2d0
                                                 1d0))
                                            t)
                              (error ()
                                nil)))
                           (duration-policy-p
                            (handler-case (progn
                                            (ntop.e2e::%validate-duration
                                             definition
                                             (if duration-p
                                                 2
                                                 0)
                                             (if duration-p
                                                 2d0
                                                 1d0))
                                            (not duration-p))
                              (ntop.e2e::measurement-error ()
                                duration-p)))
                           (key-cycle ntop.e2e::*measurement-key-cycle*))
                      (cl-weave:expect
                       (and (eq (first expansion) 'defparameter)
                            (eq (second expansion)
                                'ntop.e2e::*measurement-procedures*)
                            (eq (first (third expansion)) 'quote)
                            (equal (second (third expansion))
                                   (list
                                    (list :name
                                          name
                                          :function
                                          procedure-symbol
                                          :uses-duration-p
                                          duration-p)))
                            (eq (getf definition :function) procedure-symbol)
                            (eq (getf definition :uses-duration-p) duration-p)
                            (functionp (symbol-function procedure-symbol))
                            window-valid-p
                            duration-policy-p
                            (typep key-cycle 'vector)
                            (= (length key-cycle) 2)
                            (and (char= (getf (aref key-cycle 0) :key) #\?)
                                 (equalp (getf (aref key-cycle 0) :octets)
                                         #(#x3f))
                                 (char= (getf (aref key-cycle 1) :key) #\Escape)
                                 (equalp (getf (aref key-cycle 1) :octets)
                                         #(#x1b))))
                       :to-be
                       t)))
                   (define-ps-duration-cases ("0:00.00" 0d0)
                                             ("1:02.50" 62.5d0)
                                             ("1:02:03.50" 3723.5d0)
                                             ("2-01:02:03.50" 176523.5d0))
                   (cl-weave:it-property
                    "integer CPU seconds round-trip through ps clock text"
                    ((values (cl-weave:gen-integer :min 0 :max 100000)))
                    (let* ((minutes (floor values 60))
                           (seconds (mod values 60))
                           (seconds-field
                            (if (< seconds 10)
                                (concatenate (quote string)
                                             "0"
                                             (write-to-string seconds))
                                (write-to-string seconds)))
                           (text (format nil "~d:~a.00" minutes seconds-field)))
                      (cl-weave:expect (ntop.e2e::%parse-ps-cpu-duration text)
                                       :to-satisfy
                                       (lambda (actual)
                                         (= actual values)))))
                   (cl-weave:it "parses the three system load averages"
                                (cl-weave:expect
                                 (equalp
                                  (ntop.e2e::%parse-load-average
                                   "{ 1.00 2.50 3.75 }")
                                  #(1d0 2.5d0 3.75d0))
                                 :to-be
                                 t))
                   (define-n6-reply-cases (" 1b 5b 3f 32 35 3b 30 24 79" 0)
                                          (" 1b 5b 3f 32 35 3b 00 24 79" nil)
                                          ("  1b  5b  3f  32  35  3b  31  24  79"
                                           1)
                                          (" 1b 5b 3f 32 35 3b 32 24 79" 2)
                                          (" 1b 5b 3f 32 35 3b 33 24 79" 3)
                                          (" 1b 5b 3f 32 35 3b 34 24 79" 4)
                                          ("query timed out" nil))
                   (cl-weave:it
                    "expands N6 reply rows into cl-weave table tests"
                    (let ((expansion
                           (macroexpand-1
                            (quote
                             (define-n6-reply-cases
                              (" 1b 5b 3f 32 35 3b 30 24 79" 0))))))
                      (cl-weave:expect
                       (eq (first expansion) (find-symbol "IT-EACH" "CL-WEAVE"))
                       :to-be
                       t)))
                   (cl-weave:it "tracks private mode set and reset events"
                                (let ((events
                                       (ntop.e2e::%private-mode-events
                                        (format nil
                                                "~c[?25h~c[?25l"
                                                (code-char 27)
                                                (code-char 27)))))
                                  (cl-weave:expect
                                   (and
                                    (equal events
                                           (list (cons 25 :set)
                                                 (cons 25 :reset)))
                                    (ntop.e2e::%private-mode-reset-seen-p
                                     events))
                                   :to-be
                                   t))))

(cl-weave:describe "measurement manifest"
                   (cl-weave:it-each
                    (("n1" :running nil) ("n1" :complete nil)
                     ("n4" :failed "timed out")
                     ("n6" :unsupported "not supported"))
                    "persists measurement status for ~a"
                    (measurement status reason)
                    (let* ((directory (ntop.e2e::%make-measurement-directory))
                           (manifest-path
                            (merge-pathnames "manifest.sexp" directory)))
                      (ntop.e2e::%write-manifest directory
                                                 "btm"
                                                 measurement
                                                 status
                                                 reason)
                      (cl-weave:expect
                       (with-open-file
                           (stream manifest-path :external-format :utf-8)
                         (read stream))
                       :to-equal
                       (list :target
                             "btm"
                             :measurement
                             measurement
                             :status
                             status
                             :reason
                             reason)))))
