(in-package #:ntop.test)

(cl-weave:describe "process search syntax"
                   (cl-weave:it-each
                    (("(" 1 1) (")" 0 1)
                     ("cpu > nope" 6 10)
                     ("cpu >" 5 5)
                     ("alpha and" 9 9)
                     ("alpha or )" 9 10)
                     ("é and cpu > nope" 12 16)
                     ("\"unterminated" 0 13)
                     ("()" 0 1)
                     ("!" 0 1)
                     ("state =" 7 7)
                     ("a or" 4 4)
                     ("a >" 2 3)
                     ("cpu != abc" 7 10)
                     ("cpu = ! 5" 6 7)
                     ("cpu =!" 5 6)
                     ("pid = !" 6 7)
                     ("user = !" 7 8)
                     ("state !" 6 7)
                     ("!cpu > 5" 0 1)
                     ("! cpu > 5" 0 1)
                     ("user != !" 8 9)
                     ("gmem = \"what\"" 7 13)
                     ("cpu = 食" 6 7)
                     ("gmem% = \"the\"" 8 13)
                     ("gpu% = \"heck\"" 7 13)
                     ("user !=" 7 7)
                     ("state !=" 8 8)
                     ("time !=" 7 7)
                     ("cpu = \"42.5\"" 6 12)
                     ("time = \"1h\"" 7 11))
                    "invalid query ~S has character span ~S to ~S"
                    (query start-offset end-offset)
                    (let ((syntax-error (process-query-test-syntax-error query)))
                      (cl-weave:expect
                       (typep syntax-error
                              'ntop.process.domain::process-search-syntax-error)
                       :to-be
                       t)
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-search-syntax-error-start-offset
                         syntax-error)
                        (ntop.process.domain::process-search-syntax-error-end-offset
                         syntax-error))
                       :to-equal
                       (list start-offset end-offset))))
                   (cl-weave:it-each
                    (("cpu = 1.2.3" 6 11) ("cpu = 1e2e3" 6 11)
                     ("cpu = 1e309" 6 11)
                     ("cpu = 1e-325" 6 12)
                     ("cpu 5" 0 3)
                     ("time 5" 0 4)
                     ("time = 1" 7 8)
                     ("time = xyz" 7 10)
                     ("time = 1fortnight" 7 17)
                     ("time = 18446744074s" 7 19))
                    "invalid numeric or duration query ~S has character span ~S to ~S"
                    (query start-offset end-offset)
                    (let ((syntax-error (process-query-test-syntax-error query)))
                      (cl-weave:expect
                       (typep syntax-error
                              'ntop.process.domain::process-search-syntax-error)
                       :to-be
                       t)
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-search-syntax-error-start-offset
                         syntax-error)
                        (ntop.process.domain::process-search-syntax-error-end-offset
                         syntax-error))
                       :to-equal
                       (list start-offset end-offset))))
                   (cl-weave:it
 "rejects empty duration text after parsing no segments"
 (let ((state (ntop.process.domain::%process-search-parser-state ""))
       (token (cl-parser-kit:make-token
               :type :word
               :text ""
               :value ""
               :start 0
               :end 0)))
   (cl-weave:signals ntop.process.domain::process-search-query-error
     (ntop.process.domain::%parse-process-search-duration state token))))
                   (cl-weave:it
                    "reports the regex kit position for an invalid expression"
                    (let ((syntax-error
                           (process-query-test-syntax-error "[Alpha" t)))
                      (cl-weave:expect
                       (typep syntax-error
                              'ntop.process.domain::process-search-syntax-error)
                       :to-be
                       t)
                      (cl-weave:expect
                       (list
                        (ntop.process.domain::process-search-syntax-error-start-offset
                         syntax-error)
                        (ntop.process.domain::process-search-syntax-error-end-offset
                         syntax-error))
                       :to-equal
                       '(6 6))))
                   (cl-weave:it
                    "rejects oversized and overnested queries at their exact spans"
                    (let* ((maximum-depth
                            ntop.process.domain::*process-search-maximum-depth*)
                           (too-long-number
                            (concatenate 'string
                                         "cpu = "
                                         (make-string 129 :initial-element #\1)))
                           (too-long-duration
                            (concatenate 'string
                                         "time = "
                                         (make-string 129 :initial-element #\1)))
                           (too-long-source
                            (make-string
                             (1+
                              cl-parser-kit:*maximum-tokenizer-source-length*)
                             :initial-element
                             #\a))
                           (too-deep-groups
                            (concatenate 'string
                                         (make-string (1+ maximum-depth)
                                                      :initial-element
                                                      #\()
                                         "Alpha"
                                         (make-string (1+ maximum-depth)
                                                      :initial-element
                                                      #\))))
                           (too-deep-negations
                            (concatenate 'string
                                         (make-string (1+ maximum-depth)
                                                      :initial-element
                                                      #\!)
                                         "Alpha"))
                           (cases
                            (list
                             (list too-long-number 6 (length too-long-number))
                             (list too-long-duration
                                   7
                                   (length too-long-duration))
                             (list too-long-source 0 (length too-long-source))
                             (list too-deep-groups
                                   (1+ maximum-depth)
                                   (+ (1+ maximum-depth) (length "Alpha")))
                             (list too-deep-negations
                                   (1+ maximum-depth)
                                   (+ (1+ maximum-depth) (length "Alpha"))))))
                      (dolist
                          (case cases)
                        (destructuring-bind (query start-offset end-offset) case
                          (let ((syntax-error
                                 (process-query-test-syntax-error query)))
                            (cl-weave:expect
                             (typep syntax-error
                                    'ntop.process.domain::process-search-syntax-error)
                             :to-be
                             t)
                            (cl-weave:expect
                             (list
                              (ntop.process.domain::process-search-syntax-error-start-offset
                               syntax-error)
                              (ntop.process.domain::process-search-syntax-error-end-offset
                               syntax-error))
                             :to-equal
                             (list start-offset end-offset)))))))
                   (cl-weave:it
 "rejects values that overflow the supported double-float range"
 (let* ((query
         (concatenate 'string
                      "cpu = "
                      (make-string 64 :initial-element (char "9" 0))
                      "e308"))
        (syntax-error (process-query-test-syntax-error query)))
   (cl-weave:expect
    (typep syntax-error 'ntop.process.domain::process-search-syntax-error)
    :to-be t)
   (cl-weave:expect
    (list
     (ntop.process.domain::process-search-syntax-error-start-offset syntax-error)
     (ntop.process.domain::process-search-syntax-error-end-offset syntax-error))
    :to-equal
    (list 6 (length query)))))
                   (cl-weave:it-each
 ((nil 0 0 5) (nil 1 5 5) (t 0 0 5))
 "uses parser failure details when no query error was recorded"
 (failure-p parser-position expected-start expected-end)
 (let ((syntax-error nil))
   (cl-weave:with-mocked-functions
     (((symbol-function 'cl-parser-kit:parse-all)
       (lambda (parser tokens)
         (declare (ignore parser tokens))
         (values
          nil
          nil
          parser-position
          (when failure-p
            (cl-parser-kit:make-parse-failure
             :position parser-position
             :expected :process-search-expression
             :actual nil))))))
     (setf syntax-error (process-query-test-syntax-error "Alpha")))
   (cl-weave:expect
    (typep syntax-error 'ntop.process.domain::process-search-syntax-error)
    :to-be t)
   (cl-weave:expect
    (list
     (ntop.process.domain::process-search-syntax-error-start-offset syntax-error)
     (ntop.process.domain::process-search-syntax-error-end-offset syntax-error))
    :to-equal
    (list expected-start expected-end)))))
