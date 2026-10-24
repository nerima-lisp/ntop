(in-package #:ntop.test)

(defmethod ntop.process.domain::compile-process-search ((query
                                                         (eql :successful-query))
                                                        regex-p
                                                        case-sensitive-p
                                                        whole-word-p
                                                        command-p
                                                        on-success
                                                        on-syntax-error)
  (declare (ignore query regex-p case-sensitive-p whole-word-p on-syntax-error))
  (funcall on-success
           (lambda (row)
             (string=
              (if command-p
                  (ntop.process.domain:process-row-command row)
                  (ntop.process.domain:process-row-name row))
              "worker"))))

(defmethod ntop.process.domain::compile-process-search ((query
                                                         (eql :invalid-query))
                                                        regex-p
                                                        case-sensitive-p
                                                        whole-word-p
                                                        command-p
                                                        on-success
                                                        on-syntax-error)
  (declare (ignore query
                   regex-p
                   case-sensitive-p
                   whole-word-p
                   command-p
                   on-success))
  (funcall on-syntax-error
           (ntop.process.domain::make-process-search-syntax-error :start-offset
                                                                  2
                                                                  :end-offset
                                                                  5
                                                                  :message
                                                                  "unexpected token")))

(cl-weave:describe "process search protocol"
                   (cl-weave:it
                    "passes command mode and a process-row predicate to success"
                    (let ((predicate nil)
                          (syntax-error nil)
                          (row
                           (ntop.process.domain:make-process-row :name
                                                                 "other"
                                                                 :command
                                                                 "worker")))
                      (ntop.process.domain::compile-process-search
                       :successful-query
                       nil
                       t
                       nil
                       t
                       (lambda (value)
                         (setf predicate value))
                       (lambda (value)
                         (setf syntax-error value)))
                      (cl-weave:expect (funcall predicate row) :to-be t)
                      (ntop.process.domain::compile-process-search
                       :successful-query
                       nil
                       t
                       nil
                       nil
                       (lambda (value)
                         (setf predicate value))
                       (lambda (value)
                         (setf syntax-error value)))
                      (cl-weave:expect (funcall predicate row) :to-be nil)
                      (cl-weave:expect syntax-error :to-be nil)))
                   (cl-weave:it
                    "sends a positioned syntax error to its continuation"
                    (let ((predicate nil)
                          (syntax-error nil))
                      (ntop.process.domain::compile-process-search
                       :invalid-query
                       nil
                       nil
                       t
                       nil
                       (lambda (value)
                         (setf predicate value))
                       (lambda (value)
                         (setf syntax-error value)))
                      (cl-weave:expect predicate :to-be nil)
                      (cl-weave:expect
                       (list
                        (typep syntax-error
                               'ntop.process.domain::process-search-syntax-error)
                        (ntop.process.domain::process-search-syntax-error-start-offset
                         syntax-error)
                        (ntop.process.domain::process-search-syntax-error-end-offset
                         syntax-error)
                        (ntop.process.domain::process-search-syntax-error-message
                         syntax-error))
                       :to-equal
                       '(t 2 5 "unexpected token")))))

(cl-weave:describe "process search syntax error constructor types"
                   (cl-weave:it-each
                    (("start-offset"
                      ntop.process.domain::make-process-search-syntax-error
                      ntop.process.domain::process-search-syntax-error-start-offset
                      nil
                      :start-offset
                      0
                      :type-error
                      2
                      -1)
                     ("end-offset"
                      ntop.process.domain::make-process-search-syntax-error
                      ntop.process.domain::process-search-syntax-error-end-offset
                      nil
                      :end-offset
                      0
                      :type-error
                      5
                      -1)
                     ("message"
                      ntop.process.domain::make-process-search-syntax-error
                      ntop.process.domain::process-search-syntax-error-message
                      nil
                      :message
                      ""
                      :type-error
                      "syntax"
                      3))
                    "checks constructor outcomes for ~A"
                    (slot constructor
                          accessor
                          required-initargs
                          initarg
                          default-value
                          nil-value
                          accepted-value
                          invalid-value)
                    (cl-weave:expect
                     (constructor-slot-outcomes constructor
                                                accessor
                                                required-initargs
                                                initarg
                                                nil-value
                                                accepted-value
                                                invalid-value)
                     :to-equal
                     (list default-value nil-value accepted-value :type-error))))

(cl-weave:describe "process search syntax error mutation"
                   (cl-weave:it-each
                    ((:start-offset 3 -1) (:end-offset 5 -1)
                     (:message "updated" 3))
                    "preserves the type of the mutable ~S slot"
                    (slot accepted-value invalid-value)
                    (let* ((accessor
                            (intern
                             (format nil "PROCESS-SEARCH-SYNTAX-ERROR-~A" slot)
                             "NTOP.PROCESS.DOMAIN"))
                           (condition
                            (ntop.process.domain:make-process-search-syntax-error)))
                      (funcall (fdefinition (list 'setf accessor))
                               accepted-value
                               condition)
                      (cl-weave:expect
                       (funcall (symbol-function accessor) condition)
                       :to-be
                       accepted-value)
                      (cl-weave:signals type-error
                                        (funcall
                                         (fdefinition (list 'setf accessor))
                                         invalid-value
                                         condition))
                      (cl-weave:expect
                       (funcall (symbol-function accessor) condition)
                       :to-be
                       accepted-value))))
