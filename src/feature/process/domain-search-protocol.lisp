(in-package #:ntop.process.domain)

(progn
  (defun %validate-syntax-error-value (value expected-type)
    (if (typep value expected-type)
        value
        (error (quote type-error) :datum value :expected-type expected-type)))
  (defun %validate-syntax-error-slots (instance)
    (dolist
        (specification
         '((process-search-syntax-error-start-offset (integer 0 *))
           (process-search-syntax-error-end-offset (integer 0 *))
           (process-search-syntax-error-message string))
         instance)
      (destructuring-bind (accessor expected-type) specification
        (%validate-syntax-error-value
         (funcall (symbol-function accessor) instance)
         expected-type)))))

(progn
  (defstruct
      (process-search-syntax-error
       (:constructor make-process-search-syntax-error-raw)
       (:conc-name %process-search-syntax-error-))
    start-offset
    end-offset
    message)
  (defun make-process-search-syntax-error (&key (start-offset 0)
                                                (end-offset 0)
                                                (message ""))
    (let ((instance
           (make-process-search-syntax-error-raw :start-offset
                                                 start-offset
                                                 :end-offset
                                                 end-offset
                                                 :message
                                                 message)))
      (%validate-syntax-error-slots instance)
      instance))
  (defun process-search-syntax-error-start-offset (condition)
    (%process-search-syntax-error-start-offset condition))
  (defun (setf process-search-syntax-error-start-offset) (value condition)
    (%validate-syntax-error-slots
     (make-process-search-syntax-error-raw :start-offset
                                           value
                                           :end-offset
                                           (%process-search-syntax-error-end-offset
                                            condition)
                                           :message
                                           (%process-search-syntax-error-message
                                            condition)))
    (setf (%process-search-syntax-error-start-offset condition) value))
  (defun process-search-syntax-error-end-offset (condition)
    (%process-search-syntax-error-end-offset condition))
  (defun (setf process-search-syntax-error-end-offset) (value condition)
    (%validate-syntax-error-slots
     (make-process-search-syntax-error-raw :start-offset
                                           (%process-search-syntax-error-start-offset
                                            condition)
                                           :end-offset
                                           value
                                           :message
                                           (%process-search-syntax-error-message
                                            condition)))
    (setf (%process-search-syntax-error-end-offset condition) value))
  (defun process-search-syntax-error-message (condition)
    (%process-search-syntax-error-message condition))
  (defun (setf process-search-syntax-error-message) (value condition)
    (%validate-syntax-error-slots
     (make-process-search-syntax-error-raw :start-offset
                                           (%process-search-syntax-error-start-offset
                                            condition)
                                           :end-offset
                                           (%process-search-syntax-error-end-offset
                                            condition)
                                           :message
                                           value))
    (setf (%process-search-syntax-error-message condition) value)))

(defgeneric compile-process-search (query regex-p
                                          case-sensitive-p
                                          whole-word-p
                                          command-p
                                          on-success
                                          on-syntax-error)
  (:documentation
   "Compile QUERY. COMMAND-P makes Name-field terms search the full command.
   ON-SUCCESS receives a PROCESS-ROW predicate. ON-SYNTAX-ERROR receives a
   PROCESS-SEARCH-SYNTAX-ERROR with zero-based, half-open offsets."))
