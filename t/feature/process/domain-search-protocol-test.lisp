(in-package #:ntop.test)

(defmethod ntop.process.domain::compile-process-search
    ((query (eql :successful-query)) regex-p case-sensitive-p whole-word-p
     on-success on-syntax-error)
  (declare (ignore query regex-p case-sensitive-p whole-word-p on-syntax-error))
  (funcall on-success
           (lambda (row)
             (string= (ntop.process.domain:process-row-name row) "worker"))))

(defmethod ntop.process.domain::compile-process-search
    ((query (eql :invalid-query)) regex-p case-sensitive-p whole-word-p
     on-success on-syntax-error)
  (declare (ignore query regex-p case-sensitive-p whole-word-p on-success))
  (funcall on-syntax-error
           (ntop.process.domain::make-process-search-syntax-error
            :start-offset 2
            :end-offset 5
            :message "unexpected token")))

(cl-weave:describe "process search protocol"
  (cl-weave:it "sends a predicate that accepts one process row"
    (let ((predicate nil)
          (syntax-error nil)
          (row (ntop.process.domain:make-process-row :name "worker")))
      (ntop.process.domain::compile-process-search
       :successful-query nil t nil
       (lambda (value) (setf predicate value))
       (lambda (value) (setf syntax-error value)))
      (cl-weave:expect (funcall predicate row) :to-be t)
      (cl-weave:expect
       (funcall predicate
                (ntop.process.domain:make-process-row :name "other"))
       :to-be nil)
      (cl-weave:expect syntax-error :to-be nil)))

  (cl-weave:it "sends a positioned syntax error to its continuation"
    (let ((predicate nil)
          (syntax-error nil))
      (ntop.process.domain::compile-process-search
       :invalid-query nil nil t
       (lambda (value) (setf predicate value))
       (lambda (value) (setf syntax-error value)))
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
       :to-equal '(t 2 5 "unexpected token")))))
