(in-package #:ntop.test)

(defmethod ntop.process.domain::compile-process-search
    ((query (eql :successful-query)) regex-p case-sensitive-p whole-word-p command-p
     on-success on-syntax-error)
  (declare (ignore query regex-p case-sensitive-p whole-word-p on-syntax-error))
  (funcall on-success
           (lambda (row)
             (string=
              (if command-p
                  (ntop.process.domain:process-row-command row)
                  (ntop.process.domain:process-row-name row))
              "worker"))))

(defmethod ntop.process.domain::compile-process-search
    ((query (eql :invalid-query)) regex-p case-sensitive-p whole-word-p command-p
     on-success on-syntax-error)
  (declare
   (ignore query regex-p case-sensitive-p whole-word-p command-p on-success))
  (funcall on-syntax-error
           (ntop.process.domain::make-process-search-syntax-error
            :start-offset 2
            :end-offset 5
            :message "unexpected token")))

(cl-weave:describe "process search protocol"
  (cl-weave:it "passes command mode and a process-row predicate to success"
    (let ((predicate nil)
          (syntax-error nil)
          (row (ntop.process.domain:make-process-row
                :name "other" :command "worker")))
      (ntop.process.domain::compile-process-search
       :successful-query nil t nil t
       (lambda (value) (setf predicate value))
       (lambda (value) (setf syntax-error value)))
      (cl-weave:expect (funcall predicate row) :to-be t)
      (ntop.process.domain::compile-process-search
       :successful-query nil t nil nil
       (lambda (value) (setf predicate value))
       (lambda (value) (setf syntax-error value)))
      (cl-weave:expect (funcall predicate row) :to-be nil)
      (cl-weave:expect syntax-error :to-be nil)))

  (cl-weave:it "sends a positioned syntax error to its continuation"
    (let ((predicate nil)
          (syntax-error nil))
      (ntop.process.domain::compile-process-search
       :invalid-query nil nil t nil
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
