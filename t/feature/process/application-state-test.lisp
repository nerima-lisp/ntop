(in-package #:ntop.test)

(defvar *wp4-process-search-call* nil)
(defvar *wp4-process-search-error* nil)

(defmethod ntop.process.domain:compile-process-search
    ((query (eql :wp4-search-success)) regex-p case-sensitive-p whole-word-p
     command-p on-success on-syntax-error)
  (declare (ignore query))
  (setf *wp4-process-search-call*
        (list regex-p case-sensitive-p whole-word-p command-p))
  (funcall on-success #'identity))

(defmethod ntop.process.domain:compile-process-search
    ((query (eql :wp4-search-error)) regex-p case-sensitive-p whole-word-p
     command-p on-success on-syntax-error)
  (declare (ignore query regex-p case-sensitive-p whole-word-p command-p on-success))
  (funcall on-syntax-error *wp4-process-search-error*))

(cl-weave:describe "process application search state"
  (cl-weave:it "starts without a query, predicate, or syntax error"
    (let ((state (ntop.process.application::make-process-search-state)))
      (cl-weave:expect
       (list
        (ntop.process.application::process-search-state-query state)
        (ntop.process.application::process-search-state-predicate state)
        (ntop.process.application::process-search-state-syntax-error state))
       :to-equal '(nil nil nil))))
  (cl-weave:it-each
    ((:literal nil nil nil nil)
     (:case-sensitive nil t nil nil)
     (:whole-word nil nil t nil)
     (:command nil nil nil t)
     (:all-options t t t t))
    "passes the configured search flags for ~S"
    (case regex-p case-sensitive-p whole-word-p command-p)
    (declare (ignorable case))
    (let* ((*wp4-process-search-call* nil)
           (config
             (ntop.shared.application:make-application-config
              :process-regex-p regex-p
              :process-case-sensitive-p case-sensitive-p
              :process-whole-word-p whole-word-p
              :process-show-command-p command-p))
           (state (ntop.process.application::make-process-search-state)))
      (ntop.process.application::compile-process-search-state
       state :wp4-search-success config)
      (cl-weave:expect *wp4-process-search-call*
                       :to-equal (list regex-p case-sensitive-p whole-word-p command-p))))
  (cl-weave:it "stores the predicate delivered to the success continuation"
    (let* ((*wp4-process-search-call* nil)
           (config (ntop.shared.application:make-application-config))
           (state (ntop.process.application::make-process-search-state)))
      (ntop.process.application::compile-process-search-state
       state :wp4-search-success config)
      (cl-weave:expect
       (list
        (ntop.process.application::process-search-state-query state)
        (funcall
         (ntop.process.application::process-search-state-predicate state)
         :value)
        (ntop.process.application::process-search-state-syntax-error state))
       :to-equal '(:wp4-search-success :value nil))))
  (cl-weave:it "stores the opaque syntax error from the error continuation"
    (let* ((*wp4-process-search-error* (list :opaque-error))
           (config (ntop.shared.application:make-application-config))
           (state (ntop.process.application::make-process-search-state)))
      (ntop.process.application::compile-process-search-state
       state :wp4-search-error config)
      (cl-weave:expect
       (ntop.process.application::process-search-state-query state)
       :to-be :wp4-search-error)
      (cl-weave:expect
       (ntop.process.application::process-search-state-syntax-error state)
       :to-be *wp4-process-search-error*)
      (cl-weave:expect
       (ntop.process.application::process-search-state-predicate state)
       :to-be nil)))
  (cl-weave:it "keeps the last successful predicate when a later query is invalid"
    (let* ((*wp4-process-search-call* nil)
           (*wp4-process-search-error* :opaque-error)
           (config (ntop.shared.application:make-application-config))
           (state (ntop.process.application::make-process-search-state)))
      (ntop.process.application::compile-process-search-state
       state :wp4-search-success config)
      (let ((predicate
              (ntop.process.application::process-search-state-predicate state)))
        (ntop.process.application::compile-process-search-state
         state :wp4-search-error config)
        (cl-weave:expect
         (ntop.process.application::process-search-state-predicate state)
         :to-be predicate)
        (cl-weave:expect
         (ntop.process.application::process-search-state-syntax-error state)
         :to-be :opaque-error))))
  (cl-weave:it "clears a prior syntax error after a successful compile"
    (let* ((*wp4-process-search-call* nil)
           (*wp4-process-search-error* :opaque-error)
           (config (ntop.shared.application:make-application-config))
           (state (ntop.process.application::make-process-search-state)))
      (ntop.process.application::compile-process-search-state
       state :wp4-search-error config)
      (ntop.process.application::compile-process-search-state
       state :wp4-search-success config)
      (cl-weave:expect
       (ntop.process.application::process-search-state-syntax-error state)
       :to-be nil)
      (cl-weave:expect
       (ntop.process.application::process-search-state-query state)
       :to-be :wp4-search-success))))
