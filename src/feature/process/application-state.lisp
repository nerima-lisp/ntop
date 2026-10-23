(in-package #:ntop.process.application)

(defstruct process-search-state
  query
  predicate
  syntax-error)

(defun compile-process-search-state (state query config)
  (let ((regex-p
          (ntop.shared.application:application-config-process-regex-p config))
        (case-sensitive-p
          (ntop.shared.application:application-config-process-case-sensitive-p
           config))
        (whole-word-p
          (ntop.shared.application:application-config-process-whole-word-p config)))
    (ntop.process.domain:compile-process-search
     query regex-p case-sensitive-p whole-word-p
     (lambda (predicate)
       (setf (process-search-state-query state) query
             (process-search-state-predicate state) predicate
             (process-search-state-syntax-error state) nil))
     (lambda (syntax-error)
       (setf (process-search-state-query state) query
             (process-search-state-syntax-error state) syntax-error))))
  state)
