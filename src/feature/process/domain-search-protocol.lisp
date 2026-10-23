(in-package #:ntop.process.domain)

(defstruct process-search-syntax-error
  (start-offset 0 :type (integer 0 *))
  (end-offset 0 :type (integer 0 *))
  (message "" :type string))

(defgeneric compile-process-search
    (query regex-p case-sensitive-p whole-word-p on-success on-syntax-error)
  (:documentation
   "Compile QUERY; error offsets are zero-based and half-open character positions."))
