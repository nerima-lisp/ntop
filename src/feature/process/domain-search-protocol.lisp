(in-package #:ntop.process.domain)

(defstruct process-search-syntax-error
  (start-offset 0 :type (integer 0 *))
  (end-offset 0 :type (integer 0 *))
  (message "" :type string))

(defgeneric compile-process-search
    (query regex-p case-sensitive-p whole-word-p command-p on-success on-syntax-error)
  (:documentation
   "Compile QUERY. COMMAND-P makes Name-field terms search the full command.
   ON-SUCCESS receives a PROCESS-ROW predicate. ON-SYNTAX-ERROR receives a
   PROCESS-SEARCH-SYNTAX-ERROR with zero-based, half-open offsets."))
