;;; The catalog shares read-only records; callers must treat them as immutable.
;;; Use parity-entry-kind to dispatch and the parity-<kind>-<slot> accessors.
;;; parity-flag-kebab-case derives the CLI spelling from each snake-case value.
(in-package #:ntop.app.domain)

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *parity-record-schemas*
      '((:key parity-key make-parity-key parity-key-
         (action keys contexts behavior source))
        (:mouse parity-mouse make-parity-mouse parity-mouse-
         (action gesture contexts behavior condition source))
        (:flag parity-flag make-parity-flag parity-flag-
         (snake-case short aliases value-type values condition source))
        (:column parity-column make-parity-column parity-column-
         (header field aliases condition default slot source))
        (:search-field parity-search-field make-parity-search-field
         parity-search-field- (field tokens condition implicit source))
        (:extension parity-extension make-parity-extension parity-extension-
         (action key contexts behavior relation source))))
    (defstruct (parity-entry
                 (:conc-name parity-entry-)
                 (:constructor nil)
                 (:copier nil))
      (kind nil :read-only t)))
  (defmacro define-parity-table (table-name kind &body rows)
    (unless (symbolp table-name)
      (error "Parity table name must be a symbol: ~S" table-name))
    (let ((schema (assoc kind *parity-record-schemas*)))
      (unless schema
        (error "Unknown parity record kind: ~S" kind))
      (destructuring-bind (schema-kind record-type constructor prefix slots)
          schema
        (dolist (row rows)
          (unless (and (listp row) (= (length row) (length slots)))
            (error "A ~S row must have ~D fields: ~S"
                   kind (length slots) row)))
        `(progn
           (defstruct (,record-type
                        (:include parity-entry (kind ,schema-kind))
                        (:conc-name ,prefix)
                        (:constructor ,constructor (&key ,@slots))
                        (:copier nil))
             ,@(loop for slot in slots
                     collect `(,slot nil :read-only t)))
           (defparameter ,table-name
             (list
              ,@(loop for row in rows
                      collect
                      `(,constructor
                        ,@(loop for slot in slots
                                for value in row
                                append
                                (list (intern (symbol-name slot) :keyword)
                                      (list 'quote value))))))))))))

