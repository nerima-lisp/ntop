;;; btm-parity-catalog returns fresh kind-specific records with source evidence.
;;; Use parity-entry-kind to dispatch and the parity-<kind>-<slot> accessors.
;;; parity-flag-kebab-case derives the CLI spelling from each snake-case value.
(in-package #:ntop.app.domain)

(progn
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *parity-record-schemas*
      (quote ((:key parity-key make-parity-key parity-key-
               (action keys contexts behavior source))
              (:mouse parity-mouse make-parity-mouse parity-mouse-
               (action gesture contexts behavior condition source))
              (:flag parity-flag make-parity-flag parity-flag-
               (snake-case short aliases value-type values condition source))
              (:column parity-column make-parity-column parity-column-
               (header field aliases condition default slot source))
              (:search-field parity-search-field make-parity-search-field parity-search-field-
               (field tokens condition implicit source))
              (:extension parity-extension make-parity-extension parity-extension-
               (action key contexts behavior relation source)))))
    (defgeneric parity-entry-kind (entry))
    (defgeneric copy-parity-entry (entry))
    (defun parity-record-accessor-symbol (prefix record-type slot)
      (intern (concatenate 'string
                           (symbol-name prefix)
                           (symbol-name slot))
              (symbol-package record-type)))
    (defun parity-record-structure-form (record-type kind constructor prefix slots)
      (cons 'defstruct
            (cons (list record-type
                        (list :conc-name prefix)
                        (list :constructor constructor)
                        (list :copier nil))
                  (cons (list 'kind kind :read-only t)
                        (loop for slot in slots
                              collect (list slot nil :read-only t))))))
    (defun parity-record-constructor-form (constructor slots values)
      (cons constructor
            (loop for slot in slots
                  for value in values
                  append (list (intern (symbol-name slot) :keyword) value))))
    (defun parity-entry-kind-method-form (record-type prefix)
      (list 'defmethod 'parity-entry-kind
            (list (list 'entry record-type))
            (list (parity-record-accessor-symbol prefix record-type 'kind)
                  'entry)))
    (defun parity-copy-method-form (record-type constructor prefix slots)
      (list 'defmethod 'copy-parity-entry
            (list (list 'entry record-type))
            (parity-record-constructor-form
             constructor
             slots
             (loop for slot in slots
                   collect (list 'copy-parity-value
                                 (list (parity-record-accessor-symbol
                                        prefix record-type slot)
                                       'entry))))))
    (defun copy-parity-value (value)
      (typecase value
        (string (copy-seq value))
        (cons (cons (copy-parity-value (car value))
                    (copy-parity-value (cdr value))))
        (t value))))
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
        (list 'progn
              (parity-record-structure-form
               record-type schema-kind constructor prefix slots)
              (parity-entry-kind-method-form record-type prefix)
              (parity-copy-method-form record-type constructor prefix slots)
              (list 'defparameter
                    table-name
                    (cons 'list
                          (mapcar
                           (lambda (row)
                             (parity-record-constructor-form
                              constructor slots
                              (mapcar (lambda (value) (list 'quote value))
                                      row)))
                           rows))))))))

