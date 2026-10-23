(in-package #:ntop.e2e)

(defmacro define-domain-parity-json-schema (&body schemas)
  `(defparameter *domain-parity-json-schema*
     ',schemas))

(progn
  (define-domain-parity-json-schema
   (:key ("kind" :keyword ntop.app.domain:parity-entry-kind)
         ("action" :identity ntop.app.domain:parity-key-action)
         ("keys" :array ntop.app.domain:parity-key-keys)
         ("contexts" :array ntop.app.domain:parity-key-contexts)
         ("behavior" :identity ntop.app.domain:parity-key-behavior)
         ("source" :array ntop.app.domain:parity-key-source))
   (:mouse ("kind" :keyword ntop.app.domain:parity-entry-kind)
           ("action" :identity ntop.app.domain:parity-mouse-action)
           ("gesture" :identity ntop.app.domain:parity-mouse-gesture)
           ("contexts" :array ntop.app.domain:parity-mouse-contexts)
           ("behavior" :identity ntop.app.domain:parity-mouse-behavior)
           ("condition" :identity ntop.app.domain:parity-mouse-condition)
           ("source" :array ntop.app.domain:parity-mouse-source))
   (:flag ("kind" :keyword ntop.app.domain:parity-entry-kind)
          ("snake_case" :identity ntop.app.domain:parity-flag-snake-case)
          ("kebab_case" :identity ntop.app.domain:parity-flag-kebab-case)
          ("short" :array ntop.app.domain:parity-flag-short)
          ("aliases" :array ntop.app.domain:parity-flag-aliases)
          ("value_type" :identity ntop.app.domain:parity-flag-value-type)
          ("values" :array ntop.app.domain:parity-flag-values)
          ("condition" :identity ntop.app.domain:parity-flag-condition)
          ("source" :array ntop.app.domain:parity-flag-source))
   (:column ("kind" :keyword ntop.app.domain:parity-entry-kind)
            ("header" :identity ntop.app.domain:parity-column-header)
            ("field" :identity ntop.app.domain:parity-column-field)
            ("aliases" :array ntop.app.domain:parity-column-aliases)
            ("condition" :identity ntop.app.domain:parity-column-condition)
            ("default" :nullable ntop.app.domain:parity-column-default)
            ("slot" :identity ntop.app.domain:parity-column-slot)
            ("source" :array ntop.app.domain:parity-column-source))
   (:search-field ("kind" :keyword ntop.app.domain:parity-entry-kind)
                  ("field" :identity ntop.app.domain:parity-search-field-field)
                  ("tokens" :array ntop.app.domain:parity-search-field-tokens)
                  ("condition" :identity
                               ntop.app.domain:parity-search-field-condition)
                  ("implicit" :identity
                              ntop.app.domain:parity-search-field-implicit)
                  ("source" :array ntop.app.domain:parity-search-field-source))
   (:extension ("kind" :keyword ntop.app.domain:parity-entry-kind)
               ("action" :identity ntop.app.domain:parity-extension-action)
               ("key" :identity ntop.app.domain:parity-extension-key)
               ("contexts" :array ntop.app.domain:parity-extension-contexts)
               ("behavior" :identity ntop.app.domain:parity-extension-behavior)
               ("relation" :identity ntop.app.domain:parity-extension-relation)
               ("source" :array ntop.app.domain:parity-extension-source)))
  (defmacro define-domain-parity-json-cases (&body cases)
    `(cl-weave:it-each ,cases
                       "encodes typed parity records for ~A"
                       (case-name constructor initargs expected)
                       (declare (ignore case-name))
                       (cl-weave:expect
                        (json-kit:stringify
                         (domain-parity-entry-to-json-object
                          (apply constructor initargs)))
                        :to-equal
                        (apply #'concatenate 'string expected)))))

(progn
  (defun domain-parity-json-value (entry accessor conversion)
    (let ((value (funcall accessor entry)))
      (ecase conversion
        (:identity value)
        (:keyword (string-downcase (symbol-name value)))
        (:array (coerce value 'vector))
        (:nullable
         (if (null value)
             json-kit:+json-null+
             value)))))
  (defun domain-parity-entry-fields (entry schema)
    (loop for (key conversion accessor) in (cdr schema)
          collect (cons key
                        (domain-parity-json-value entry accessor conversion))))
  (defun call-with-domain-parity-schema (entry on-known-kind on-unknown-kind)
    (let* ((kind (ntop.app.domain:parity-entry-kind entry))
           (schema (assoc kind *domain-parity-json-schema*)))
      (if schema
          (funcall on-known-kind schema)
          (funcall on-unknown-kind kind))))
  (defun domain-parity-entry-to-json-object (entry)
    (call-with-domain-parity-schema entry
                                    (lambda (schema)
                                      (json-kit:alist->json-object
                                       (domain-parity-entry-fields entry schema)
                                       :duplicate-key-policy
                                       :preserve))
                                    (lambda (kind)
                                      (error
                                       "No JSON schema for parity entry kind ~S"
                                       kind)))))

(defun domain-parity-json-array (&optional
                                 (catalog (ntop.app.domain::btm-parity-catalog)))
  (map 'vector #'domain-parity-entry-to-json-object catalog))

(defun export-domain-parity (&key (output "-"))
  (let ((value (domain-parity-json-array)))
    (if (string= output "-")
        (progn
          (json-kit:write-json value *standard-output*)
          (terpri *standard-output*))
        (with-open-file
            (stream output
                    :direction
                    :output
                    :external-format
                    :utf-8
                    :if-exists
                    :supersede
                    :if-does-not-exist
                    :create)
          (json-kit:write-json value stream)
          (terpri stream)))
    0))
