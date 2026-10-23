;;; Entries are string-keyed alists with string or list values.
;;; Every entry has kind and source; sources cite code, spec, or runtime evidence.
;;; Key rows add action, keys, contexts, and behavior. Mouse rows add action,
;;; gesture, contexts, behavior, and condition. Flag rows add snake_case,
;;; kebab_case, short, aliases, value_type, values, and condition. Column rows add
;;; header, field, aliases, condition, default, and slot. Search rows add field,
;;; tokens, condition, and implicit-match status. Extension rows add action, key,
;;; contexts, behavior, and relation. The getter returns a fresh cons tree.
(in-package #:ntop.app.domain)

(defmacro define-parity-keys (&body rows)
  `(list
    ,@(loop for (action keys contexts behavior source) in rows
            collect `(quote
                      (("kind" . "key") ("action" . ,action)
                       ("keys" . ,keys)
                       ("contexts" . ,contexts)
                       ("behavior" . ,behavior)
                       ("source" . ,source))))))

(defmacro define-parity-mice (&body rows)
  `(list
    ,@(loop for (action gesture contexts behavior source) in rows
            for cited-source = (append source (quote ("src/lib.rs:363-369")))
            collect `(quote
                      (("kind" . "mouse") ("action" . ,action)
                       ("gesture" . ,gesture)
                       ("contexts" . ,contexts)
                       ("behavior" . ,behavior)
                       ("condition" .
                                    "--disable_click disables mouse capture")
                       ("source" . ,cited-source))))))

(defmacro define-parity-flags (&body rows)
  `(list
    ,@(loop for (name short aliases value-type values condition source) in rows
            for kebab = (substitute #\- #\_ name)
            collect `(quote
                      (("kind" . "flag") ("snake_case" . ,name)
                       ("kebab_case" . ,kebab)
                       ("short" . ,short)
                       ("aliases" . ,aliases)
                       ("value_type" . ,value-type)
                       ("values" . ,values)
                       ("condition" . ,condition)
                       ("source" . ,source))))))

(defmacro define-parity-columns (&body rows)
  `(list
    ,@(loop for (header field aliases condition default slot source) in rows
            collect `(quote
                      (("kind" . "column") ("header" . ,header)
                       ("field" . ,field)
                       ("aliases" . ,aliases)
                       ("condition" . ,condition)
                       ("default" . ,default)
                       ("slot" . ,slot)
                       ("source" . ,source))))))

(defmacro define-parity-search-fields (&body rows)
  `(list
    ,@(loop for (field tokens condition implicit source) in rows
            collect `(quote
                      (("kind" . "search-field") ("field" . ,field)
                       ("tokens" . ,tokens)
                       ("condition" . ,condition)
                       ("implicit" . ,implicit)
                       ("source" . ,source))))))

(defmacro define-parity-extensions (&body rows)
  `(list
    ,@(loop for (action key contexts behavior relation source) in rows
            collect `(quote
                      (("kind" . "extension") ("action" . ,action)
                       ("key" . ,key)
                       ("contexts" . ,contexts)
                       ("behavior" . ,behavior)
                       ("relation" . ,relation)
                       ("source" . ,source))))))
