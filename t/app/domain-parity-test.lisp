(in-package #:ntop.test)

(defvar *btm-parity-catalog-fixture*
  nil)

(defun parity-entry-value (entry key)
  (cdr (assoc key entry :test #'equal)))

(defun parity-rows (kind)
  (remove-if-not
   (lambda (entry)
     (equal kind (parity-entry-value entry "kind")))
   *btm-parity-catalog-fixture*))

(cl-weave:describe "btm parity catalog"
                   (cl-weave:before-all
                    (setf *btm-parity-catalog-fixture* (ntop.app.domain:btm-parity-catalog)))
                   (cl-weave:after-all
                    (setf *btm-parity-catalog-fixture* nil))
                   (cl-weave:it-each
                    (("key" 68) ("mouse" 12)
                     ("flag" 52)
                     ("column" 20)
                     ("search-field" 17)
                     ("extension" 4))
                    "matches the recorded btm count for ~A"
                    (kind expected-count)
                    (cl-weave:expect (length (parity-rows kind))
                                     :to-equal
                                     expected-count))
                   (cl-weave:it-each
                    (("key" ("action" "keys" "contexts" "behavior" "source"))
                     ("mouse"
                      ("action" "gesture"
                                "contexts"
                                "behavior"
                                "condition"
                                "source"))
                     ("flag"
                      ("snake_case" "kebab_case"
                                    "short"
                                    "aliases"
                                    "value_type"
                                    "values"
                                    "condition"
                                    "source"))
                     ("column"
                      ("header" "field"
                                "aliases"
                                "condition"
                                "default"
                                "slot"
                                "source"))
                     ("search-field"
                      ("field" "tokens" "condition" "implicit" "source"))
                     ("extension"
                      ("action" "key" "contexts" "behavior" "relation" "source")))
                    "uses the ~A row schema"
                    (kind required-keys)
                    (cl-weave:expect
                     (every
                      (lambda (entry)
                        (every
                         (lambda (key)
                           (assoc key entry :test #'equal))
                         required-keys))
                      (parity-rows kind))
                     :to-be
                     t))
                   (cl-weave:it "cites a source for every entry"
                                (cl-weave:expect
                                 (every
                                  (lambda (entry)
                                    (let ((sources
                                           (parity-entry-value entry "source")))
                                      (and (consp sources)
                                           (every
                                            (lambda (source)
                                              (and (stringp source)
                                                   (plusp (length source))))
                                            sources))))
                                  *btm-parity-catalog-fixture*)
                                 :to-be
                                 t))
                   (cl-weave:it-property
                    "derives kebab-case for generated flag indexes"
                    ((index (cl-weave:gen-integer :min 0 :max 51)))
                    (let* ((flags (parity-rows "flag"))
                           (entry (nth index flags))
                           (snake (parity-entry-value entry "snake_case")))
                      (cl-weave:expect (parity-entry-value entry "kebab_case")
                                       :to-equal
                                       (substitute #\- #\_ snake))))
                   (cl-weave:it-each
                    (("define-parity-keys"
                      (ntop.app.domain::define-parity-keys
                       ("probe" ("x") ("normal") "probe action" ("test:1")))
                      "key")
                     ("define-parity-mice"
                      (ntop.app.domain::define-parity-mice
                       ("probe" "left-down"
                                ("widget")
                                "probe gesture"
                                ("test:1")))
                      "mouse")
                     ("define-parity-flags"
                      (ntop.app.domain::define-parity-flags
                       ("sample_flag" "" () "boolean" () "all" ("test:1")))
                      "flag")
                     ("define-parity-columns"
                      (ntop.app.domain::define-parity-columns
                       ("CPU%" "CpuPercent"
                               ("cpu%")
                               "all"
                               "yes"
                               "1 CPU"
                               ("test:1")))
                      "column")
                     ("define-parity-search-fields"
                      (ntop.app.domain::define-parity-search-fields
                       ("PID" ("pid") "all" "no" ("test:1")))
                      "search-field")
                     ("define-parity-extensions"
                      (ntop.app.domain::define-parity-extensions
                       ("history" "[" ("graphs") "back" "extra" ("test:1")))
                      "extension"))
                    "expands ~A into literal catalog rows"
                    (macro-name form expected-kind)
                    (multiple-value-bind (expansion expanded-p)
                        (macroexpand-1 form)
                      (cl-weave:expect expanded-p :to-be t)
                      (cl-weave:expect (car expansion) :to-equal 'list)
                      (let ((entry (first (eval expansion))))
                        (cl-weave:expect (parity-entry-value entry "kind")
                                         :to-equal
                                         expected-kind)
                        (when (equal expected-kind "flag")
                          (cl-weave:expect
                           (parity-entry-value entry "kebab_case")
                           :to-equal
                           "sample-flag"))
                        (when (equal expected-kind "mouse")
                          (cl-weave:expect
                           (parity-entry-value entry "condition")
                           :to-equal
                           "--disable_click disables mouse capture")))))
                   (cl-weave:it "returns a fresh catalog tree"
                                (let ((catalog
                                       (ntop.app.domain:btm-parity-catalog)))
                                  (setf (cdr
                                         (assoc "action"
                                                (first catalog)
                                                :test
                                                #'equal)) "changed")
                                  (cl-weave:expect
                                   (parity-entry-value
                                    (first (ntop.app.domain:btm-parity-catalog))
                                    "action")
                                   :to-equal
                                   "Quit"))))
