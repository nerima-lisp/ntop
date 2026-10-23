(in-package #:ntop.e2e.test)

(cl-weave:describe "domain parity JSON export"
                   (cl-weave:it "expands the JSON schema declaration into data"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e::define-domain-parity-json-schema
                                    (:probe
                                     ("kind" :keyword
                                             ntop.app.domain:parity-entry-kind))))
                                 :to-equal
                                 '(defparameter
                                      ntop.e2e::*domain-parity-json-schema*
                                    '((:probe
                                       ("kind" :keyword
                                               ntop.app.domain:parity-entry-kind))))))
                   (cl-weave:it "expands typed record cases declaratively"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e::define-domain-parity-json-cases
                                    (:probe ntop.app.domain::make-parity-key
                                            (:action "probe")
                                            "{}")))
                                 :to-equal
                                 '(cl-weave:it-each
                                   ((:probe ntop.app.domain::make-parity-key
                                            (:action "probe")
                                            "{}"))
                                   "encodes typed parity records for ~A"
                                   (ntop.e2e::case-name ntop.e2e::constructor
                                                        ntop.e2e::initargs
                                                        ntop.e2e::expected)
                                   (declare (ignore ntop.e2e::case-name))
                                   (cl-weave:expect
                                    (json-kit:stringify
                                     (ntop.e2e::domain-parity-entry-to-json-object
                                      (apply ntop.e2e::constructor
                                             ntop.e2e::initargs)))
                                    :to-equal
                                    ntop.e2e::expected))))
                   (ntop.e2e::define-domain-parity-json-cases
                    (:key ntop.app.domain::make-parity-key
                          (:action "quit"
                                   :keys
                                   ("q" "C-c")
                                   :contexts
                                   ("main")
                                   :behavior
                                   "exit"
                                   :source
                                   ("src/keys.lisp:1"))
                          "{\"kind\":\"key\",\"action\":\"quit\",\"keys\":[\"q\",\"C-c\"],\"contexts\":[\"main\"],\"behavior\":\"exit\",\"source\":[\"src/keys.lisp:1\"]}")
                    (:mouse ntop.app.domain::make-parity-mouse
                            (:action "select"
                                     :gesture
                                     "left click"
                                     :contexts
                                     ("process table")
                                     :behavior
                                     "select row"
                                     :condition
                                     "mouse enabled"
                                     :source
                                     ("src/mouse.lisp:1"))
                            "{\"kind\":\"mouse\",\"action\":\"select\",\"gesture\":\"left click\",\"contexts\":[\"process table\"],\"behavior\":\"select row\",\"condition\":\"mouse enabled\",\"source\":[\"src/mouse.lisp:1\"]}")
                    (:flag ntop.app.domain::make-parity-flag
                           (:snake-case "show_cpu"
                                        :short
                                        nil
                                        :aliases
                                        ("cpu")
                                        :value-type
                                        "boolean"
                                        :values
                                        nil
                                        :condition
                                        "always"
                                        :source
                                        ("src/flags.lisp:1"))
                           "{\"kind\":\"flag\",\"snake_case\":\"show_cpu\",\"kebab_case\":\"show-cpu\",\"short\":[],\"aliases\":[\"cpu\"],\"value_type\":\"boolean\",\"values\":[],\"condition\":\"always\",\"source\":[\"src/flags.lisp:1\"]}")
                    (:column ntop.app.domain::make-parity-column
                             (:header "CPU%"
                                      :field
                                      "cpu"
                                      :aliases
                                      nil
                                      :condition
                                      "always"
                                      :default
                                      nil
                                      :slot
                                      "cpu"
                                      :source
                                      ("src/columns.lisp:1"))
                             "{\"kind\":\"column\",\"header\":\"CPU%\",\"field\":\"cpu\",\"aliases\":[],\"condition\":\"always\",\"default\":null,\"slot\":\"cpu\",\"source\":[\"src/columns.lisp:1\"]}")
                    (:search-field ntop.app.domain::make-parity-search-field
                                   (:field "command"
                                           :tokens
                                           ("cmd" "command")
                                           :condition
                                           "always"
                                           :implicit
                                           t
                                           :source
                                           ("src/search.lisp:1"))
                                   "{\"kind\":\"search-field\",\"field\":\"command\",\"tokens\":[\"cmd\",\"command\"],\"condition\":\"always\",\"implicit\":true,\"source\":[\"src/search.lisp:1\"]}")
                    (:extension ntop.app.domain::make-parity-extension
                                (:action "open"
                                         :key
                                         "o"
                                         :contexts
                                         ("main")
                                         :behavior
                                         "open details"
                                         :relation
                                         "extends default"
                                         :source
                                         ("src/extensions.lisp:1"))
                                "{\"kind\":\"extension\",\"action\":\"open\",\"key\":\"o\",\"contexts\":[\"main\"],\"behavior\":\"open details\",\"relation\":\"extends default\",\"source\":[\"src/extensions.lisp:1\"]}"))
                   (cl-weave:it "exports every parity record as a JSON object"
                                (let ((records
                                       (ntop.e2e::domain-parity-json-array))
                                      (catalog
                                       (ntop.app.domain:btm-parity-catalog)))
                                  (cl-weave:expect (vectorp records) :to-be t)
                                  (cl-weave:expect (length records)
                                                   :to-equal
                                                   (length catalog))
                                  (cl-weave:expect (plusp (length records))
                                                   :to-be
                                                   t)
                                  (cl-weave:expect
                                   (every #'json-kit:json-object-p records)
                                   :to-be
                                   t))))
