(in-package #:ntop.e2e.test)

(cl-weave:describe "E2E command-line specification"
                   (cl-weave:it "expands declarative option and app definitions"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(cl-cli:define-option *format-option*
                                                         :name
                                                         "format"
                                                         :kind
                                                         :value))
                                 :to-equal
                                 '(defparameter *format-option*
                                    (cl-cli:make-option :name
                                                        "format"
                                                        :kind
                                                        :value)))
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(cl-cli:define-app *macro-app*
                                                      (:name "macro-app"
                                                             :require-command
                                                             t)
                                                      (:command "run"
                                                                (:description
                                                                 "Run a command.")
                                                                (:option
                                                                 "format"
                                                                 :kind
                                                                 :value
                                                                 :choices
                                                                 '("json"
                                                                   "text")))))
                                 :to-equal
                                 '(defparameter *macro-app*
                                    (cl-cli:make-app :name
                                                     "macro-app"
                                                     :require-command
                                                     t
                                                     :global-options
                                                     (list)
                                                     :positionals
                                                     (list)
                                                     :commands
                                                     (append
                                                      (list
                                                       (cl-cli:make-command
                                                        :name
                                                        "run"
                                                        :description
                                                        "Run a command."
                                                        :options
                                                        (list
                                                         (cl-cli:make-option
                                                          :name
                                                          "format"
                                                          :kind
                                                          :value
                                                          :choices
                                                          '("json" "text")))
                                                        :positionals
                                                        (list)
                                                        :subcommands
                                                        (append))))))))
                   (cl-weave:it-each
                    ((:goldens ("ntop-e2e" "goldens")
                               (:output :btm :rate :timeout-seconds)
                               ("t/e2e/goldens" "btm" 700 30))
                     (:compare ("ntop-e2e" "compare" "left" "right")
                               (:scope)
                               ("condition-a"))
                     (:measure ("ntop-e2e" "measure" "--measurement" "n1")
                               (:measurement :target
                                             :rows
                                             :columns
                                             :rate
                                             :runs
                                             :duration-seconds
                                             :timeout-seconds)
                               ("n1" "btm" 48 160 700 1 60 90))
                     (:export-parity ("ntop-e2e" "export-parity")
                                     (:output)
                                     ("-"))
                     (:n7 ("ntop-e2e" "n7") (:root) ("t/e2e/goldens")))
                    "uses declared defaults for the ~A command"
                    (command argv option-keys expected-values)
                    (let* ((invocation
                            (cl-cli:parse-argv ntop.e2e::*e2e-app* argv))
                           (actual-values
                            (mapcar
                             (lambda (key)
                               (cl-cli:option-value invocation key))
                             option-keys)))
                      (progn
                        (cl-weave:expect (keywordp command) :to-be t)
                        (cl-weave:expect actual-values
                                         :to-equal
                                         expected-values))))
                   (cl-weave:it-each
                    ((:missing-measurement ("ntop-e2e" "measure")
                                           cl-cli:cli-missing-required-option)
                     (:missing-both-positionals ("ntop-e2e" "compare")
                                                cl-cli:cli-missing-positional)
                     (:missing-right-positional ("ntop-e2e" "compare" "left")
                                                cl-cli:cli-missing-positional))
                    "rejects required input for ~A"
                    (case argv
                      expected-condition)
                    (declare (ignore case))
                    (let ((condition
                           (handler-case (progn
                                           (cl-cli:parse-argv
                                            ntop.e2e::*e2e-app*
                                            argv)
                                           nil)
                             (cl-cli:cli-usage-error (condition)
                               condition))))
                      (cl-weave:expect condition
                                       :to-satisfy
                                       (lambda (value)
                                         (typep value expected-condition)))))
                   (cl-weave:it-each
                    ((:bad-target
                      ("ntop-e2e" "measure"
                                  "--measurement"
                                  "n1"
                                  "--target"
                                  "ntop"))
                     (:bad-measurement
                      ("ntop-e2e" "measure" "--measurement" "n7"))
                     (:zero-rate
                      ("ntop-e2e" "measure" "--measurement" "n1" "--rate" "0"))
                     (:non-integer-runs
                      ("ntop-e2e" "measure"
                                  "--measurement"
                                  "n1"
                                  "--runs"
                                  "several")))
                    "rejects invalid values for ~A"
                    (case argv)
                    (declare (ignore case))
                    (let ((condition
                           (handler-case (progn
                                           (cl-cli:parse-argv
                                            ntop.e2e::*e2e-app*
                                            argv)
                                           nil)
                             (cl-cli:cli-usage-error (condition)
                               condition))))
                      (cl-weave:expect
                       (typep condition 'cl-cli:cli-invalid-option-value)
                       :to-be
                       t))))
