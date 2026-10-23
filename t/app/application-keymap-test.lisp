(in-package #:ntop.test)

(cl-weave:describe "keymap catalog and contextual bindings"
                   (cl-weave:it
                    "accounts for every typed key, mouse, and extension catalog row"
                    (let* ((catalog (ntop.app.domain:btm-parity-catalog))
                           (key-count
                            (count :key catalog :key
                                   (function ntop.app.domain:parity-entry-kind)))
                           (mouse-count
                            (count :mouse catalog :key
                                   (function ntop.app.domain:parity-entry-kind)))
                           (extension-count
                            (count :extension catalog :key
                                   (function ntop.app.domain:parity-entry-kind))))
                      (cl-weave:expect (list key-count mouse-count extension-count)
                                       :to-equal
                                       (list 68 12 4))
                      (cl-weave:expect
                       (every
                        (function ntop.app.application::keymap-entry-classification)
                        (remove-if-not
                         (lambda (entry)
                           (member (ntop.app.domain:parity-entry-kind entry)
                                   (list :key :mouse :extension)
                                   :test (function eq)))
                         catalog))
                       :to-be
                       t)
                      (cl-weave:expect
                       (not
                        (null
                         (some
                          (lambda (entry)
                            (and (eq :key (ntop.app.domain:parity-entry-kind entry))
                                 (string= "Documented horizontal movement"
                                          (keymap-test-action-label entry))
                                 (eq :classified
                                     (ntop.app.application::keymap-entry-classification entry))))
                          catalog)))
                       :to-be
                       t))
                    (let ((help-scroll (keymap-test-find-entry :key "Scroll help"))
                          (move-row (keymap-test-find-entry :key "Move to previous row"))
                          (generic-mouse (keymap-test-find-entry :mouse "Focus widget"))
                          (specific-mouse (keymap-test-find-entry :mouse "Select process row")))
                      (cl-weave:expect
                       (mapcar
                        (function ntop.app.application::keymap-context-specificity)
                        (list "all" "all widgets" "normal" "normal process table"
                              "tables" "graphs" "help"))
                       :to-equal
                       (list 0 0 1 1 1 1 5))
                      (cl-weave:expect
                       (ntop.app.application::better-keymap-match-p
                        (list help-scroll "help")
                        (list move-row "help"))
                       :to-be
                       t)
                      (cl-weave:expect
                       (ntop.app.application::better-keymap-match-p
                        (list specific-mouse "process table")
                        (list generic-mouse "all widgets"))
                       :to-be
                       t)
                      (cl-weave:expect
                       (ntop.app.application::better-keymap-match-p
                        (list generic-mouse "all widgets")
                        (list specific-mouse "process table"))
                       :to-be
                       nil)
                      (cl-weave:expect
                       (ntop.app.application::better-keymap-match-p
                        (list specific-mouse "process table")
                        (list specific-mouse "process table"))
                       :to-be
                       nil)
                      (cl-weave:expect
                       (ntop.app.application::better-keymap-match-p
                        (list specific-mouse "process table")
                        nil)
                       :to-be
                       t)))
                   (cl-weave:it
                    "resolves every catalog context and each binding through the dispatcher"
                    (dolist (entry (ntop.app.domain:btm-parity-catalog))
                      (dolist
                          (context-label
                           (ntop.app.application::keymap-entry-contexts entry))
                        (let ((context
                               (keymap-test-context-for context-label entry)))
                          (cl-weave:expect (not (null context)) :to-be t)
                          (dolist
                              (binding
                               (ntop.app.application::keymap-entry-bindings
                                entry))
                            (let* ((kind
                                    (ntop.app.domain:parity-entry-kind entry))
                                   (printable-p
                                    (string= "Enter search text"
                                             (keymap-test-action-label entry))))
                              (multiple-value-bind (selected
                                                    selected-context-label)
                                  (if printable-p
                                      (ntop.app.application::find-printable-keymap-binding
                                       context)
                                    (ntop.app.application::find-keymap-binding
                                     (if (eq :mouse kind)
                                         :mouse
                                       :key)
                                     binding
                                     context))
                                (cl-weave:expect (or (eq selected entry) (and (eq :key kind) (string= context-label "help") (member (keymap-test-action-label entry) (list "Move to previous row" "Move to next row") :test (function string=)) (member binding (list "j" "k") :test (function string=)) (string= (keymap-test-action-label selected) "Scroll help"))) :to-be t)
                                (let ((semantic
                                       (ntop.app.application::keymap-entry-action
                                        selected
                                        selected-context-label)))
                                  (multiple-value-bind (results actions
                                                                unhandled)
                                      (keymap-test-run-binding entry
                                                               binding
                                                               context)
                                    (cl-weave:expect (not (null results))
                                                     :to-be
                                                     t)
                                    (if (or (null semantic)
                                            (eq :unhandled (third semantic))
                                            (ntop.app.application::keymap-basic-mode-suppresses-p
                                             selected
                                             context))
                                        (progn
                                          (cl-weave:expect actions
                                                           :to-equal
                                                           nil)
                                          (cl-weave:expect (length unhandled)
                                                           :to-equal
                                                           1))
                                      (progn
                                        (cl-weave:expect (length actions)
                                                         :to-equal
                                                         1)
                                        (cl-weave:expect unhandled
                                                         :to-equal
                                                         nil)
                                        (cl-weave:expect
                                         (ntop.app.domain:app-action-kind
                                          (first actions))
                                         :to-equal
                                         (third semantic))
                                        (cl-weave:expect (not (null (member (ntop.app.domain:app-action-kind (first actions)) ntop.app.domain:*app-action-kinds*))) :to-be t))))))))))))
                   (cl-weave:it "emits declared actions and validates semantic rows" (dolist (table (list ntop.app.application::*keymap-key-semantics* ntop.app.application::*keymap-mouse-semantics*)) (dolist (semantic table) (unless (eq :unhandled (third semantic)) (cl-weave:expect (not (null (member (third semantic) ntop.app.domain:*app-action-kinds*))) :to-be t)))) (let ((macro-symbol (find-symbol "DEFINE-KEYMAP-SEMANTICS" "NTOP.APP.APPLICATION")) (name (make-symbol "VALID-KEYS"))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol "not-a-symbol"))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name 1))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action")))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list 7 nil :quit nil)))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action" 7 :quit nil)))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action" nil "quit" nil)))) (cl-weave:expect (nth-value 1 (macroexpand-1 (list macro-symbol name (list "Action" nil :quit nil) (list "Other action" "normal" :help nil)))) :to-be t))))

