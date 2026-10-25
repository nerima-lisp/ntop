(in-package #:ntop.test)

(cl-weave:describe "keymap catalog and contextual bindings"
                   (cl-weave:it
                    "accounts for every typed key, mouse, and extension catalog row"
                    (let* ((catalog (ntop.app.domain:btm-parity-catalog))
                           (key-count
                            (count :key
                                   catalog
                                   :key
                                   (function ntop.app.domain:parity-entry-kind)))
                           (mouse-count
                            (count :mouse
                                   catalog
                                   :key
                                   (function ntop.app.domain:parity-entry-kind)))
                           (extension-count
                            (count :extension
                                   catalog
                                   :key
                                   (function ntop.app.domain:parity-entry-kind))))
                      (cl-weave:expect
                       (list key-count mouse-count extension-count)
                       :to-equal
                       (list 68 12 4))
                      (cl-weave:expect
                       (every
                        (function
                         ntop.app.application::keymap-entry-classification)
                        (remove-if-not
                         (lambda (entry)
                           (member (ntop.app.domain:parity-entry-kind entry)
                                   (list :key :mouse :extension)
                                   :test
                                   (function eq)))
                         catalog))
                       :to-be
                       t)
                      (cl-weave:expect
                       (not
                        (null
                         (some
                          (lambda (entry)
                            (and
                             (eq :key (ntop.app.domain:parity-entry-kind entry))
                             (string= "Documented horizontal movement"
                                      (keymap-test-action-label entry))
                             (eq :classified
                                 (ntop.app.application::keymap-entry-classification
                                  entry))))
                          catalog)))
                       :to-be
                       t))
                    (let ((help-scroll
                           (keymap-test-find-entry :key "Scroll help"))
                          (move-row
                           (keymap-test-find-entry :key "Move to previous row"))
                          (generic-mouse
                           (keymap-test-find-entry :mouse "Focus widget"))
                          (specific-mouse
                           (keymap-test-find-entry :mouse "Select process row")))
                      (cl-weave:expect
                       (mapcar
                        (function
                         ntop.app.application::keymap-context-specificity)
                        (list "all"
                              "all widgets"
                              "normal"
                              "normal process table"
                              "tables"
                              "graphs"
                              "help"))
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
                                (cl-weave:expect
                                 (or (eq selected entry)
                                     (and (eq :key kind)
                                          (string= context-label "help")
                                          (member
                                           (keymap-test-action-label entry)
                                           (list "Move to previous row"
                                                 "Move to next row")
                                           :test
                                           (function string=))
                                          (member binding
                                                  (list "j" "k")
                                                  :test
                                                  (function string=))
                                          (string=
                                           (keymap-test-action-label selected)
                                           "Scroll help")))
                                 :to-be
                                 t)
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
                                          (cl-weave:expect
                                           (not
                                            (null
                                             (member
                                              (ntop.app.domain:app-action-kind
                                               (first actions))
                                              ntop.app.domain:*app-action-kinds*)))
                                           :to-be
                                           t))))))))))))
                   (cl-weave:it
                    "emits declared actions and validates semantic rows"
                    (dolist
                        (table
                         (list ntop.app.application::*keymap-key-semantics*
                               ntop.app.application::*keymap-mouse-semantics*))
                      (dolist (semantic table)
                        (unless (eq :unhandled (third semantic))
                          (cl-weave:expect
                           (not
                            (null
                             (member (third semantic)
                                     ntop.app.domain:*app-action-kinds*)))
                           :to-be
                           t))))
                    (let ((macro-symbol
                           (find-symbol "DEFINE-KEYMAP-SEMANTICS"
                                        "NTOP.APP.APPLICATION"))
                          (name (make-symbol "VALID-KEYS")))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol "not-a-symbol")))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol name 1)))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol
                                               name
                                               (list "Action"))))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol
                                               name
                                               (list 7 nil :quit nil))))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol
                                               name
                                               (list "Action" 7 :quit nil))))
                      (cl-weave:signals simple-error
                                        (macroexpand-1
                                         (list macro-symbol
                                               name
                                               (list "Action" nil "quit" nil))))
                      (cl-weave:expect
                       (nth-value 1
                                  (macroexpand-1
                                   (list macro-symbol
                                         name
                                         (list "Action" nil :quit nil)
                                         (list "Other action"
                                               "normal"
                                               :help
                                               nil))))
                       :to-be
                       t))))

(cl-weave:describe "keymap context routing and search text"
                   (cl-weave:it
                    "routes G to the last row in table and help contexts and to now in graphs"
                    (let* ((last-row-entry
                            (keymap-test-find-entry :key "Go to last row"))
                           (last-row-binding
                            (find-if
                             (lambda (binding)
                               (and (= (length binding) 1)
                                    (upper-case-p (char binding 0))))
                             (ntop.app.application::keymap-entry-bindings
                              last-row-entry)))
                           (graph-entry
                            (keymap-test-find-entry :extension
                                                    "Return graph to current"))
                           (graph-binding
                            (first
                             (ntop.app.application::keymap-entry-bindings
                              graph-entry))))
                      (dolist (context-label '("tables" "help"))
                        (let ((context
                               (keymap-test-context-for context-label
                                                        last-row-entry)))
                          (multiple-value-bind (result actions unhandled)
                              (keymap-test-dispatch-once
                               (ntop.app.application:make-application-keymap-state)
                               (ntop.ui::make-input-meaning :kind
                                                            :key
                                                            :key
                                                            last-row-binding)
                               context
                               0d0)
                            (declare (ignore result))
                            (cl-weave:expect unhandled :to-equal nil)
                            (cl-weave:expect
                             (ntop.app.domain:app-action-kind (first actions))
                             :to-equal
                             :navigation)
                            (cl-weave:expect
                             (ntop.app.domain:app-action-value (first actions))
                             :to-equal
                             :last-row))))
                      (let ((context
                             (keymap-test-context-for "graphs" graph-entry)))
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once
                             (ntop.app.application:make-application-keymap-state)
                             (ntop.ui::make-input-meaning :kind
                                                          :key
                                                          :key
                                                          graph-binding)
                             context
                             0d0)
                          (declare (ignore result))
                          (cl-weave:expect unhandled :to-equal nil)
                          (cl-weave:expect
                           (ntop.app.domain:app-action-kind (first actions))
                           :to-equal
                           :return-to-now)))))
                   (cl-weave:it
                    "inserts pasted text only for active process or help search dialogs"
                    (dolist (dialog '(:process-search :help-search))
                      (let ((context
                             (ntop.app.domain::make-input-context
                              :selected-widget
                              nil
                              :active-dialog
                              dialog
                              :basic-mode-p
                              nil
                              :search-mode-p
                              t)))
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once
                             (ntop.app.application:make-application-keymap-state)
                             (ntop.ui::make-input-meaning :kind
                                                          :paste
                                                          :text
                                                          "alpha beta")
                             context
                             0d0)
                          (declare (ignore result))
                          (cl-weave:expect unhandled :to-equal nil)
                          (cl-weave:expect
                           (ntop.app.domain:app-action-kind (first actions))
                           :to-equal
                           :search)
                          (cl-weave:expect
                           (ntop.app.domain:app-action-value (first actions))
                           :to-equal
                           "alpha beta"))))
                    (dolist
                        (context
                         (list
                          (ntop.app.domain::make-input-context :selected-widget
                                                               nil
                                                               :active-dialog
                                                               :process-search
                                                               :basic-mode-p
                                                               nil
                                                               :search-mode-p
                                                               nil)
                          (ntop.app.domain::make-input-context :selected-widget
                                                               nil
                                                               :active-dialog
                                                               nil
                                                               :basic-mode-p
                                                               nil
                                                               :search-mode-p
                                                               t)))
                      (multiple-value-bind (result actions unhandled)
                          (keymap-test-dispatch-once
                           (ntop.app.application:make-application-keymap-state)
                           (ntop.ui::make-input-meaning :kind
                                                        :paste
                                                        :text
                                                        "alpha beta")
                           context
                           0d0)
                        (declare (ignore result))
                        (cl-weave:expect actions :to-equal nil)
                        (cl-weave:expect (length unhandled) :to-equal 1))))
                   (cl-weave:it
                    "handles fallback text and invalid dispatch state"
                    (cl-weave:expect
                     (ntop.app.application::input-meaning-text
                      (ntop.ui::make-input-meaning :kind :key :key "q"))
                     :to-equal
                     "q")
                    (cl-weave:expect
                     (ntop.app.application::input-meaning-text
                      (ntop.ui::make-input-meaning :kind :key))
                     :to-be
                     nil)
                    (let* ((state
                            (ntop.app.application:make-application-keymap-state))
                           (context (ntop.app.domain::make-input-context))
                           (input (ntop.ui::make-input-meaning :kind :unknown))
                           (unhandled nil))
                      (cl-weave:expect
                       (ntop.app.application:dispatch-application-input state
                                                                        input
                                                                        context
                                                                        (lambda
                                                                            (action)
                                                                          (declare (ignore
                                                                                    action)))
                                                                        (lambda
                                                                            (meaning)
                                                                          (setf unhandled meaning))
                                                                        :now
                                                                        0d0)
                       :to-be
                       input)
                      (cl-weave:expect unhandled :to-be input))
                    (cl-weave:signals simple-error
                                      (ntop.app.application:dispatch-application-input
                                       (ntop.app.application:make-application-keymap-state)
                                       (ntop.ui::make-input-meaning :kind
                                                                    :key
                                                                    :key
                                                                    "a")
                                       (ntop.app.domain::make-input-context)
                                       (lambda (action)
                                         (declare (ignore action)))
                                       (lambda (meaning)
                                         (declare (ignore meaning)))
                                       :now
                                       nil))))

(cl-weave:describe
 "Escape priority (btm 0.14.9: src/event.rs:69; src/app.rs:228-291)"
 (cl-weave:it
  "clears expansion only when no dialog or search is active"
  (let ((input (ntop.ui::make-input-meaning :kind :key :key "Escape"))
        (cases '((nil nil :clear-expanded nil nil t)
                 (:process-search t (:close :process-search) nil t t)
                 (:process-sort nil (:close :process-sort) nil t t)
                 (:help nil (:close :help) nil t t)
                 (:help-search t (:close :help-search) nil t t)
                 (nil t nil nil t nil))))
    (dolist (case cases)
      (destructuring-bind
          (dialog search-p expected-value expected-dialog expected-expanded
           handled-p)
          case
        (let ((state (ntop.app.domain:make-app-state
                      :active-dialog dialog
                      :expanded-p t))
              (context (ntop.app.domain:make-input-context
                        :selected-widget :process
                        :active-dialog dialog
                        :search-mode-p search-p)))
          (multiple-value-bind (result actions unhandled)
              (keymap-test-dispatch-once
               (ntop.app.application:make-application-keymap-state)
               input context 0d0)
            (declare (ignore result))
            (cl-weave:expect (length actions)
                             :to-equal
                             (if handled-p 1 0))
            (cl-weave:expect (length unhandled)
                             :to-equal
                             (if handled-p 0 1))
            (when handled-p
              (cl-weave:expect
               (ntop.app.domain:app-action-kind (first actions))
               :to-equal
               :dialog)
              (cl-weave:expect
               (ntop.app.domain:app-action-value (first actions))
               :to-equal
               expected-value)
              (setf state
                    (ntop.app.application::transition-application-state
                     state (first actions))))
            (cl-weave:expect
             (ntop.app.domain:app-state-active-dialog state)
             :to-equal
             expected-dialog)
            (cl-weave:expect
             (ntop.app.domain:app-state-expanded-p state)
             :to-equal
             expected-expanded))))))))
