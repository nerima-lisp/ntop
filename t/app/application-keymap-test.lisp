(in-package #:ntop.test)

(defun keymap-test-contexts ()
  (loop for selected-widget in '(nil :process
                                     :process-tree
                                     :disk
                                     :disk-io-graph
                                     :temperature
                                     :temperature-graph
                                     :battery
                                     :cpu
                                     :memory
                                     :network
                                     :gpu
                                     :gpu-graph)
        append (loop for active-dialog in '(nil :help
                                                :help-search
                                                :process-search
                                                :process-sort
                                                :process-kill
                                                :expanded-widget)
                     append (loop for basic-mode-p in '(nil t)
                                  append (loop for search-mode-p in '(nil t)
                                               collect (ntop.app.domain::make-input-context
                                                        :selected-widget
                                                        selected-widget
                                                        :active-dialog
                                                        active-dialog
                                                        :basic-mode-p
                                                        basic-mode-p
                                                        :search-mode-p
                                                        search-mode-p))))))

(defun keymap-test-context-for (context-label &optional entry)
  (find-if
   (lambda (context)
     (and (ntop.app.application::keymap-context-matches-p context-label context)
          (or
           (not
            (and entry
                 (string= "Enter search text"
                          (ntop.app.application::keymap-entry-action-label
                           entry))))
           (ntop.app.domain:input-context-search-mode-p context))))
   (keymap-test-contexts)))

(defun keymap-test-action-label (entry)
  (ntop.app.application::keymap-entry-action-label entry))

(defun keymap-test-find-entry (kind action-label)
  (find-if
   (lambda (entry)
     (and (eq kind (ntop.app.domain:parity-entry-kind entry))
          (string= action-label (keymap-test-action-label entry))))
   (ntop.app.domain:btm-parity-catalog)))

(defun keymap-test-binding-input (entry binding)
  (if (eq :mouse (ntop.app.domain:parity-entry-kind entry))
      (ntop.ui::make-input-meaning :kind :mouse :gesture binding :x 7 :y 11)
      (ntop.ui::make-input-meaning :kind :key :key binding :text binding)))

(defun keymap-test-dispatch-once (state input
                                        context
                                        now
                                        &key
                                        disable-keys-p
                                        disable-click-p)
  (let ((actions nil)
        (unhandled nil))
    (let ((result
           (ntop.app.application:dispatch-application-input state
                                                            input
                                                            context
                                                            (lambda (action)
                                                              (push action
                                                                    actions))
                                                            (lambda (meaning)
                                                              (push meaning
                                                                    unhandled))
                                                            :now
                                                            now
                                                            :disable-keys-p
                                                            disable-keys-p
                                                            :disable-click-p
                                                            disable-click-p)))
      (values result (nreverse actions) (nreverse unhandled)))))

(defun keymap-test-prefix-binding-p (binding context)
  (and (> (length binding) 1)
       (nth-value 0
                  (ntop.app.application::find-keymap-prefix (subseq binding 0 1)
                                                            context))))

(defun keymap-test-sequence-rows ()
  (loop for entry in (ntop.app.domain:btm-parity-catalog)
        when (eq :key (ntop.app.domain:parity-entry-kind entry))
          append (loop for binding in (ntop.app.application::keymap-entry-bindings
                                       entry)
                       unless (string= "Enter search text"
                                       (keymap-test-action-label entry))
                         append (loop for context-label in (ntop.app.application::keymap-entry-contexts
                                                            entry)
                                      for context = (keymap-test-context-for
                                                     context-label
                                                     entry)
                                      when (and context
                                                (keymap-test-prefix-binding-p
                                                 binding
                                                 context))
                                        collect (list entry binding context)))))

(defun keymap-test-run-binding (entry binding context)
  (let ((state (ntop.app.application:make-application-keymap-state))
        (results nil)
        (actions nil)
        (unhandled nil)
        (now 0d0))
    (labels ((run-input (input)
               (multiple-value-bind (result call-actions call-unhandled)
                   (keymap-test-dispatch-once state input context now)
                 (push result results)
                 (setf actions (nconc actions call-actions)
                       unhandled (nconc unhandled call-unhandled))
                 (incf now 0.1d0))))
      (cond
        ((string= "Enter search text" (keymap-test-action-label entry))
         (run-input
          (ntop.ui::make-input-meaning :kind
                                       :paste
                                       :text
                                       "generated search text")))
        ((and (eq :key (ntop.app.domain:parity-entry-kind entry))
              (keymap-test-prefix-binding-p binding context))
         (loop for character across binding
               do (run-input
                   (ntop.ui::make-input-meaning :kind
                                                :key
                                                :key
                                                (string character)
                                                :text
                                                (string character)))))
        (t (run-input (keymap-test-binding-input entry binding))))
      (values (nreverse results) (nreverse actions) (nreverse unhandled)))))

(cl-weave:describe "application keymap"
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
                   (cl-weave:it "emits declared actions and validates semantic rows" (dolist (table (list ntop.app.application::*keymap-key-semantics* ntop.app.application::*keymap-mouse-semantics*)) (dolist (semantic table) (unless (eq :unhandled (third semantic)) (cl-weave:expect (not (null (member (third semantic) ntop.app.domain:*app-action-kinds*))) :to-be t)))) (let ((macro-symbol (find-symbol "DEFINE-KEYMAP-SEMANTICS" "NTOP.APP.APPLICATION")) (name (make-symbol "VALID-KEYS"))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol "not-a-symbol"))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name 1))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action")))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list 7 nil :quit nil)))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action" 7 :quit nil)))) (cl-weave:signals simple-error (macroexpand-1 (list macro-symbol name (list "Action" nil "quit" nil)))) (cl-weave:expect (nth-value 1 (macroexpand-1 (list macro-symbol name (list "Action" nil :quit nil) (list "Other action" "normal" :help nil)))) :to-be t)))
                   (cl-weave:it
                    "waits for catalog-derived prefixes and completes them within one second"
                    (let ((sequence-rows (keymap-test-sequence-rows)))
                      (cl-weave:expect (length (remove-duplicates (mapcar (function second) sequence-rows) :test (function string=))) :to-equal 2)
                      (dolist (sequence-row sequence-rows)
                        (destructuring-bind (entry binding context) sequence-row
                          (let* ((state
                                  (ntop.app.application:make-application-keymap-state))
                                 (first-key (string (char binding 0)))
                                 (second-key (string (char binding 1))))
                            (multiple-value-bind (result actions unhandled)
                                (keymap-test-dispatch-once state
                                                           (ntop.ui::make-input-meaning
                                                            :kind
                                                            :key
                                                            :key
                                                            first-key)
                                                           context
                                                           0d0)
                              (cl-weave:expect result :to-equal :pending)
                              (cl-weave:expect actions :to-equal nil)
                              (cl-weave:expect unhandled :to-equal nil))
                            (multiple-value-bind (result actions unhandled)
                                (keymap-test-dispatch-once state
                                                           (ntop.ui::make-input-meaning
                                                            :kind
                                                            :key
                                                            :key
                                                            second-key)
                                                           context
                                                           0.2d0)
                              (let* ((context-label
                                      (nth-value 1
                                                 (ntop.app.application::find-keymap-binding
                                                  :key
                                                  binding
                                                  context)))
                                     (semantic
                                      (ntop.app.application::keymap-entry-action
                                       entry
                                       context-label)))
                                (cl-weave:expect (length actions) :to-equal 1)
                                (cl-weave:expect unhandled :to-equal nil)
                                (cl-weave:expect result :to-be actions)
                                (cl-weave:expect
                                 (ntop.app.domain:app-action-kind
                                  (first actions))
                                 :to-equal
                                 (third semantic)))))))))
                   (cl-weave:it "restarts a prefix at the timeout boundary"
                                (dolist
                                    (sequence-row (keymap-test-sequence-rows))
                                  (destructuring-bind (entry binding context)
                                      sequence-row
                                    (declare (ignore entry))
                                    (let* ((state
                                            (ntop.app.application:make-application-keymap-state))
                                           (first-key (string (char binding 0)))
                                           (second-key
                                            (string (char binding 1))))
                                      (keymap-test-dispatch-once state
                                                                 (ntop.ui::make-input-meaning
                                                                  :kind
                                                                  :key
                                                                  :key
                                                                  first-key)
                                                                 context
                                                                 0d0)
                                      (multiple-value-bind (result actions
                                                                   unhandled)
                                          (keymap-test-dispatch-once state
                                                                     (ntop.ui::make-input-meaning
                                                                      :kind
                                                                      :key
                                                                      :key
                                                                      first-key)
                                                                     context
                                                                     1d0)
                                        (cl-weave:expect result
                                                         :to-equal
                                                         :pending)
                                        (cl-weave:expect actions :to-equal nil)
                                        (cl-weave:expect unhandled
                                                         :to-equal
                                                         nil))
                                      (multiple-value-bind (result actions
                                                                   unhandled)
                                          (keymap-test-dispatch-once state
                                                                     (ntop.ui::make-input-meaning
                                                                      :kind
                                                                      :key
                                                                      :key
                                                                      second-key)
                                                                     context
                                                                     1.1d0)
                                        (cl-weave:expect (length actions)
                                                         :to-equal
                                                         1)
                                        (cl-weave:expect unhandled
                                                         :to-equal
                                                         nil)
                                        (cl-weave:expect result :to-be actions))))))
                   (cl-weave:it
                    "clears a prefix when the context changes or a mouse event arrives"
                    (let* ((sequence-row (first (keymap-test-sequence-rows)))
                           (entry (first sequence-row))
                           (binding (second sequence-row))
                           (context (third sequence-row))
                           (first-key (string (char binding 0)))
                           (second-key (string (char binding 1)))
                           (changed-context
                            (ntop.app.domain::make-input-context
                             :selected-widget
                             :memory
                             :active-dialog
                             nil
                             :basic-mode-p
                             nil
                             :search-mode-p
                             nil))
                           (ignored-mouse
                            (keymap-test-find-entry :mouse
                                                    "Ignore other buttons"))
                           (mouse-gesture
                            (first
                             (ntop.app.application::keymap-entry-bindings
                              ignored-mouse))))
                      (declare (ignorable entry))
                      (let ((state
                             (ntop.app.application:make-application-keymap-state)))
                        (keymap-test-dispatch-once state
                                                   (ntop.ui::make-input-meaning
                                                    :kind
                                                    :key
                                                    :key
                                                    first-key)
                                                   context
                                                   0d0)
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once state
                                                       (ntop.ui::make-input-meaning
                                                        :kind
                                                        :key
                                                        :key
                                                        second-key)
                                                       changed-context
                                                       0.1d0)
                          (declare (ignore result unhandled))
                          (cl-weave:expect
                           (and (= (+ (length actions) (length unhandled)) 1)
                                (or (null actions)
                                    (not
                                     (eq
                                      (ntop.app.domain:app-action-kind
                                       (first actions))
                                      (third
                                       (ntop.app.application::keymap-entry-action
                                        entry
                                        (nth-value 1
                                                   (ntop.app.application::find-keymap-binding
                                                    :key
                                                    binding
                                                    context))))))))
                           :to-be
                           t)
                          (cl-weave:expect
                           (ntop.app.application::application-keymap-state-pending-prefix
                            state)
                           :to-be
                           nil)))
                      (let ((state
                             (ntop.app.application:make-application-keymap-state)))
                        (keymap-test-dispatch-once state
                                                   (ntop.ui::make-input-meaning
                                                    :kind
                                                    :key
                                                    :key
                                                    first-key)
                                                   context
                                                   0d0)
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once state
                                                       (ntop.ui::make-input-meaning
                                                        :kind
                                                        :mouse
                                                        :gesture
                                                        mouse-gesture
                                                        :x
                                                        0
                                                        :y
                                                        0)
                                                       context
                                                       0.1d0)
                          (declare (ignore result))
                          (cl-weave:expect actions :to-equal nil)
                          (cl-weave:expect (length unhandled) :to-equal 1)
                          (cl-weave:expect
                           (ntop.app.application::application-keymap-state-pending-prefix
                            state)
                           :to-be
                           nil))
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once state
                                                       (ntop.ui::make-input-meaning
                                                        :kind
                                                        :key
                                                        :key
                                                        second-key)
                                                       context
                                                       0.2d0)
                          (cl-weave:expect result :to-equal :pending)
                          (cl-weave:expect actions :to-equal nil)
                          (cl-weave:expect unhandled :to-equal nil)))))
                   (cl-weave:it
                    "applies all disable-key and disable-click flag combinations"
                    (let* ((key-entry
                            (keymap-test-find-entry :key "Freeze updates"))
                           (key-context-label
                            (first
                             (ntop.app.application::keymap-entry-contexts
                              key-entry)))
                           (key-context
                            (keymap-test-context-for key-context-label
                                                     key-entry))
                           (key-binding
                            (first
                             (ntop.app.application::keymap-entry-bindings
                              key-entry)))
                           (mouse-entry
                            (keymap-test-find-entry :mouse "Select process row"))
                           (mouse-context-label
                            (first
                             (ntop.app.application::keymap-entry-contexts
                              mouse-entry)))
                           (mouse-context
                            (keymap-test-context-for mouse-context-label
                                                     mouse-entry))
                           (mouse-gesture
                            (first
                             (ntop.app.application::keymap-entry-bindings
                              mouse-entry))))
                      (dolist (disable-keys-p '(nil t))
                        (dolist (disable-click-p '(nil t))
                          (multiple-value-bind (result actions unhandled)
                              (keymap-test-dispatch-once
                               (ntop.app.application:make-application-keymap-state)
                               (ntop.ui::make-input-meaning :kind
                                                            :key
                                                            :key
                                                            key-binding
                                                            :text
                                                            key-binding)
                               key-context
                               0d0
                               :disable-keys-p
                               disable-keys-p
                               :disable-click-p
                               disable-click-p)
                            (declare (ignore result))
                            (if disable-keys-p
                                (progn
                                  (cl-weave:expect actions :to-equal nil)
                                  (cl-weave:expect (length unhandled)
                                                   :to-equal
                                                   1))
                                (progn
                                  (cl-weave:expect (length actions) :to-equal 1)
                                  (cl-weave:expect unhandled :to-equal nil))))
                          (multiple-value-bind (result actions unhandled)
                              (keymap-test-dispatch-once
                               (ntop.app.application:make-application-keymap-state)
                               (ntop.ui::make-input-meaning :kind
                                                            :mouse
                                                            :gesture
                                                            mouse-gesture
                                                            :x
                                                            3
                                                            :y
                                                            5)
                               mouse-context
                               0d0
                               :disable-keys-p
                               disable-keys-p
                               :disable-click-p
                               disable-click-p)
                            (declare (ignore result))
                            (if disable-click-p
                                (progn
                                  (cl-weave:expect actions :to-equal nil)
                                  (cl-weave:expect (length unhandled)
                                                   :to-equal
                                                   1))
                                (progn
                                  (cl-weave:expect (length actions) :to-equal 1)
                                  (cl-weave:expect unhandled :to-equal nil))))))))
                   (cl-weave:it
                    "suppresses expand and view-cycle keys in basic mode"
                    (dolist
                        (action-label
                         '("Expand widget" "Cycle selected widget view"))
                      (let* ((entry
                              (find-if
                               (lambda (candidate)
                                 (and
                                  (member
                                   (ntop.app.domain:parity-entry-kind candidate)
                                   '(:key :extension)
                                   :test
                                   #'eq)
                                  (string= action-label
                                           (keymap-test-action-label candidate))))
                               (ntop.app.domain:btm-parity-catalog)))
                             (context-label
                              (first
                               (ntop.app.application::keymap-entry-contexts
                                entry)))
                             (normal-context
                              (keymap-test-context-for context-label entry))
                             (context
                              (ntop.app.domain::make-input-context
                               :selected-widget
                               (ntop.app.domain:input-context-selected-widget
                                normal-context)
                               :active-dialog
                               nil
                               :basic-mode-p
                               t
                               :search-mode-p
                               nil))
                             (binding
                              (first
                               (ntop.app.application::keymap-entry-bindings
                                entry))))
                        (multiple-value-bind (result actions unhandled)
                            (keymap-test-dispatch-once
                             (ntop.app.application:make-application-keymap-state)
                             (ntop.ui::make-input-meaning :kind
                                                          :key
                                                          :key
                                                          binding)
                             context
                             0d0)
                          (declare (ignore result))
                          (cl-weave:expect actions :to-equal nil)
                          (cl-weave:expect (length unhandled) :to-equal 1)))))
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
                   (cl-weave:it "handles fallback text and invalid dispatch state" (cl-weave:expect (ntop.app.application::input-meaning-text (ntop.ui::make-input-meaning :kind :key :key "q")) :to-equal "q") (cl-weave:expect (ntop.app.application::input-meaning-text (ntop.ui::make-input-meaning :kind :key)) :to-be nil) (let* ((state (ntop.app.application:make-application-keymap-state)) (context (ntop.app.domain::make-input-context)) (input (ntop.ui::make-input-meaning :kind :unknown)) (unhandled nil)) (cl-weave:expect (ntop.app.application:dispatch-application-input state input context (lambda (action) (declare (ignore action))) (lambda (meaning) (setf unhandled meaning)) :now 0d0) :to-be input) (cl-weave:expect unhandled :to-be input)) (cl-weave:signals simple-error (ntop.app.application:dispatch-application-input (ntop.app.application:make-application-keymap-state) (ntop.ui::make-input-meaning :kind :key :key "a") (ntop.app.domain::make-input-context) (lambda (action) (declare (ignore action))) (lambda (meaning) (declare (ignore meaning))) :now nil))))
