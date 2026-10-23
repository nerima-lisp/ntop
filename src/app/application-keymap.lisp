(in-package #:ntop.app.application)

(defmacro define-keymap-semantics (name &body rows)
  (unless (symbolp name)
    (error "Keymap semantics name must be a symbol: ~S" name))
  (dolist (row rows)
    (unless (and (listp row)
                 (= (length row) 4)
                 (stringp (first row))
                 (or (null (second row)) (stringp (second row)))
                 (keywordp (third row)))
      (error "Invalid keymap semantics row: ~S" row)))
  `(defparameter ,name ',rows))

(define-keymap-semantics
    *keymap-key-semantics*
  ("Quit" nil :quit nil)
  ("Open help" nil :help nil)
  ("Freeze updates" nil :pause nil)
  ("Expand widget" nil :expand nil)
  ("Reset collection" nil :navigation :reset-collection)
  ("Move widget selection left" nil :navigation :left)
  ("Move widget selection right" nil :navigation :right)
  ("Move widget selection up" nil :navigation :up)
  ("Move widget selection down" nil :navigation :down)
  ("Go to first row" nil :navigation :first-row)
  ("Go to last row" nil :navigation :last-row)
  ("Move to previous row" nil :navigation :previous-row)
  ("Move to next row" nil :navigation :next-row)
  ("Page up" nil :navigation :page-up)
  ("Page down" nil :navigation :page-down)
  ("Half-page up" nil :navigation :half-page-up)
  ("Half-page down" nil :navigation :half-page-down)
  ("Move left within widget" nil :navigation :left)
  ("Move right within widget" nil :navigation :right)
  ("Open process search" nil :search :process)
  ("Open help search" nil :search :help)
  ("Enter search text" nil :search :input-text)
  ("Toggle search case" nil :search :toggle-case)
  ("Toggle whole-word search" nil :search :toggle-whole-word)
  ("Toggle regex search" nil :search :toggle-regex)
  ("Move search cursor to start" nil :search :cursor-start)
  ("Move search cursor to end" nil :search :cursor-end)
  ("Clear search" nil :search :clear)
  ("Delete previous word" nil :search :delete-word-backward)
  ("Delete previous character" nil :search :delete-backward)
  ("Delete next character" nil :search :delete-forward)
  ("Delete selected process" nil :dialog :process-kill)
  ("Accept current choice" "process search" :search :submit)
  ("Accept current choice" "help search" :search :submit)
  ("Accept current choice" "process sort" :sort-processes :apply)
  ("Accept current choice" "kill dialog" :process-terminate :confirm)
  ("Close current modal" nil :dialog :close-dialog)
  ("Toggle process table tab" nil :select :toggle-process-tab)
  ("Sort process by CPU" nil :sort-processes :cpu)
  ("Sort process by memory" nil :sort-processes :memory)
  ("Sort process by PID or count" nil :sort-processes :pid-or-count)
  ("Sort process by name" nil :sort-processes :name)
  ("Sort process by GPU utilization" nil :sort-processes :gpu-utilization)
  ("Sort process by GPU memory" nil :sort-processes :gpu-memory)
  ("Toggle process command display" nil :set-process-display-mode :command)
  ("Open process sort list" nil :dialog :process-sort)
  ("Reverse sort order" nil :reverse-process-sort nil)
  ("Toggle process tree" nil :toggle-process-tree nil)
  ("Open process kill dialog" nil :dialog :process-kill)
  ("Toggle process tree branch" nil :toggle-process-tree-branch nil)
  ("Zoom graph in" nil :zoom :in)
  ("Zoom graph out" nil :zoom :out)
  ("Reset graph zoom" nil :zoom :reset)
  ("Toggle memory values" nil :set-process-display-mode :memory-values)
  ("Toggle kernel threads" nil :set-process-display-mode :kernel-threads)
  ("Select disk column 0" nil :select (:disk-column 0))
  ("Select disk column 1" nil :select (:disk-column 1))
  ("Select disk column 2" nil :select (:disk-column 2))
  ("Select disk column 3" nil :select (:disk-column 3))
  ("Select disk column 4" nil :select (:disk-column 4))
  ("Select disk column 5" nil :select (:disk-column 5))
  ("Select disk column 6" nil :select (:disk-column 6))
  ("Select disk column 7" nil :select (:disk-column 7))
  ("Sort temperatures by name" nil :sort-processes :temperature-name)
  ("Sort temperatures by value" nil :sort-processes :temperature-value)
  ("Select help section" nil :navigation :help-section)
  ("Scroll help" nil :navigation :help-line-scroll)
  ;; The catalog records h/l in help but reports no runtime binding.
  ("Documented horizontal movement" nil :unhandled nil)
  ("Cycle selected widget view" nil :toggle-widget-v nil)
  ("Scroll history backward" nil :history-scroll :backward)
  ("Scroll history forward" nil :history-scroll :forward)
  ("Return graph to current" nil :return-to-now nil))

(define-keymap-semantics
    *keymap-mouse-semantics*
  ("Scroll up" "graphs" :zoom :in)
  ("Scroll up" "tables" :navigation :scroll-up)
  ("Scroll up" "help" :navigation :scroll-up)
  ("Scroll up" "kill dialog" :dialog :choice-up)
  ("Scroll down" "graphs" :zoom :out)
  ("Scroll down" "tables" :navigation :scroll-down)
  ("Scroll down" "help" :navigation :scroll-down)
  ("Scroll down" "kill dialog" :dialog :choice-down)
  ("Focus widget" "all widgets" :select (:pointer :widget))
  ("Move basic-mode widget" "basic-mode arrows"
   :basic (:pointer :basic-mode-widget))
  ("Select process row" "process table" :select (:pointer :process-row))
  ("Select process sort" "process sort list"
   :sort-processes (:pointer :sort-choice))
  ("Select CPU series" "CPU legend" :select (:pointer :cpu-series))
  ("Select table row or column" "temperature table"
   :select (:pointer :table-cell))
  ("Select table row or column" "disk table"
   :select (:pointer :table-cell))
  ("Select battery" "battery widget" :select (:pointer :battery-tab))
  ("Choose kill action" "kill dialog"
   :process-terminate (:pointer :kill-choice))
  ("Ignore other buttons" nil :unhandled nil)
  ("Ignore pointer motion" nil :unhandled nil))

(defconstant +keymap-prefix-timeout-seconds+ 1.0d0)

(defstruct (application-keymap-state
            (:constructor make-application-keymap-state ()))
  pending-prefix
  pending-context
  pending-at)

(defun keymap-context-signature (context)
  (list (ntop.app.domain:input-context-selected-widget context)
        (ntop.app.domain:input-context-active-dialog context)
        (ntop.app.domain:input-context-basic-mode-p context)
        (ntop.app.domain:input-context-search-mode-p context)))

(defun clear-keymap-prefix (state)
  (setf (application-keymap-state-pending-prefix state) nil
        (application-keymap-state-pending-context state) nil
        (application-keymap-state-pending-at state) nil))

(defun keymap-context-specificity (label)
  (let ((name (string-downcase label)))
    (cond
      ((or (string= name "all")
           (string= name "all widgets"))
       0)
      ((or (string= name "normal")
           (string= name "normal process table")
           (string= name "tables")
           (string= name "graphs"))
       1)
      (t 5))))

(defun keymap-context-matches-p (label context)
  (let ((name (string-downcase label))
        (widget (ntop.app.domain:input-context-selected-widget context))
        (dialog (ntop.app.domain:input-context-active-dialog context))
        (basic-p (ntop.app.domain:input-context-basic-mode-p context))
        (search-p (ntop.app.domain:input-context-search-mode-p context)))
    (labels ((ends-with-p (value suffix)
               (and (>= (length value) (length suffix))
                    (string= value suffix
                             :start1 (- (length value) (length suffix)))))
             (strip-suffix (value suffix)
               (if (ends-with-p value suffix)
                   (subseq value 0 (- (length value) (length suffix)))
                   value))
             (space-name (symbol)
               (substitute (code-char 32) (code-char 45)
                           (string-downcase (symbol-name symbol))))
             (normal-context-p ()
               (and (null dialog) (not search-p)))
             (widget-context-p ()
               (let ((stem name))
                 (dolist (suffix (list " table" " tabs" " widget" " legend"))
                   (setf stem (strip-suffix stem suffix)))
                 (or (string= stem (space-name widget))
                     (string= name
                              (format nil "~as" (space-name widget))))))
             (dialog-context-p ()
               (when (symbolp dialog)
                 (let* ((dialog-name (space-name dialog))
                        (last-space (position (code-char 32) dialog-name
                                              :from-end t))
                        (last-word (if last-space
                                       (subseq dialog-name (1+ last-space))
                                       dialog-name))
                        (list-stem (strip-suffix name " list"))
                        (dialog-stem (strip-suffix name " dialog")))
                   (or (string= name dialog-name)
                       (string= list-stem dialog-name)
                       (and (ends-with-p name " dialog")
                            (string= dialog-stem last-word)))))))
      (cond
        ((or (string= name "all") (string= name "all widgets")) t)
        ((string= name "normal") (normal-context-p))
        ((string= name "tables")
         (and (normal-context-p)
              (member widget (list :process :process-tree :disk :temperature)
                      :test (function eq))))
        ((string= name "graphs")
         (and (normal-context-p)
              (or (member widget (list :cpu :memory :network :gpu)
                          :test (function eq))
                  (let ((widget-name (and (symbolp widget)
                                          (string-downcase
                                           (symbol-name widget)))))
                    (and widget-name
                         (ends-with-p widget-name "-graph"))))))
        ((string= name "search cursor") search-p)
        ((string= name "process table")
         (and (normal-context-p)
              (member widget (list :process :process-tree)
                      :test (function eq))))
        ((string= name "normal process table")
         (and (normal-context-p)
              (member widget (list :process :process-tree)
                      :test (function eq))))
        ((string= name "basic-mode arrows")
         (and basic-p (normal-context-p)))
        ((dialog-context-p) t)
        ((and (normal-context-p) widget (widget-context-p)) t)
        (t nil)))))

(defun keymap-semantics (table action context-label)
  (or (find-if (lambda (row)
                 (and (string= action (first row))
                      (string= context-label (or (second row) ""))))
               table)
      (find-if (lambda (row)
                 (and (string= action (first row))
                      (null (second row))))
               table)))

(defun keymap-entry-action (entry context-label)
  (keymap-semantics
   (if (eq :mouse (ntop.app.domain:parity-entry-kind entry))
       *keymap-mouse-semantics*
       *keymap-key-semantics*)
   (keymap-entry-action-label entry)
   context-label))
(defun keymap-entry-action-label (entry)
  (case (ntop.app.domain:parity-entry-kind entry)
    (:key (ntop.app.domain:parity-key-action entry))
    (:extension (ntop.app.domain:parity-extension-action entry))
    (:mouse (ntop.app.domain:parity-mouse-action entry))))

(defun keymap-entry-classification (entry)
  (if (some (lambda (context-label)
              (keymap-entry-action entry context-label))
            (keymap-entry-contexts entry))
      :classified
      nil))

(defun keymap-entry-bindings (entry)
  (case (ntop.app.domain:parity-entry-kind entry)
    (:key (ntop.app.domain:parity-key-keys entry))
    (:extension (list (ntop.app.domain:parity-extension-key entry)))
    (:mouse (list (ntop.app.domain:parity-mouse-gesture entry)))))

(defun keymap-entry-contexts (entry)
  (case (ntop.app.domain:parity-entry-kind entry)
    (:key (ntop.app.domain:parity-key-contexts entry))
    (:extension (ntop.app.domain:parity-extension-contexts entry))
    (:mouse (ntop.app.domain:parity-mouse-contexts entry))))

(defun better-keymap-match-p (candidate current)
  (cond
    ((null current) t)
    ((> (keymap-context-specificity (second candidate))
        (keymap-context-specificity (second current)))
     t)
    ((= (keymap-context-specificity (second candidate))
        (keymap-context-specificity (second current)))
     (< (length (keymap-entry-contexts (first candidate)))
        (length (keymap-entry-contexts (first current)))))
    (t nil)))

(defun find-keymap-binding (kind binding context)
  (let ((best nil))
    (dolist (entry (ntop.app.domain:btm-parity-catalog))
      (let ((entry-kind (ntop.app.domain:parity-entry-kind entry)))
        (when (or (and (eq kind :key)
                       (or (eq entry-kind :key)
                           (eq entry-kind :extension)))
                  (eq kind entry-kind))
          (dolist (context-label (keymap-entry-contexts entry))
            (when (keymap-context-matches-p context-label context)
              (when (and (keymap-entry-action entry context-label)
                         (member binding (keymap-entry-bindings entry)
                                 :test (function string=)))
                (let ((candidate (list entry context-label)))
                  (when (better-keymap-match-p candidate best)
                    (setf best candidate)))))))))
    (values (first best) (second best))))

(defun find-keymap-prefix (key context)
  (let ((best nil))
    (dolist (entry (ntop.app.domain:btm-parity-catalog))
      (when (eq :key (ntop.app.domain:parity-entry-kind entry))
        (dolist (context-label (keymap-entry-contexts entry))
          (when (keymap-context-matches-p context-label context)
            (dolist (binding (keymap-entry-bindings entry))
              (when (and (> (length binding) (length key))
                         (every (lambda (character)
                                  (and (alpha-char-p character)
                                       (lower-case-p character)))
                                binding)
                         (string= key binding :end2 (length key))
                         (keymap-entry-action entry context-label))
                (let ((candidate (list entry context-label)))
                  (when (better-keymap-match-p candidate best)
                    (setf best candidate)))))))))
    (values (first best) (second best))))

(defun find-printable-keymap-binding (context)
  (dolist (entry (ntop.app.domain:btm-parity-catalog))
    (when (and (eq :key (ntop.app.domain:parity-entry-kind entry))
               (ntop.app.domain:input-context-search-mode-p context)
               (string= (ntop.app.domain:parity-key-action entry)
                        "Enter search text"))
      (dolist (context-label (keymap-entry-contexts entry))
        (when (and (keymap-context-matches-p context-label context)
                   (keymap-entry-action entry context-label))
          (return-from find-printable-keymap-binding
            (values entry context-label))))))
  (values nil nil))

(defun input-meaning-text (input)
  (if (eq :paste (ntop.ui:input-meaning-kind input))
      (ntop.ui:input-meaning-text input)
      (or (ntop.ui:input-meaning-text input)
          (ntop.ui:input-meaning-key input))))

(defun keymap-value (semantic entry input context binding)
  (let ((value (fourth semantic)))
    (cond
      ((eq value :input-text) (input-meaning-text input))
      ((eq value :close-dialog)
       (list :close (ntop.app.domain:input-context-active-dialog context)))
      ((eq value :help-section) (list :help-section binding))
      ((eq value :help-line-scroll)
       (if (string= binding (first (keymap-entry-bindings entry)))
           :down
           :up))
      ((and (consp value) (eq (first value) :pointer))
       (list (second value)
             (ntop.ui:input-meaning-x input)
             (ntop.ui:input-meaning-y input)))
      (t value))))

(defun keymap-basic-mode-suppresses-p (entry context)
  (and (ntop.app.domain:input-context-basic-mode-p context)
       (member (ntop.app.domain:parity-entry-kind entry)
               '(:key :extension)
               :test (function eq))
       (member (keymap-entry-action-label entry)
               '("Expand widget" "Cycle selected widget view")
               :test (function string=))))

(defun dispatch-keymap-match (entry context-label input context binding
                              on-action on-unhandled)
  (let ((semantic (keymap-entry-action entry context-label)))
    (if (or (null semantic)
            (eq :unhandled (third semantic))
            (keymap-basic-mode-suppresses-p entry context))
        (funcall on-unhandled input)
        (funcall on-action
                 (ntop.app.domain:make-app-action
                  :kind (third semantic)
                  :value (keymap-value semantic entry input context binding))))) )

(defun dispatch-keymap-mouse (input context on-action on-unhandled)
  (multiple-value-bind (entry context-label)
      (find-keymap-binding :mouse (ntop.ui:input-meaning-gesture input) context)
    (if entry
        (dispatch-keymap-match entry context-label input context
                               (ntop.ui:input-meaning-gesture input)
                               on-action on-unhandled)
        (funcall on-unhandled input))))

(defun dispatch-keymap-key (state input context now on-action on-unhandled)
  (let* ((key (ntop.ui:input-meaning-key input))
         (pending (application-keymap-state-pending-prefix state)))
    (when pending
      (let ((pending-at (application-keymap-state-pending-at state)))
        (when (or (not (equal (application-keymap-state-pending-context state)
                              (keymap-context-signature context)))
                  (<= now pending-at)
                  (>= (- now pending-at) +keymap-prefix-timeout-seconds+))
          (clear-keymap-prefix state)
          (setf pending nil))))
    (when pending
      (let* ((sequence (concatenate 'string pending (or key ""))))
        (multiple-value-bind (entry context-label)
            (find-keymap-binding :key sequence context)
          (clear-keymap-prefix state)
          (when entry
            (return-from dispatch-keymap-key
              (dispatch-keymap-match entry context-label input context sequence
                                     on-action on-unhandled))))))
    (when key
      (multiple-value-bind (entry context-label)
          (find-keymap-binding :key key context)
        (when entry
          (return-from dispatch-keymap-key
            (dispatch-keymap-match entry context-label input context key
                                   on-action on-unhandled)))
        (multiple-value-bind (prefix-entry)
            (find-keymap-prefix key context)
          (when prefix-entry
            (setf (application-keymap-state-pending-prefix state) key
                  (application-keymap-state-pending-context state)
                  (keymap-context-signature context)
                  (application-keymap-state-pending-at state) now)
            (return-from dispatch-keymap-key :pending))))
    (multiple-value-bind (entry context-label)
        (find-printable-keymap-binding context)
      (if entry
          (dispatch-keymap-match entry context-label input context key
                                 on-action on-unhandled)
          (funcall on-unhandled input))))))

(defun dispatch-application-input
    (state input context on-action on-unhandled
     &key now disable-keys-p disable-click-p)
  "Dispatch INPUT using CONTEXT and call ON-ACTION or ON-UNHANDLED.

STATE retains a pending multi-key prefix between calls. Keep the same STATE for
the input sequence. NOW is monotonic seconds and must be supplied on every
call; prefixes expire after one second.

CONTEXT must describe the current selected widget, active dialog, basic mode,
and search mode. The dispatcher matches those fields against the typed parity
catalog. For dialog-specific bindings, set active-dialog to the symbol matching
the catalog context, such as :help-search or :process-search. Set search-mode-p
when accepting printable search input.

ON-ACTION receives an app-action; ON-UNHANDLED receives the input-meaning.
When DISABLE-KEYS-P is true, key and paste meanings go to ON-UNHANDLED. When
DISABLE-CLICK-P is true, mouse meanings go to ON-UNHANDLED."
  (unless (realp now)
    (error "NOW must be monotonic seconds."))
  (when (and (application-keymap-state-pending-prefix state)
             (not (equal (application-keymap-state-pending-context state)
                         (keymap-context-signature context))))
    (clear-keymap-prefix state))
  (case (ntop.ui:input-meaning-kind input)
    (:mouse
     (clear-keymap-prefix state)
     (if disable-click-p
         (funcall on-unhandled input)
         (dispatch-keymap-mouse input context on-action on-unhandled)))
    ((:key :paste)
     (if disable-keys-p
         (progn
           (clear-keymap-prefix state)
           (funcall on-unhandled input))
         (if (eq :paste (ntop.ui:input-meaning-kind input))
             (progn
               (clear-keymap-prefix state)
               (multiple-value-bind (entry context-label)
                   (find-printable-keymap-binding context)
                 (if entry
                     (dispatch-keymap-match
                      entry context-label input context
                      (ntop.ui:input-meaning-text input)
                      on-action on-unhandled)
                     (funcall on-unhandled input))))
             (dispatch-keymap-key state input context now
                                  on-action on-unhandled))))
    (otherwise
     (clear-keymap-prefix state)
     (funcall on-unhandled input))))
