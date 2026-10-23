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

(defun keymap-test-entry-contexts (entry) (ntop.app.application::keymap-entry-contexts entry))

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
                         append (loop for context-label in (keymap-test-entry-contexts
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
