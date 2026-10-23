(in-package #:ntop.test)

(cl-weave:describe "keymap mouse and input suppression flags"
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
                          (cl-weave:expect (length unhandled) :to-equal 1))))))
