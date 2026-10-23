(in-package #:ntop.test)

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
                   (cl-weave:it "handles fallback text and invalid dispatch state" (cl-weave:expect (ntop.app.application::input-meaning-text (ntop.ui::make-input-meaning :kind :key :key "q")) :to-equal "q") (cl-weave:expect (ntop.app.application::input-meaning-text (ntop.ui::make-input-meaning :kind :key)) :to-be nil) (let* ((state (ntop.app.application:make-application-keymap-state)) (context (ntop.app.domain::make-input-context)) (input (ntop.ui::make-input-meaning :kind :unknown)) (unhandled nil)) (cl-weave:expect (ntop.app.application:dispatch-application-input state input context (lambda (action) (declare (ignore action))) (lambda (meaning) (setf unhandled meaning)) :now 0d0) :to-be input) (cl-weave:expect unhandled :to-be input)) (cl-weave:signals simple-error (ntop.app.application:dispatch-application-input (ntop.app.application:make-application-keymap-state) (ntop.ui::make-input-meaning :kind :key :key "a") (ntop.app.domain::make-input-context) (lambda (action) (declare (ignore action))) (lambda (meaning) (declare (ignore meaning))) :now nil))))
