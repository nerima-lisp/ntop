(in-package #:ntop.test)

(cl-weave:describe "keymap sequences and prefix invalidation"
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
                          (cl-weave:expect unhandled :to-equal nil))))))
