(in-package #:ntop.test)

(cl-weave:describe "keymap sequences and prefix invalidation"
                   (cl-weave:it
                    "waits for catalog-derived prefixes and completes them within one second"
                    (let ((sequence-rows (keymap-test-sequence-rows)))
                      (cl-weave:expect
                       (length
                        (remove-duplicates
                         (mapcar (function second) sequence-rows)
                         :test
                         (function string=)))
                       :to-equal
                       2)
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
(cl-weave:describe
 "application help section navigation"
 (cl-weave:it
  "jumps to every numbered section in the 25-row help viewport"
  (let ((state (ntop.app.presentation::make-help-state
                :selected-widget :process))
        (sections '((1 "1 - General")
                    (2 "2 - CPU widget")
                    (3 "3 - Process widget")
                    (4 "4 - Process search widget")
                    (5 "5 - Process sort widget")
                    (6 "6 - Temperature widget")
                    (7 "7 - Disk widget")
                    (8 "8 - Battery widget")
                    (9 "9 - Basic memory widget"))))
    (dolist (section sections)
      (destructuring-bind (number heading) section
        (let* ((value (list :help-section (format nil "~D" number)))
               (action (ntop.app.domain:make-app-action
                        :kind :navigation :value value))
               (next (ntop.app.presentation::help-state-transition
                      state action :page-size 25))
               (lines (ntop.app.presentation::%help-visible-lines next))
               (position (position heading lines :test #'string=))
               (last-offset (max 0 (- (length lines) 25)))
               (expected-offset (and position (min position last-offset)))
               (offset (ntop.app.presentation::help-state-scroll-offset next))
               (visible (subseq lines offset (min (length lines) (+ offset 25))))
               (initial-lines (ntop.app.presentation::%help-visible-lines state))
               (initial (subseq initial-lines 0 (min 25 (length initial-lines))))
               (content (and position (nth (1+ position) lines)))
               (away (ntop.app.presentation::help-state-transition
                      next
                      (ntop.app.domain:make-app-action
                       :kind :navigation :value (if (plusp offset) :up :down))
                      :page-size 25)))
          (cl-weave:expect
           (ntop.app.application::help-section-value-p value) :to-be t)
          (cl-weave:expect
           (ntop.app.application::%help-action-p action) :to-be t)
          (cl-weave:expect
           (ntop.app.presentation::help-state-sectioned-p next) :to-be t)
          (cl-weave:expect (not (null position)) :to-be t)
          (cl-weave:expect offset :to-equal expected-offset)
          (cl-weave:expect (not (null content)) :to-be t)
          (cl-weave:expect
           (not (null (position heading visible :test #'string=))) :to-be t)
          (cl-weave:expect
           (not (null (position content visible :test #'string=))) :to-be t)
          (cl-weave:expect (not (equal initial visible)) :to-be t)
          (cl-weave:expect
           (/= (ntop.app.presentation::help-state-scroll-offset away) offset)
           :to-be t))))))
 (cl-weave:it
  "rejects invalid section values and keeps search contextual"
  (dolist (value '((:help-section "0")
                   (:help-section "10")
                   (:help-section "x")
                   (:help-section "２")
                   (:help-section "2" "extra")
                   "2"
                   2))
    (let ((action (ntop.app.domain:make-app-action
                   :kind :navigation :value value)))
      (cl-weave:expect
       (ntop.app.application::help-section-value-p value) :to-be nil)
      (cl-weave:expect
       (ntop.app.application::%help-action-p action) :to-be nil)))
  (let* ((state (ntop.app.presentation::make-help-state :selected-widget :process))
         (search-state
           (ntop.app.presentation::help-state-transition
            state
            (ntop.app.domain:make-app-action :kind :search :value "CPU")))
         (invalid-state
           (ntop.app.presentation::help-state-transition
            search-state
            (ntop.app.domain:make-app-action
             :kind :navigation :value '(:help-section "0"))
            :page-size 25)))
    (cl-weave:expect
     (ntop.app.application::%help-action-p
      (ntop.app.domain:make-app-action
       :kind :search :value '(:help-section "2")))
     :to-be nil)
    (cl-weave:expect
     (ntop.app.presentation::help-state-sectioned-p search-state) :to-be nil)
    (cl-weave:expect
     (position "2 - CPU widget"
               (ntop.app.presentation::%help-visible-lines search-state)
               :test #'string=)
     :to-be nil)
    (cl-weave:expect
     (ntop.app.presentation::help-state-query invalid-state) :to-equal "CPU")
    (cl-weave:expect
     (ntop.app.presentation::help-state-scroll-offset invalid-state) :to-equal 0)
    (cl-weave:expect
     (ntop.app.presentation::help-state-sectioned-p invalid-state) :to-be nil)))
 (cl-weave:it
 "dispatches bindings 1 through 9 as section actions"
 (let* ((entry (keymap-test-find-entry :key "Select help section"))
        (context-label
          (first (ntop.app.application::keymap-entry-contexts entry)))
        (context (keymap-test-context-for context-label entry)))
   (dolist (binding '("1" "2" "3" "4" "5" "6" "7" "8" "9"))
     (multiple-value-bind (result actions unhandled)
         (keymap-test-dispatch-once
          (ntop.app.application:make-application-keymap-state)
          (keymap-test-binding-input entry binding)
          context
          0d0)
       (declare (ignore result))
       (cl-weave:expect unhandled :to-equal nil)
       (cl-weave:expect (length actions) :to-equal 1)
       (cl-weave:expect
        (ntop.app.domain:app-action-kind (first actions)) :to-equal :navigation)
       (cl-weave:expect
        (ntop.app.domain:app-action-value (first actions))
        :to-equal
        (list :help-section binding))
       (cl-weave:expect
        (ntop.app.application::%help-action-p (first actions)) :to-be t))))))
