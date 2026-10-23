(in-package #:ntop.test)

(defun %f8b-process-search-row (name)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        4242
                                        :name
                                        name
                                        :command
                                        "example-worker --fixture"
                                        :cpu-percent
                                        12.5d0
                                        :memory-percent
                                        10d0
                                        :memory-bytes
                                        1024
                                        :read-bytes-per-second
                                        20
                                        :write-bytes-per-second
                                        30
                                        :total-read-bytes
                                        40
                                        :total-write-bytes
                                        50
                                        :user-name
                                        "fixture-user"
                                        :state
                                        "sleeping"
                                        :elapsed-seconds
                                        60d0
                                        :priority
                                        20
                                        :nice
                                        0))

(cl-weave:describe "Process search field state"
                   (cl-weave:it "edits query text and keeps the cursor at character boundaries"
                                (let ((state (ntop.process.presentation::make-process-search-field-state)))
                                  (cl-weave:expect
                                   (ntop.process.presentation::transition-process-search-field state
                                                                                               :insert-paste
                                                                                               "Alpha Worker")
                                   :to-be
                                   state)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   12)
                                  (ntop.process.presentation::transition-process-search-field state :move-left)
                                  (ntop.process.presentation::transition-process-search-field state :backspace)
                                  (cl-weave:expect
                                   (ntop.process.application:process-search-state-query
                                    (ntop.process.presentation::process-search-field-state-application-state state))
                                   :to-equal
                                   "Alpha Workr")
                                  (ntop.process.presentation::transition-process-search-field state :move-home)
                                  (ntop.process.presentation::transition-process-search-field state :delete-forward)
                                  (ntop.process.presentation::transition-process-search-field state :move-end)
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :delete-previous-word)
                                  (cl-weave:expect
                                   (ntop.process.application:process-search-state-query
                                    (ntop.process.presentation::process-search-field-state-application-state state))
                                   :to-equal
                                   "lpha ")))
                   (cl-weave:it-each
                    ((:toggle-case-sensitive ntop.process.presentation::process-search-field-state-case-sensitive-p)
                     (:toggle-whole-word ntop.process.presentation::process-search-field-state-whole-word-p)
                     (:toggle-regex ntop.process.presentation::process-search-field-state-regex-p))
                    "toggles the ~A search option"
                    (action accessor)
                    (let ((state (ntop.process.presentation::make-process-search-field-state)))
                      (ntop.process.presentation::transition-process-search-field state action)
                      (cl-weave:expect (funcall accessor state) :to-be t)))
                   (cl-weave:it "recompiles the predicate after query and option changes"
                                (let* ((state (ntop.process.presentation::make-process-search-field-state))
                                       (application-state
                                        (ntop.process.presentation::process-search-field-state-application-state state))
                                       (row (%f8b-process-search-row "Alpha Worker")))
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :insert-text
                                                                                              "alpha")
                                  (cl-weave:expect
                                   (funcall (ntop.process.application:process-search-state-predicate application-state)
                                            row)
                                   :to-be
                                   t)
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :toggle-case-sensitive)
                                  (cl-weave:expect
                                   (funcall (ntop.process.application:process-search-state-predicate application-state)
                                            row)
                                   :to-be
                                   nil)))
                   (cl-weave:it "initializes existing query and flags from application state"
                                (let* ((application-state
                                        (ntop.process.application:make-process-search-state :query "seed"))
                                       (config
                                        (ntop.shared.application:make-application-config :process-regex-p
                                                                                         t
                                                                                         :process-case-sensitive-p
                                                                                         t
                                                                                         :process-whole-word-p
                                                                                         t
                                                                                         :process-show-command-p
                                                                                         t))
                                       (state
                                        (ntop.process.presentation::make-process-search-field-state :config
                                                                                                    config
                                                                                                    :application-state
                                                                                                    application-state))
                                       (defaults (ntop.process.presentation::%make-process-search-field-state)))
                                  (cl-weave:expect
                                   (list (ntop.process.presentation::%process-search-query state)
                                         (ntop.process.presentation::process-search-field-state-cursor-index state)
                                         (ntop.process.presentation::process-search-field-state-application-state state))
                                   :to-equal
                                   (list "seed" 4 application-state))
                                  (cl-weave:expect
                                   (list (ntop.process.presentation::process-search-field-state-regex-p state)
                                         (ntop.process.presentation::process-search-field-state-case-sensitive-p state)
                                         (ntop.process.presentation::process-search-field-state-whole-word-p state)
                                         (ntop.process.presentation::process-search-field-state-command-p state)
                                         (ntop.process.presentation::process-search-field-state-focused-p state))
                                   :to-equal
                                   (list t t t t t))
                                  (cl-weave:expect
                                   (list (ntop.process.presentation::process-search-field-state-cursor-index defaults)
                                         (ntop.process.presentation::process-search-field-state-regex-p defaults)
                                         (ntop.process.presentation::process-search-field-state-case-sensitive-p
                                          defaults)
                                         (ntop.process.presentation::process-search-field-state-whole-word-p defaults)
                                         (ntop.process.presentation::process-search-field-state-command-p defaults)
                                         (ntop.process.presentation::process-search-field-state-focused-p defaults)
                                         (ntop.process.presentation::%process-search-query defaults))
                                   :to-equal
                                   (list 0 nil nil nil nil t ""))
                                  (let* ((command-config
                                          (ntop.shared.application:make-application-config :process-show-command-p t))
                                         (command-state
                                          (ntop.process.presentation::make-process-search-field-state :config
                                                                                                      command-config)))
                                    (ntop.process.presentation::transition-process-search-field command-state
                                                                                                :insert-text
                                                                                                "example")
                                    (cl-weave:expect
                                     (funcall
                                      (ntop.process.application:process-search-state-predicate
                                       (ntop.process.presentation::process-search-field-state-application-state
                                        command-state))
                                      (%f8b-process-search-row "Alpha Worker"))
                                     :to-be
                                     t))))
                   (cl-weave:it "moves by words and keeps edits within query boundaries"
                                (let ((state (ntop.process.presentation::make-process-search-field-state)))
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :insert-text
                                                                                              "Alpha  Beta Gamma")
                                  (ntop.process.presentation::transition-process-search-field state :move-home)
                                  (ntop.process.presentation::transition-process-search-field state :move-left)
                                  (ntop.process.presentation::transition-process-search-field state :backspace)
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :delete-previous-word)
                                  (cl-weave:expect (ntop.process.presentation::%process-search-query state)
                                                   :to-equal
                                                   "Alpha  Beta Gamma")
                                  (ntop.process.presentation::transition-process-search-field state :move-word-right)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   7)
                                  (ntop.process.presentation::transition-process-search-field state :move-word-right)
                                  (ntop.process.presentation::transition-process-search-field state :move-word-right)
                                  (ntop.process.presentation::transition-process-search-field state :move-right)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   17)
                                  (ntop.process.presentation::transition-process-search-field state :move-word-left)
                                  (ntop.process.presentation::transition-process-search-field state :move-word-left)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   7)
                                  (ntop.process.presentation::transition-process-search-field state :set-cursor 100)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   17)
                                  (ntop.process.presentation::transition-process-search-field state :set-cursor 12)
                                  (ntop.process.presentation::transition-process-search-field state :move-word-left)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-cursor-index state)
                                   :to-equal
                                   7)
                                  (ntop.process.presentation::transition-process-search-field state :set-cursor 0)
                                  (ntop.process.presentation::transition-process-search-field state :delete-forward)
                                  (cl-weave:expect (ntop.process.presentation::%process-search-query state)
                                                   :to-equal
                                                   "lpha  Beta Gamma")
                                  (ntop.process.presentation::transition-process-search-field state :set-cursor 17)
                                  (ntop.process.presentation::transition-process-search-field state :delete-forward)
                                  (ntop.process.presentation::transition-process-search-field state :set-cursor 12)
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :delete-previous-word)
                                  (cl-weave:expect (ntop.process.presentation::%process-search-query state)
                                                   :to-equal
                                                   "lpha  Beta amma")
                                  (ntop.process.presentation::transition-process-search-field state :clear)
                                  (cl-weave:expect
                                   (list (ntop.process.presentation::%process-search-query state)
                                         (ntop.process.presentation::process-search-field-state-cursor-index state)
                                         (ntop.process.application:process-search-state-predicate
                                          (ntop.process.presentation::process-search-field-state-application-state
                                           state))
                                         (ntop.process.application:process-search-state-syntax-error
                                          (ntop.process.presentation::process-search-field-state-application-state
                                           state)))
                                   :to-equal
                                   (list "" 0 nil nil))
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :toggle-case-sensitive)
                                  (ntop.process.presentation::transition-process-search-field state
                                                                                              :toggle-case-sensitive)
                                  (ntop.process.presentation::transition-process-search-field state :toggle-whole-word)
                                  (ntop.process.presentation::transition-process-search-field state :toggle-whole-word)
                                  (ntop.process.presentation::transition-process-search-field state :toggle-regex)
                                  (ntop.process.presentation::transition-process-search-field state :toggle-regex)
                                  (cl-weave:expect
                                   (list (ntop.process.presentation::process-search-field-state-case-sensitive-p state)
                                         (ntop.process.presentation::process-search-field-state-whole-word-p state)
                                         (ntop.process.presentation::process-search-field-state-regex-p state))
                                   :to-equal
                                   (list nil nil nil))
                                  (ntop.process.presentation::transition-process-search-field state :blur)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-focused-p state)
                                   :to-be
                                   nil)
                                  (ntop.process.presentation::transition-process-search-field state :focus)
                                  (cl-weave:expect
                                   (ntop.process.presentation::process-search-field-state-focused-p state)
                                   :to-be
                                   t)
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.process.presentation::transition-process-search-field state :set-cursor -1))
                                   :to-throw
                                   (quote type-error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.process.presentation::transition-process-search-field state :insert-text 7))
                                   :to-throw
                                   (quote type-error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.process.presentation::transition-process-search-field state :unknown))
                                   :to-throw
                                   (quote error))
                                  (cl-weave:expect
                                   (lambda ()
                                     (ntop.process.presentation::transition-process-search-field nil :move-left))
                                   :to-throw
                                   (quote type-error)))))
