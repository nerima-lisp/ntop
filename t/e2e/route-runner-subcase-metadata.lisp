(in-package #:ntop.e2e)

(declaim (special +route-subcase-plans+))

(defun %route-subcase-fixture (context fixture)
  (let ((context (or context "")))
    (cond
      ((or (string-equal context "help")
           (search "help" context :test (function char-equal)))
       :help)
      ((search "expanded widget" context :test (function char-equal))
       :dashboard)
      ((search "search" context :test (function char-equal))
       :search)
      (t fixture))))

(defun %route-subcase-metadata (route-id action-name index subcase)
  (let ((plan
          (find-if
           (lambda (entry)
             (and (string= route-id (first entry))
                  (member index (second entry))))
           +route-subcase-plans+)))
    (when plan
      (let* ((probe (fourth plan))
             (input (getf subcase :input))
             (fixture (%route-subcase-fixture
                       (getf subcase :context) (third plan)))
             (query-procedure (getf (sixth plan) :query-procedure))
             (query-fixture
               (when query-procedure
                 (cond
                   ((eq fixture :help) :help-search)
                   ((member query-procedure
                            '(:toggle-case :toggle-whole-word :toggle-regex))
                    :sleep-variants)
                   (t :process-search))))
             (query-initial
               (case query-procedure
                 ((:toggle-case :toggle-whole-word)
                  (if (eq query-fixture :help-search)
                      "process"
                    "ntop-t-sleep"))
                 (:toggle-regex
                  (if (eq query-fixture :help-search)
                      "process"
                    "^ntop-t-sleep$"))
                 ((:cursor-start :cursor-end :clear
                   :delete-previous-character :delete-next-character)
                  "abcd")
                 (:delete-previous-word "alpha beta")))
             (query-cursor
               (case query-procedure
                 (:cursor-start 4)
                 (:cursor-end 0)
                 (:clear 2)
                 (:delete-previous-word 10)
                 ((:delete-previous-character :delete-next-character) 2)))
             (query-sentinel
               (when (member query-procedure '(:cursor-start :cursor-end))
                 "X"))
             (option-label
               (and (eq probe :search-results)
                    (cond
                      ((member input (list "F1" "Alt+c" "Alt+C")
                               :test (function string=))
                       "Case")
                      ((member input (list "F2" "Alt+w" "Alt+W")
                               :test (function string=))
                       "Whole")
                      ((member input (list "F3" "Alt+r" "Alt+R")
                               :test (function string=))
                       "Regex")))))
        (append
         (list :fixture fixture :probe probe)
         (when (fifth plan)
           (list :applicability (fifth plan)))
         (append
          (sixth plan)
          (let* ((context (or (getf subcase :context) ""))
                 (gesture (getf subcase :gesture))
                 (input (getf subcase :input))
                 (observer-completion
                   (cond
                     ((string-equal context "expanded widget")
                      :expanded-widget-restored)
                     ((and (string-equal context "help")
                           (member gesture (list "wheel-up" "wheel-down")
                                   :test (function string=)))
                      :help-section-transition)
                     ((and (string-equal context "graphs")
                           (member gesture (list "wheel-up" "wheel-down")
                                   :test (function string=)))
                      :graph-window-transition)))
                 (fixture-setup-inputs
                   (cond
                     ((and (string= action-name "Select help section")
                           (string= input "1"))
                      (list "2"))
                     ((and (string= action-name "Select help section")
                           (member input (list "7" "8" "9")
                                   :test (function string=)))
                      (list "1"))
                     ((and (string-equal context "graphs")
                           (member action-name
                                   (list "Zoom graph out" "Reset graph zoom")
                                   :test (function string=)))
                      (list "+"))))
                 (fixture-setup-gesture
                   (cond
                     ((and (string-equal context "help")
                           (member gesture (list "wheel-up" "wheel-down")
                                   :test (function string=)))
                      (if (string= gesture "wheel-up")
                          "wheel-down"
                        "wheel-up"))
                     ((and (string-equal context "graphs")
                           (string= gesture "wheel-down"))
                      "wheel-up"))))
            (append
             (when observer-completion
               (list :observer-completion observer-completion))
             (when fixture-setup-inputs
               (list :fixture-setup-inputs fixture-setup-inputs))
             (when (and (string= action-name "Select help section")
                        (or (string= input "1")
                            (member input (list "7" "8" "9")
                                    :test (function string=))))
               (list :fixture-setup-section
                     (if (string= input "1") 2 1)))
             (when fixture-setup-gesture
               (list :fixture-setup-gesture fixture-setup-gesture)))))
         (when query-fixture
           (list :query-fixture query-fixture))
         (when (eq query-fixture :sleep-variants)
           (list :query-fixture-prefix "ntop-t-"
                 :query-fixture-tags '("sleep" "sleepy" "Sleep")))
         (when (eq fixture :process-tree)
           (list :fixture-tree-parent-name "ntop-t-parent"
                 :fixture-tree-child-name "ntop-t-child"))
         (when query-initial
           (list :query-initial query-initial))
         (when (integerp query-cursor)
           (list :query-cursor query-cursor))
         (when query-sentinel
           (list :query-sentinel query-sentinel))
         (when option-label
           (list :search-option-label option-label)))))))

(defun %route-subcases-with-metadata (route-id action-name subcases)
  (loop for subcase in subcases
        for index from 1
        for metadata = (append
                        (%route-subcase-metadata
                         route-id action-name index subcase)
                        (when (member
                               action-name
                               (list "Move left within widget"
                                     "Move right within widget"
                                     "Select CPU series"
                                     "Select battery"
                                     "Terminal resize")
                               :test (function string=))
                          (list :minimum-rate 1000)))
        collect (if metadata
                    (append (copy-list subcase) metadata)
                  subcase)))
