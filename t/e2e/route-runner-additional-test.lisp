(in-package #:ntop.e2e.test)

(defun %route-context-test-grid (&rest lines)
  (let ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 80)))
    (loop for line in lines
          for row from 0
          do (criterion-test-write grid row 0 "0" line))
    grid))

(defun %route-context-test-frame-grid (full-screen-p)
  (let* ((rows 12)
         (columns 40)
         (right (if full-screen-p (1- columns) 19))
         (bottom (if full-screen-p (1- rows) 5))
         (grid (ntop.e2e:make-terminal-grid :rows rows :cols columns)))
    (criterion-test-write grid 0 0 "0" "┌")
    (criterion-test-write grid 0 right "0" "┐")
    (criterion-test-write grid bottom 0 "0" "└")
    (criterion-test-write grid bottom right "0" "┘")
    grid))

(cl-weave:describe
 "route context input decomposition"
 (cl-weave:it
  "waits for help, help search, and query states separately"
  (cl-weave:expect
   (mapcar (lambda (step)
             (list (getf step :input) (getf step :state)))
           (ntop.e2e::%route-context-input-steps "?/process" "help search"))
   :to-equal
   (list (list "?" :help)
         (list "/" :help-search)
         (list "process" :help-search-query)))
  (cl-weave:expect
   (mapcar (lambda (step)
             (list (getf step :input) (getf step :state)))
           (ntop.e2e::%route-context-input-steps "/btm" "process search"))
   :to-equal
   (list (list "/" :process-search)
         (list "btm" :process-search-query)))
  (cl-weave:expect
   (mapcar (lambda (step)
             (list (getf step :input) (getf step :state)))
           (ntop.e2e::%route-context-input-steps "?9" "help"))
   :to-equal
   (list (list "?" :help)
         (list "9" :help-section)))
  (cl-weave:expect
   (mapcar (lambda (step)
             (list (getf step :input) (getf step :state)))
           (ntop.e2e::%route-context-input-steps "?/" "help search"))
   :to-equal
   (list (list "?" :help) (list "/" :help-search)))))

(cl-weave:describe
 "route context help and query states"
 (cl-weave:it
  "distinguishes help, help search, and help search results"
  (let* ((regular-help
           (%route-context-test-grid
            "Help" "│1 - General" "Type to search, Esc to close"))
         (search-prompt (%route-context-test-grid "Search: |"))
         (search-results
           (%route-context-test-grid "Search: process" "1 - Processes"))
         (search-without-results (%route-context-test-grid "Search: process")))
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :state :help) nil regular-help)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :state :help-search) nil regular-help)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-help-search-open-state-p
      (ntop.e2e::%route-grid-text regular-help))
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :state :help-search) regular-help search-prompt)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "process" :state :help-search-query)
      search-prompt search-results)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "process" :state :help-search-query)
      search-prompt search-without-results)
     :to-be nil)))
 (cl-weave:it
  "waits for process search and the entered process query"
  (let* ((initial (%route-context-test-grid "Processes"))
         (search-prompt (%route-context-test-grid "Case Whole Regex > "))
         (search-query (%route-context-test-grid "> btm")))
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :state :process-search) initial search-prompt)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :state :process-search) search-prompt search-prompt)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "btm" :state :process-search-query)
      search-prompt search-query)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "btm" :state :process-search-query)
      search-prompt initial)
     :to-be nil))))

(cl-weave:describe
 "route context transitions"
 (cl-weave:it
  "waits for the requested help section and expanded widget"
  (let* ((help-before (%route-context-test-grid "Help" "│1 - General"))
         (help-after (%route-context-test-grid "Help" "│2 - Processes"))
         (expanded (%route-context-test-frame-grid t))
         (dashboard (%route-context-test-frame-grid nil)))
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "2" :state :help-section) help-before help-after)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "2" :state :help-section) help-after help-after)
     :to-be nil)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "e" :state :expanded-widget) dashboard expanded)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-context-stage-state-p
      (list :input "e" :state :expanded-widget) expanded expanded)
     :to-be nil)))
 (cl-weave:it
  "recognizes help section one without depending on its title"
  (let* ((route (ntop.e2e::make-route-case
                 :id "open-help" :kind :key :action "Open help"))
         (dashboard (%route-context-test-grid "Processes"))
         (section-one (%route-context-test-grid "Help" "│1 - Overview")))
    (cl-weave:expect
     (ntop.e2e::%route-action-state-default-p
      route "btm" nil (list :grid section-one :initial-grid dashboard)
      :reference)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-action-state-default-p
      route "btm" nil (list :grid section-one :initial-grid section-one)
      :reference)
     :to-be nil))))
