(in-package #:ntop.e2e)

(defun %route-observer-answer (passed actual cause)
  (values
   (if passed
       t
       nil)
   actual
   (unless passed
     cause)))

(defun %route-observer-prompt-cursor-moved-p (route side initial grid)
  (let* ((steps (getf side :action-grids))
         (before (nth-value 0 (%route-observer-prompt-state initial)))
         (action-query (nth-value 0 (%route-observer-prompt-state (first steps))))
         (sentinel-query (nth-value 0 (%route-observer-prompt-state (second steps))))
         (final-query (nth-value 0 (%route-observer-prompt-state grid)))
         (direction
          (cond
            ((equal (route-action route) "Move left within widget") :left)
            ((equal (route-action route) "Move right within widget") :right)))
         (expected
          (case direction
            (:left "aXbcd")
            (:right "abcXd")))
         (passed
          (and expected
               (equal before "abcd")
               (equal action-query before)
               (equal sentinel-query expected)
               (equal final-query sentinel-query))))
    (values passed
            (list :before
                  before
                  :after-action
                  action-query
                  :after-sentinel
                  sentinel-query
                  :final
                  final-query)
            (unless passed
              "cursor movement and sentinel placement did not match"))))

(defun %route-observer-delete-previous-word (text cursor)
  (when (and text (integerp cursor) (<= 0 cursor (length text)))
    (let ((start cursor))
      (loop while (and (plusp start) (find (char text (1- start)) '(#\Space #\Tab)))
            do (decf start))
      (loop while (and (plusp start) (not (find (char text (1- start)) '(#\Space #\Tab))))
            do (decf start))
      (concatenate 'string (subseq text 0 start) (subseq text cursor)))))

(defun %route-observer-query-edit-state-p (route subcase side initial grid)
  (if (eq (route-kind route) :search-field)
      (let ((passed (%route-search-token-visible-p (getf subcase :input) grid)))
        (values passed
                (getf subcase :input)
                (unless passed "search token was not accepted")))
    (multiple-value-bind (before before-pos)
        (%route-observer-prompt-state initial)
      (multiple-value-bind (after after-pos)
          (%route-observer-prompt-state grid)
        (let* ((input (getf subcase :input))
               (before-pos (or (getf side :initial-cursor-position) before-pos))
               (after-pos (or (getf side :cursor-position) after-pos))
               (expected
                 (cond
                   ((equal input "Ctrl+U") "")
                   ((equal input "Ctrl+W")
                    (%route-observer-delete-previous-word before before-pos))
                   ((member input (list "Backspace" "Ctrl+H") :test (function equal))
                    (and before before-pos (plusp before-pos)
                         (concatenate (quote string)
                                      (subseq before 0 (1- before-pos))
                                      (subseq before before-pos))))
                   ((equal input "Delete")
                    (and before before-pos (< before-pos (length before))
                         (concatenate (quote string)
                                      (subseq before 0 before-pos)
                                      (subseq before (1+ before-pos)))))))
               (passed
                 (if (member input (list "Ctrl+A" "Ctrl+E") :test (function equal))
                     (and before after (string= before after) after-pos
                          (if (equal input "Ctrl+A")
                              (zerop after-pos)
                            (= after-pos (length after))))
                   (and expected after (string= expected after)))))
          (values passed
                  (list before after expected)
                  (unless passed "query edit did not match its expected effect")))))))

(defun %route-observer-modal-p (route subcase initial grid)
  (let* ((action (route-action route))
         (context (or (getf subcase :context) ""))
         (before (%route-grid-text initial))
         (after (%route-grid-text grid))
         (existing (%route-modal-action-state-p route subcase initial grid)))
    (or (eq existing t)
        (cond
          ((string= action "Open process kill dialog")
           (and before
                after
                (notany
                 (lambda (s)
                   (search s before :test #'char-equal))
                 '("Kill process" "Send signal" "Confirm kill"))
                (some
                 (lambda (s)
                   (search s after :test #'char-equal))
                 '("Kill process" "Send signal" "Confirm kill" "Cancel"))))
          ((string= action "Accept current choice")
           (and before
                after
                (cond
                  ((string-equal context "process search")
                   (and (search "Case(F1)" before :test #'char-equal)
                        (not (search "Case(F1)" after :test #'char-equal))))
                  ((string-equal context "process sort")
                   (and (search "Sort By" before :test #'char-equal)
                        (not (search "Sort By" after :test #'char-equal))))
                  ((string-equal context "kill dialog")
                   (notany
                    (lambda (s)
                      (search s after :test #'char-equal))
                    '("Kill process" "Send signal" "Confirm kill" "Cancel"))))))
          (t nil)))))

(defun %route-observer-state-p (route mode subcase side side-name)
  (let* ((probe (getf subcase :probe))
         (initial (getf side :initial-grid))
         (grid (getf side :grid))
         (context (or (getf subcase :context) "")))
    (unless (keywordp probe)
      (error "Route ~a has no keyword behavior probe: ~s." (route-id route) probe))
    (case probe
      (:transition
       (let ((result (%route-action-state-default-p route mode subcase side side-name)))
         (%route-observer-answer (and result (not (eq result :manual)))
                                 result
                                 "transition was not observed")))
      (:selection
       (let* ((key6 (string= (route-id route) "key-6"))
              (steps (getf side :action-grids))
              (expanded (first steps))
              (restored (second steps))
              (before-frames (and key6 initial (criterion-projection-frames initial)))
              (expanded-frames (and key6 expanded (criterion-projection-frames expanded)))
              (key-result
               (if key6
                   (and before-frames
                        expanded-frames
                        (> (length before-frames) 1)
                        (< (length expanded-frames) (length before-frames))
                        (%route-observer-grid-changed-p initial expanded)
                        restored
                        (not (%route-observer-grid-changed-p initial restored)))
                 (%route-key-context-state route
                                           subcase
                                           initial
                                           grid
                                           (and (getf side :initial-projection)
                                                (getf side :projection)
                                                (criterion-projections-equal-p
                                                 (getf side :initial-projection)
                                                 (getf side :projection))))))
              (title (%route-observer-table-title route subcase))
              (fallback
               (and title
                    (member (route-action route)
                            '("Select battery" "Select process row" "Focus widget")
                            :test
                            #'equal)
                    (%route-observer-grid-changed-p initial grid)
                    (not (%route-mouse-target-unavailable-p subcase initial))))
              (passed (or (and key-result (not (eq key-result :manual))) fallback)))
         (%route-observer-answer passed
                                 (if key6
                                     "expanded widget then restored"
                                   (route-action route))
                                 "selection was not observed")))
      (:tree
       (let ((passed (%route-observer-tree-p route subcase initial grid)))
         (%route-observer-answer
          passed
          (%route-observer-process-rows grid)
          "tree action did not produce its expected transition")))
      (:search-cursor
       (multiple-value-bind (passed actual cause)
           (%route-observer-prompt-cursor-moved-p route side initial grid)
         (%route-observer-answer passed actual cause)))
      (:search-results
       (let* ((help-p (eq (getf subcase :fixture) :help))
              (query (if help-p
                         "process"
                       (nth-value 0 (%route-observer-prompt-state grid))))
              (passed
               (%route-observer-search-results-p route subcase initial grid)))
         (%route-observer-answer
          passed query "search results did not show the expected transition")))
      (:query-edit
       (multiple-value-bind (passed actual cause)
           (%route-observer-query-edit-state-p route subcase side initial grid)
         (%route-observer-answer passed actual cause)))
      (:navigation
       (if (string-equal context "help")
           (let ((passed
                  (%route-observer-help-navigation-p
                   route subcase initial grid)))
             (%route-observer-answer
              passed
              (list (%route-observer-help-section-range initial)
                    (%route-observer-help-section-range grid))
              "help navigation missed its expected section range"))
         (multiple-value-bind (passed actual cause)
             (%route-observer-table-p route subcase initial grid)
           (%route-observer-answer passed actual cause))))
      (:modal
       (let ((passed (%route-observer-modal-p route subcase initial grid)))
         (%route-observer-answer passed (route-action route) "modal state did not change")))
      (:sort
       (let ((passed (%route-observer-sort-p route initial grid)))
         (%route-observer-answer passed (route-action route) "sort state was not observed")))
      (:table
       (multiple-value-bind (passed actual cause)
           (%route-observer-table-p route subcase initial grid)
         (%route-observer-answer passed actual cause)))
      (:mouse
       (let* ((action (route-action route))
              (title (%route-observer-table-title route subcase))
              (unavailable (%route-mouse-target-unavailable-p subcase initial))
              (changed (%route-observer-grid-changed-p initial grid))
              (passed
               (cond
                ((member action '("Ignore other buttons" "Ignore pointer motion") :test #'equal)
                 (and initial grid (not changed)))
                ((and (string-equal context "graphs")
                      (member action '("Scroll up" "Scroll down") :test #'equal))
                 (%route-graph-window-transition-p route initial grid))
                (unavailable nil)
                ((and title
                      (member action
                              '("Select process row" "Select process sort"
                                                     "Select CPU series"
                                                     "Select table row or column"
                                                     "Select battery"
                                                     "Choose kill action")
                              :test
                              #'equal)
                      (%route-observer-table-rows grid title)) changed)
                (t changed))))
         (%route-observer-answer passed
                                 (list :mouse action context)
                                 (unless passed
                                   (if unavailable
                                       "mouse target unavailable"
                                     "mouse state unchanged")))))
      (:column
       (multiple-value-bind (passed expected-p) (%route-column-visible-p route grid)
         (%route-observer-answer passed
                                 (list (ntop.app.domain:parity-column-header (route-entry route))
                                       expected-p)
                                 "process column visibility did not match its condition")))
      (:graph
       (let* ((action (route-action route))
              (text (and grid (%route-grid-text grid)))
              (passed
               (cond
                ((member action
                         '("Zoom graph in" "Zoom graph out" "Reset graph zoom")
                         :test
                         #'equal) (%route-graph-window-transition-p route initial grid))
                ((equal action "Reset collection")
                 (and text
                      (search "CPU" text :test #'char-equal)
                      (search "60s" text)
                      (%route-observer-grid-changed-p initial grid))))))
         (%route-observer-answer passed action "graph state was not observed")))
      (:extension
       (let ((passed
              (or (and (string= mode "btm-self") (not (route-comparable-p route)))
                  (%route-extension-state-p route subcase side))))
         (%route-observer-answer passed
                                 (route-action route)
                                 "extension state cycle was not observed")))
      (:resize
       (let ((passed
              (and initial
                   grid
                   (= (grid-width initial) 80)
                   (= (grid-height initial) 24)
                   (= (grid-width grid) 120)
                   (= (grid-height grid) 36)
                   (char= (cell-character (grid-cell-at grid 0 119)) (code-char #x2510))
                   (char= (cell-character (grid-cell-at grid 35 119)) (code-char #x2518)))))
         (%route-observer-answer passed
                                 (and grid (list (grid-width grid) (grid-height grid)))
                                 "resize did not reach 120x36")))
      (:restore
       (let ((passed (eq (getf side :passed) t)))
         (%route-observer-answer passed
                                 (getf side :actual)
                                 (or (getf side :cause) "terminal settings were not restored"))))
      (:cpu-frequency
       (multiple-value-bind (passed actual cause) (%route-observer-cpu-frequency-p side grid)
         (%route-observer-answer passed actual cause)))
      (:capability (error "Probe :CAPABILITY is applicability-only for route ~a." (route-id route)))
      (otherwise (error "Unknown route behavior probe ~s for route ~a." probe (route-id route))))))
