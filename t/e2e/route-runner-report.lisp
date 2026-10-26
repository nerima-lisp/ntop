(in-package #:ntop.e2e)
(progn
  (defparameter *route-classification-definitions*
    '((:reference-error "Reference harness errors" :harness-error)
      (:candidate-error "Candidate harness errors" :harness-error)
      (:harness-error "Unattributed harness errors" :harness-error)
      (:semantic-mismatch "Semantic mismatches" :failure)
      (:candidate-failure "Candidate failures" :failure)
      (:assertion-failure "Other assertion failures" :failure)))
  (defun %route-classification-count (subresults classification)
    (count classification subresults
           :key (lambda (subresult) (getf subresult :classification))))
  (defun %route-classification-group-count (subresults group)
    (loop for definition in *route-classification-definitions*
          when (eq group (third definition))
          sum (%route-classification-count subresults (first definition)))))

(progn
  (defun %route-result-status (route subresults)
    (declare (ignore route))
    (let ((statuses (mapcar (function %route-subresult-status) subresults)))
      (cond
        ((or (null statuses) (member :fail statuses)) :fail)
        ((member :pass statuses) :pass)
        (t :na))))
  (defun %route-status-label (status)
    (ecase status
      (:pass "PASS")
      (:fail "FAIL")
      (:na "N/A"))))
(defun %route-subresult-status (subresult)
  (let* ((status (or (getf subresult :status)
                     (if (getf subresult :passed) :pass :fail)))
         (applicability (getf subresult :applicability))
         (evidence (getf subresult :evidence)))
    (cond
      ((eq status :pass) :pass)
      ((eq status :fail) :fail)
      ((and (eq status :na)
            (stringp (getf subresult :reason))
            (plusp (length (getf subresult :reason)))
            (stringp (getf subresult :actual))
            (plusp (length (getf subresult :actual)))
            (listp evidence)
            evidence
            (every (lambda (path)
                     (and (stringp path) (plusp (length path))
                          (probe-file path)))
                   evidence)
            (or (member applicability
                        (list :battery-tabs :linux-only :mouse-other :powermetrics))
                (and (eq applicability :mode)
                     (string= (or (getf subresult :mode) "") "btm-self")
                     (member (getf subresult :route-id)
                             (list "extension-170" "extension-171"
                                   "extension-172" "extension-173"
                                   "restore-exception")
                             :test #'string=))))
       :na)
      (t :fail))))
(defun %route-btm-self-na-result
    (route subcase subcase-index directory)
  (let* ((id (route-id route))
         (context (or (getf subcase :context) "widget"))
         (reason
           (cond
             ((string= id "extension-170")
              (format nil "BTM has no ~a widget-view cycle for this route."
                      context))
             ((string= id "extension-171")
              "BTM has no backward graph-history route.")
             ((string= id "extension-172")
              "BTM has no forward graph-history route.")
             ((string= id "extension-173")
              "BTM has no return-to-current graph route.")
             ((string= id "restore-exception")
              "BTM has no terminal-session exception-boundary route.")
             (t (error "Route ~a is not allowed to be N/A in btm-self." id))))
         (actual "Skipped because btm-self has no BTM equivalent.")
         (path
           (merge-pathnames
            (format nil "~a-~d-no-btm.md" id subcase-index)
            directory)))
    (%write-text
     path
     (format nil "Route: ~a~%Mode: btm-self~%Applicability: mode~%Reason: ~a~%Observed: ~a~%"
             id reason actual))
    (list :status :na
          :passed nil
          :route-id id
          :mode "btm-self"
          :applicability :mode
          :input (%route-input-description route subcase)
          :expected (route-expected route)
          :actual actual
          :reason reason
          :cause reason
          :evidence (list (namestring path)))))

(defun %route-report-cell (value)
  (let ((text (princ-to-string (or value ""))))
    (with-output-to-string (stream)
      (loop for character across text
            do (case character
                 (#\| (write-string "\\|" stream))
                 (#\Newline (write-string "<br>" stream))
                 (#\Return nil)
                 (t (write-char character stream)))))))

(defun %route-write-report (path mode btm ntop results evidence-directory)
  (%write-text
   path
   (with-output-to-string (stream)
     (format stream
             "# Route execution report~%~%Mode: ~a~%Reference: ~a~%Candidate: ~a~%"
             mode btm ntop)
     (format stream "Evidence: ~a~%~%" (namestring evidence-directory))
     (format stream
             (concatenate 'string
                          "| ID | Status | Input sequence | Expected | Actual | "
                          "First differing cell | Cause assessment | Evidence |~%"))
     (format stream "| --- | --- | --- | --- | --- | --- | --- | --- |~%")
     (dolist (result results)
       (format stream "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |~%"
               (%route-report-cell (getf result :id))
               (%route-status-label (getf result :status))
               (%route-report-cell (getf result :input))
               (%route-report-cell (getf result :expected))
               (%route-report-cell (getf result :actual))
               (%route-report-cell (getf result :first-difference))
               (%route-report-cell (getf result :cause))
               (%route-report-cell
                (format nil "~{~a~^, ~}" (getf result :evidence))))
       (loop for subresult in (getf result :subresults)
             for subcase-index from 1
             do (format stream
                        (concatenate 'string
                                     "  - ~a (~a): ~a; classification ~a; "
                                     "expected ~a; actual ~a; first difference ~a; "
                                     "cause ~a; evidence ~{~a~^, ~}~%")
                        (%route-report-cell
                         (or (getf subresult :subcase-id)
                             (format nil "~a-~a~d"
                                     (getf result :id)
                                     (if (< subcase-index 10) "00"
                                       (if (< subcase-index 100) "0" ""))
                                     subcase-index)))
                        (%route-report-cell (getf subresult :input))
                        (string-upcase
                         (symbol-name (%route-subresult-status subresult)))
                        (%route-report-cell
                         (or (getf subresult :classification) "unclassified"))
                        (%route-report-cell (getf subresult :expected))
                        (%route-report-cell (getf subresult :actual))
                        (%route-report-cell
                         (getf subresult :first-difference))
                        (%route-report-cell (getf subresult :cause))
                        (getf subresult :evidence))))
     (let ((subresults
            (loop for result in results
                  append (copy-list (getf result :subresults)))))
       (format stream "~%Route counts: ~d total, ~d PASS, ~d FAIL, ~d N/A.~%"
               (length results)
               (count :pass results :key (lambda (result) (getf result :status)))
               (count :fail results :key (lambda (result) (getf result :status)))
               (count :na results :key (lambda (result) (getf result :status))))
       (format stream "Subcase counts: ~d total, ~d PASS, ~d FAIL, ~d N/A.~%"
               (length subresults)
               (count :pass subresults :key #'%route-subresult-status)
               (count :fail subresults :key #'%route-subresult-status)
               (count :na subresults :key #'%route-subresult-status))
       (format stream "Harness errors: ~d~%"
               (%route-classification-group-count subresults :harness-error))
       (loop for definition in *route-classification-definitions*
             do (format stream "~a: ~d~%" (second definition)
                        (%route-classification-count subresults
                                                     (first definition))))))))
(progn
  (defun %route-report-widget-no-data-p (grid title)
    (let* ((top (and grid
                     (loop for row below (grid-height grid)
                           for text = (criterion-row-text grid row)
                           when (search title text :test #'char-equal) return row)))
           (frame (and top
                       (find-if (lambda (bounds) (= top (first bounds)))
                                (criterion-projection-frames grid)))))
      (and frame
           (loop for row from (first frame) to (second frame)
                 thereis (search "No data" (criterion-row-text grid row)
                                 :test #'char-equal)))))
  (defun %route-manual-reason (route subcase reference candidate)
    (let* ((id (route-id route))
           (action (route-action route))
           (context (or (getf subcase :context) "default"))
           (input (or (getf subcase :input) ""))
           (reference-grid (getf reference :initial-grid))
           (candidate-grid (getf candidate :initial-grid))
           (reference-current (getf reference :grid))
           (candidate-current (getf candidate :grid)))
      (labels ((cursor-hidden-p (side)
                 (let ((path (getf side :evidence)))
                   (and path
                        (probe-file path)
                        (let ((ansi (uiop:read-file-string path)))
                          (and (search (format nil "~c[?25l" #\Escape) ansi)
                               (not (search (format nil "~c[?25h" #\Escape)
                                            ansi)))))))
               (help-search-without-mode-p (grid)
                 (let ((text (and grid (%route-grid-text grid))))
                   (and text
                        (search "Search:" text)
                        (not (search "Whole(F2)" text))
                        (not (search "Regex(F3)" text))))))
        (cond
          ((and (member id '("key-20" "key-21") :test #'string=)
                (string-equal context "battery tabs"))
           "The Battery view showed one panel and no selectable tab marker on this host.")
          ((and (member id '("key-20" "key-21") :test #'string=)
                (string-equal context "process tree")
                (%route-report-widget-no-data-p reference-grid "Processes")
                (%route-report-widget-no-data-p candidate-grid "Processes"))
           (concatenate 'string
                        "Both Processes panes showed No data, so no parent or child row "
                        "was available to observe tree collapse or expansion."))
          ((and (member id '("key-20" "key-21") :test #'string=)
                (string-equal context "search cursor")
                (cursor-hidden-p reference)
                (cursor-hidden-p candidate))
           (concatenate 'string
                        "Both saved ANSI captures hide the cursor with CSI ?25l and never "
                        "show it with CSI ?25h; the raw grid cannot expose Left or Right "
                        "caret placement."))
          ((and (string= id "key-27") (string-equal context "process search")
                (string= input "F3")
                (%route-report-widget-no-data-p reference-grid "Processes")
                (%route-report-widget-no-data-p candidate-grid "Processes"))
           "Both Processes panes showed No data, so F3 regex mode had no result rows to compare.")
          ((and (member id '("key-26" "key-27") :test #'string=)
                (string-equal context "process search")
                (member action '("Toggle whole-word search" "Toggle regex search")
                        :test #'string=)
                (%route-report-widget-no-data-p reference-grid "Processes")
                (%route-report-widget-no-data-p candidate-grid "Processes"))
           (if (string= action "Toggle whole-word search")
               (concatenate 'string
                            "Both Processes panes showed No data, so no process names were "
                            "available to distinguish whole-word matches.")
             (concatenate 'string
                          "Both Processes panes showed No data, so no result rows were "
                          "available to distinguish regex matching.")))
          ((and (member id '("key-26" "key-27") :test #'string=)
                (string-equal context "help search")
                (help-search-without-mode-p reference-current)
                (help-search-without-mode-p candidate-current))
           (concatenate 'string
                        "Both Help-search grids showed Search but no Whole(F2) or "
                        "Regex(F3) marker; BTM 0.14.9 exposes no mode state in this view."))
          ((and (string= id "key-35") (string-equal context "process search")
                (%route-report-widget-no-data-p reference-grid "Processes")
                (%route-report-widget-no-data-p candidate-grid "Processes"))
           "Both Processes panes showed No data, so Enter had no search result row to select.")
          ((and (string= id "key-35") (string-equal context "process sort")
                (%route-report-widget-no-data-p reference-grid "Processes")
                (%route-report-widget-no-data-p candidate-grid "Processes"))
           "Both Processes panes showed No data; Enter had no sort option or row order to verify.")
          ((and (member id '("key-35" "key-36") :test #'string=)
                (string-equal context "kill dialog"))
           (concatenate 'string
                        "The kill prompt needs a visible PID and "
                        "ntop-owned marker must share that row before checking the choice."))
          (t
 (if (and (string= id "key-36") (string-equal context "help"))
     "Both paired BTM captures still showed Help after Escape; verify it closes interactively."
   (format nil "~a in ~a lacks a measurable starting state in this run."
           action context)))))))
)

