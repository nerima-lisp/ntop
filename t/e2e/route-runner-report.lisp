(in-package #:ntop.e2e)

(defun %route-result-status (route subresults)
  (cond
    ((some (lambda (result)
             (and (not (eq :manual (getf result :status)))
                  (not (getf result :passed))))
           subresults)
     :fail)
    ((route-manual-reason route) :manual)
    ((some (lambda (result) (eq :manual (getf result :status))) subresults)
     :manual)
    ((and subresults
          (every (lambda (result) (getf result :passed)) subresults))
     :pass)
    (t :fail)))
(defun %route-noncomparable-manual-result
    (route subcase subcase-index directory)
  (let* ((id (route-id route))
         (context (or (getf subcase :context) "CLI"))
         (input (%route-input-description route subcase))
         (path (merge-pathnames
                (format nil "~a-~d-no-btm.md" id subcase-index)
                directory))
         (reason
           (cond
             ((string= id "cpu-frequency")
              (concatenate 'string
                           "powermetrics needs sudo and device-specific cluster readings; "
                           "BTM has no equivalent frequency route."))
             ((string= id "extension-170")
              (cond
                ((string= context "Memory")
                 "BTM has no Memory-to-GPU view cycle matching this ntop route.")
                ((string= context "Temperatures")
                 "BTM has no Temperatures-to-graph-to-Battery cycle matching this route.")
                ((string= context "Disks")
                 "BTM has no Disks-to-I/O-graph view cycle matching this ntop route.")
                (t (format nil "BTM has no ~a widget-view cycle matching this ntop route."
                           context))))
             ((string= id "extension-171")
              "BTM has no route for ntop's one-window backward history scroll.")
             ((string= id "extension-172")
              "BTM has no route for ntop's one-window forward history scroll.")
             ((string= id "extension-173")
              "BTM has no route for ntop's return-to-current graph command.")
             ((string= id "sleep-child")
              (concatenate 'string
                           "BTM 0.14.9 showed No data for the harness-owned PID on this "
                           "macOS host, leaving no BTM row control."))
             ((string= id "restore-exception")
              (concatenate 'string
                           "BTM has no call-with-terminal-session exception-boundary route "
                           "to compare with ntop."))
             (t (format nil "BTM has no control route for ~a." (route-action route)))))
         (procedure (%route-manual-procedure route subcase input context)))
    (%write-text
     path
     (format nil
             "# ~a~%~%Reason: ~a~%~%Procedure: ~a~%~%Expected: ~a~%~%"
             id reason procedure (route-expected route)))
    (list :status :manual
          :passed nil
          :input input
          :expected (route-expected route)
          :actual "Not run in btm-self control; no btm equivalent."
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
     (format stream
             "| --- | --- | --- | --- | --- | --- | --- | --- |~%")
     (dolist (result results)
       (format stream "| ~a | ~a | ~a | ~a | ~a | ~a | ~a | ~a |~%"
               (%route-report-cell (getf result :id))
               (string-upcase (symbol-name (getf result :status)))
               (%route-report-cell (getf result :input))
               (%route-report-cell (getf result :expected))
               (%route-report-cell (getf result :actual))
               (%route-report-cell (getf result :first-difference))
               (%route-report-cell (getf result :cause))
               (%route-report-cell
                (format nil "~{~a~^, ~}" (getf result :evidence))))
       (dolist (subresult (getf result :subresults))
         (let ((status (or (getf subresult :status)
                           (if (getf subresult :passed) :pass :fail))))
           (format stream
                   (concatenate 'string
                                "  - ~a: ~a; expected ~a; actual ~a; "
                                "first difference ~a; cause ~a; evidence ~{~a~^, ~}~%")
                   (%route-report-cell (getf subresult :input))
                   (string-upcase (symbol-name status))
                   (%route-report-cell (getf subresult :expected))
                   (%route-report-cell (getf subresult :actual))
                   (%route-report-cell (getf subresult :first-difference))
                   (%route-report-cell (getf subresult :cause))
                   (getf subresult :evidence)))))
     (format stream "~%Counts: ~d total, ~d PASS, ~d FAIL, ~d MANUAL.~%"
             (length results)
             (count :pass results :key (lambda (result) (getf result :status)))
             (count :fail results :key (lambda (result) (getf result :status)))
             (count :manual results :key (lambda (result) (getf result :status)))))))
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
          (t (format nil "~a in ~a lacks a measurable starting state in this run."
                     action context))))))
)

