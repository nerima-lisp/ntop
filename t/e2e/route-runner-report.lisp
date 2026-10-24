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
  (let ((path (merge-pathnames
               (format nil "~a-~d-no-btm.md"
                       (route-id route) subcase-index)
               directory)))
    (%write-text
     path
     (format nil
             (concatenate 'string
                          "# ~a~%~%This route has no btm equivalent and was not "
                          "executed in the btm-self control.~%~%Expected: ~a~%~%")
             (route-id route) (route-expected route)))
    (list :status :manual
          :passed nil
          :input (%route-input-description route subcase)
          :expected (route-expected route)
          :actual "Not run in btm-self control; no btm equivalent."
          :cause "No btm equivalent; control execution is not applicable."
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
