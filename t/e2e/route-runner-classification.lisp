(in-package #:ntop.e2e)
(progn
  (define-condition route-side-capture-error (error)
    ((side :initarg :side :reader route-side-capture-error-side)
     (cause :initarg :cause :reader route-side-capture-error-cause)
     (result :initarg :result :initform nil
             :reader route-side-capture-error-result))
    (:report (lambda (condition stream)
               (format stream "~a side preparation/capture error: ~a"
                       (route-side-capture-error-side condition)
                       (route-side-capture-error-cause condition)))))
  (defun %route-capture-side (side function &rest arguments)
    (handler-case
        (let* ((result (apply function arguments))
               (cause (getf result :capture-error))
               (readiness-failure-p
                 (or (eq :manual (getf result :readiness-status))
                     (eq :timeout (getf result :readiness-result)))))
          (cond
            (cause
             (error 'route-side-capture-error :side side
                    :cause cause :result result))
            (readiness-failure-p
             (error 'route-side-capture-error
                    :side side
                    :cause (or (getf result :readiness-diagnostic)
                               (getf result :cause)
                               (getf result :actual)
                               "Process-data readiness timed out.")
                    :result result))
            (t result)))
      (route-side-capture-error (condition) (error condition))
      (error (condition)
        (error 'route-side-capture-error :side side :cause condition))))
  (defun %route-capture-error-result (route subcase subcase-index directory
                                      condition)
    (let* ((side (route-side-capture-error-side condition))
           (side-result (route-side-capture-error-result condition))
           (message (format nil "~a side preparation/capture error: ~a"
                            side (route-side-capture-error-cause condition)))
           (error-path
             (merge-pathnames
              (format nil "~a-~3,'0d-error.txt" (route-id route) subcase-index)
              directory))
           (evidence
             (append (let ((side-evidence (getf side-result :evidence)))
                       (cond ((null side-evidence) nil)
                             ((listp side-evidence) side-evidence)
                             (t (list side-evidence))))
                     (loop for name in '("reference" "candidate")
                           for frame-path =
                             (merge-pathnames
                              (format nil "~a-~3,'0d-~a.ansi"
                                      (route-id route) subcase-index name)
                              directory)
                           when (probe-file frame-path)
                             collect (namestring frame-path))
                     (list (namestring error-path)))))
      (%write-text error-path message)
      (list :status :fail :passed nil
            :classification
            (case side
              (:reference :reference-error)
              (:candidate :candidate-error)
              (otherwise :harness-error))
            :input (%route-input-description route subcase)
            :expected (route-expected route)
            :actual (format nil "~a; ~a" message
                            (or (getf side-result :actual) "no side output"))
            :cause message :first-difference nil :evidence evidence)))
  (defun %route-results-exit-code (results)
    (if (every (lambda (result)
                 (member (getf result :status) '(:pass :manual)))
               results)
        0
        1)))
(defun %route-capture-subcase (route subcase subcase-index directory thunk)
  (handler-case
      (funcall thunk)
    (route-side-capture-error (condition)
                              (%route-capture-error-result route subcase subcase-index directory
                                                           condition))
    (error (condition)
           (%route-capture-error-result
            route subcase subcase-index directory
            (make-condition 'route-side-capture-error
                            :side :harness :cause condition)))))
