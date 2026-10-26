(in-package #:ntop.e2e.test)
(cl-weave:describe
 "route fixture applicability"
 (cl-weave:it
  "classifies unavailable powermetrics on both sides as N/A"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (reference-evidence
           (ntop.e2e::%route-fixture-write-capability-evidence
            (namestring (merge-pathnames "reference-powermetrics" directory))
            "reference powermetrics unavailable"))
         (candidate-evidence
           (ntop.e2e::%route-fixture-write-capability-evidence
            (namestring (merge-pathnames "candidate-powermetrics" directory))
            "candidate powermetrics unavailable"))
         (reference-state
           (list :powermetrics-before
                 (list :capability :unavailable :reason "reference unavailable")))
         (candidate-state
           (list :powermetrics-before
                 (list :capability :unavailable :reason "candidate unavailable")))
         (reference
           (list :fixture-state reference-state :fixture-evidence reference-evidence))
         (candidate
           (list :fixture-state candidate-state :fixture-evidence candidate-evidence)))
    (unwind-protect
         (let ((result
                 (ntop.e2e::%route-fixture-applicability-result
                  nil (list :applicability :powermetrics) "ntop"
                  reference candidate)))
           (cl-weave:expect (getf result :status) :to-be :na)
           (cl-weave:expect (ntop.e2e::%route-subresult-status result) :to-be :na))
      (uiop:delete-directory-tree directory :validate t))))
 (cl-weave:it
  "classifies Linux-only capability with evidence on unsupported hosts"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (prefix (namestring (merge-pathnames "linux-only" directory)))
         (result
           (ntop.e2e::%route-fixture-applicability-result
            nil (list :applicability :linux-only) "ntop" nil nil prefix)))
    (unwind-protect
         (if (member :linux *features*)
             (cl-weave:expect result :to-be nil)
           (progn
             (cl-weave:expect (getf result :status) :to-be :na)
             (cl-weave:expect
              (ntop.e2e::%route-subresult-status result)
              :to-be :na)
             (cl-weave:expect
              (not (null (probe-file (first (getf result :evidence)))))
              :to-be t)))
      (uiop:delete-directory-tree directory :validate t)))))

(cl-weave:describe
 "route fixture process lifecycle"
 (cl-weave:it
  "starts metadata-named process rows and reaps them during cleanup"
  (let* ((route-subcase
           (loop for route in (ntop.e2e::route-catalog)
                 thereis
                 (loop for subcase in (ntop.e2e::%route-subcases-expanded route)
                       when (and (eq (getf subcase :fixture) :search)
                                 (getf subcase :query-fixture-prefix))
                         return (list route subcase)))))
    (unless route-subcase
      (error "No metadata-named process fixture is declared."))
    (let* ((route (first route-subcase))
           (subcase (second route-subcase))
           (state (ntop.e2e::%route-fixture-start route subcase))
           (children (copy-list (getf state :children)))
           (processes
             (mapcar (lambda (child) (getf child :process)) children))
           (directory (getf state :process-directory))
           (expected-names
             (sort
              (mapcar
               (lambda (tag)
                 (concatenate 'string (getf subcase :query-fixture-prefix) tag))
               (getf subcase :query-fixture-tags))
              (function string<))))
      (flet ((process-name (process)
               (multiple-value-bind (output error-output exit-code)
                   (uiop:run-program
                    (list "ps" "-p"
                          (princ-to-string (sb-ext:process-pid process))
                          "-o" "comm=")
                    :ignore-error-status t
                    :output :string
                    :error-output :string)
                 (declare (ignore error-output))
                 (unless (zerop exit-code)
                   (error "Could not read the process name for PID ~d."
                          (sb-ext:process-pid process)))
                 (let* ((name
                          (string-trim '(#\Space #\Tab #\Newline #\Return) output))
                        (slash (position #\/ name :from-end t)))
                   (if slash (subseq name (1+ slash)) name)))))
        (unwind-protect
             (progn
               (cl-weave:expect (length children) :to-equal 3)
               (cl-weave:expect
                (every #'sb-ext:process-alive-p processes)
                :to-be t)
               (cl-weave:expect
                (sort (mapcar #'process-name processes) (function string<))
                :to-equal expected-names))
          (ntop.e2e::%route-fixture-cleanup state))
        (cl-weave:expect
         (every (lambda (process) (not (sb-ext:process-alive-p process))) processes)
         :to-be t)
        (cl-weave:expect (getf state :children) :to-equal nil)
        (cl-weave:expect (not (probe-file directory)) :to-be t)))))
 (cl-weave:it
  "starts and reaps the declared process-tree fixture with named rows"
  (let* ((route-subcase
           (loop for route in (ntop.e2e::route-catalog)
                 thereis
                 (loop for subcase in (ntop.e2e::%route-subcases-expanded route)
                       when (eq (getf subcase :fixture) :process-tree)
                         return (list route subcase)))))
    (unless route-subcase
      (error "No subcase declares the process-tree fixture."))
    (let* ((route (first route-subcase))
           (subcase (second route-subcase))
           (state (ntop.e2e::%route-fixture-start route subcase))
           (entry (first (getf state :children)))
           (process (getf entry :process))
           (child-pid (first (getf entry :child-pids)))
           (directory (getf state :process-directory)))
      (flet ((pid-alive-p (pid)
               (multiple-value-bind (output error-output exit-code)
                   (uiop:run-program
                    (list "kill" "-0" (princ-to-string pid))
                    :ignore-error-status t
                    :output nil
                    :error-output nil)
                 (declare (ignore output error-output))
                 (zerop exit-code)))
             (process-name (pid)
               (multiple-value-bind (output error-output exit-code)
                   (uiop:run-program
                    (list "ps" "-p" (princ-to-string pid) "-o" "comm=")
                    :ignore-error-status t
                    :output :string
                    :error-output :string)
                 (declare (ignore error-output))
                 (unless (zerop exit-code)
                   (error "Could not read the process name for PID ~d." pid))
                 (let* ((name
                          (string-trim '(#\Space #\Tab #\Newline #\Return) output))
                        (slash (position #\/ name :from-end t)))
                   (if slash (subseq name (1+ slash)) name)))))
        (unwind-protect
             (progn
               (cl-weave:expect (length (getf state :children)) :to-equal 1)
               (cl-weave:expect (sb-ext:process-alive-p process) :to-be t)
               (cl-weave:expect (pid-alive-p child-pid) :to-be t)
               (cl-weave:expect
                (process-name (sb-ext:process-pid process))
                :to-equal (getf subcase :fixture-tree-parent-name))
               (cl-weave:expect
                (process-name child-pid)
                :to-equal (getf subcase :fixture-tree-child-name)))
          (ntop.e2e::%route-fixture-cleanup state))
        (cl-weave:expect (sb-ext:process-alive-p process) :to-be nil)
        (cl-weave:expect (pid-alive-p child-pid) :to-be nil)
        (cl-weave:expect (getf state :children) :to-equal nil)
        (cl-weave:expect (not (probe-file directory)) :to-be t)))))
 (cl-weave:it
  "bounds a child PID read and reaps the quiet process"
  (let* ((route-subcase
           (loop for route in (ntop.e2e::route-catalog)
                 thereis
                 (loop for subcase in (ntop.e2e::%route-subcases-expanded route)
                       when (eq (getf subcase :fixture) :process-tree)
                         return (list route subcase)))))
    (unless route-subcase
      (error "No subcase declares the process-tree fixture."))
    (let* ((route (first route-subcase))
           (subcase (second route-subcase))
           (process
             (sb-ext:run-program "/bin/bash"
                                 (list "-c" "exec sleep 300")
                                 :wait nil
                                 :output :stream
                                 :error nil))
           (state
             (list :route route
                   :subcase subcase
                   :children
                   (list (list :process process :child-pids nil)))))
      (unwind-protect
           (progn
             (cl-weave:expect (sb-ext:process-alive-p process) :to-be t)
             (cl-weave:expect
              (handler-case
                  (progn
                    (ntop.e2e::%route-fixture-read-child-pid state process)
                    :unexpected-pid)
                (error () :pid-read-failed))
              :to-be :pid-read-failed))
        (ntop.e2e::%route-fixture-cleanup state))
      (cl-weave:expect (sb-ext:process-alive-p process) :to-be nil)
      (cl-weave:expect (getf state :children) :to-be nil))))
 (cl-weave:it
  "rejects an overlong child PID line with the PID-read stage"
  (let* ((process
           (sb-ext:run-program
            "/bin/bash"
            (list "-c" "printf '%s\\n' 123456789012345678901; exec sleep 300")
            :wait nil
            :output :stream
            :error nil))
         (state
           (list :children (list (list :process process :child-pids nil))))
         (failure nil))
    (unwind-protect
         (setf failure
               (handler-case
                   (progn
                     (ntop.e2e::%route-fixture-read-child-pid state process)
                     "PID line was accepted")
                 (error (condition)
                   (princ-to-string condition))))
      (ntop.e2e::%route-fixture-cleanup state))
    (cl-weave:expect
     (if (and (search "process-tree-pid-read" failure :test #'char-equal)
              (search "20-character limit" failure))
         t
       nil)
     :to-be t))))

(cl-weave:describe
 "battery fixture capability"
 (cl-weave:it
  "requires a Battery frame and reports its tab labels before N/A"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (make-battery-grid
           (lambda (tabs)
             (let ((grid (criterion-test-hierarchy-grid 3 12 "38;5;8;49")))
               (criterion-test-write grid 2 6 "38;5;8;49" "Battery")
               (loop for tab in tabs
                     for row from 4
                     do (criterion-test-write
                         grid row 6 "38;5;8;49" (format nil "Battery ~a" tab)))
               grid)))
         (reference-grid (funcall make-battery-grid nil))
         (candidate-grid (funcall make-battery-grid nil))
         (two-tabs-grid (funcall make-battery-grid '("1" "2")))
         (missing-frame-grid (criterion-test-grid 12 60 ""))
         (reference-evidence
           (ntop.e2e::%route-fixture-write-capability-evidence
            (namestring (merge-pathnames "reference-battery" directory))
            "reference battery fixture"))
         (candidate-evidence
           (ntop.e2e::%route-fixture-write-capability-evidence
            (namestring (merge-pathnames "candidate-battery" directory))
            "candidate battery fixture"))
         (reference
           (list :initial-grid reference-grid
                 :fixture-state (list :evidence-path (pathname (first reference-evidence)))
                 :fixture-evidence reference-evidence))
         (candidate
           (list :initial-grid candidate-grid
                 :fixture-state (list :evidence-path (pathname (first candidate-evidence)))
                 :fixture-evidence candidate-evidence)))
    (unwind-protect
         (let ((result
                 (ntop.e2e::%route-fixture-applicability-result
                  nil (list :applicability :battery-tabs) "btm-self"
                  reference candidate)))
           (cl-weave:expect (getf result :status) :to-be :na)
           (cl-weave:expect
            (ntop.e2e::%route-subresult-status result)
            :to-be :na)
           (cl-weave:expect
            (ntop.e2e::%route-fixture-applicability-result
             nil (list :applicability :battery-tabs) "btm-self"
             reference
             (list :initial-grid missing-frame-grid
                   :fixture-state (list :evidence-path (pathname (first candidate-evidence)))
                   :fixture-evidence candidate-evidence))
            :to-be nil)
           (cl-weave:expect
            (ntop.e2e::%route-fixture-applicability-result
             nil (list :applicability :battery-tabs) "btm-self"
             reference
             (list :initial-grid two-tabs-grid
                   :fixture-state (list :evidence-path (pathname (first candidate-evidence)))
                   :fixture-evidence candidate-evidence))
            :to-be nil))
      (uiop:delete-directory-tree directory :validate t)))))
