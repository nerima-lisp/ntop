(in-package #:ntop.e2e.test)
(cl-weave:describe
 "route outcome classification"
 (cl-weave:it
  "stops after reference capture errors and skips comparison"
  (let ((side-function (symbol-function 'ntop.e2e::%route-run-interactive-side))
        (comparison-function (symbol-function 'ntop.e2e::%route-all-expected-actual))
        (candidate-called nil)
        (comparison-called nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'ntop.e2e::%route-run-interactive-side)
                 (lambda (program &rest arguments)
                   (declare (ignore arguments))
                   (if (string= program "btm")
                       (error "reference capture failed")
                       (setf candidate-called t))))
           (setf (symbol-function 'ntop.e2e::%route-all-expected-actual)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf comparison-called t)))
           (let ((condition
                   (handler-case
                       (ntop.e2e::%route-run-interactive-pair
                        nil nil "classification test" "ntop" "btm" "ntop"
                        "reference" "candidate" 24 80 250 1 nil #p"/tmp/")
                     (ntop.e2e::route-side-capture-error (condition)
                       condition))))
             (cl-weave:expect
              (ntop.e2e::route-side-capture-error-side condition)
              :to-equal :reference)
             (cl-weave:expect candidate-called :to-be nil)
             (cl-weave:expect comparison-called :to-be nil)))
      (setf (symbol-function 'ntop.e2e::%route-run-interactive-side)
            side-function)
      (setf (symbol-function 'ntop.e2e::%route-all-expected-actual)
            comparison-function))))
 (cl-weave:it
  "stops after candidate capture errors before comparison"
  (let ((side-function (symbol-function 'ntop.e2e::%route-run-interactive-side))
        (comparison-function (symbol-function 'ntop.e2e::%route-all-expected-actual))
        (comparison-called nil))
    (unwind-protect
         (progn
           (setf (symbol-function 'ntop.e2e::%route-run-interactive-side)
                 (lambda (program &rest arguments)
                   (declare (ignore arguments))
                   (if (string= program "btm")
                       (list :actual "reference frame captured")
                       (error "candidate capture failed"))))
           (setf (symbol-function 'ntop.e2e::%route-all-expected-actual)
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf comparison-called t)))
           (let ((condition
                   (handler-case
                       (ntop.e2e::%route-run-interactive-pair
                        nil nil "classification test" "ntop" "btm" "ntop"
                        "reference" "candidate" 24 80 250 1 nil #p"/tmp/")
                     (ntop.e2e::route-side-capture-error (condition)
                       condition))))
             (cl-weave:expect
              (ntop.e2e::route-side-capture-error-side condition)
              :to-equal :candidate)
             (cl-weave:expect comparison-called :to-be nil)))
      (setf (symbol-function 'ntop.e2e::%route-run-interactive-side)
            side-function)
      (setf (symbol-function 'ntop.e2e::%route-all-expected-actual)
            comparison-function))))
 (cl-weave:it
  "separates semantic mismatches from assertion failures"
  (let ((mismatch
          (ntop.e2e::%route-paired-result
           "projection" "equal" nil "frames differ" "condition-A differs"
           "row 1 column 2" (list :projection "left") (list :projection "right")))
        (reference-predicate-failure
          (ntop.e2e::%route-paired-result
           "predicate" "expected state" nil "observed state" "reference predicate false"
           nil (list :passed nil) (list :passed t)))
        (candidate-failure
          (ntop.e2e::%route-paired-result
           "candidate" "expected state" nil "observed state" "candidate predicate false"
           nil (list :passed t) (list :passed nil))))
    (cl-weave:expect (getf mismatch :classification)
                     :to-equal :semantic-mismatch)
    (cl-weave:expect (getf reference-predicate-failure :classification)
                     :to-equal :assertion-failure)
    (cl-weave:expect (getf candidate-failure :classification)
                     :to-equal :candidate-failure)
    (cl-weave:expect (ntop.e2e::%route-subresult-status mismatch)
                     :to-equal :fail)))
 (cl-weave:it
  "counts harness errors separately while keeping failed exit status"
  (let* ((subresults
           (list (list :status :fail :passed nil
                       :classification :reference-error
                       :subcase-id "ref" :input "capture reference"
                       :expected "frame" :actual "reference error"
                       :cause "reference capture failed" :evidence nil)
                 (list :status :fail :passed nil
                       :classification :candidate-error
                       :subcase-id "candidate" :input "capture candidate"
                       :expected "frame" :actual "candidate error"
                       :cause "candidate capture failed" :evidence nil)
                 (list :status :fail :passed nil
                       :classification :semantic-mismatch
                       :subcase-id "difference" :input "compare frames"
                       :expected "equal" :actual "different"
                       :cause "condition-A differs" :evidence nil)))
         (results
           (list (list :id "classification" :status :fail :input "test"
                       :expected "pass" :actual "fail" :cause "test"
                       :evidence nil :subresults subresults)))
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (report-path (merge-pathnames "report.md" directory)))
    (unwind-protect
         (progn
           (ntop.e2e::%route-write-report
            report-path "ntop" "btm" "ntop" results directory)
           (let ((report (uiop:read-file-string report-path)))
             (cl-weave:expect (search "Subcase counts: 3 total, 0 PASS, 3 FAIL" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Harness errors: 2" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Reference harness errors: 1" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Candidate harness errors: 1" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Semantic mismatches: 1" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Candidate failures: 0" report)
                              :to-satisfy (function plusp)))
           (cl-weave:expect (ntop.e2e::%route-results-exit-code results)
                            :to-equal 1))
      (uiop:delete-directory-tree directory :validate t)))))
(cl-weave:describe
 "route harness capture evidence"
 (cl-weave:it
  "preserves side details and turns unclassified errors into failed harness results"
  (let* ((route (find "basic-mode" (ntop.e2e::route-catalog)
                      :key #'ntop.e2e::route-id :test #'string=))
         (subcase (first (ntop.e2e::%route-subcases-expanded route)))
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (condition
           (make-condition 'ntop.e2e::route-side-capture-error
                           :side :reference :cause "capture stream closed"
                           :result (list :actual "partial reference frame"
                                         :evidence "reference-frame.ansi"))))
    (unwind-protect
         (let ((result
                 (ntop.e2e::%route-capture-error-result
                  route subcase 1 directory condition)))
           (cl-weave:expect (getf result :status) :to-equal :fail)
           (cl-weave:expect (getf result :classification)
                            :to-equal :reference-error)
           (cl-weave:expect (search "partial reference frame"
                                    (getf result :actual))
                            :to-satisfy (function plusp))
           (cl-weave:expect (search "capture stream closed"
                                    (getf result :cause))
                            :to-satisfy (function plusp))
           (cl-weave:expect
            (not (null
                  (member "reference-frame.ansi"
                          (getf result :evidence)
                          :test (function equal))))
            :to-be t)
           (cl-weave:expect (probe-file (car (last (getf result :evidence))))
                            :to-satisfy (function identity))
           (let ((harness-result
                   (ntop.e2e::%route-capture-subcase
                    route subcase 2 directory
                    (lambda () (error "comparison failed")))))
             (cl-weave:expect (getf harness-result :status) :to-equal :fail)
             (cl-weave:expect (getf harness-result :classification)
                              :to-equal :harness-error)
             (cl-weave:expect
              (ntop.e2e::%route-results-exit-code
               (list (list :status (getf harness-result :status))))
              :to-equal 1)))
      (uiop:delete-directory-tree directory :validate t))))
 (cl-weave:it
  "counts candidate capture errors as harness errors and failed outcomes"
  (let* ((route (find "basic-mode" (ntop.e2e::route-catalog)
                      :key #'ntop.e2e::route-id :test #'string=))
         (subcase (first (ntop.e2e::%route-subcases-expanded route)))
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (subresult
           (ntop.e2e::%route-capture-subcase
            route subcase 1 directory
            (lambda ()
              (ntop.e2e::%route-capture-side
               :candidate
               (lambda () (error "candidate capture failed"))))))
         (subresults (list subresult))
         (results
           (list (list :id (ntop.e2e::route-id route)
                       :status (ntop.e2e::%route-result-status route subresults)
                       :input "candidate capture"
                       :expected (ntop.e2e::route-expected route)
                       :actual (getf subresult :actual)
                       :cause (getf subresult :cause)
                       :evidence (getf subresult :evidence)
                       :subresults subresults)))
         (report-path (merge-pathnames "report.md" directory)))
    (unwind-protect
         (progn
           (cl-weave:expect (getf subresult :classification)
                            :to-equal :candidate-error)
           (cl-weave:expect (getf subresult :status) :to-equal :fail)
           (cl-weave:expect
            (ntop.e2e::%route-classification-group-count
             subresults :harness-error)
            :to-equal 1)
           (cl-weave:expect
            (ntop.e2e::%route-classification-group-count
             subresults :failure)
            :to-equal 0)
           (ntop.e2e::%route-write-report
            report-path "ntop" "btm" "ntop" results directory)
           (let ((report (uiop:read-file-string report-path)))
              (cl-weave:expect (search "Subcase counts: 1 total, 0 PASS, 1 FAIL, 0 N/A."
                                      report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Harness errors: 1" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Candidate harness errors: 1" report)
                              :to-satisfy (function plusp))
             (cl-weave:expect (search "Candidate failures: 0" report)
                              :to-satisfy (function plusp)))
           (cl-weave:expect (ntop.e2e::%route-results-exit-code results)
                            :to-equal 1))
      (uiop:delete-directory-tree directory :validate t)))))
(cl-weave:describe
 "route pair predicate classification"
 (cl-weave:it
  "classifies control candidate predicate failures and valid differences"
  (let* ((route
           (find-if (function (lambda (entry)
                               (eq (ntop.e2e::route-kind entry) :flag)))
                    (ntop.e2e::route-catalog)))
         (side-symbol (find-symbol "%ROUTE-RUN-CLI-SIDE" "NTOP.E2E"))
         (valid-symbol (find-symbol "%ROUTE-CONTROL-OUTPUT-VALID-P" "NTOP.E2E"))
         (side-function (symbol-function side-symbol))
         (valid-function (symbol-function valid-symbol))
         (candidate-valid-p nil)
         (different-output-p nil))
    (unwind-protect
         (progn
           (setf (symbol-function side-symbol)
                 (lambda (program &rest arguments)
                   (declare (ignore arguments))
                   (list :exit-code 0
                         :stdout (if different-output-p program "same output")
                         :stderr ""
                         :actual "captured output")))
           (setf (symbol-function valid-symbol)
                 (lambda (route program side)
                   (declare (ignore route side))
                   (or (string= (pathname-name (pathname program)) "btm")
                       candidate-valid-p)))
           (let ((result
                   (ntop.e2e::%route-run-control-pair
                    route "control classification" nil nil "btm-self"
                    "btm" "ntop" "reference" "candidate" 1)))
             (cl-weave:expect (getf result :classification)
                              :to-equal :candidate-failure)
             (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                              :to-equal :fail))
           (setf candidate-valid-p t different-output-p t)
           (let ((result
                   (ntop.e2e::%route-run-control-pair
                    route "control comparison" nil nil "btm-self"
                    "btm" "ntop" "reference" "candidate" 1)))
             (cl-weave:expect (getf result :classification)
                              :to-equal :semantic-mismatch)))
      (setf (symbol-function side-symbol) side-function
            (symbol-function valid-symbol) valid-function))))
 (cl-weave:it
  "classifies resize candidate predicate failures and preserves valid differences"
  (let* ((route (find "basic-mode" (ntop.e2e::route-catalog)
                      :key (function ntop.e2e::route-id)
                      :test (function string=)))
         (side-symbol (find-symbol "%ROUTE-RUN-RESIZE-SIDE" "NTOP.E2E"))
         (difference-symbol
           (find-symbol "%ROUTE-PROJECTION-FIRST-DIFFERENCE" "NTOP.E2E"))
         (side-function (symbol-function side-symbol))
         (difference-function (symbol-function difference-symbol))
         (candidate-size-valid-p nil)
         (different-projection-p nil)
         (initial-grid (ntop.e2e:make-terminal-grid :rows 24 :cols 80))
         (invalid-initial-grid
           (ntop.e2e:make-terminal-grid :rows 24 :cols 79))
         (final-grid (ntop.e2e:make-terminal-grid :rows 36 :cols 120)))
    (unwind-protect
         (progn
           (setf (symbol-function side-symbol)
                 (lambda (program route subcase prefix rate timeout config-path
                          evidence-directory)
                   (declare (ignore route subcase prefix rate timeout config-path
                                    evidence-directory))
                   (list :initial-grid
                         (if (or (string= program "btm") candidate-size-valid-p)
                             initial-grid invalid-initial-grid)
                         :grid final-grid
                         :projection
                         (if (and different-projection-p
                                  (string= program "ntop"))
                             :candidate :same)
                         :actual "captured resized frame")))
           (setf (symbol-function difference-symbol)
                 (lambda (reference candidate)
                   (unless (equal reference candidate)
                     "condition-A differs")))
           (let ((result
                   (ntop.e2e::%route-run-resize-pair
                    route nil "resize classification" "btm" "ntop"
                    "reference" "candidate" 250 1 nil #p"/tmp/")))
             (cl-weave:expect (getf result :classification)
                              :to-equal :candidate-failure)
             (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                              :to-equal :fail))
           (setf candidate-size-valid-p t different-projection-p t)
           (let ((result
                   (ntop.e2e::%route-run-resize-pair
                    route nil "resize comparison" "btm" "ntop"
                    "reference" "candidate" 250 1 nil #p"/tmp/")))
             (cl-weave:expect (getf result :classification)
                              :to-equal :semantic-mismatch)))
      (setf (symbol-function side-symbol) side-function
            (symbol-function difference-symbol) difference-function))))
 (cl-weave:it
  "classifies invalid-flag exit-code-only differences as semantic mismatches"
  (let* ((route
           (find-if (function (lambda (entry)
                               (eq (ntop.e2e::route-kind entry) :flag)))
                    (ntop.e2e::route-catalog)))
         (side-symbol (find-symbol "%ROUTE-RUN-CLI-SIDE" "NTOP.E2E"))
         (side-function (symbol-function side-symbol)))
    (unwind-protect
         (progn
           (setf (symbol-function side-symbol)
                 (lambda (program arguments evidence-prefix timeout)
                   (declare (ignore arguments evidence-prefix timeout))
                   (list :exit-code (if (string= program "btm") 1 2)
                         :stdout "invalid option"
                         :stderr ""
                         :evidence (if (string= program "btm")
                                       #p"/tmp/reference.txt"
                                       #p"/tmp/candidate.txt"))))
           (let ((result
                   (ntop.e2e::%route-run-invalid-flag-pair
                    route "invalid option" nil nil "btm-self"
                    "btm" "ntop" "reference" "candidate" 1)))
             (cl-weave:expect (getf result :classification)
                              :to-equal :semantic-mismatch)))
      (setf (symbol-function side-symbol) side-function)))))

(cl-weave:describe
 "route applicability status aggregation"
 (cl-weave:it
  "records environmental N/A reason and evidence without procedure output"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (prefix (namestring (merge-pathnames "powermetrics" directory)))
         (evidence
          (ntop.e2e::%route-fixture-write-capability-evidence
           prefix "powermetrics unavailable"))
         (subcase (list :applicability :powermetrics))
         (reason "powermetrics is unavailable on this host")
         (actual "the capability probe reported unavailable")
         (result
          (ntop.e2e::%route-fixture-na-result
           subcase "ntop" reason actual evidence)))
    (unwind-protect
         (progn
           (cl-weave:expect (getf result :status) :to-equal :na)
           (cl-weave:expect (getf result :applicability)
                            :to-equal :powermetrics)
           (cl-weave:expect (getf result :reason) :to-equal reason)
           (cl-weave:expect (getf result :actual) :to-equal actual)
           (cl-weave:expect (getf result :evidence) :to-equal evidence)
           (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                            :to-equal :na)
           (cl-weave:expect (getf result :procedure) :to-be nil)
           (cl-weave:expect
            (fboundp (find-symbol "%ROUTE-MANUAL-PROCEDURE" "NTOP.E2E"))
            :to-be nil)
           (cl-weave:expect
            (fboundp (find-symbol "%ROUTE-BTM-SELF-MANUAL-SPEC" "NTOP.E2E"))
            :to-be nil))
      (uiop:delete-directory-tree directory :validate t))))
 (cl-weave:it
  "keeps failed siblings visible around an evidence-backed N/A result"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (prefix (namestring (merge-pathnames "applicability" directory)))
         (evidence
          (ntop.e2e::%route-fixture-write-capability-evidence
           prefix "platform capability unavailable"))
         (subcase (list :applicability :linux-only))
         (na-result
          (ntop.e2e::%route-fixture-na-result
           subcase "ntop" "Linux-only feature unavailable" "macOS host" evidence))
         (route (ntop.e2e::make-route-case :id "test")))
    (unwind-protect
         (progn
           (cl-weave:expect (ntop.e2e::%route-subresult-status na-result)
                            :to-be :na)
           (cl-weave:expect
            (ntop.e2e::%route-result-status
             route (list (list :passed t) na-result (list :passed t)))
            :to-be :pass)
           (cl-weave:expect
            (ntop.e2e::%route-result-status
             route (list (list :passed t) na-result (list :passed nil)))
            :to-be :fail)
           (cl-weave:expect
            (ntop.e2e::%route-result-status route (list na-result))
            :to-be :na))
      (uiop:delete-directory-tree directory :validate t))))
 (cl-weave:it
  "fails when expected route state is unobserved"
  (let* ((route
          (find "basic-mode" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase (first (ntop.e2e::%route-subcases-expanded route)))
         (empty-side
          (list :projection nil :initial-projection nil :grid nil :initial-grid nil))
         (visible-grid
          (let ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 100)))
            (criterion-test-write grid 1 0 "0"
                                  "RAM RX: TX: Total RX: Total TX: Disks Temperature")
            grid))
         (visible-side
          (list :projection nil :initial-projection nil
                :grid visible-grid :initial-grid visible-grid))
         (unobserved
          (loop for mode in (list "ntop" "btm-self")
                collect
                (multiple-value-list
                 (ntop.e2e::%route-all-expected-actual
                  route mode subcase empty-side empty-side))))
         (asymmetric
          (multiple-value-list
           (ntop.e2e::%route-all-expected-actual
            route "ntop" subcase visible-side empty-side))))
    (cl-weave:expect (mapcar (function first) unobserved)
                     :to-equal (list nil nil))
    (cl-weave:expect
     (every (lambda (result)
              (and (stringp (third result))
                   (plusp (length (third result)))
                   (not (member :manual result))))
            unobserved)
     :to-be t)
    (cl-weave:expect (first asymmetric) :to-be nil)
    (cl-weave:expect
     (and (stringp (third asymmetric))
          (plusp (length (third asymmetric)))
          (not (member :manual asymmetric)))
     :to-be t)))
 (cl-weave:it
  "uses one N/A classification for subcase rows and totals"
  (let* ((directory (ntop.e2e::%make-e2e-temporary-directory))
         (prefix (namestring (merge-pathnames "status" directory)))
         (evidence
          (ntop.e2e::%route-fixture-write-capability-evidence
           prefix "powermetrics unavailable"))
         (na-result
          (ntop.e2e::%route-fixture-na-result
           (list :applicability :powermetrics) "ntop"
           "powermetrics unavailable" "host does not support powermetrics"
           evidence))
         (subresults (list (list :passed t) na-result (list :passed nil)))
         (route (ntop.e2e::make-route-case :id "test")))
    (unwind-protect
         (progn
           (cl-weave:expect
            (mapcar (function ntop.e2e::%route-subresult-status) subresults)
            :to-equal (list :pass :na :fail))
           (cl-weave:expect (ntop.e2e::%route-result-status route subresults)
                            :to-be :fail))
      (uiop:delete-directory-tree directory :validate t)))))
