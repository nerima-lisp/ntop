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
  "starts named process rows and reaps them during cleanup"
  (let* ((state
           (ntop.e2e::%route-fixture-start
            nil (list :fixture :processes)))
         (children (copy-list (getf state :children)))
         (processes
           (mapcar (lambda (child) (getf child :process)) children)))
    (unwind-protect
         (progn
           (cl-weave:expect (length children) :to-be 2)
           (cl-weave:expect
            (every #'sb-ext:process-alive-p processes)
            :to-be t))
      (ntop.e2e::%route-fixture-cleanup state))
    (cl-weave:expect
     (every (lambda (process) (not (sb-ext:process-alive-p process))) processes)
     :to-be t)
    (cl-weave:expect (getf state :children) :to-be nil))))
