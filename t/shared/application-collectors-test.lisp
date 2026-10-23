(in-package #:ntop.test)

(ntop.shared.application::define-collector collect-fixture-sample)

(defmethod collect-fixture-sample ((collector (eql :success)) on-sample on-unavailable)
  (declare (ignore collector on-unavailable))
  (funcall on-sample :fixture-sample)
  (values))

(defmethod collect-fixture-sample ((collector (eql :unavailable)) on-sample on-unavailable)
  (declare (ignore collector on-sample))
  (funcall on-unavailable :read-failed)
  (values))

(cl-weave:describe "application collectors"
                   (cl-weave:it "expands define-collector to a three-argument generic"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.shared.application::define-collector sample-collector))
                                 :to-equal
                                 (list 'defgeneric
                                       'sample-collector
                                       '(ntop.shared.application::collector
                                         ntop.shared.application::on-sample
                                         ntop.shared.application::on-unavailable))))
                   (cl-weave:it-each
                    (("cpu") ("memory")
                     ("gpu")
                     ("network")
                     ("disk")
                     ("temperature")
                     ("battery")
                     ("process"))
                    "declares the ~A sample collector"
                    (feature)
                    (let* ((name (format nil "COLLECT-~A-SAMPLE" (string-upcase feature)))
                           (collector (intern name (find-package "NTOP.SHARED.APPLICATION"))))
                      (cl-weave:expect (fboundp collector) :to-be-truthy)
                      (cl-weave:expect (functionp (fdefinition collector)) :to-be-truthy)))
                   (cl-weave:it "calls the sample callback once and returns no values"
                                (let* ((samples nil)
                                       (reasons nil)
                                       (returned-values
                                        (multiple-value-list
                                         (collect-fixture-sample :success
                                                                 (lambda (sample)
                                                                   (push sample samples)
                                                                   :callback-result)
                                                                 (lambda (reason)
                                                                   (push reason reasons)
                                                                   :callback-result)))))
                                  (cl-weave:expect samples :to-equal '(:fixture-sample))
                                  (cl-weave:expect reasons :to-equal '())
                                  (cl-weave:expect returned-values :to-equal '())))
                   (cl-weave:it "calls the unavailable callback once and returns no values"
                                (let* ((samples nil)
                                       (reasons nil)
                                       (returned-values
                                        (multiple-value-list
                                         (collect-fixture-sample :unavailable
                                                                 (lambda (sample)
                                                                   (push sample samples)
                                                                   :callback-result)
                                                                 (lambda (reason)
                                                                   (push reason reasons)
                                                                   :callback-result)))))
                                  (cl-weave:expect samples :to-equal '())
                                  (cl-weave:expect reasons :to-equal '(:read-failed))
                                  (cl-weave:expect returned-values :to-equal '()))))
(cl-weave:describe "collector unavailability reasons"
  (cl-weave:it "accepts the six shared reason keywords and rejects unknown ones"
    (let ((reasons (list :not-present :disabled :permission-denied
                         :unsupported-platform :private-api-unavailable
                         :read-failed)))
      (cl-weave:expect
       (every (lambda (reason)
                (typep reason
                       (quote ntop.shared.application:collector-unavailability-reason)))
              reasons)
       :to-be t)
      (cl-weave:expect
       (typep :fixture-reason
              (quote ntop.shared.application:collector-unavailability-reason))
       :to-be nil))))
