(in-package #:ntop.test)

(ntop.shared.domain:define-sample-type measurement-fixture-sample (amount 0 :type integer))

(cl-weave:describe "measurement values distinguish missing data from zero"
  (cl-weave:it-each ((nil nil)
                     (0 t)
                     (0.0 t)
                     (-1 t)
                     (1/3 t)
                     (42 t))
      "recognizes availability for ~A"
      (value expected)
    (cl-weave:expect
     (ntop.shared.domain:measurement-available-p value)
     :to-be expected))

  (cl-weave:it "defines value types with a copier-free structure"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.shared.domain:define-value-type sample-value (amount 0)))
     :to-satisfy
     (lambda (form)
       (equal form
              '(defstruct (ntop.test::sample-value
                            (:copier nil))
                 (ntop.test::amount 0))))))

  (cl-weave:it "defines sample types with the shared timestamp record"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.shared.domain:define-sample-type sample-value (amount 0)))
     :to-satisfy
     (lambda (form)
       (equal form
              '(defstruct (ntop.test::sample-value
                            (:include ntop.shared.domain:sample-record)
                            (:copier nil))
                 (ntop.test::amount 0 :read-only t))))))

  (cl-weave:it "constructs a shared record with its monotonic timestamp"
    (let ((sample (ntop.shared.domain:make-sample-record :sampled-at 12.5)))
      (cl-weave:expect
       (ntop.shared.domain:sample-record-sampled-at sample)
       :to-be 12.5))))

(cl-weave:it
  "requires a sample timestamp before construction"
  (cl-weave:signals simple-error
    (funcall (symbol-function
              (intern "MAKE-MEASUREMENT-FIXTURE-SAMPLE" "NTOP.TEST"))
             :amount 3)))
(cl-weave:it
  "does not generate sample slot writers"
  (cl-weave:expect
   (list
    (fboundp '(setf ntop.shared.domain:sample-record-sampled-at))
    (fboundp '(setf ntop.test::measurement-fixture-sample-amount)))
   :to-equal '(nil nil)))
