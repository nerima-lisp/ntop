(in-package #:ntop.test)

(defun constructor-slot-outcomes (constructor accessor
                                              required-initargs
                                              initarg
                                              nil-outcome
                                              accepted-value
                                              invalid-value)
  (let ((make (symbol-function constructor))
        (read-slot (symbol-function accessor)))
    (labels ((construct (&optional (include-initarg-p nil) value)
               (apply make
                      (if include-initarg-p
                          (append required-initargs (list initarg value))
                          required-initargs)))
             (read-value (object)
               (funcall read-slot object))
             (slot-outcome (value)
               (handler-case (progn
                               (construct t value)
                               :accepted)
                 (type-error ()
                   :type-error)))
             (slot-value-or-error (&optional (value nil))
               (handler-case (read-value (construct t value))
                 (type-error ()
                   :type-error))))
      (let ((default-value (read-value (construct)))
            (nil-result (slot-value-or-error))
            (accepted-result (slot-value-or-error accepted-value))
            (invalid-result (slot-outcome invalid-value)))
        (cl-weave:expect nil-result :to-equal nil-outcome)
        (list default-value nil-result accepted-result invalid-result)))))

(ntop.shared.domain:define-sample-type measurement-fixture-sample
                                       (amount 0 :type integer))

(ntop.shared.domain:define-value-type measurement-fixture-value (amount 0))

(cl-weave:describe "measurement values distinguish missing data from zero"
                   (cl-weave:it-each
                    ((nil nil) (0 t) (0.0 t) (-1 t) (1/3 t) (42 t))
                    "recognizes availability for ~A"
                    (value expected)
                    (cl-weave:expect
                     (ntop.shared.domain:measurement-available-p value)
                     :to-be
                     expected))
                   (cl-weave:it "defines value types with read-only slots"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.shared.domain:define-value-type
                                    sample-value
                                    amount
                                    (count 0 :type integer)))
                                 :to-equal
                                 '(defstruct
                                      (ntop.test::sample-value (:copier nil))
                                    (ntop.test::amount nil :read-only t)
                                    (count 0 :type integer :read-only t))))
                   (cl-weave:it
                    "defines sample types with the shared timestamp record"
                    (cl-weave:expect
                     (macroexpand-1
                      '(ntop.shared.domain:define-sample-type sample-value
                                                              (amount 0)))
                     :to-satisfy
                     (lambda (form)
                       (equal form
                              '(defstruct
                                   (ntop.test::sample-value
                                    (:include ntop.shared.domain:sample-record)
                                    (:copier nil))
                                 (ntop.test::amount 0 :read-only t))))))
                   (cl-weave:it
                    "constructs a shared record with its monotonic timestamp"
                    (let ((sample
                           (ntop.shared.domain:make-sample-record :sampled-at
                                                                  12.5)))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       12.5))))

(cl-weave:describe "shared measurement type boundaries"
                   (cl-weave:it "expands sample types with symbol slots"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.shared.domain:define-sample-type
                                    sample-value
                                    amount))
                                 :to-equal
                                 '(defstruct
                                      (ntop.test::sample-value
                                       (:include
                                        ntop.shared.domain:sample-record)
                                       (:copier nil))
                                    (ntop.test::amount nil :read-only t))))
                   (cl-weave:it-each
                    ((ntop.shared.domain:measurement-value nil t)
                     (ntop.shared.domain:measurement-value 0 t)
                     (ntop.shared.domain:measurement-value -1.5 t)
                     (ntop.shared.domain:measurement-value :bad nil)
                     (ntop.shared.domain:measurement-counter 0 t)
                     (ntop.shared.domain:measurement-counter 10 t)
                     (ntop.shared.domain:measurement-counter -1 nil)
                     (ntop.shared.domain:optional-counter nil t)
                     (ntop.shared.domain:optional-counter 0 t)
                     (ntop.shared.domain:optional-counter -1 nil)
                     (ntop.shared.domain:monotonic-seconds 0.0 t)
                     (ntop.shared.domain:monotonic-seconds -1.0 t)
                     (ntop.shared.domain:monotonic-seconds :bad nil))
                    "recognizes ~S value ~S"
                    (type value expected)
                    (cl-weave:expect (typep value type) :to-be expected))
                   (cl-weave:it
                    "checks typed sample slots for nil, valid, and invalid values"
                    (cl-weave:expect
                     (constructor-slot-outcomes
                      'ntop.test::make-measurement-fixture-sample
                      'ntop.test::measurement-fixture-sample-amount
                      '(:sampled-at 0.0)
                      :amount
                      :type-error
                      3
                      "bad")
                     :to-equal
                     '(0 :type-error 3 :type-error)))
                   (cl-weave:it-each ((0.0) (1.5) (-2.0))
                                     "constructs shared records at timestamp ~S"
                                     (timestamp)
                                     (cl-weave:expect
                                      (ntop.shared.domain:sample-record-sampled-at
                                       (ntop.shared.domain:make-sample-record
                                        :sampled-at
                                        timestamp))
                                      :to-be
                                      timestamp))
                   (cl-weave:it-each (("bad"))
                                     "rejects non-real record timestamp ~S"
                                     (bad-timestamp)
                                     (cl-weave:expect
                                      (handler-case (progn
                                                      (apply
                                                       (symbol-function
                                                        'ntop.shared.domain:make-sample-record)
                                                       (list :sampled-at
                                                             bad-timestamp))
                                                      :accepted)
                                        (type-error ()
                                          :type-error))
                                      :to-be
                                      :type-error)))

(cl-weave:it "requires a sample timestamp before construction"
             (cl-weave:signals simple-error
                               (funcall
                                (symbol-function
                                 (intern "MAKE-MEASUREMENT-FIXTURE-SAMPLE"
                                         "NTOP.TEST"))
                                :amount
                                3)))

(cl-weave:it "does not generate sample or value type slot writers"
             (cl-weave:expect
              (list
               (fboundp
                '(setf ntop.shared.domain:sample-record-sampled-at))
               (fboundp
                '(setf ntop.test::measurement-fixture-sample-amount))
               (fboundp
                '(setf ntop.test::measurement-fixture-value-amount)))
              :to-equal
              '(nil nil nil)))

(cl-weave:it "loads instrumented shared measurement definitions"
             (uiop:with-temporary-file (:pathname fasl :type "fasl")
                                       (multiple-value-bind (output warnings
                                                                    failure)
                                           (compile-file
                                            (asdf:system-relative-pathname
                                             "ntop"
                                             "src/shared/domain-measurement.lisp")
                                            :output-file
                                            fasl)
                                         (declare (ignore warnings))
                                         (when failure
                                           (error
                                            "Could not compile shared measurement definitions."))
                                         (load output)))
             (cl-weave:expect
              (typep 10 (quote ntop.shared.domain:measurement-counter))
              :to-be
              t))

(cl-weave:it "requires a timestamp for the shared sample record"
             (cl-weave:signals simple-error
                               (funcall
                                (symbol-function
                                 (quote ntop.shared.domain:make-sample-record)))))
