(in-package #:ntop.test)

(defun %network-sysfs-fixture-root (fixture)
  (asdf:system-relative-pathname
   "ntop"
   (format nil "t/platform/linux/fixtures/sysfs-network/~A/" fixture)))

(defun %read-linux-network-fixture (fixture)
  (let ((result nil))
    (ntop.platform.linux:with-linux-root (%network-sysfs-fixture-root fixture)
      (ntop.network.infrastructure::read-linux-network-interface-data
       (lambda (value)
         (setf result (list :value value)))
       (lambda (reason)
         (setf result (list :unavailable reason)))))
    result))

(defclass network-fixture-clock ()
  ((unavailable-reason :initarg :unavailable-reason
                       :initform nil
                       :reader network-fixture-clock-unavailable-reason)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock network-fixture-clock) on-time on-unavailable)
  (let ((reason (network-fixture-clock-unavailable-reason clock)))
    (if reason
        (funcall on-unavailable reason)
        (funcall on-time 21.0))))

(defclass linux-fixture-network-collector
    (ntop.network.infrastructure:network-collector)
  ())

(defmethod ntop.network.infrastructure:read-network-interface-data
    ((collector linux-fixture-network-collector) on-value on-unavailable)
  (declare (ignore collector))
  (ntop.network.infrastructure::read-linux-network-interface-data
   on-value on-unavailable))

(defun %collect-linux-network-fixture (fixture &optional unavailable-reason)
  (let ((result nil)
        (collector
          (make-instance (quote linux-fixture-network-collector)
                         :clock
                         (make-instance (quote network-fixture-clock)
                                        :unavailable-reason unavailable-reason))))
    (ntop.platform.linux:with-linux-root (%network-sysfs-fixture-root fixture)
      (ntop.shared.application:collect-network-sample
       collector
       (lambda (sample)
         (setf result (list :value sample)))
       (lambda (reason)
         (setf result (list :unavailable reason)))))
    result))

(cl-weave:describe
 "Linux network infrastructure fixtures"
 (cl-weave:it "reads all interfaces, including loopback, through the Linux reader"
   (cl-weave:expect (%read-linux-network-fixture "complete")
                    :to-equal
                    (quote
                     (:value
                      ((:name "eth0"
                        :rx-bytes 123456789012
                        :tx-bytes 9876543210
                        :rx-packets 234567
                        :tx-packets 765432)
                       (:name "lo"
                        :rx-bytes 0
                        :tx-bytes 0
                        :rx-packets 0
                        :tx-packets 0))))))
 (cl-weave:it "preserves zero and unavailable counters in partial rows"
   (cl-weave:expect (%read-linux-network-fixture "partial")
                    :to-equal
                    (quote
                     (:value
                      ((:name "partial0"
                        :rx-bytes 0
                        :tx-bytes 7
                        :rx-packets nil
                        :tx-packets 2))))))
 (cl-weave:it "maps Linux failures to the shared unavailable reasons"
   (cl-weave:expect (%read-linux-network-fixture "absent")
                    :to-equal
                    (quote (:unavailable :not-present)))
   (cl-weave:expect (%read-linux-network-fixture "invalid-network")
                    :to-equal
                    (quote (:unavailable :read-failed)))
   (let ((cases
           (quote ((:unavailable :read-failed)
                   (:not-present :not-present)
                   (:disabled :disabled)
                   (:permission-denied :permission-denied)
                   (:unsupported-platform :unsupported-platform)
                   (:private-api-unavailable :private-api-unavailable)
                   (:read-failed :read-failed)
                   (:invalid-data :read-failed)))))
     (dolist (case cases)
       (let ((result nil)
             (source-reason (first case)))
         (cl-weave:with-mocked-functions
           (((symbol-function
              (quote ntop.platform.linux:read-linux-network-statistics))
             (lambda (on-value on-unavailable)
               (declare (ignore on-value))
               (funcall on-unavailable source-reason))))
           (ntop.network.infrastructure::read-linux-network-interface-data
            (lambda (rows)
              (setf result (list :value rows)))
            (lambda (reason)
              (setf result (list :unavailable reason)))))
         (cl-weave:expect result
                          :to-equal
                          (list :unavailable (second case)))))
     (let ((result nil))
       (cl-weave:with-mocked-functions
         (((symbol-function
            (quote ntop.platform.linux:read-linux-network-statistics))
           (lambda (on-value on-unavailable)
             (declare (ignore on-unavailable))
             (funcall on-value nil))))
         (ntop.network.infrastructure::read-linux-network-interface-data
          (lambda (rows)
            (setf result (list :value rows)))
          (lambda (reason)
            (setf result (list :unavailable reason)))))
       (cl-weave:expect result
                        :to-equal
                        (quote (:unavailable :not-present))))))
 (cl-weave:it "passes each declared unavailable reason as one keyword"
   (let* ((reasons
            (quote (:not-present :disabled :permission-denied
                     :unsupported-platform :private-api-unavailable :read-failed)))
          (normalize
            (ntop.network.infrastructure::%network-unavailable-callback
             (lambda (&rest arguments) arguments))))
     (cl-weave:expect
      (mapcar normalize reasons)
      :to-equal
      (mapcar (lambda (reason) (list reason)) reasons))
     (cl-weave:expect (funcall normalize :invalid-data)
                      :to-equal
                      (quote (:read-failed)))))
 (cl-weave:it "constructs collectors and normalizes unavailable reasons"
   (let* ((result (%collect-linux-network-fixture "complete"))
          (sample (second result))
          (interfaces (ntop.network.domain:network-sample-interfaces sample))
          (loopback (second interfaces)))
     (cl-weave:expect (first result) :to-be :value)
     (cl-weave:expect
      (ntop.shared.domain:sample-record-sampled-at sample)
      :to-be 21.0)
     (cl-weave:expect (length interfaces) :to-be 2)
     (cl-weave:expect
      (ntop.network.domain:network-interface-name loopback)
      :to-equal "lo")
     (cl-weave:expect
      (ntop.network.domain:network-interface-rx-bytes loopback)
      :to-be 0)
     (cl-weave:expect
      (%collect-linux-network-fixture "complete" :permission-denied)
      :to-equal
      (quote (:unavailable :permission-denied)))
     (cl-weave:expect
      (%collect-linux-network-fixture "complete" :invalid-data)
      :to-equal
      (quote (:unavailable :read-failed)))
     (let* ((clock (make-instance (quote network-fixture-clock)))
            (collector
              (ntop.network.infrastructure:make-network-collector clock)))
       (cl-weave:expect
        (typep collector (quote ntop.network.infrastructure:network-collector))
        :to-be
        t)
       (cl-weave:expect
        (ntop.network.infrastructure::%network-collector-clock collector)
        :to-be
        clock)))))
