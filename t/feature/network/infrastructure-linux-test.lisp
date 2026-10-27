(in-package #:ntop.test)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load
   (merge-pathnames
    "helpers-linux-infrastructure.lisp"
    (uiop:pathname-directory-pathname
     (asdf:component-pathname
      (asdf:find-component "ntop/test"
                           "t/feature/network/infrastructure-linux-test"))))))

(cl-weave:describe
 "Linux network infrastructure fixtures"
 (cl-weave:it "reads all interfaces, including loopback, through the Linux reader"
   (cl-weave:expect (%read-linux-network-feature-fixture "complete")
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
   (cl-weave:expect (%read-linux-network-feature-fixture "partial")
                    :to-equal
                    (quote
                     (:value
                      ((:name "partial0"
                        :rx-bytes 0
                        :tx-bytes 7
                        :rx-packets nil
                        :tx-packets 2))))))
 (cl-weave:it "maps Linux failures to the shared unavailable reasons"
  (cl-weave:expect
   (list (%read-linux-network-feature-fixture "absent")
         (%read-linux-network-feature-fixture "invalid-network"))
   :to-equal
   (quote ((:unavailable :not-present)
           (:unavailable :read-failed))))
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
    (cl-weave:expect
     (list (first result)
           (ntop.shared.domain:sample-record-sampled-at sample)
           (length interfaces)
           (ntop.network.domain:network-interface-name loopback)
           (ntop.network.domain:network-interface-rx-bytes loopback))
     :to-equal
     (quote (:value 21.0 2 "lo" 0)))
    (cl-weave:expect
     (list (%collect-linux-network-fixture "complete" :permission-denied)
           (%collect-linux-network-fixture "complete" :invalid-data))
     :to-equal
     (quote ((:unavailable :permission-denied)
             (:unavailable :read-failed))))
    (let* ((clock (make-instance (quote network-fixture-clock)))
           (collector (ntop.network.infrastructure:make-network-collector clock)))
      (cl-weave:expect
       (and (typep collector (quote ntop.network.infrastructure:network-collector))
            (eq (ntop.network.infrastructure::%network-collector-clock collector)
                clock))
       :to-be-truthy))))
 (cl-weave:it
  "dispatches a base collector through the Linux reader when Linux is active"
  (if (member :linux *features*)
      (let ((result nil)
            (collector
              (ntop.network.infrastructure:make-network-collector
               (make-instance (quote network-fixture-clock)))))
        (ntop.platform.linux:with-linux-root (%network-sysfs-fixture-root "complete")
          (ntop.shared.application:collect-network-sample
           collector
           (lambda (sample)
             (setf result (list :value sample)))
           (lambda (reason)
             (setf result (list :unavailable reason)))))
        (cl-weave:expect (first result) :to-be :value)
        (cl-weave:expect
         (ntop.network.domain:network-interface-name
          (first
           (ntop.network.domain:network-sample-interfaces (second result))))
         :to-equal
         "eth0"))
      (cl-weave:expect (member :linux *features*) :to-be nil))))
(cl-weave:describe
   "network infrastructure"
   (cl-weave:it "creates a collector with its clock"
     (let* ((clock (make-instance (quote network-fixture-clock)))
            (collector (ntop.network.infrastructure:make-network-collector clock)))
       (cl-weave:expect
        (typep collector (quote ntop.network.infrastructure:network-collector))
        :to-be
        t)
       (cl-weave:expect
        (ntop.network.infrastructure::%network-collector-clock collector)
        :to-be
        clock)))

   (cl-weave:it "collects Linux interface rows through the shared collector"
  (let* ((result nil)
         (collector
           (make-instance
            'linux-fixture-network-collector
            :clock (make-instance 'network-fixture-clock))))
    (cl-weave:with-mocked-functions
      (((symbol-function
         'ntop.platform.linux:read-linux-network-statistics)
        (lambda (on-value on-unavailable)
          (declare (ignore on-unavailable))
          (funcall
           on-value
           '((:name "eth0"
              :rx-bytes 123456789012
              :tx-bytes 9876543210
              :rx-packets 234567
              :tx-packets 765432)
             (:name "lo"
              :rx-bytes 0
              :tx-bytes 0
              :rx-packets 0
              :tx-packets 0))))))
      (ntop.shared.application:collect-network-sample
       collector
       (lambda (sample) (setf result (list :value sample)))
       (lambda (reason) (setf result (list :unavailable reason)))))
    (let* ((sample (second result))
           (interfaces (ntop.network.domain:network-sample-interfaces sample))
           (interface (first interfaces))
           (loopback (second interfaces)))
      (cl-weave:expect
       (list
        (first result)
        (ntop.shared.domain:sample-record-sampled-at sample)
        (length interfaces)
        (ntop.network.domain:network-interface-name interface)
        (ntop.network.domain:network-interface-rx-bytes interface)
        (ntop.network.domain:network-interface-tx-bytes interface)
        (ntop.network.domain:network-interface-name loopback))
       :to-equal
       '(:value 21.0 2 "eth0" 123456789012 9876543210 "lo")))))

   (cl-weave:it "passes clock unavailability through the collector"
     (let* ((result nil)
            (collector
              (make-instance
               (quote linux-fixture-network-collector)
               :clock
               (make-instance
                (quote network-fixture-clock)
                :unavailable-reason
                :permission-denied))))
       (cl-weave:with-mocked-functions
         (((symbol-function
            (quote ntop.platform.linux:read-linux-network-statistics))
           (lambda (on-value on-unavailable)
             (declare (ignore on-unavailable))
             (funcall on-value (quote ((:name "eth0" :rx-bytes 1 :tx-bytes 2)))))))
         (ntop.shared.application:collect-network-sample
          collector
          (lambda (sample)
            (setf result (list :value sample)))
          (lambda (reason)
            (setf result (list :unavailable reason)))))
       (cl-weave:expect
        result
        :to-equal
        (quote (:unavailable :permission-denied)))))

   (cl-weave:it "normalizes missing rows and Linux reader unavailable reasons"
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
            (setf result (list :unavailable reason))))
         (cl-weave:expect result :to-equal (quote (:unavailable :not-present))))
       (dolist
           (case
            (quote
             ((:unavailable :read-failed)
              (:not-present :not-present)
              (:disabled :disabled)
              (:permission-denied :permission-denied)
              (:unsupported-platform :unsupported-platform)
              (:private-api-unavailable :private-api-unavailable)
              (:read-failed :read-failed)
              (:invalid-data :read-failed))))
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
           (cl-weave:expect
            result
            :to-equal
            (list :unavailable (second case)))))))

   (cl-weave:it "preserves declared collector unavailable reasons and maps unknown ones"
     (let ((normalize
             (ntop.network.infrastructure::%network-unavailable-callback
              (lambda (&rest arguments) arguments))))
       (cl-weave:expect
        (mapcar normalize
                (quote (:not-present :disabled :permission-denied
                         :unsupported-platform :private-api-unavailable :read-failed)))
        :to-equal
        (quote ((:not-present) (:disabled) (:permission-denied)
                (:unsupported-platform) (:private-api-unavailable) (:read-failed))))
       (cl-weave:expect
        (funcall normalize :invalid-data)
        :to-equal
        (quote (:read-failed)))))

   (cl-weave:it "uses the Linux default reader for a base collector on Linux"
     (if (member :linux *features*)
         (let* ((result nil)
                (collector
                  (ntop.network.infrastructure:make-network-collector
                   (make-instance (quote network-fixture-clock)))))
           (cl-weave:with-mocked-functions
             (((symbol-function
                (quote ntop.platform.linux:read-linux-network-statistics))
               (lambda (on-value on-unavailable)
                 (declare (ignore on-unavailable))
                 (funcall
                  on-value
                  (quote
                   ((:name "eth0"
                     :rx-bytes 10
                     :tx-bytes 20
                     :rx-packets 1
                     :tx-packets 2)))))))
             (ntop.shared.application:collect-network-sample
              collector
              (lambda (sample)
                (setf result (list :value sample)))
              (lambda (reason)
                (setf result (list :unavailable reason))))
           (cl-weave:expect (first result) :to-be :value)
           (cl-weave:expect
            (ntop.network.domain:network-interface-name
             (first
              (ntop.network.domain:network-sample-interfaces (second result))))
            :to-equal
            "eth0")))
         (cl-weave:expect (member :linux *features*) :to-be nil)))
   #+darwin
(cl-weave:it
 "delegates interface reads to Darwin and forwards the provider result"
 (let ((collector
         (ntop.network.infrastructure:make-network-collector
          (make-instance 'network-fixture-clock)))
       (result nil))
   (cl-weave:with-mocked-functions
     (((symbol-function 'ntop.platform.darwin:darwin-read-network-interface-data)
       (lambda (on-value on-unavailable)
         (declare (ignore on-unavailable))
         (funcall on-value
                  '((:name "en0" :rx-bytes 11 :tx-bytes 22
                     :rx-packets 3 :tx-packets 4))))))
     (ntop.network.infrastructure:read-network-interface-data
      collector
      (lambda (rows) (setf result (list :value rows)))
      (lambda (reason) (setf result (list :unavailable reason)))))
   (cl-weave:expect
    result
    :to-equal
    '(:value ((:name "en0" :rx-bytes 11 :tx-bytes 22
              :rx-packets 3 :tx-packets 4)))))))
