(in-package #:ntop.test)

(defclass %battery-fixture-clock ()
  ((time :initarg :time :accessor %battery-fixture-clock-time)
   (failure :initarg :failure :initform nil :reader %battery-fixture-clock-failure)
   (reads :initform 0 :accessor %battery-fixture-clock-reads)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock %battery-fixture-clock) on-time on-unavailable)
  (incf (%battery-fixture-clock-reads clock))
  (if (%battery-fixture-clock-failure clock)
      (funcall on-unavailable (%battery-fixture-clock-failure clock))
      (funcall on-time (%battery-fixture-clock-time clock))))

(defclass %battery-fixture-collector
    (ntop.battery.infrastructure::battery-collector)
  ((devices :initarg :devices :reader %battery-fixture-collector-devices)
   (failure :initarg :failure :initform nil :reader %battery-fixture-collector-failure)
   (additional-failure :initarg :additional-failure :initform nil
                       :reader %battery-fixture-collector-additional-failure)))

(defmethod ntop.battery.infrastructure::read-battery-devices
    ((collector %battery-fixture-collector) on-value on-unavailable)
  (if (%battery-fixture-collector-failure collector)
      (if (%battery-fixture-collector-additional-failure collector)
          (funcall on-unavailable
                   (%battery-fixture-collector-failure collector)
                   (%battery-fixture-collector-additional-failure collector))
          (funcall on-unavailable (%battery-fixture-collector-failure collector)))
      (funcall on-value (%battery-fixture-collector-devices collector))))

(defun %collect-battery-fixture (collector)
  (let ((result nil))
    (ntop.shared.application:collect-battery-sample
     collector
     (lambda (sample)
       (setf result (list :value sample)))
     (lambda (&rest reasons)
       (setf result (cons :unavailable reasons))))
    result))

(defun %expect-battery-unavailable (result expected-reason)
  (cl-weave:expect (first result) :to-be :unavailable)
  (cl-weave:expect (length (rest result)) :to-be 1)
  (cl-weave:expect (second result) :to-be expected-reason)
  (cl-weave:expect
   (and (keywordp (second result))
        (not (null
              (member (second result)
                      '(:not-present :disabled :permission-denied
                        :unsupported-platform :private-api-unavailable
                        :read-failed)
                      :test #'eq))))
   :to-be t))

(cl-weave:describe
 "Battery infrastructure collector"
 (cl-weave:it "constructs a collector with its clock"
   (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
          (collector
            (ntop.battery.infrastructure:make-battery-collector clock)))
     (cl-weave:expect
      (ntop.battery.infrastructure::%battery-collector-clock collector)
      :to-be clock)))
 (cl-weave:it-each
  ((:not-present :not-present)
   (:disabled :disabled)
   (:permission-denied :permission-denied)
   (:unsupported-platform :unsupported-platform)
   (:private-api-unavailable :private-api-unavailable)
   (:read-failed :read-failed)
   (:no-battery :not-present)
   (:unavailable :read-failed)
   (:invalid-data :read-failed)
   (:reader-failed :read-failed)
   (:unexpected :read-failed)
   (17 :read-failed)
   ("unexpected" :read-failed))
  "normalizes reader reasons to one allowed reason"
  (failure reason)
  (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
         (collector
           (make-instance '%battery-fixture-collector
                          :clock clock
                          :devices nil
                          :failure failure)))
    (%expect-battery-unavailable (%collect-battery-fixture collector)
                                 reason)
    (cl-weave:expect (%battery-fixture-clock-reads clock) :to-be 0)))
 (cl-weave:it "reports an empty device list as :not-present without reading the clock"
   (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
          (collector
            (make-instance '%battery-fixture-collector
                           :clock clock
                           :devices nil)))
     (%expect-battery-unavailable
      (%collect-battery-fixture collector)
      :not-present)
     (cl-weave:expect (%battery-fixture-clock-reads clock) :to-be 0)))
 (cl-weave:it "normalizes a reader callback with extra reasons"
  (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
         (collector
           (make-instance '%battery-fixture-collector
                          :clock clock
                          :devices nil
                          :failure :disabled
                          :additional-failure :invalid-data)))
    (%expect-battery-unavailable (%collect-battery-fixture collector)
                                 :read-failed)
    (cl-weave:expect (%battery-fixture-clock-reads clock) :to-be 0)))
 (cl-weave:it "creates a timestamped sample with the reader devices"
   (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
          (device (ntop.battery.domain:make-battery-device :name "A"))
          (collector
            (make-instance '%battery-fixture-collector
                           :clock clock
                           :devices (list device)))
          (result (%collect-battery-fixture collector))
          (sample (second result)))
     (cl-weave:expect (first result) :to-be :value)
     (cl-weave:expect
      (ntop.shared.domain:sample-record-sampled-at sample)
      :to-be 21.0)
     (cl-weave:expect
      (first (ntop.battery.domain:battery-sample-devices sample))
      :to-be device)
     (cl-weave:expect (%battery-fixture-clock-reads clock) :to-be 1)))
 (cl-weave:it "creates a fresh sample for each collection"
   (let* ((clock (make-instance '%battery-fixture-clock :time 21.0))
          (device (ntop.battery.domain:make-battery-device :name "A"))
          (collector
            (make-instance '%battery-fixture-collector
                           :clock clock
                           :devices (list device)))
          (first-sample (second (%collect-battery-fixture collector))))
     (setf (%battery-fixture-clock-time clock) 22.0)
     (let ((second-sample (second (%collect-battery-fixture collector))))
       (cl-weave:expect (not (eq first-sample second-sample)) :to-be t)
       (cl-weave:expect
        (ntop.shared.domain:sample-record-sampled-at second-sample)
        :to-be 22.0))))
 (cl-weave:it-each
  ((:not-present :not-present)
   (:disabled :disabled)
   (:permission-denied :permission-denied)
   (:unsupported-platform :unsupported-platform)
   (:private-api-unavailable :private-api-unavailable)
   (:read-failed :read-failed)
   (:no-battery :not-present)
   (:unavailable :read-failed)
   (:invalid-data :read-failed)
   (:clock-failed :read-failed)
   (:unexpected :read-failed)
   (17 :read-failed)
   ("unexpected" :read-failed))
  "normalizes clock reasons to one allowed reason"
  (failure reason)
  (let* ((clock (make-instance '%battery-fixture-clock
                               :time 21.0
                               :failure failure))
         (device (ntop.battery.domain:make-battery-device :name "A"))
         (collector
           (make-instance '%battery-fixture-collector
                          :clock clock
                          :devices (list device))))
    (%expect-battery-unavailable (%collect-battery-fixture collector)
                                 reason)
    (cl-weave:expect (%battery-fixture-clock-reads clock) :to-be 1))))
