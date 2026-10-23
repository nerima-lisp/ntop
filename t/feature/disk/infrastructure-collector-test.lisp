(in-package #:ntop.test)

(defclass %disk-fixture-clock ()
  ((time :initarg :time :accessor %disk-fixture-clock-time)
   (failure :initarg :failure :initform nil :reader %disk-fixture-clock-failure)
   (reads :initform 0 :accessor %disk-fixture-clock-reads)))

(defmethod ntop.shared.domain:read-monotonic-time ((clock %disk-fixture-clock)
                                                   on-time
                                                   on-unavailable)
  (incf (%disk-fixture-clock-reads clock))
  (if (%disk-fixture-clock-failure clock)
      (funcall on-unavailable (%disk-fixture-clock-failure clock))
      (funcall on-time (%disk-fixture-clock-time clock))))

(defclass %disk-fixture-collector (ntop.disk.infrastructure:disk-collector)
  ((values :initarg :values :reader %disk-fixture-collector-values)
   (failure :initarg
            :failure
            :initform
            nil
            :reader
            %disk-fixture-collector-failure)))

(defmethod ntop.disk.infrastructure::read-disk-values ((collector
                                                        %disk-fixture-collector)
                                                       on-value
                                                       on-unavailable)
  (if (%disk-fixture-collector-failure collector)
      (funcall on-unavailable (%disk-fixture-collector-failure collector))
      (funcall on-value (%disk-fixture-collector-values collector))))

(defun %collect-disk-fixture (collector)
  (let ((result nil))
    (ntop.shared.application:collect-disk-sample collector
                                                 (lambda (sample)
                                                   (setf result (list :value
                                                                      sample)))
                                                 (lambda (reason)
                                                   (setf result (list
                                                                 :unavailable
                                                                 reason))))
    result))

(cl-weave:describe "Disk infrastructure collector"
                   (cl-weave:it "constructs a collector with its clock"
                                (let* ((clock
                                        (make-instance '%disk-fixture-clock
                                                       :time
                                                       21.0))
                                       (collector
                                        (ntop.disk.infrastructure:make-disk-collector
                                         clock)))
                                  (cl-weave:expect
                                   (ntop.disk.infrastructure::%disk-collector-clock
                                    collector)
                                   :to-be
                                   clock)))
                   (cl-weave:it-each
                    ((:reader-failed :read-failed) (:invalid-data :read-failed)
                     (:permission-denied :permission-denied)
                     (:not-present :not-present)
                     (:disabled :disabled)
                     (:unsupported-platform :unsupported-platform)
                     (:private-api-unavailable :private-api-unavailable)
                     (:read-failed :read-failed)
                     ((:getfsstat-error -1 13) :read-failed))
                    "forwards only the shared unavailable reason without reading the clock"
                    (failure reason)
                    (let* ((clock
                            (make-instance '%disk-fixture-clock :time 21.0))
                           (collector
                            (make-instance '%disk-fixture-collector
                                           :clock
                                           clock
                                           :values
                                           '(:mounts nil :devices nil)
                                           :failure
                                           failure)))
                      (cl-weave:expect (%collect-disk-fixture collector)
                                       :to-equal
                                       (list :unavailable reason))
                      (cl-weave:expect (%disk-fixture-clock-reads clock)
                                       :to-be
                                       0)))
                   (cl-weave:it
                    "creates a timestamped sample with mounts and devices"
                    (let* ((clock
                            (make-instance '%disk-fixture-clock :time 21.0))
                           (mount
                            (ntop.disk.domain:make-disk-mount :device-name
                                                              "/dev/disk1"
                                                              :mount-path
                                                              "/"
                                                              :filesystem-type
                                                              "apfs"))
                           (device
                            (ntop.disk.domain:make-disk-device :device-name
                                                               "disk1"
                                                               :read-bytes
                                                               200
                                                               :write-bytes
                                                               400))
                           (collector
                            (make-instance '%disk-fixture-collector
                                           :clock
                                           clock
                                           :values
                                           (list :mounts
                                                 (list mount)
                                                 :devices
                                                 (list device))))
                           (result (%collect-disk-fixture collector))
                           (sample (second result)))
                      (cl-weave:expect (first result) :to-be :value)
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       21.0)
                      (cl-weave:expect
                       (ntop.disk.domain:disk-sample-mounts sample)
                       :to-equal
                       (list mount))
                      (cl-weave:expect
                       (ntop.disk.domain:disk-sample-devices sample)
                       :to-equal
                       (list device))
                      (cl-weave:expect (%disk-fixture-clock-reads clock)
                                       :to-be
                                       1)))
                   (cl-weave:it "normalizes clock failures to the shared reason"
                                (let* ((clock
                                        (make-instance '%disk-fixture-clock
                                                       :time
                                                       21.0
                                                       :failure
                                                       :clock-failed))
                                       (collector
                                        (make-instance '%disk-fixture-collector
                                                       :clock
                                                       clock
                                                       :values
                                                       '(:mounts nil
                                                                 :devices
                                                                 nil)))
                                       (result
                                        (%collect-disk-fixture collector)))
                                  (cl-weave:expect result
                                                   :to-equal
                                                   '(:unavailable :read-failed))
                                  (cl-weave:expect
                                   (%disk-fixture-clock-reads clock)
                                   :to-be
                                   1))))
