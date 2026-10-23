(in-package #:ntop.test)

(defun %gpu-linux-fixture-root (fixture)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/platform/linux/fixtures/sysfs-device/~A/"
                                         fixture)))

(defun %read-linux-gpu-fixture (fixture &optional
                                        (nvml-loader
                                         (lambda ()
                                           (error
                                            "Injected missing NVML library.")))
                                        (nvml-reader
                                         (lambda ()
                                           (values nil nil))))
  (let ((result nil)
        (ntop.gpu.infrastructure::*nvml-shared-library-loader* nvml-loader)
        (ntop.gpu.infrastructure::*nvml-device-data-reader* nvml-reader))
    (ntop.platform.linux:with-linux-root (%gpu-linux-fixture-root fixture)
                                         (ntop.gpu.infrastructure::read-linux-gpu-device-data
                                          (lambda (rows)
                                            (setf result (list :value rows)))
                                          (lambda (reason)
                                            (setf result (list :unavailable
                                                               reason)))))
    result))

(cl-weave:describe "Linux GPU infrastructure fixtures"
                   (cl-weave:it
                    "converts multiple AMD devices and preserves optional fields"
                    (let* ((result (%read-linux-gpu-fixture "complete"))
                           (rows (second result))
                           (first-gpu (first rows))
                           (second-gpu (second rows)))
                      (cl-weave:expect (first result) :to-be :value)
                      (cl-weave:expect (length rows) :to-be 2)
                      (cl-weave:expect (getf first-gpu :name)
                                       :to-equal
                                       "Radeon Fixture")
                      (cl-weave:expect (getf first-gpu :usage-percent)
                                       :to-be
                                       41)
                      (cl-weave:expect (getf first-gpu :memory-used-bytes)
                                       :to-be
                                       2147483648)
                      (cl-weave:expect (getf first-gpu :memory-total-bytes)
                                       :to-be
                                       8589934592)
                      (cl-weave:expect (getf first-gpu :temperature-celsius)
                                       :to-be
                                       57.0)
                      (cl-weave:expect (getf second-gpu :usage-percent)
                                       :to-be
                                       0)
                      (cl-weave:expect (getf second-gpu :memory-used-bytes)
                                       :to-be
                                       nil)
                      (cl-weave:expect (getf second-gpu :memory-total-bytes)
                                       :to-be
                                       nil)
                      (cl-weave:expect (getf second-gpu :temperature-celsius)
                                       :to-be
                                       nil)))
                   (cl-weave:it
                    "continues with AMD results when loading NVML fails"
                    (let* ((result (%read-linux-gpu-fixture "complete"))
                           (rows (second result)))
                      (cl-weave:expect (first result) :to-be :value)
                      (cl-weave:expect (length rows) :to-be 2)
                      (cl-weave:expect (getf (first rows) :name)
                                       :to-equal
                                       "Radeon Fixture")))
                   (cl-weave:it
                    "reports private API unavailability when the AMD sysfs tree is absent"
                    (cl-weave:expect (%read-linux-gpu-fixture "empty")
                                     :to-equal
                                     '(:unavailable :private-api-unavailable)))
                   (cl-weave:it
                    "combines AMD and injected NVML rows without loading NVML"
                    (let ((result nil)
                          (ntop.gpu.infrastructure::*nvml-shared-library-loader*
                           (lambda ()
                             nil))
                          (ntop.gpu.infrastructure::*nvml-device-data-reader*
                           (lambda ()
                             (values
                              '((:name "NVIDIA Fixture"
                                       :usage-percent
                                       88
                                       :memory-used-bytes
                                       4294967296
                                       :memory-total-bytes
                                       8589934592
                                       :temperature-celsius
                                       60))
                              nil))))
                      (ntop.platform.linux:with-linux-root
                       (%gpu-linux-fixture-root "complete")
                       (ntop.gpu.infrastructure::read-linux-gpu-device-data
                        (lambda (rows)
                          (setf result (list :value rows)))
                        (lambda (reason)
                          (setf result (list :unavailable reason)))))
                      (let ((rows (second result)))
                        (cl-weave:expect (first result) :to-be :value)
                        (cl-weave:expect (length rows) :to-be 3)
                        (cl-weave:expect (getf (third rows) :name)
                                         :to-equal
                                         "NVIDIA Fixture")
                        (cl-weave:expect (getf (third rows) :usage-percent)
                                         :to-be
                                         88)))))

(defclass gpu-fixture-clock ()
  ())

(defmethod ntop.shared.domain:read-monotonic-time ((clock gpu-fixture-clock)
                                                   on-time
                                                   on-unavailable)
  (declare (ignore clock on-unavailable))
  (funcall on-time 21.0))

(defvar *gpu-device-read-count*
  0)

(defclass gpu-fixture-collector (ntop.gpu.infrastructure::gpu-collector)
  ())

(defmethod ntop.gpu.infrastructure::read-gpu-device-data ((collector
                                                           gpu-fixture-collector)
                                                          on-value
                                                          on-unavailable)
  (declare (ignore collector on-unavailable))
  (incf *gpu-device-read-count*)
  (funcall on-value
           '((:name "Radeon Fixture"
                    :usage-percent
                    41
                    :memory-used-bytes
                    2147483648
                    :memory-total-bytes
                    8589934592
                    :temperature-celsius
                    57.0)
             (:name "NVIDIA Fixture"
                    :usage-percent
                    88
                    :memory-used-bytes
                    4294967296
                    :memory-total-bytes
                    8589934592
                    :temperature-celsius
                    60))))

(cl-weave:describe "GPU collection"
                   (cl-weave:it
                    "does not read GPU data when collection is disabled"
                    (let* ((*gpu-device-read-count* 0)
                           (collector
                            (make-instance 'gpu-fixture-collector
                                           :clock
                                           (make-instance 'gpu-fixture-clock)
                                           :config
                                           (ntop.shared.application:make-application-config
                                            :disable-gpu-p
                                            t)))
                           (result nil))
                      (ntop.shared.application:collect-gpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (setf result (list
                                                                                  :value
                                                                                  sample)))
                                                                  (lambda
                                                                      (reason)
                                                                    (setf result (list
                                                                                  :unavailable
                                                                                  reason))))
                      (cl-weave:expect result
                                       :to-equal
                                       '(:unavailable :disabled))
                      (cl-weave:expect *gpu-device-read-count* :to-be 0)))
                   (cl-weave:it
                    "timestamps a GPU sample and applies short names in device order"
                    (let* ((*gpu-device-read-count* 0)
                           (collector
                            (make-instance 'gpu-fixture-collector
                                           :clock
                                           (make-instance 'gpu-fixture-clock)
                                           :config
                                           (ntop.shared.application:make-application-config
                                            :short-gpu-names-p
                                            t)))
                           (result nil))
                      (ntop.shared.application:collect-gpu-sample collector
                                                                  (lambda
                                                                      (sample)
                                                                    (setf result (list
                                                                                  :value
                                                                                  sample)))
                                                                  (lambda
                                                                      (reason)
                                                                    (setf result (list
                                                                                  :unavailable
                                                                                  reason))))
                      (let* ((sample (second result))
                             (devices
                              (ntop.gpu.domain:gpu-sample-devices sample)))
                        (cl-weave:expect (first result) :to-be :value)
                        (cl-weave:expect
                         (ntop.shared.domain:sample-record-sampled-at sample)
                         :to-be
                         21.0)
                        (cl-weave:expect
                         (mapcar #'ntop.gpu.domain:gpu-device-name devices)
                         :to-equal
                         '("GPU0" "GPU1"))
                        (cl-weave:expect *gpu-device-read-count* :to-be 1)))))

(cl-weave:describe "Linux GPU unavailable reasons"
                   (cl-weave:it
                    "reports no GPU when NVML succeeds with no devices"
                    (cl-weave:expect
                     (%read-linux-gpu-fixture "empty"
                                              (lambda ()
                                                nil)
                                              (lambda ()
                                                (values nil nil)))
                     :to-equal
                     '(:unavailable :not-present)))
                   (cl-weave:it "reports NVML query failures as read failures"
                                (cl-weave:expect
                                 (%read-linux-gpu-fixture "empty"
                                                          (lambda ()
                                                            nil)
                                                          (lambda ()
                                                            (values nil
                                                                    :read-failed)))
                                 :to-equal
                                 '(:unavailable :read-failed)))
                   (cl-weave:it
                    "maps Linux GPU failures to the shared reason keywords"
                    (dolist
                        (case '((nil nil :not-present)
                                (nil :not-present :not-present)
                                (:permission-denied nil :permission-denied)
                                (nil :permission-denied :permission-denied)
                                (:root-unavailable nil :unsupported-platform)
                                (nil :unsupported-platform
                                     :unsupported-platform)
                                (:invalid-data nil :read-failed)
                                (nil :private-api-unavailable
                                     :private-api-unavailable)
                                (:not-present
                                 :private-api-unavailable
                                 :private-api-unavailable)
                                (:not-present
                                 :not-present
                                 :not-present)
                                (:not-present
                                 :read-failed
                                 :read-failed)
                                (:not-present
                                 :nvml-unavailable
                                 :read-failed)))
                      (destructuring-bind (amd-reason nvml-reason expected) case
                        (cl-weave:expect
                         (ntop.gpu.infrastructure::%linux-gpu-unavailable-reason
                          amd-reason
                          nvml-reason)
                         :to-be
                         expected)))))

(progn
  (defclass gpu-failure-clock ()
    ())
  (defmethod ntop.shared.domain:read-monotonic-time ((clock gpu-failure-clock)
                                                     on-time
                                                     on-unavailable)
    (declare (ignore clock on-time))
    (funcall on-unavailable
             (make-condition 'simple-error
                             :format-control
                             "Injected clock failure."
                             :format-arguments
                             nil)))
  (cl-weave:describe "GPU collection clock failure"
                     (cl-weave:it
                      "passes a permitted reason when sample time is unavailable"
                      (let* ((*gpu-device-read-count* 0)
                             (collector
                              (make-instance 'gpu-fixture-collector
                                             :clock
                                             (make-instance 'gpu-failure-clock)
                                             :config
                                             (ntop.shared.application:make-application-config)))
                             (result nil))
                        (ntop.shared.application:collect-gpu-sample collector
                                                                    (lambda
                                                                        (sample)
                                                                      (setf result (list
                                                                                    :value
                                                                                    sample)))
                                                                    (lambda
                                                                        (reason)
                                                                      (setf result (list
                                                                                    :unavailable
                                                                                    reason))))
                        (cl-weave:expect result
                                         :to-equal
                                         '(:unavailable :read-failed))
                        (cl-weave:expect *gpu-device-read-count* :to-be 1)))))
