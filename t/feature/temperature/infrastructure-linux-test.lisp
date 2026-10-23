(in-package #:ntop.test)

(defun %linux-temperature-fixture-root (fixture)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/platform/linux/fixtures/sysfs-device/~A/"
                                         fixture)))

(defun %collect-linux-temperature-fixture (fixture &optional collector)
  (let ((result nil)
        (active-collector
         (or collector
             (ntop.temperature.infrastructure:make-linux-temperature-collector
              :now
              (lambda ()
                10.0d0)))))
    (ntop.platform.linux:with-linux-root
     (%linux-temperature-fixture-root fixture)
     (ntop.shared.application:collect-temperature-sample active-collector
                                                         (lambda (sample)
                                                           (setf result (list
                                                                         :sample
                                                                         sample)))
                                                         (lambda (reason)
                                                           (setf result (list
                                                                         :unavailable
                                                                         reason)))))
    result))

(defun %collect-linux-temperature-without-root ()
  (let ((result nil)
        (collector
         (ntop.temperature.infrastructure:make-linux-temperature-collector :now
                                                                           (lambda
                                                                               ()
                                                                             10.0d0))))
    (let ((ntop.platform.linux:*linux-root* nil))
      (ntop.shared.application:collect-temperature-sample collector
                                                          (lambda (sample)
                                                            (setf result (list
                                                                          :sample
                                                                          sample)))
                                                          (lambda (reason)
                                                            (setf result (list
                                                                          :unavailable
                                                                          reason)))))
    result))

(defun %linux-temperature-sample-rows (sample)
  (mapcar
   (lambda (sensor)
     (list (ntop.temperature.domain:temperature-sensor-name sensor)
           (ntop.temperature.domain:temperature-sensor-temperature-celsius
            sensor)))
   (ntop.temperature.domain:temperature-sample-sensors sample)))

(cl-weave:describe "Linux temperature collector fixtures"
                   (cl-weave:it-each
                    (("complete"
                      (("coretemp: Package id 0" 42) ("coretemp" 0)
                       ("amdgpu: Edge" 58)))
                     ("thermal-only" (("acpitz" -5) ("x86_pkg_temp" 65))))
                    "collects ~A sensors at the collector boundary"
                    (fixture expected-rows)
                    (let* ((result (%collect-linux-temperature-fixture fixture))
                           (sample (second result)))
                      (cl-weave:expect (first result) :to-be :sample)
                      (cl-weave:expect (%linux-temperature-sample-rows sample)
                                       :to-equal
                                       expected-rows)
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       10.0d0)))
                   (cl-weave:it
                    "caches an empty source set until the 60-second refresh boundary"
                    (let* ((sampled-at 10.0d0)
                           (collector
                            (ntop.temperature.infrastructure:make-linux-temperature-collector
                             :now
                             (lambda ()
                               sampled-at))))
                      (cl-weave:expect
                       (%collect-linux-temperature-fixture "empty" collector)
                       :to-equal
                       '(:unavailable :not-present))
                      (setf sampled-at 69.0d0)
                      (cl-weave:expect
                       (%collect-linux-temperature-fixture "complete" collector)
                       :to-equal
                       '(:unavailable :not-present))
                      (setf sampled-at 70.0d0)
                      (let* ((result
                              (%collect-linux-temperature-fixture "complete"
                                                                  collector))
                             (sample (second result)))
                        (cl-weave:expect (first result) :to-be :sample)
                        (cl-weave:expect (%linux-temperature-sample-rows sample)
                                         :to-equal
                                         '(("coretemp: Package id 0" 42)
                                           ("coretemp" 0)
                                           ("amdgpu: Edge" 58))))))
                   (cl-weave:it
                    "reports unsupported platform without a Linux root"
                    (cl-weave:expect (%collect-linux-temperature-without-root)
                                     :to-equal
                                     '(:unavailable :unsupported-platform)))
                   (cl-weave:it
                    "keeps malformed individual readings as nil values"
                    (let* ((result
                            (%collect-linux-temperature-fixture "malformed"))
                           (sample (second result)))
                      (cl-weave:expect (first result) :to-be :sample)
                      (cl-weave:expect (%linux-temperature-sample-rows sample)
                                       :to-equal
                                       '(("fixture-sensor" nil)))))
                   (cl-weave:it
                    "refreshes sources at 60 seconds and rereads values each sample"
                    (let* ((sampled-at 10.0d0)
                           (collector
                            (ntop.temperature.infrastructure:make-linux-temperature-collector
                             :now
                             (lambda ()
                               sampled-at)))
                           (initial
                            (%collect-linux-temperature-fixture "complete"
                                                                collector)))
                      (cl-weave:expect (first initial) :to-be :sample)
                      (cl-weave:expect
                       (%linux-temperature-sample-rows (second initial))
                       :to-equal
                       '(("coretemp: Package id 0" 42) ("coretemp" 0)
                         ("amdgpu: Edge" 58)))
                      (setf sampled-at 69.0d0)
                      (let ((cached
                             (%collect-linux-temperature-fixture "thermal-only"
                                                                 collector)))
                        (cl-weave:expect (first cached) :to-be :sample)
                        (cl-weave:expect
                         (%linux-temperature-sample-rows (second cached))
                         :to-equal
                         '(("coretemp: Package id 0" nil) ("coretemp" nil)
                           ("amdgpu: Edge" nil)))
                        (setf sampled-at 70.0d0)
                        (let ((refreshed
                               (%collect-linux-temperature-fixture
                                "thermal-only"
                                collector)))
                          (cl-weave:expect (first refreshed) :to-be :sample)
                          (cl-weave:expect
                           (%linux-temperature-sample-rows (second refreshed))
                           :to-equal
                           '(("acpitz" -5) ("x86_pkg_temp" 65))))))))
