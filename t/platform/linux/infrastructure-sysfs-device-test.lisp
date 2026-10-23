(in-package #:ntop.test)

(defmacro define-linux-device-case-table (suite cases
                                                format-control
                                                bindings
                                                &body
                                                body)
  `(cl-weave:describe ,suite
                      (cl-weave:it-each ,cases ,format-control ,bindings ,@body)))

(defun linux-device-fixture-root (fixture)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/platform/linux/fixtures/sysfs-device/~A/"
                                         fixture)))

(defun linux-device-reader-result (reader fixture)
  (let ((value nil)
        (reason nil))
    (ntop.platform.linux::with-linux-root (linux-device-fixture-root fixture)
                                          (funcall reader
                                                   (lambda (result)
                                                     (setf value result))
                                                   (lambda (unavailable)
                                                     (setf reason unavailable))))
    (values value reason)))

(defun linux-device-temperature-values-result (fixture sources)
  (let ((result nil)
        (reason nil))
    (ntop.platform.linux:with-linux-root (linux-device-fixture-root fixture)
                                         (ntop.platform.linux:read-linux-temperature-values
                                          sources
                                          (lambda (values)
                                            (setf result values))
                                          (lambda (unavailable)
                                            (setf reason unavailable))))
    (values result reason)))

(cl-weave:describe "Linux device fixture test helper"
                   (cl-weave:it "expands the table helper into cl-weave cases"
                                (let ((expansion
                                       (macroexpand-1
                                        '(define-linux-device-case-table
                                          "fixture"
                                          (("capacity" :capacity-percent 73))
                                          "~A"
                                          (file key expected)
                                          (cl-weave:expect key :to-be expected)))))
                                  (cl-weave:expect (first expansion)
                                                   :to-be
                                                   'cl-weave:describe)
                                  (cl-weave:expect (first (third expansion))
                                                   :to-be
                                                   'cl-weave:it-each))))

(cl-weave:describe "Linux temperature source and value APIs"
                   (cl-weave:it
                    "enumerates stable hwmon descriptors and preserves hwmon priority"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect
                       (mapcar
                        (lambda (source)
                          (list (getf source :identity) (getf source :name)))
                        sources)
                       :to-equal
                       '(("hwmon/hwmon0/1" "coretemp: Package id 0")
                         ("hwmon/hwmon0/2" "coretemp")
                         ("hwmon/hwmon1/1" "amdgpu: Edge")))
                      (cl-weave:expect
                       (getf (getf (third sources) :handle) :directory)
                       :to-equal
                       "sys/class/hwmon/hwmon1/device")
                      (cl-weave:expect
                       (getf (getf (third sources) :handle) :filename)
                       :to-equal
                       "temp1_input")))
                   (cl-weave:it
                    "enumerates thermal-zone descriptors and reads negative millidegrees"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "thermal-only")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect
                       (mapcar
                        (lambda (source)
                          (list (getf source :identity) (getf source :name)))
                        sources)
                       :to-equal
                       '(("thermal-zone/thermal_zone0/0" "acpitz")
                         ("thermal-zone/thermal_zone1/1" "x86_pkg_temp")))
                      (multiple-value-bind (values value-reason)
                          (linux-device-temperature-values-result "thermal-only"
                                                                  sources)
                        (cl-weave:expect value-reason :to-be nil)
                        (cl-weave:expect values
                                         :to-equal
                                         '(("thermal-zone/thermal_zone0/0" .
                                                                           -5000)
                                           ("thermal-zone/thermal_zone1/1" .
                                                                           65000))))))
                   (cl-weave:it
                    "reads only supplied handles and returns current values"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (multiple-value-bind (values value-reason)
                          (linux-device-temperature-values-result "complete"
                                                                  sources)
                        (cl-weave:expect value-reason :to-be nil)
                        (cl-weave:expect values
                                         :to-equal
                                         '(("hwmon/hwmon0/1" . 42000)
                                           ("hwmon/hwmon0/2" . 0)
                                           ("hwmon/hwmon1/1" . 58000))))
                      (multiple-value-bind (values value-reason)
                          (linux-device-temperature-values-result "thermal-only"
                                                                  sources)
                        (cl-weave:expect value-reason :to-be nil)
                        (cl-weave:expect values
                                         :to-equal
                                         '(("hwmon/hwmon0/1" . nil)
                                           ("hwmon/hwmon0/2" . nil)
                                           ("hwmon/hwmon1/1" . nil))))))
                   (cl-weave:it
                    "keeps malformed individual readings as nil values"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "malformed")
                      (cl-weave:expect reason :to-be nil)
                      (multiple-value-bind (values value-reason)
                          (linux-device-temperature-values-result "malformed"
                                                                  sources)
                        (cl-weave:expect value-reason :to-be nil)
                        (cl-weave:expect values
                                         :to-equal
                                         '(("hwmon/hwmon0/1" . nil))))))
                   (cl-weave:it
                    "reports an unsupported platform when the Linux root is unavailable"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (let ((values :not-called)
                            (unavailable nil))
                        (let ((ntop.platform.linux:*linux-root* nil))
                          (ntop.platform.linux:read-linux-temperature-values
                           sources
                           (lambda (result)
                             (setf values result))
                           (lambda (reason)
                             (setf unavailable reason))))
                        (cl-weave:expect unavailable
                                         :to-be
                                         :unsupported-platform)
                        (cl-weave:expect values :to-be :not-called))))
                   (cl-weave:it
                    "enumerates an empty source set and reports not-present when reading it"
                    (multiple-value-bind (sources reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux:enumerate-linux-temperature-sources
                         "empty")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect sources :to-equal nil)
                      (multiple-value-bind (values value-reason)
                          (linux-device-temperature-values-result "empty"
                                                                  sources)
                        (cl-weave:expect values :to-be nil)
                        (cl-weave:expect value-reason :to-be :not-present)))))

(define-linux-device-case-table "Linux power_supply integer values"
                                (("capacity" :capacity-percent 73)
                                 ("energy_now" :energy-now-microwatt-hours
                                               43800000)
                                 ("power_now" :power-now-microwatts 14500000)
                                 ("current_now" :current-now-microamps 1200000)
                                 ("voltage_now" :voltage-now-microvolts
                                                12000000))
                                "~A -> ~S is parsed as ~D"
                                (filename key expected)
                                (multiple-value-bind (batteries reason)
                                    (linux-device-reader-result
                                     #'ntop.platform.linux::read-linux-batteries
                                     "complete")
                                  (cl-weave:expect reason :to-be nil)
                                  (cl-weave:expect
                                   (assoc filename
                                          ntop.platform.linux::+linux-power-supply-integer-fields+
                                          :test
                                          #'string=)
                                   :to-equal
                                   (list filename key))
                                  (cl-weave:expect (getf (first batteries) key)
                                                   :to-be
                                                   expected)))

(cl-weave:describe "Linux sysfs battery readers"
                   (cl-weave:it
                    "filters non-battery supplies and absent batteries"
                    (multiple-value-bind (batteries reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-batteries
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect
                       (mapcar
                        (lambda (battery)
                          (getf battery :name))
                        batteries)
                       :to-equal
                       '("fixture-pack" "BAT1"))))
                   (cl-weave:it
                    "preserves zero capacity and omits missing optional fields"
                    (multiple-value-bind (batteries reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-batteries
                         "complete")
                      (let ((minimal (second batteries)))
                        (cl-weave:expect reason :to-be nil)
                        (cl-weave:expect (getf minimal :capacity-percent)
                                         :to-be
                                         0)
                        (cl-weave:expect (member :power-now-microwatts minimal)
                                         :to-be
                                         nil)
                        (cl-weave:expect (getf minimal :status) :to-be nil))))
                   (cl-weave:it
                    "returns an empty list when power_supply is missing"
                    (multiple-value-bind (batteries reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-batteries
                         "empty")
                      (cl-weave:expect batteries :to-equal nil)
                      (cl-weave:expect reason :to-be nil))))

(cl-weave:describe "Linux AMD GPU sysfs readers"
                   (cl-weave:it
                    "reads AMD usage, VRAM bytes, and hwmon temperature"
                    (multiple-value-bind (gpus reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-amd-gpus
                         "complete")
                      (let ((gpu (first gpus)))
                        (cl-weave:expect reason :to-be nil)
                        (cl-weave:expect (getf gpu :pci-address)
                                         :to-equal
                                         "0000:00:01.0")
                        (cl-weave:expect (getf gpu :name)
                                         :to-equal
                                         "Radeon Fixture")
                        (cl-weave:expect (getf gpu :usage-percent) :to-be 41)
                        (cl-weave:expect (getf gpu :vram-used-bytes)
                                         :to-be
                                         2147483648)
                        (cl-weave:expect (getf gpu :vram-total-bytes)
                                         :to-be
                                         8589934592)
                        (cl-weave:expect (getf gpu :temperature-millidegrees)
                                         :to-be
                                         57000))))
                   (cl-weave:it
                    "preserves zero usage and leaves absent optional GPU metrics missing"
                    (multiple-value-bind (gpus reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-amd-gpus
                         "complete")
                      (let ((gpu (second gpus)))
                        (cl-weave:expect reason :to-be nil)
                        (cl-weave:expect (getf gpu :card) :to-equal "card1")
                        (cl-weave:expect (getf gpu :usage-percent) :to-be 0)
                        (cl-weave:expect (getf gpu :vram-used-bytes) :to-be nil)
                        (cl-weave:expect (member :vram-total-bytes gpu)
                                         :to-be
                                         nil))))
                   (cl-weave:it
                    "returns an empty list when the AMD driver directory is missing"
                    (multiple-value-bind (gpus reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-amd-gpus
                         "empty")
                      (cl-weave:expect gpus :to-equal nil)
                      (cl-weave:expect reason :to-be nil))))

(cl-weave:describe "Linux sysfs reader edge cases"
                   (cl-weave:it-each
                    (("hwmon" "hwmon" nil) ("other0" "hwmon" nil)
                     ("hwmonx" "hwmon" nil)
                     ("hwmon-1" "hwmon" nil)
                     ("hwmon0" "hwmon" 0))
                    "parses sysfs index ~S for prefix ~S"
                    (name prefix expected)
                    (cl-weave:expect
                     (ntop.platform.linux::linux-sysfs-name-index name prefix)
                     :to-be
                     expected))
                   (cl-weave:it-each
                    (("temp1" nil) ("xemp1_input" nil)
                     ("temp1-input" nil)
                     ("tempx_input" nil)
                     ("temp-1_input" nil)
                     ("temp0_input" 0))
                    "parses hwmon input index in ~S"
                    (name expected)
                    (cl-weave:expect
                     (ntop.platform.linux::linux-hwmon-input-index name)
                     :to-be
                     expected))
                   (cl-weave:it
                    "distinguishes empty required sysfs text from missing text"
                    (ntop.platform.linux:with-linux-root
                     (linux-device-fixture-root "empty-required")
                     (multiple-value-bind (value reason)
                         (ntop.platform.linux::linux-sysfs-required-text
                          "sys/class/power_supply/BAT-empty/type")
                       (cl-weave:expect value :to-be nil)
                       (cl-weave:expect reason :to-be :invalid-data))
                     (multiple-value-bind (battery reason)
                         (ntop.platform.linux::linux-power-supply-battery
                          "BAT-empty")
                       (cl-weave:expect battery :to-be nil)
                       (cl-weave:expect reason :to-be :invalid-data))))
                   (cl-weave:it
                    "ignores missing required battery files"
                    (ntop.platform.linux:with-linux-root
                     (linux-device-fixture-root "empty")
                     (multiple-value-bind (battery reason)
                         (ntop.platform.linux::linux-power-supply-battery
                          "BAT0")
                       (cl-weave:expect battery :to-be nil)
                       (cl-weave:expect reason :to-be nil))))
                   (cl-weave:it
                    "reports missing roots through battery and GPU readers"
                    (let ((ntop.platform.linux:*linux-root* nil))
                      (dolist
                          (reader
                           (list #'ntop.platform.linux:read-linux-batteries
                                 #'ntop.platform.linux:read-linux-amd-gpus))
                        (let ((value nil)
                              (reason :unset))
                          (funcall reader
                                   (lambda (result)
                                     (setf value result))
                                   (lambda (unavailable)
                                     (setf reason unavailable)))
                          (cl-weave:expect value :to-be nil)
                          (cl-weave:expect reason :to-be :root-unavailable))))))
