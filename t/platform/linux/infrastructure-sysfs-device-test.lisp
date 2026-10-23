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

(cl-weave:describe "Linux sysfs temperature readers"
                   (cl-weave:it "reads every hwmon input and its optional label"
                                (multiple-value-bind (sensors reason)
                                    (linux-device-reader-result
                                     #'ntop.platform.linux::read-linux-temperatures
                                     "complete")
                                  (cl-weave:expect reason :to-be nil)
                                  (cl-weave:expect
                                   (mapcar
                                    (lambda (sensor)
                                      (list (getf sensor :hwmon)
                                            (getf sensor :sensor-index)
                                            (getf sensor :name)
                                            (getf sensor :label)
                                            (getf sensor
                                                  :temperature-millidegrees)))
                                    sensors)
                                   :to-equal
                                   '(("hwmon0" 1
                                               "coretemp"
                                               "Package id 0"
                                               42000)
                                     ("hwmon0" 2 "coretemp" nil 0)
                                     ("hwmon1" 1 "amdgpu" "edge" 58000)))))
                   (cl-weave:it
                    "uses the hwmon device subdirectory when direct inputs are absent"
                    (multiple-value-bind (sensors reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-temperatures
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect
                       (getf (third sensors) :temperature-millidegrees)
                       :to-be
                       58000)))
                   (cl-weave:it
                    "prefers hwmon over thermal zones to avoid duplicate sensors"
                    (multiple-value-bind (sensors reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-temperatures
                         "complete")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect (length sensors) :to-be 3)
                      (cl-weave:expect
                       (find "ignored-zone"
                             sensors
                             :key
                             (lambda (sensor)
                               (getf sensor :name))
                             :test
                             #'string=)
                       :to-be
                       nil)))
                   (cl-weave:it
                    "falls back to thermal-zone values and preserves negatives"
                    (multiple-value-bind (sensors reason)
                        (linux-device-reader-result
                         #'ntop.platform.linux::read-linux-temperatures
                         "thermal-only")
                      (cl-weave:expect reason :to-be nil)
                      (cl-weave:expect
                       (mapcar
                        (lambda (sensor)
                          (list (getf sensor :name)
                                (getf sensor :sensor-index)
                                (getf sensor :temperature-millidegrees)))
                        sensors)
                       :to-equal
                       '(("acpitz" 0 -5000) ("x86_pkg_temp" 1 65000)))))
                   (cl-weave:it "reports malformed required temperature data"
                                (multiple-value-bind (sensors reason)
                                    (linux-device-reader-result
                                     #'ntop.platform.linux::read-linux-temperatures
                                     "malformed")
                                  (cl-weave:expect sensors :to-be nil)
                                  (cl-weave:expect reason :to-be :invalid-data)))
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
                    "ignores missing required battery and thermal-zone files"
                    (ntop.platform.linux:with-linux-root
                     (linux-device-fixture-root "empty")
                     (multiple-value-bind (battery reason)
                         (ntop.platform.linux::linux-power-supply-battery
                          "BAT0")
                       (cl-weave:expect battery :to-be nil)
                       (cl-weave:expect reason :to-be nil))
                     (multiple-value-bind (sensor reason)
                         (ntop.platform.linux::linux-thermal-zone-temperature
                          "thermal_zone99"
                          99)
                       (cl-weave:expect sensor :to-be nil)
                       (cl-weave:expect reason :to-be nil))))
                   (cl-weave:it
                    "returns no hwmon data when an empty device has no inputs"
                    (ntop.platform.linux:with-linux-root
                     (linux-device-fixture-root "complete")
                     (multiple-value-bind (sensors reason)
                         (ntop.platform.linux::linux-hwmon-device-temperatures
                          "hwmon2")
                       (cl-weave:expect sensors :to-be nil)
                       (cl-weave:expect reason :to-be nil))))
                   (cl-weave:it
                    "reports missing roots through temperature, battery, and GPU readers"
                    (let ((ntop.platform.linux:*linux-root* nil))
                      (dolist
                          (reader
                           (list #'ntop.platform.linux:read-linux-temperatures
                                 #'ntop.platform.linux:read-linux-batteries
                                 #'ntop.platform.linux:read-linux-amd-gpus))
                        (let ((value nil)
                              (reason :unset))
                          (funcall reader
                                   (lambda (result)
                                     (setf value result))
                                   (lambda (unavailable)
                                     (setf reason unavailable)))
                          (cl-weave:expect value :to-be nil)
                          (cl-weave:expect reason :to-be :root-unavailable)))
                      (multiple-value-bind (sensors reason)
                          (ntop.platform.linux::linux-hwmon-device-temperatures
                           "hwmon0")
                        (cl-weave:expect sensors :to-be nil)
                        (cl-weave:expect reason :to-be :root-unavailable))
                      (multiple-value-bind (sensors reason)
                          (ntop.platform.linux::linux-thermal-zone-temperatures)
                        (cl-weave:expect sensors :to-be nil)
                        (cl-weave:expect reason :to-be :root-unavailable)))))
