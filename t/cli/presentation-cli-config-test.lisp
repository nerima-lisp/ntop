(in-package #:ntop.test)

(defun cli-config-test-sort-key (field)
  (cdr
   (assoc field
          '(("CpuPercent" . :cpu-percent) ("MemValue" . :memory)
            ("MemPercent" . :memory)
            ("VirtualMem" . :virtual-memory-bytes)
            ("Pid" . :pid)
            ("Count" . :count)
            ("Name" . :name)
            ("Command" . :command)
            ("ReadPerSecond" . :read-bytes-per-second)
            ("WritePerSecond" . :write-bytes-per-second)
            ("TotalRead" . :total-read-bytes)
            ("TotalWrite" . :total-write-bytes)
            ("State" . :state)
            ("User" . :user)
            ("Time" . :runtime-seconds)
            ("Nice" . :nice)
            ("Priority" . :priority)
            ("GpuMemValue" . :gpu-memory)
            ("GpuMemPercent" . :gpu-memory)
            ("GpuUtilPercent" . :gpu-usage-percent))
          :test
          #'string=)))

(defun cli-config-test-column (column)
  (let* ((field (ntop.app.domain:parity-column-field column))
         (expected (cli-config-test-sort-key field)))
    (cl-weave:expect (not (null expected)) :to-be t)
    (dolist (alias (ntop.app.domain:parity-column-aliases column))
      (let ((invocation (cli-test-parse (list "--process_default_sort" alias))))
        (cl-weave:expect (ntop.cli::%column-sort-key alias invocation) :to-equal expected)
        (multiple-value-bind (config status)
            (ntop.cli::%config-from-invocation invocation)
          (cl-weave:expect status :to-equal 0)
          (cl-weave:expect
           (ntop.shared.application:application-config-process-sort-column config)
           :to-equal
           expected))))))

(cl-weave:describe "CLI configuration conversion"
                   (cl-weave:it "parses every duration unit and rejects malformed input"
                                (dolist (unit ntop.cli::*duration-units*)
                                  (cl-weave:expect
                                   (ntop.cli::%parse-duration-ms
                                    (concatenate 'string "2" (car unit)))
                                   :to-equal
                                   (* 2 (cdr unit))))
                                (dolist
                                    (case '(("  2 SECONDS " 2000) ("1h 2m 3s" 3723000)
                                            ("1m30s" 90000)
                                            ("123" 123)))
                                  (cl-weave:expect
                                   (ntop.cli::%parse-duration-ms (first case))
                                   :to-equal
                                   (second case)))
                                (dolist
                                    (value
                                     '(nil "" "  " "x" "1x" "1s x" "1s 2" "1.5s" "-1s"))
                                  (cl-weave:expect (ntop.cli::%parse-duration-ms value)
                                                   :to-be
                                                   nil)))
                   (cl-weave:it "maps every process sort column alias"
                                (dolist
                                    (column
                                     (remove-if-not
                                      (lambda (entry)
                                        (eq (ntop.app.domain:parity-entry-kind entry)
                                            :column))
                                      (ntop.app.domain:btm-parity-catalog)))
                                  (cli-config-test-column column))
                                (let ((invocation
                                       (cli-test-parse
                                        '("--process_default_sort" "memory"
                                                                   "--process_memory_as_value"))))
                                  (cl-weave:expect
                                   (ntop.cli::%column-sort-key "memory" invocation)
                                   :to-equal
                                   :memory))
                                (cl-weave:expect
                                 (not
                                  (ntop.cli::%provided-name-p (cli-test-parse nil)
                                                              "not-a-flag"))
                                 :to-be
                                 t)
                                (multiple-value-bind (key value status)
                                    (ntop.cli::%config-binding "process_default_sort"
                                                               "unknown"
                                                               (cli-test-parse nil))
                                  (cl-weave:expect key :to-equal :process-sort-column)
                                  (cl-weave:expect value :to-equal :cpu-percent)
                                  (cl-weave:expect status :to-equal 0)))
                   (cl-weave:it "maps every widget type and CPU entry value"
                                (dolist
                                    (case '(("cpu" :cpu) ("mem" :memory)
                                            ("memory" :memory)
                                            ("net" :network)
                                            ("network" :network)
                                            ("proc" :process)
                                            ("process" :process)
                                            ("processes" :process)
                                            ("temp" :temperature)
                                            ("temperature" :temperature)
                                            ("disk" :disk)
                                            ("batt" :battery)
                                            ("battery" :battery)))
                                  (cl-weave:expect
                                   (ntop.cli::%widget-type-value (first case))
                                   :to-equal
                                   (second case)))
                                (cl-weave:expect (ntop.cli::%widget-type-value "MEMORY")
                                                 :to-equal
                                                 :memory)
                                (cl-weave:expect (ntop.cli::%widget-type-value "unknown")
                                                 :to-be
                                                 nil)
                                (dolist
                                    (case '(("all" :all) ("avg" :average)
                                            ("average" :average)))
                                  (multiple-value-bind (key value status)
                                      (ntop.cli::%config-binding "default_cpu_entry"
                                                                 (first case)
                                                                 (cli-test-parse nil))
                                    (cl-weave:expect key :to-equal :default-cpu-entry)
                                    (cl-weave:expect value :to-equal (second case))
                                    (cl-weave:expect status :to-equal 0))))
                   (cl-weave:it "maps all declared theme and legend values"
                                (dolist (name '("theme" "memory_legend" "network_legend"))
                                  (let ((entry (ntop.cli::%flag-by-name name)))
                                    (dolist
                                        (value (ntop.app.domain:parity-flag-values entry))
                                      (let ((invocation
                                             (cli-test-parse
                                              (list (concatenate 'string "--" name) value))))
                                        (multiple-value-bind (key actual status)
                                            (ntop.cli::%config-binding name
                                                                       value
                                                                       invocation)
                                          (cl-weave:expect status :to-equal 0)
                                          (cl-weave:expect actual
                                                           :to-equal
                                                           (intern (string-upcase value)
                                                                   :keyword))
                                          (cl-weave:expect key
                                                           :to-equal
                                                           (cond
                                                             ((string= name "theme")
                                                              :theme)
                                                             ((string= name
                                                                       "memory_legend")
                                                              :memory-legend-position)
                                                             (t :network-legend-position)))))))
                                  (multiple-value-bind (key value status)
                                      (ntop.cli::%config-binding "config_location"
                                                                 "/tmp/config"
                                                                 (cli-test-parse
                                                                  '("--config_location"
                                                                    "/tmp/config")))
                                    (cl-weave:expect key :to-be nil)
                                    (cl-weave:expect value :to-be nil)
                                    (cl-weave:expect status :to-equal 2))))
                   (cl-weave:it "maps temperature units and rejects conflicting units"
                                (dolist
                                    (case '(("celsius" :celsius)
                                            ("fahrenheit" :fahrenheit)
                                            ("kelvin" :kelvin)))
                                  (let* ((arguments
                                          (list (concatenate 'string "--" (first case))))
                                         (invocation (cli-test-parse arguments)))
                                    (multiple-value-bind (key value status)
                                        (ntop.cli::%config-binding (first case)
                                                                   t
                                                                   invocation)
                                      (cl-weave:expect key :to-equal :temperature-unit)
                                      (cl-weave:expect value :to-equal (second case))
                                      (cl-weave:expect status :to-equal 0))))
                                (cl-weave:expect
                                 (multiple-value-bind (value status)
                                     (ntop.cli::%temperature-value
                                      (cli-test-parse '("--celsius" "--fahrenheit")))
                                   (declare (ignore value))
                                   status)
                                 :to-equal
                                 2))
                   (cl-weave:it "uses the minimal renderer and action handler"
                                (let* ((calls 0)
                                       (config
                                        (ntop.shared.application:make-application-config))
                                       (ntop.cli::*application-runner*
                                        (lambda
                                            (passed-config collectors
                                                           clock
                                                           renderer
                                                           &key
                                                           on-action)
                                          (cl-weave:expect passed-config :to-be config)
                                          (cl-weave:expect collectors :to-be nil)
                                          (cl-weave:expect (not (null clock)) :to-be t)
                                          (cl-weave:expect (functionp renderer) :to-be t)
                                          (cl-weave:expect (functionp on-action) :to-be t)
                                          (cl-weave:expect
                                           (funcall renderer nil config nil 80 24 nil)
                                           :to-be
                                           nil)
                                          (cl-weave:expect (funcall on-action nil nil)
                                                           :to-be
                                                           nil)
                                          (incf calls))))
                                  (cl-weave:expect (ntop.cli::%run-application config)
                                                   :to-equal
                                                   0)
                                  (cl-weave:expect calls :to-equal 1)))
                   (cl-weave:it
                    "returns the process entry status for explicit and default argv"
                    (let ((captured nil))
                      (sb-ext:without-package-locks
                       (let ((original-exit (symbol-function 'sb-ext:exit)))
                         (unwind-protect
                             (labels ((intercept-exit (&key code abort)
                                        (throw 'cli-process-exit
                                          (list code abort)))
                                      (call-main (argv)
                                        (catch 'cli-process-exit
                                          (ntop.cli:main argv)
                                          :did-not-exit)))
                               (setf (symbol-function 'sb-ext:exit) #'intercept-exit)
                               (let ((sb-ext:*posix-argv* '("ntop" "--version")))
                                 (setf captured (list (call-main nil)
                                                      (call-main '("ntop" "--version"))))))
                           (setf (symbol-function 'sb-ext:exit) original-exit))))
                      (cl-weave:expect captured :to-equal '((0 nil) (0 nil))))))
