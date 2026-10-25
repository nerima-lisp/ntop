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
                                  (cl-weave:expect value :to-be nil)
                                  (cl-weave:expect status :to-equal 1)))
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
                                    (cl-weave:expect status :to-equal 0))))
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
                   (cl-weave:it
                    "passes platform collectors and composition callbacks"
                    (let* ((calls 0)
                           (clock-calls 0)
                           (clock (list :clock))
                           (config
                            (ntop.shared.application:make-application-config))
                           (ntop.cli::*application-runner*
                            (lambda (passed-config collectors passed-clock
                                     renderer
                                     &key on-action on-snapshot
                                       history-stores)
                              (cl-weave:expect passed-config :to-be config)
                              (cl-weave:expect passed-clock :to-be clock)
                              (cl-weave:expect
                               (every
                                (lambda (key) (getf collectors key))
                                (quote (:cpu :memory :gpu :network :disk
                                             :temperature :process)))
                               :to-be
                               t)
                              (cl-weave:expect (getf collectors :battery)
                                               :to-equal
                                               nil)
                              (cl-weave:expect (functionp renderer)
                                               :to-be
                                               t)
                              (cl-weave:expect (functionp on-action)
                                               :to-be
                                               t)
                              (cl-weave:expect (functionp on-snapshot)
                                               :to-be
                                               t)
                              (cl-weave:expect
                               (and (arrayp history-stores)
                                    (adjustable-array-p history-stores))
                               :to-be
                               t)
                              (incf calls))))
                      (cl-weave:with-mocked-functions
                       (((symbol-function
                          (quote ntop.app.infrastructure:make-system-clock))
                         (lambda ()
                           (incf clock-calls)
                           clock)))
                       (cl-weave:expect
                        (ntop.cli::%run-application config)
                        :to-equal
                        0))
                      (cl-weave:expect calls :to-equal 1)
                      (cl-weave:expect clock-calls :to-equal 1)))
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
(progn
  (defun cli-config-location-test-with-umask (mask thunk)
    (let ((previous (sb-posix:umask mask)))
      (unwind-protect (funcall thunk)
        (sb-posix:umask previous))))

  (defun cli-config-location-test-write (path content)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :error
                            :if-does-not-exist :create)
      (write-string content stream)))

  (progn
    (defun cli-config-location-test-bytes (path)
      (with-open-file (stream path
                              :direction :input
                              :element-type '(unsigned-byte 8))
        (let ((bytes (make-array (file-length stream)
                                 :element-type '(unsigned-byte 8))))
          (read-sequence bytes stream)
          (coerce bytes 'list))))

    (defun cli-config-location-test-octets (text)
      (coerce (sb-ext:string-to-octets text :external-format :utf-8)
              'list)))

  (defun cli-config-location-test-mode (path)
    (logand (sb-posix:stat-mode (sb-posix:stat (namestring path))) #o777))

  (cl-weave:describe
   "CLI config location creation"
   (cl-weave:it
    "creates the default config beneath an existing parent with umask permissions"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let ((path (merge-pathnames "config.toml" directory)))
         (cli-config-location-test-with-umask
          #o027
          (lambda ()
            (multiple-value-bind (status calls config)
                (cli-test-captured-run
                 (list "--config_location" (namestring path)))
              (cl-weave:expect status :to-equal 0)
              (cl-weave:expect calls :to-equal 1)
              (cl-weave:expect (not (null config)) :to-be t)
              (cl-weave:expect
               (ntop.shared.application:application-config-rate-ms config)
               :to-equal 700)
              (cl-weave:expect
               (cli-config-location-test-bytes path)
               :to-equal
               (cli-config-location-test-octets ntop.cli::*default-config-text*))
              (cl-weave:expect
               (length ntop.cli::*default-config-text*)
               :to-equal 15946)
              (cl-weave:expect (cli-config-location-test-mode path)
                               :to-equal #o640))))))
     :prefix "ntop-config-location-"))
   (cl-weave:it
    "creates missing parent directories with umask permissions"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let* ((outer (merge-pathnames "nested/" directory))
              (parent (merge-pathnames "child/" outer))
              (path (merge-pathnames "config.toml" parent)))
         (cli-config-location-test-with-umask
          #o027
          (lambda ()
            (multiple-value-bind (status calls config)
                (cli-test-captured-run
                 (list "--config_location" (namestring path)))
              (cl-weave:expect status :to-equal 0)
              (cl-weave:expect calls :to-equal 1)
              (cl-weave:expect (not (null config)) :to-be t)
              (cl-weave:expect (not (null (probe-file outer))) :to-be t)
              (cl-weave:expect (not (null (probe-file parent))) :to-be t)
              (cl-weave:expect (cli-config-location-test-mode outer)
                               :to-equal #o750)
              (cl-weave:expect (cli-config-location-test-mode parent)
                               :to-equal #o750)
              (cl-weave:expect
               (cli-config-location-test-bytes path)
               :to-equal
               (cli-config-location-test-octets ntop.cli::*default-config-text*))
              (cl-weave:expect (cli-config-location-test-mode path)
                               :to-equal #o640))))))
     :prefix "ntop-config-location-"))
   (cl-weave:it
    "returns status 1 without running the app for an unreadable and unwritable file"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let ((path (merge-pathnames "existing.toml" directory)))
         (cli-config-location-test-write path "preserve this file")
         (sb-posix:chmod (namestring path) #o000)
         (unwind-protect
              (progn
                (multiple-value-bind (status calls config)
                    (cli-test-captured-run
                     (list "--config_location" (namestring path)))
                  (cl-weave:expect status :to-equal 1)
                  (cl-weave:expect calls :to-equal 0)
                  (cl-weave:expect config :to-be nil))
                (cl-weave:expect (cli-config-location-test-mode path)
                                 :to-equal #o000)
                (sb-posix:chmod (namestring path) #o600)
                (cl-weave:expect
                 (cli-config-location-test-bytes path)
                 :to-equal (cli-config-location-test-octets "preserve this file")))
           (sb-posix:chmod (namestring path) #o600))))
     :prefix "ntop-config-location-"))
   (cl-weave:it
    "runs the app with defaults while preserving an unreadable writable file"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let ((path (merge-pathnames "existing.toml" directory)))
         (cli-config-location-test-write path "preserve this file")
         (sb-posix:chmod (namestring path) #o200)
         (unwind-protect
              (progn
                (multiple-value-bind (status calls config)
                    (cli-test-captured-run
                     (list "--config_location" (namestring path)))
                  (cl-weave:expect status :to-equal 0)
                  (cl-weave:expect calls :to-equal 1)
                  (cl-weave:expect (not (null config)) :to-be t)
                  (cl-weave:expect
                   (ntop.shared.application:application-config-rate-ms config)
                   :to-equal 700))
                (cl-weave:expect (cli-config-location-test-mode path)
                                 :to-equal #o200)
                (sb-posix:chmod (namestring path) #o600)
                (cl-weave:expect
                 (cli-config-location-test-bytes path)
                 :to-equal (cli-config-location-test-octets "preserve this file")))
           (sb-posix:chmod (namestring path) #o600))))
     :prefix "ntop-config-location-"))
   (cl-weave:it
    "rejects a final symlink without changing its target"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let ((target (merge-pathnames "target.toml" directory))
             (link (merge-pathnames "config.toml" directory)))
         (cli-config-location-test-write target "preserve this target")
         (sb-posix:symlink (namestring target) (namestring link))
         (multiple-value-bind (status calls config)
             (cli-test-captured-run
              (list "--config_location" (namestring link)))
           (cl-weave:expect status :to-equal 1)
           (cl-weave:expect calls :to-equal 0)
           (cl-weave:expect config :to-be nil))
         (cl-weave:expect
          (cli-config-location-test-bytes target)
          :to-equal (cli-config-location-test-octets "preserve this target"))))
     :prefix "ntop-config-location-"))
   (cl-weave:it
    "does not create a config file when CLI values conflict"
    (host-kit:call-with-temporary-directory
     (lambda (directory)
       (let ((path (merge-pathnames "config.toml" directory)))
         (multiple-value-bind (status calls config)
             (cli-test-captured-run
              (list "--config_location" (namestring path)
                    "--celsius" "--kelvin"))
           (cl-weave:expect status :to-equal 2)
           (cl-weave:expect calls :to-equal 0)
           (cl-weave:expect config :to-be nil)
           (cl-weave:expect (probe-file path) :to-be nil))))
     :prefix "ntop-config-location-"))))
