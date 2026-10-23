(in-package #:ntop.test)

(defun %linux-proc-system-fixture-root (variant)
  (asdf:system-relative-pathname "ntop"
                                 (format nil
                                         "t/platform/linux/fixtures/proc-system/~(~A~)/"
                                         variant)))

(defun %capture-linux-system-reader (root reader)
  (let ((value :unset)
        (reason :unset))
    (ntop.platform.linux::with-linux-root root
                                          (funcall reader
                                                   (lambda (result)
                                                     (setf value result))
                                                   (lambda (unavailable-reason)
                                                     (setf reason unavailable-reason))))
    (values value reason)))

(defun %capture-linux-parser (parser text)
  (let ((value :unset)
        (reason :unset))
    (funcall parser
             text
             (lambda (result)
               (setf value result))
             (lambda (unavailable-reason)
               (setf reason unavailable-reason)))
    (values value reason)))

(cl-weave:describe "Linux proc system parsers"
                   (cl-weave:it
                    "reads CPU rows, boot time, and a real zero from proc/stat"
                    (multiple-value-bind (value reason)
                        (%capture-linux-system-reader
                         (%linux-proc-system-fixture-root :v2)
                         #'ntop.platform.linux::read-linux-proc-stat)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (cdr (assoc "cpu" value :test #'string=))
                                       :to-equal
                                       '(111 7 23 400 5 3 4 2 6 1))
                      (cl-weave:expect
                       (cdr (assoc "btime" value :test #'string=))
                       :to-equal
                       '(1700000000))
                      (cl-weave:expect
                       (cdr (assoc "procs_blocked" value :test #'string=))
                       :to-equal
                       '(0))))
                   (cl-weave:it "rejects malformed proc/stat rows"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-parser
                                     #'ntop.platform.linux::parse-linux-proc-stat
                                     "cpu 1 nope 3\n")
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it
                    "parses load averages and the running process ratio"
                    (multiple-value-bind (value reason)
                        (%capture-linux-system-reader
                         (%linux-proc-system-fixture-root :v2)
                         #'ntop.platform.linux::read-linux-proc-loadavg)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect value
                                       :to-equal
                                       '(:one 5/4
                                              :five
                                              1/2
                                              :fifteen
                                              1/4
                                              :running
                                              2
                                              :total
                                              103
                                              :last-pid
                                              9876))))
                   (cl-weave:it "rejects an incomplete loadavg row"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-parser
                                     #'ntop.platform.linux::parse-linux-proc-loadavg
                                     "0.50 0.25 0.10 1/4\n")
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it-each
                    (("MemTotal" 16384 "kB") ("MemAvailable" 8192 "kB")
                     ("HugePages_Total" 0 nil))
                    "preserves the ~A value and unit in proc/meminfo"
                    (key amount unit)
                    (multiple-value-bind (value reason)
                        (%capture-linux-system-reader
                         (%linux-proc-system-fixture-root :v2)
                         #'ntop.platform.linux::read-linux-proc-meminfo)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (assoc key value :test #'string=)
                                       :to-equal
                                       (list key amount unit))))
                   (cl-weave:it "rejects a malformed meminfo value"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-parser
                                     #'ntop.platform.linux::parse-linux-proc-meminfo
                                     "MemTotal: many kB\n")
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it
                    "reads cgroup v2 CPU and memory usage and limits"
                    (multiple-value-bind (value reason)
                        (%capture-linux-system-reader
                         (%linux-proc-system-fixture-root :v2)
                         #'ntop.platform.linux::read-linux-cgroup-data)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (getf value :version) :to-be 2)
                      (cl-weave:expect (getf value :cpu-quota) :to-be 150000)
                      (cl-weave:expect (getf value :cpu-period) :to-be 100000)
                      (cl-weave:expect (getf value :cpu-usage) :to-be 125000)
                      (cl-weave:expect (getf value :memory-limit)
                                       :to-be
                                       3221225472)
                      (cl-weave:expect (getf value :memory-usage) :to-be 123456)))
                   (cl-weave:it
                    "reads cgroup v1 CPU accounting and memory values"
                    (multiple-value-bind (value reason)
                        (%capture-linux-system-reader
                         (%linux-proc-system-fixture-root :v1)
                         #'ntop.platform.linux::read-linux-cgroup-data)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (getf value :version) :to-be 1)
                      (cl-weave:expect (getf value :cpu-quota) :to-be -1)
                      (cl-weave:expect (getf value :cpu-period) :to-be 100000)
                      (cl-weave:expect (getf value :cpu-usage) :to-be 250000000)
                      (cl-weave:expect (getf value :memory-limit)
                                       :to-be
                                       1073741824)
                      (cl-weave:expect (getf value :memory-usage) :to-be 262144)))
                   (cl-weave:it
                    "represents unlimited cgroup v2 limits distinctly from missing data"
                    (multiple-value-bind (value reason)
                        (%capture-linux-parser
                         #'ntop.platform.linux::parse-linux-cgroup-files
                         '(("sys/fs/cgroup/cpu.max" . "max 100000")
                           ("sys/fs/cgroup/memory.max" . "max")))
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (getf value :cpu-quota)
                                       :to-be
                                       :unlimited)
                      (cl-weave:expect (getf value :memory-limit)
                                       :to-be
                                       :unlimited)
                      (cl-weave:expect (getf value :memory-usage) :to-be-null)))
                   (cl-weave:it "reports no cgroup controller as unavailable"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-parser
                                     #'ntop.platform.linux::parse-linux-cgroup-files
                                     nil)
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :unavailable)))
                   (cl-weave:it-property
                    "generated proc/stat fixtures preserve every generated row"
                    ((cpu-values
                      (cl-weave:gen-list
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       :min-length
                       10
                       :max-length
                       10))
                     (boot-time (cl-weave:gen-integer :min 0 :max 2000000000))
                     (processes (cl-weave:gen-integer :min 0 :max 1000000)))
                    (let* ((text
                            (format nil
                                    "cpu ~{~D~^ ~}~%btime ~D~%processes ~D~%"
                                    cpu-values
                                    boot-time
                                    processes))
                           (expected
                            (list (cons "cpu" cpu-values)
                                  (cons "btime" (list boot-time))
                                  (cons "processes" (list processes)))))
                      (multiple-value-bind (value reason)
                          (%capture-linux-parser
                           #'ntop.platform.linux::parse-linux-proc-stat
                           text)
                        (cl-weave:expect reason :to-be :unset)
                        (cl-weave:expect value :to-equal expected)))))
