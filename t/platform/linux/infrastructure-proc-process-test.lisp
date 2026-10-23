(in-package #:ntop.test)

(defun %linux-proc-process-fixture-root ()
  (asdf:system-relative-pathname "ntop"
                                 "t/platform/linux/fixtures/proc-process/"))

(defun %capture-linux-process-reader (root pid reader)
  (let ((value :unset)
        (reason :unset))
    (ntop.platform.linux::with-linux-root root
                                          (funcall reader
                                                   pid
                                                   (lambda (result)
                                                     (setf value result))
                                                   (lambda (unavailable-reason)
                                                     (setf reason unavailable-reason))))
    (values value reason)))

(defun %capture-linux-process-parser (parser text)
  (let ((value :unset)
        (reason :unset))
    (funcall parser
             text
             (lambda (result)
               (setf value result))
             (lambda (unavailable-reason)
               (setf reason unavailable-reason)))
    (values value reason)))

(cl-weave:describe "Linux proc process parsers"
                   (cl-weave:it
                    "parses stat comm text with spaces and nested parentheses"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         4242
                         #'ntop.platform.linux::read-linux-process-stat)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect (getf value :pid) :to-be 4242)
                      (cl-weave:expect (getf value :comm)
                                       :to-equal
                                       "worker (io) thread")
                      (cl-weave:expect (getf value :state) :to-equal "R")
                      (cl-weave:expect (getf value :ppid) :to-be 4000)
                      (cl-weave:expect (getf value :utime) :to-be 120)
                      (cl-weave:expect (getf value :stime) :to-be 34)
                      (cl-weave:expect (getf value :vsize) :to-be 5242880)
                      (cl-weave:expect (getf value :rss) :to-be 12)))
                   (cl-weave:it "rejects a truncated proc stat record"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-process-parser
                                     #'ntop.platform.linux::parse-linux-process-stat
                                     (format nil "12 (short) R 1 2 3~%"))
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it
                    "parses proc status scalar, vector, and unit values"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         4242
                         #'ntop.platform.linux::read-linux-process-status)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect
                       (cdr (assoc "Name" value :test #'string=))
                       :to-equal
                       "worker")
                      (cl-weave:expect (cdr (assoc "Uid" value :test #'string=))
                                       :to-equal
                                       '(1000 1001 1002 1003))
                      (cl-weave:expect
                       (cdr (assoc "VmPeak" value :test #'string=))
                       :to-equal
                       '(5120 "kB"))
                      (cl-weave:expect (assoc "NSpid" value :test #'string=)
                                       :to-be-null)))
                   (cl-weave:it "rejects malformed proc status lines"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-process-parser
                                     #'ntop.platform.linux::parse-linux-process-status
                                     (format nil
                                             "Name: worker~%not a status field~%"))
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it "preserves zero and nonzero proc io counters"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-process-reader
                                     (%linux-proc-process-fixture-root)
                                     4242
                                     #'ntop.platform.linux::read-linux-process-io)
                                  (cl-weave:expect reason :to-be :unset)
                                  (cl-weave:expect
                                   (cdr
                                    (assoc "read_bytes" value :test #'string=))
                                   :to-be
                                   0)
                                  (cl-weave:expect
                                   (cdr
                                    (assoc "write_bytes" value :test #'string=))
                                   :to-be
                                   8192)))
                   (cl-weave:it "rejects an invalid proc io counter"
                                (multiple-value-bind (value reason)
                                    (%capture-linux-process-parser
                                     #'ntop.platform.linux::parse-linux-process-io
                                     "read_bytes: many\n")
                                  (cl-weave:expect value :to-be :unset)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it
                    "splits NUL-delimited cmdline arguments without shell parsing"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         4242
                         #'ntop.platform.linux::read-linux-process-cmdline)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect value
                                       :to-equal
                                       '("/usr/bin/worker" "--label"
                                                           "two words"))))
                   (cl-weave:it
                    "preserves empty arguments and an unterminated final argument"
                    (cl-weave:expect
                     (ntop.platform.linux::split-linux-nul-arguments
                      (format nil "first~C~Cfinal" #\Null #\Null))
                     :to-equal
                     '("first" "" "final")))
                   (cl-weave:it
                    "maps an empty cmdline to an empty argument list"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-parser
                         #'ntop.platform.linux::parse-linux-process-cmdline
                         "")
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect value :to-be-null)))
                   (cl-weave:it
                    "sorts numeric task ids and ignores non-pid entries"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         4242
                         #'ntop.platform.linux::read-linux-process-tasks)
                      (cl-weave:expect reason :to-be :unset)
                      (cl-weave:expect value :to-equal '(4242 4260 4270))))
                   (cl-weave:it
                    "reports absent optional process files through the unavailable continuation"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         5555
                         #'ntop.platform.linux::read-linux-process-io)
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be :not-present)))
                   (cl-weave:it
                    "rejects a nonpositive pid before accessing proc"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         0
                         #'ntop.platform.linux::read-linux-process-stat)
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be :invalid-pid)))
                   (cl-weave:it-property
                    "generated proc stat fixtures preserve all parsed source fields"
                    ((pid (cl-weave:gen-integer :min 1 :max 1000000))
                     (comm
                      (cl-weave:gen-string :min-length
                                           1
                                           :max-length
                                           24
                                           :alphabet
                                           "abc ()"))
                     (numeric-fields
                      (cl-weave:gen-list
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       :min-length
                       49
                       :max-length
                       49)))
                    (let* ((text
                            (format nil
                                    "~D (~A) R ~{~D~^ ~}~%"
                                    pid
                                    comm
                                    numeric-fields))
                           (expected
                            (let ((stat-fields
                                   ntop.platform.linux::+linux-process-stat-fields+))
                              (append (list :pid pid :comm comm :state "R")
                                      (loop for field in (nthcdr 3 stat-fields)
                                            for value in numeric-fields
                                            append (list field value))))))
                      (multiple-value-bind (value reason)
                          (%capture-linux-process-parser
                           #'ntop.platform.linux::parse-linux-process-stat
                           text)
                        (cl-weave:expect reason :to-be :unset)
                        (cl-weave:expect value :to-equal expected)))))

(cl-weave:describe "Linux proc process edge cases"
                   (cl-weave:it
                    "preserves stat fields beyond the kernel field table"
                    (let ((text
                           (format nil
                                   "99 (extra) R ~{~D~^ ~}"
                                   (loop repeat 50
                                         collect 0))))
                      (multiple-value-bind (value reason)
                          (%capture-linux-process-parser
                           (symbol-function
                            (find-symbol "PARSE-LINUX-PROCESS-STAT"
                                         "NTOP.PLATFORM.LINUX"))
                           text)
                        (cl-weave:expect reason :to-be :unset)
                        (cl-weave:expect (getf value :pid) :to-be 99)
                        (cl-weave:expect (getf value :extra-fields)
                                         :to-equal
                                         (list 0)))))
                   (cl-weave:it-each
                    (("bad (comm) R 1 2 3") ("12 (short R 1 2 3")
                     ("12 13 (comm) R 1 2 3")
                     ("12 (comm) RR 1 2 3"))
                    "rejects malformed proc stat header ~A"
                    (text)
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-parser
                         (symbol-function
                          (find-symbol "PARSE-LINUX-PROCESS-STAT"
                                       "NTOP.PLATFORM.LINUX"))
                         text)
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it-each ((": value") ("Name:"))
                                     "rejects proc status fields without keys or values ~A"
                                     (text)
                                     (multiple-value-bind (value reason)
                                         (%capture-linux-process-parser
                                          (symbol-function
                                           (find-symbol
                                            "PARSE-LINUX-PROCESS-STATUS"
                                            "NTOP.PLATFORM.LINUX"))
                                          text)
                                       (cl-weave:expect value :to-be :unset)
                                       (cl-weave:expect reason
                                                        :to-be
                                                        :invalid-data)))
                   (cl-weave:it-each (("read_bytes") ("read_bytes: 1 extra"))
                                     "rejects proc io rows with the wrong field count ~A"
                                     (text)
                                     (multiple-value-bind (value reason)
                                         (%capture-linux-process-parser
                                          (symbol-function
                                           (find-symbol "PARSE-LINUX-PROCESS-IO"
                                                        "NTOP.PLATFORM.LINUX"))
                                          text)
                                       (cl-weave:expect value :to-be :unset)
                                       (cl-weave:expect reason
                                                        :to-be
                                                        :invalid-data)))
                   (cl-weave:it
                    "rejects a non-integer task owner id before reading proc"
                    (multiple-value-bind (value reason)
                        (%capture-linux-process-reader
                         (%linux-proc-process-fixture-root)
                         "4242"
                         (symbol-function
                          (find-symbol "READ-LINUX-PROCESS-TASKS"
                                       "NTOP.PLATFORM.LINUX")))
                      (cl-weave:expect value :to-be :unset)
                      (cl-weave:expect reason :to-be :invalid-pid))))
