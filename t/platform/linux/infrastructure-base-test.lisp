(in-package #:ntop.test)

(defun capture-linux-base-result (function &rest arguments)
  (let ((value nil)
        (reason nil))
    (apply function
           (append arguments
                   (list
                    (lambda (result)
                      (setf value result))
                    (lambda (failure)
                      (setf reason failure)))))
    (values value reason)))

(cl-weave:describe "Linux root and file parsing"
                   (cl-weave:it "reads proc files from the replaceable root"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (text reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-file
                                      "proc/mounts")
                                   (cl-weave:expect reason :to-be nil)
                                   (cl-weave:expect
                                    (search "/dev/sda1 / ext4" text)
                                    :to-satisfy
                                    #'integerp))))
                   (cl-weave:it "reports an unset root separately"
                                (let ((ntop.platform.linux:*linux-root* nil))
                                  (multiple-value-bind (value reason)
                                      (capture-linux-base-result
                                       #'ntop.platform.linux::read-linux-file
                                       "proc/mounts")
                                    (cl-weave:expect value :to-be nil)
                                    (cl-weave:expect reason
                                                     :to-be
                                                     :root-unavailable))))
                   (cl-weave:it "rejects absolute paths"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (value reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-file
                                      "/etc/passwd")
                                   (cl-weave:expect value :to-be nil)
                                   (cl-weave:expect reason :to-be :invalid-path))))
                   (cl-weave:it "rejects parent traversal"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (value reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-file
                                      "proc/../etc/passwd")
                                   (cl-weave:expect value :to-be nil)
                                   (cl-weave:expect reason :to-be :invalid-path))))
                   (cl-weave:it "sorts directory entries"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (entries reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-directory
                                      "proc")
                                   (cl-weave:expect reason :to-be nil)
                                   (cl-weave:expect entries
                                                    :to-equal
                                                    '("diskstats" "mounts")))))
                   (cl-weave:it "returns names for directory entries"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-process/"
                                 (multiple-value-bind (entries reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-directory
                                      "proc/4242/task")
                                   (cl-weave:expect reason :to-be nil)
                                   (cl-weave:expect entries
                                                    :to-equal
                                                    '("4242" "4260"
                                                             "4270"
                                                             "not-a-pid")))))
                   (cl-weave:it "reports a missing directory as unavailable"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (entries reason)
                                     (capture-linux-base-result
                                      #'ntop.platform.linux::read-linux-directory
                                      "proc/not-present")
                                   (cl-weave:expect entries :to-be nil)
                                   (cl-weave:expect reason :to-be :unavailable))))
                   (cl-weave:it "reads a file up to its configured limit"
                                (with-input-from-string (stream "abc")
                                  (multiple-value-bind (text reason)
                                      (ntop.platform.linux::read-bounded-linux-text
                                       stream
                                       3)
                                    (cl-weave:expect text :to-equal "abc")
                                    (cl-weave:expect reason :to-be nil))))
                   (cl-weave:it "rejects data beyond its configured limit"
                                (with-input-from-string (stream "abcd")
                                  (multiple-value-bind (text reason)
                                      (ntop.platform.linux::read-bounded-linux-text
                                       stream
                                       3)
                                    (cl-weave:expect text :to-be nil)
                                    (cl-weave:expect reason :to-be :too-large))))
                   (cl-weave:it-each (("0" 0) ("-42" -42) ("+19" 19) ("0007" 7))
                                     "parses signed integer token ~S"
                                     (token expected)
                                     (cl-weave:expect
                                      (ntop.platform.linux::parse-linux-integer
                                       token)
                                      :to-be
                                      expected))
                   (cl-weave:it-each
                    (("" nil) ("+" nil)
                     ("-" nil)
                     ("12x" nil)
                     ("١" nil)
                     ("1 2" nil))
                    "rejects invalid integer token ~S"
                    (token expected)
                    (cl-weave:expect
                     (ntop.platform.linux::parse-linux-integer token)
                     :to-be
                     expected))
                   (cl-weave:it-each
                    (("0.0" 0) ("0.25" 1/4)
                     ("-1.5" -3/2)
                     (".75" 3/4)
                     ("-0.125" -1/8))
                    "parses decimal token ~S"
                    (token expected)
                    (cl-weave:expect
                     (ntop.platform.linux::parse-linux-decimal token)
                     :to-be
                     expected))
                   (cl-weave:it-each
                    (("1" nil) ("1." nil)
                     ("1.2.3" nil)
                     ("abc" nil)
                     ("--1.2" nil))
                    "rejects invalid decimal token ~S"
                    (token expected)
                    (cl-weave:expect
                     (ntop.platform.linux::parse-linux-decimal token)
                     :to-be
                     expected))
                   (cl-weave:it-property
                    "integer tokens round trip generated values"
                    ((value
                      (cl-weave:gen-integer :min -1000000000 :max 1000000000)))
                    (cl-weave:expect
                     (ntop.platform.linux::parse-linux-integer
                      (format nil "~D" value))
                     :to-be
                     value)))

(cl-weave:describe "Linux base edge cases"
                   (cl-weave:it "rejects a zero file limit"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (let ((text nil)
                                       (reason nil))
                                   (ntop.platform.linux::read-linux-file
                                    "proc/mounts"
                                    (lambda (result)
                                      (setf text result))
                                    (lambda (failure)
                                      (setf reason failure))
                                    :limit
                                    0)
                                   (cl-weave:expect text :to-be nil)
                                   (cl-weave:expect reason
                                                    :to-be
                                                    :invalid-limit))))
                   (cl-weave:it "reports an unset root for directory reads"
                                (let ((ntop.platform.linux:*linux-root* nil))
                                  (multiple-value-bind (entries reason)
                                      (capture-linux-base-result
                                       (symbol-function
                                        (find-symbol "READ-LINUX-DIRECTORY"
                                                     "NTOP.PLATFORM.LINUX"))
                                       "proc")
                                    (cl-weave:expect entries :to-be nil)
                                    (cl-weave:expect reason
                                                     :to-be
                                                     :root-unavailable))))
                   (cl-weave:it "rejects absolute directory paths"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (entries reason)
                                     (capture-linux-base-result
                                      (symbol-function
                                       (find-symbol "READ-LINUX-DIRECTORY"
                                                    "NTOP.PLATFORM.LINUX"))
                                      "/proc")
                                   (cl-weave:expect entries :to-be nil)
                                   (cl-weave:expect reason :to-be :invalid-path))))
                   (cl-weave:it "rejects an empty decimal token"
                                (cl-weave:expect
                                 (ntop.platform.linux::parse-linux-decimal "")
                                 :to-be
                                 nil)))
