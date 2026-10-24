(in-package #:ntop.test)

(defun capture-linux-result (function &rest arguments)
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

(cl-weave:describe "Linux proc mounts and disk statistics"
                   (cl-weave:it "decodes escaped mount path characters"
                                (multiple-value-bind (mounts reason)
                                    (capture-linux-result
                                     #'ntop.platform.linux::parse-proc-mounts
                                     (format nil
                                             "/dev/sdb1 /mnt/Archive\\040Disk ext4 rw 0 2~%"))
                                  (cl-weave:expect reason :to-be nil)
                                  (cl-weave:expect
                                   (getf (first mounts) :mount-point)
                                   :to-equal
                                   "/mnt/Archive Disk")))
                   (cl-weave:it "keeps pseudo filesystem devices absent"
                                (multiple-value-bind (mounts reason)
                                    (capture-linux-result
                                     #'ntop.platform.linux::parse-proc-mounts
                                     (format nil
                                             "none /sys/fs/cgroup cgroup2 rw 0 0~%"))
                                  (cl-weave:expect reason :to-be nil)
                                  (cl-weave:expect (getf (first mounts) :device)
                                                   :to-be
                                                   nil)))
                   (cl-weave:it "reads the mounts fixture"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (multiple-value-bind (mounts reason)
                                     (capture-linux-result
                                      #'ntop.platform.linux:read-proc-mounts)
                                   (cl-weave:expect reason :to-be nil)
                                   (cl-weave:expect (length mounts) :to-be 4)
                                   (cl-weave:expect
                                    (getf (third mounts) :mount-point)
                                    :to-equal
                                    "/mnt/Archive Disk"))))
                   (cl-weave:it "reports a missing proc file as unavailable"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/missing/"
                                 (multiple-value-bind (value reason)
                                     (capture-linux-result
                                      #'ntop.platform.linux:read-proc-mounts)
                                   (cl-weave:expect value :to-be nil)
                                   (cl-weave:expect reason :to-be :not-present))))
                   (cl-weave:it-each
                    (("sda" 5 10 2560 5120) ("sdb" 6 10 3072 5120)
                     ("nvme0n1" 0 0 0 0))
                    "converts ~A sector counters to bytes"
                    (name read-sectors written-sectors read-bytes written-bytes)
                    (let ((entry
                           (ntop.platform.linux::parse-proc-diskstats-line
                            (cond
                              ((string= name "sda")
                               "8 0 sda 123 4 5 6 7 8 10 10 11 12")
                              ((string= name "sdb")
                               "8 16 sdb 1 2 6 4 5 6 10 8 9 10")
                              (t "259 0 nvme0n1 0 0 0 0 0 0 0 0 0 0")))))
                      (cl-weave:expect
                       (list (getf entry :name)
                             (getf entry :read-sectors)
                             (getf entry :written-sectors)
                             (getf entry :read-bytes)
                             (getf entry :written-bytes))
                       :to-equal
                       (list name read-sectors written-sectors read-bytes
                             written-bytes))))
                   (cl-weave:it
                    "keeps zero disk counters distinct from missing data"
                    (let ((entry
                           (ntop.platform.linux::parse-proc-diskstats-line
                            "259 0 nvme0n1 0 0 0 0 0 0 0 0 0 0")))
                      (cl-weave:expect (getf entry :read-bytes) :to-be 0)
                      (cl-weave:expect (getf entry :written-bytes) :to-be 0)))
                   (cl-weave:it "ignores incomplete diskstats rows"
                                (cl-weave:expect
                                 (ntop.platform.linux::parse-proc-diskstats-line
                                  "7 0 loop0 1 2")
                                 :to-be
                                 nil))
                   (cl-weave:it
                    "reads only valid diskstats rows from the fixture"
                    (ntop.platform.linux:with-linux-root
                     #p"t/platform/linux/fixtures/proc-disk/"
                     (multiple-value-bind (disks reason)
                         (capture-linux-result
                          #'ntop.platform.linux:read-proc-diskstats)
                       (cl-weave:expect reason :to-be nil)
                       (cl-weave:expect (length disks) :to-be 3)
                       (cl-weave:expect (getf (first disks) :written-bytes)
                                        :to-be
                                        5120))))
                   (cl-weave:it
                    "uses the fragment size for filesystem byte totals"
                    (multiple-value-bind (statistics reason)
                        (capture-linux-result
                         #'ntop.platform.linux::parse-linux-statvfs
                         '(:block-size 4096
                                       :fragment-size 1024
                                       :blocks 100
                                       :free-blocks 40
                                       :available-blocks 30
                                       :files 200
                                       :free-files 80
                                       :available-files 60
                                       :name-max 255))
                      (cl-weave:expect
                       (list reason
                             (getf statistics :total-bytes)
                             (getf statistics :free-bytes)
                             (getf statistics :available-bytes))
                       :to-equal
                       '(nil 102400 40960 30720))))
                   (cl-weave:it "preserves zero free blocks"
                                (multiple-value-bind (statistics reason)
                                    (capture-linux-result
                                     #'ntop.platform.linux::parse-linux-statvfs
                                     '(:block-size 4096
                                                   :fragment-size
                                                   4096
                                                   :blocks
                                                   100
                                                   :free-blocks
                                                   0
                                                   :available-blocks
                                                   0
                                                   :files
                                                   200
                                                   :free-files
                                                   0
                                                   :available-files
                                                   0
                                                   :name-max
                                                   255))
                                  (cl-weave:expect reason :to-be nil)
                                  (cl-weave:expect (getf statistics :free-bytes)
                                                   :to-be
                                                   0)
                                  (cl-weave:expect
                                   (getf statistics :available-bytes)
                                   :to-be
                                   0)))
                   (cl-weave:it "reports missing statvfs fields"
                                (multiple-value-bind (value reason)
                                    (capture-linux-result
                                     #'ntop.platform.linux::parse-linux-statvfs
                                     '(:block-size 4096 :fragment-size 4096))
                                  (cl-weave:expect value :to-be nil)
                                  (cl-weave:expect reason :to-be :missing-field)))
                   (cl-weave:it "rejects negative statvfs values"
                                (multiple-value-bind (value reason)
                                    (capture-linux-result
                                     #'ntop.platform.linux::parse-linux-statvfs
                                     '(:block-size 4096
                                                   :fragment-size
                                                   4096
                                                   :blocks
                                                   -1
                                                   :free-blocks
                                                   1
                                                   :available-blocks
                                                   1
                                                   :files
                                                   1
                                                   :free-files
                                                   1
                                                   :available-files
                                                   1
                                                   :name-max
                                                   255))
                                  (cl-weave:expect value :to-be nil)
                                  (cl-weave:expect reason :to-be :invalid-data)))
                   (cl-weave:it
                    "maps absolute mount points below the fixture root"
                    (let ((seen-path nil)
                          (values
                           '(:block-size 4096
                                         :fragment-size
                                         4096
                                         :blocks
                                         10
                                         :free-blocks
                                         5
                                         :available-blocks
                                         4
                                         :files
                                         20
                                         :free-files
                                         10
                                         :available-files
                                         8
                                         :name-max
                                         255)))
                      (let ((ntop.platform.linux::*linux-statvfs-function*
                             (lambda (pathname)
                               (setf seen-path (namestring pathname))
                               values)))
                        (ntop.platform.linux:with-linux-root
                         #p"t/platform/linux/fixtures/proc-disk/"
                         (multiple-value-bind (statistics reason)
                             (capture-linux-result
                              #'ntop.platform.linux:read-linux-statvfs
                              "/mnt/data")
                           (cl-weave:expect reason :to-be nil)
                           (cl-weave:expect (getf statistics :total-bytes)
                                            :to-be
                                            40960)
                           (cl-weave:expect
                            (not (null (search "mnt/data/" seen-path)))
                            :to-be
                            t))))))
                   (cl-weave:it
                    "rejects mount points that escape the fixture root"
                    (ntop.platform.linux:with-linux-root
                     #p"t/platform/linux/fixtures/proc-disk/"
                     (multiple-value-bind (value reason)
                         (capture-linux-result
                          #'ntop.platform.linux:read-linux-statvfs
                          "/../tmp")
                       (cl-weave:expect value :to-be nil)
                       (cl-weave:expect reason :to-be :invalid-path))
                     (cl-weave:expect
                      (ntop.platform.linux::linux-mount-point-pathname
                       (make-pathname :directory '(:absolute :back "tmp")
                                      :name nil
                                      :type nil))
                      :to-be
                      nil)))
                   (cl-weave:it "reports unsupported statvfs on non-Linux hosts"
                                (let ((ntop.platform.linux::*linux-statvfs-function*
                                       nil))
                                  (ntop.platform.linux:with-linux-root
                                   #p"t/platform/linux/fixtures/proc-disk/"
                                   (multiple-value-bind (value reason)
                                       (capture-linux-result
                                        #'ntop.platform.linux:read-linux-statvfs
                                        "/")
                                     (cl-weave:expect value :to-be nil)
                                     (cl-weave:expect reason
                                                      :to-be
                                                      :unsupported)))))
                   (cl-weave:it-property
                    "statvfs byte totals match generated block counts"
                    ((fields
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 1 :max 1048576)
                       (cl-weave:gen-integer :min 0 :max 100000)
                       (cl-weave:gen-integer :min 0 :max 100000)
                       (cl-weave:gen-integer :min 0 :max 100000))))
                    (destructuring-bind (fragment blocks free available) fields
                      (multiple-value-bind (statistics reason)
                          (capture-linux-result
                           #'ntop.platform.linux::parse-linux-statvfs
                           (list :block-size fragment
                                 :fragment-size fragment
                                 :blocks blocks
                                 :free-blocks free
                                 :available-blocks available
                                 :files 1
                                 :free-files 1
                                 :available-files 1
                                 :name-max 255))
                        (cl-weave:expect
                         (list reason
                               (getf statistics :total-bytes)
                               (getf statistics :free-bytes)
                               (getf statistics :available-bytes))
                         :to-equal
                         (list nil
                               (* fragment blocks)
                               (* fragment free)
                               (* fragment available)))))))

(cl-weave:describe "Linux disk edge cases"
                   (cl-weave:it "ignores incomplete mount rows"
                                (multiple-value-bind (mounts reason)
                                    (capture-linux-result
                                     (symbol-function
                                      (find-symbol "PARSE-PROC-MOUNTS"
                                                   "NTOP.PLATFORM.LINUX"))
                                     (format nil "device mount type~%"))
                                  (cl-weave:expect mounts :to-equal nil)
                                  (cl-weave:expect reason :to-be nil)))
                   (cl-weave:it "preserves unknown and incomplete mount escapes"
                                (let* ((unknown
                                        (format nil "disk~C777" (code-char 92)))
                                       (incomplete
                                        (format nil "disk~C" (code-char 92))))
                                  (cl-weave:expect
                                   (ntop.platform.linux::decode-linux-mount-field
                                    unknown)
                                   :to-equal
                                   unknown)
                                  (cl-weave:expect
                                   (ntop.platform.linux::decode-linux-mount-field
                                    incomplete)
                                   :to-equal
                                   incomplete)))
                   (cl-weave:it "rejects wildcard mount points"
                                (ntop.platform.linux:with-linux-root
                                 #p"t/platform/linux/fixtures/proc-disk/"
                                 (cl-weave:expect
                                  (ntop.platform.linux::linux-mount-point-pathname
                                   "/mnt/*")
                                  :to-be
                                  nil)))
                   (cl-weave:it
                    "reports a missing statvfs function result as unavailable"
                    (let ((ntop.platform.linux::*linux-statvfs-function*
                           (lambda (pathname)
                             (declare (ignore pathname))
                             nil)))
                      (ntop.platform.linux:with-linux-root
                       #p"t/platform/linux/fixtures/proc-disk/"
                       (multiple-value-bind (value reason)
                           (capture-linux-result
                            (symbol-function
                             (find-symbol "READ-LINUX-STATVFS"
                                          "NTOP.PLATFORM.LINUX"))
                            "/mnt/data")
                         (cl-weave:expect value :to-be nil)
                         (cl-weave:expect reason :to-be :unavailable)))))
                   (cl-weave:it
                    "reports root unavailability before statvfs access"
                    (let ((ntop.platform.linux:*linux-root* nil))
                      (multiple-value-bind (value reason)
                          (capture-linux-result
                           (symbol-function
                            (find-symbol "READ-LINUX-STATVFS"
                                         "NTOP.PLATFORM.LINUX"))
                           "/mnt/data")
                        (cl-weave:expect value :to-be nil)
                        (cl-weave:expect reason :to-be :root-unavailable)))))
(cl-weave:describe "Linux statvfs foreign interface"
                   (cl-weave:it-run-if
                    (member :linux *features*)
                    "maps statvfs fields and reports a failed foreign call"
                    (let ((expected
                           '(:block-size 4096
                             :fragment-size 2048
                             :blocks 12
                             :free-blocks 5
                             :available-blocks 4
                             :files 10
                             :free-files 8
                             :available-files 7
                             :name-max 255)))
                      (cl-weave:with-mocked-functions
                        (((symbol-function
                           'ntop.platform.linux::%linux-statvfs)
                          (lambda (path buffer)
                            (cl-weave:expect path :to-equal "/mnt/data")
                            (let ((stats (sb-alien:deref buffer)))
                              (setf (sb-alien:slot stats 'ntop.platform.linux::f-bsize) 4096
                                    (sb-alien:slot stats 'ntop.platform.linux::f-frsize) 2048
                                    (sb-alien:slot stats 'ntop.platform.linux::f-blocks) 12
                                    (sb-alien:slot stats 'ntop.platform.linux::f-bfree) 5
                                    (sb-alien:slot stats 'ntop.platform.linux::f-bavail) 4
                                    (sb-alien:slot stats 'ntop.platform.linux::f-files) 10
                                    (sb-alien:slot stats 'ntop.platform.linux::f-ffree) 8
                                    (sb-alien:slot stats 'ntop.platform.linux::f-favail) 7
                                    (sb-alien:slot stats 'ntop.platform.linux::f-namemax) 255))
                            0)))
                        (cl-weave:expect
                         (ntop.platform.linux::call-linux-statvfs
                          #p"/mnt/data")
                         :to-equal
                         expected))
                      (cl-weave:with-mocked-functions
                        (((symbol-function
                           'ntop.platform.linux::%linux-statvfs)
                          (lambda (path buffer)
                            (declare (ignore path buffer))
                            -1)))
                        (cl-weave:expect
                         (ntop.platform.linux::call-linux-statvfs
                          #p"/missing")
                         :to-be
                         nil)))))
