(in-package #:ntop.test)

(cl-weave:describe "disk domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              disk-mount
                              (device-name nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (let ((defstruct-form (second expansion))
                            (constructor-form (third expansion))
                            (struct-spec (second (second expansion))))
                        (cl-weave:expect (car defstruct-form)
                                         :to-be
                                         'defstruct)
                        (cl-weave:expect (second struct-spec)
                                         :to-equal
                                         '(:constructor make-disk-mount-raw))
                        (cl-weave:expect (third struct-spec)
                                         :to-equal
                                         '(:copier nil))
                        (cl-weave:expect (third defstruct-form)
                                         :to-equal
                                         '(device-name nil :read-only t))
                        (cl-weave:expect (car constructor-form)
                                         :to-be
                                         'defun)
                        (cl-weave:expect (second constructor-form)
                                         :to-be
                                         'make-disk-mount)
                        (cl-weave:expect (car (fourth constructor-form))
                                         :to-be
                                         'ntop.shared.domain::validate-typed-slots)
                        (cl-weave:expect (car (fifth constructor-form))
                                         :to-be
                                         'make-disk-mount-raw))))
                   (cl-weave:it "includes the shared record in a sample type"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.shared.domain:define-sample-type
                                          disk-sample
                                          (mounts nil)))))
                                  (cl-weave:expect (car expansion)
                                                   :to-be
                                                   'progn)
                                  (let ((defstruct-form (second expansion))
                                        (constructor-form (third expansion))
                                        (struct-spec (second (second expansion))))
                                    (cl-weave:expect (car defstruct-form)
                                                     :to-be
                                                     'defstruct)
                                    (cl-weave:expect (second struct-spec)
                                                     :to-equal
                                                     '(:include
                                                       ntop.shared.domain:sample-record))
                                    (cl-weave:expect (third struct-spec)
                                                     :to-equal
                                                     '(:constructor make-disk-sample-raw))
                                    (cl-weave:expect (fourth struct-spec)
                                                     :to-equal
                                                     '(:copier nil))
                                    (cl-weave:expect (third defstruct-form)
                                                     :to-equal
                                                     '(mounts nil :read-only t))
                                    (cl-weave:expect (car constructor-form)
                                                     :to-be
                                                     'defun)
                                    (cl-weave:expect (second constructor-form)
                                                     :to-be
                                                     'make-disk-sample)
                                    (cl-weave:expect (car (fourth constructor-form))
                                                     :to-be
                                                     'ntop.shared.domain::validate-typed-slots)
                                    (cl-weave:expect (car (fifth constructor-form))
                                                     :to-be
                                                     'make-disk-sample-raw))))
                   (cl-weave:it "constructs a sample with a mount and device"
                                (let* ((mount
                                        (ntop.disk.domain:make-disk-mount
                                         :device-name
                                         "/dev/disk1"
                                         :mount-path
                                         "/"
                                         :free-bytes
                                         2048
                                         :io-key
                                         "disk1"))
                                       (device
                                        (ntop.disk.domain:make-disk-device
                                         :device-name
                                         "/dev/disk1"
                                         :read-bytes
                                         4096))
                                       (sample
                                        (ntop.disk.domain:make-disk-sample
                                         :sampled-at
                                         24.0
                                         :mounts
                                         (list mount)
                                         :devices
                                         (list device))))
                                  (cl-weave:expect
                                   (ntop.shared.domain:sample-record-sampled-at
                                    sample)
                                   :to-be
                                   24.0)
                                  (cl-weave:expect
                                   (ntop.disk.domain:disk-mount-free-bytes
                                    (first
                                     (ntop.disk.domain:disk-sample-mounts
                                      sample)))
                                   :to-be
                                   2048)
                                  (cl-weave:expect
                                   (ntop.disk.domain:disk-device-read-bytes
                                    device)
                                   :to-be
                                   4096)))
                   (cl-weave:it-each ((nil nil) (0 0))
                                     "preserves ~S as optional disk space"
                                     (value expected)
                                     (let ((mount
                                            (ntop.disk.domain:make-disk-mount
                                             :free-bytes
                                             value)))
                                       (cl-weave:expect
                                        (ntop.disk.domain:disk-mount-free-bytes
                                         mount)
                                        :to-be
                                        expected)))
                   (cl-weave:it-each
                    ((ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-device-name
                      nil
                      :device-name
                      nil
                      nil
                      "/dev/disk1"
                      :invalid)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-mount-path
                      nil
                      :mount-path
                      nil
                      nil
                      "/"
                      :invalid)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-filesystem-type
                      nil
                      :filesystem-type
                      nil
                      nil
                      "apfs"
                      :invalid)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-used-bytes
                      nil
                      :used-bytes
                      nil
                      nil
                      100
                      -1)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-free-bytes
                      nil
                      :free-bytes
                      nil
                      nil
                      100
                      -1)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-total-bytes
                      nil
                      :total-bytes
                      nil
                      nil
                      100
                      -1)
                     (ntop.disk.domain:make-disk-mount
                      ntop.disk.domain:disk-mount-io-key
                      nil
                      :io-key
                      nil
                      nil
                      "disk1"
                      :invalid)
                     (ntop.disk.domain:make-disk-device
                      ntop.disk.domain:disk-device-device-name
                      nil
                      :device-name
                      nil
                      nil
                      "/dev/disk1"
                      :invalid)
                     (ntop.disk.domain:make-disk-device
                      ntop.disk.domain:disk-device-read-bytes
                      nil
                      :read-bytes
                      nil
                      nil
                      100
                      -1)
                     (ntop.disk.domain:make-disk-device
                      ntop.disk.domain:disk-device-write-bytes
                      nil
                      :write-bytes
                      nil
                      nil
                      100
                      -1)
                     (ntop.disk.domain:make-disk-sample
                      ntop.disk.domain:disk-sample-mounts
                      (:sampled-at 0.0)
                      :mounts
                      nil
                      nil
                      (:mount)
                      :invalid)
                     (ntop.disk.domain:make-disk-sample
                      ntop.disk.domain:disk-sample-devices
                      (:sampled-at 0.0)
                      :devices
                      nil
                      nil
                      (:device)
                      :invalid))
                    "validates typed disk slot ~S"
                    (constructor accessor
                                 required-initargs
                                 initarg
                                 default-value
                                 nil-value
                                 accepted-value
                                 invalid-value)
                    (cl-weave:expect
                     (constructor-slot-outcomes constructor
                                                accessor
                                                required-initargs
                                                initarg
                                                nil-value
                                                accepted-value
                                                invalid-value)
                     :to-equal
                     (list default-value nil-value accepted-value :type-error))))
