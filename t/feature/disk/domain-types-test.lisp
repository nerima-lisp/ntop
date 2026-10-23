(in-package #:ntop.test)

(cl-weave:describe "disk domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type disk-mount
                (device-name nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type disk-sample
                (mounts nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a sample with a mount and device"
    (let* ((mount (ntop.disk.domain:make-disk-mount
                   :device-name "/dev/disk1"
                   :mount-path "/"
                   :free-bytes 2048
                   :io-key "disk1"))
           (device (ntop.disk.domain:make-disk-device
                    :device-name "/dev/disk1"
                    :read-bytes 4096))
           (sample (ntop.disk.domain:make-disk-sample
                    :sampled-at 24.0
                    :mounts (list mount)
                    :devices (list device))))
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be 24.0)
      (cl-weave:expect
       (ntop.disk.domain:disk-mount-free-bytes
        (first (ntop.disk.domain:disk-sample-mounts sample)))
       :to-be 2048)
      (cl-weave:expect (ntop.disk.domain:disk-device-read-bytes device)
                       :to-be 4096)))
  (cl-weave:it-each
    ((nil nil) (0 0))
    "preserves ~S as optional disk space"
    (value expected)
    (let ((mount (ntop.disk.domain:make-disk-mount :free-bytes value)))
      (cl-weave:expect (ntop.disk.domain:disk-mount-free-bytes mount)
                       :to-be expected))))
