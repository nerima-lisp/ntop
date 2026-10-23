(in-package #:ntop.test)

(cl-weave:describe "GPU domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type gpu-device
                (name nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type gpu-sample
                (devices nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a sample with a GPU device and timestamp"
    (let* ((device (ntop.gpu.domain:make-gpu-device
                    :name "GPU 0"
                    :usage-percent 42.0
                    :memory-used-bytes 2048))
           (sample (ntop.gpu.domain:make-gpu-sample
                    :sampled-at 18.0
                    :devices (list device))))
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be 18.0)
      (cl-weave:expect
       (ntop.gpu.domain:gpu-device-usage-percent
        (first (ntop.gpu.domain:gpu-sample-devices sample)))
       :to-be 42.0)
      (cl-weave:expect
       (ntop.gpu.domain:gpu-device-memory-used-bytes device)
       :to-be 2048)))
  (cl-weave:it-each
    ((nil nil) (0.0 0.0))
    "preserves ~S as optional GPU usage"
    (value expected)
    (let ((device (ntop.gpu.domain:make-gpu-device :usage-percent value)))
      (cl-weave:expect
       (ntop.gpu.domain:gpu-device-usage-percent device)
       :to-be expected))))
