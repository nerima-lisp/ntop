(in-package #:ntop.test)

(cl-weave:describe "network domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type network-interface
                (name nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type network-sample
                (interfaces nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a sample with an interface and timestamp"
    (let* ((interface (ntop.network.domain:make-network-interface
                       :name "en0"
                       :rx-bytes 4096
                       :tx-packets 3))
           (sample (ntop.network.domain:make-network-sample
                    :sampled-at 21.0
                    :interfaces (list interface))))
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be 21.0)
      (cl-weave:expect
       (ntop.network.domain:network-interface-rx-bytes interface)
       :to-be 4096)
      (cl-weave:expect
       (ntop.network.domain:network-sample-interfaces sample)
       :to-equal (list interface))))
  (cl-weave:it-each
    ((nil nil) (0 0))
    "preserves ~S as optional received bytes"
    (value expected)
    (let ((interface (ntop.network.domain:make-network-interface
                      :rx-bytes value)))
      (cl-weave:expect
       (ntop.network.domain:network-interface-rx-bytes interface)
       :to-be expected))))
