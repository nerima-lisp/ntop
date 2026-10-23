(in-package #:ntop.test)

(cl-weave:describe "memory domain types"
                   (cl-weave:it "includes the shared record in a sample type"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.shared.domain:define-sample-type
                                          memory-sample
                                          (ram-used-bytes nil)))))
                                  (cl-weave:expect (car expansion)
                                                   :to-be
                                                   'defstruct)
                                  (cl-weave:expect (second (second expansion))
                                                   :to-equal
                                                   '(:include
                                                     ntop.shared.domain:sample-record))))
                   (cl-weave:it
                    "constructs a sample with its timestamp and unavailable ARC"
                    (let ((sample
                           (ntop.memory.domain:make-memory-sample :sampled-at
                                                                  12.0
                                                                  :ram-used-bytes
                                                                  4096
                                                                  :swap-total-bytes
                                                                  8192)))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       12.0)
                      (cl-weave:expect
                       (ntop.memory.domain:memory-sample-ram-used-bytes sample)
                       :to-be
                       4096)
                      (cl-weave:expect
                       (ntop.memory.domain:memory-sample-arc-used-bytes sample)
                       :to-be
                       nil)
                      (cl-weave:expect
                       (ntop.memory.domain:memory-sample-arc-total-bytes sample)
                       :to-be
                       nil)))
                   (cl-weave:it-each ((:arc-used-bytes 1) (:arc-total-bytes 1))
                                     "rejects a value for ~A because ARC is always unavailable"
                                     (slot value)
                                     (cl-weave:signals type-error
                                                       (case slot
                                                         (:arc-used-bytes
                                                          (ntop.memory.domain:make-memory-sample
                                                           :sampled-at
                                                           1.0
                                                           :arc-used-bytes
                                                           value))
                                                         (:arc-total-bytes
                                                          (ntop.memory.domain:make-memory-sample
                                                           :sampled-at
                                                           1.0
                                                           :arc-total-bytes
                                                           value)))))
                   (cl-weave:it-each ((nil nil) (0 0))
                                     "preserves ~S as optional RAM usage"
                                     (value expected)
                                     (let ((sample
                                            (ntop.memory.domain:make-memory-sample
                                             :sampled-at
                                             0.0
                                             :ram-used-bytes
                                             value)))
                                       (cl-weave:expect
                                        (ntop.memory.domain:memory-sample-ram-used-bytes
                                         sample)
                                        :to-be
                                        expected))))
