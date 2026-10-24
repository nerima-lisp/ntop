(in-package #:ntop.test)

(cl-weave:describe "memory domain types"
                   (cl-weave:it
                    "includes the shared record in a sample type"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-sample-type
                              memory-sample
                              (ram-used-bytes nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (cl-weave:expect (car (second expansion))
                                       :to-be 'defstruct)
                      (cl-weave:expect
                       (second (second (second expansion)))
                       :to-equal
                       '(:include ntop.shared.domain:sample-record))
                      (cl-weave:expect
                       (third (second (second expansion)))
                       :to-equal
                       '(:constructor make-memory-sample-raw))
                      (cl-weave:expect
                       (fourth (second (second expansion)))
                       :to-equal
                       '(:copier nil))
                      (cl-weave:expect (car (third expansion)) :to-be 'defun)
                      (cl-weave:expect (second (third expansion))
                                       :to-be
                                       'make-memory-sample)
                      (cl-weave:expect (car (fourth (third expansion)))
                                       :to-be
                                       'ntop.shared.domain::validate-typed-slots)))
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

(cl-weave:it-each
 ((:ram-used-bytes ntop.memory.domain:make-memory-sample
                   ntop.memory.domain:memory-sample-ram-used-bytes
                   (:sampled-at 0.0)
                   nil
                   nil
                   4096
                   -1)
  (:ram-total-bytes ntop.memory.domain:make-memory-sample
                    ntop.memory.domain:memory-sample-ram-total-bytes
                    (:sampled-at 0.0)
                    nil
                    nil
                    8192
                    -1)
  (:swap-used-bytes ntop.memory.domain:make-memory-sample
                    ntop.memory.domain:memory-sample-swap-used-bytes
                    (:sampled-at 0.0)
                    nil
                    nil
                    2048
                    -1)
  (:swap-total-bytes ntop.memory.domain:make-memory-sample
                     ntop.memory.domain:memory-sample-swap-total-bytes
                     (:sampled-at 0.0)
                     nil
                     nil
                     4096
                     -1)
  (:cache-used-bytes ntop.memory.domain:make-memory-sample
                     ntop.memory.domain:memory-sample-cache-used-bytes
                     (:sampled-at 0.0)
                     nil
                     nil
                     1024
                     -1)
  (:cache-total-bytes ntop.memory.domain:make-memory-sample
                      ntop.memory.domain:memory-sample-cache-total-bytes
                      (:sampled-at 0.0)
                      nil
                      nil
                      2048
                      -1)
  (:arc-used-bytes ntop.memory.domain:make-memory-sample
                   ntop.memory.domain:memory-sample-arc-used-bytes
                   (:sampled-at 0.0)
                   nil
                   nil
                   nil
                   1)
  (:arc-total-bytes ntop.memory.domain:make-memory-sample
                    ntop.memory.domain:memory-sample-arc-total-bytes
                    (:sampled-at 0.0)
                    nil
                    nil
                    nil
                    1))
 "checks typed memory slot ~S"
 (initarg constructor
          accessor
          required-initargs
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
  (list default-value nil-value accepted-value :type-error)))
