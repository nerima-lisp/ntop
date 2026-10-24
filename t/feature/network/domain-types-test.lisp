(in-package #:ntop.test)

(cl-weave:describe "network domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              network-interface
                              (name nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (cl-weave:expect (car (second expansion))
                                       :to-be 'defstruct)
                      (cl-weave:expect
                       (second (second (second expansion)))
                       :to-equal
                       '(:constructor make-network-interface-raw))
                      (cl-weave:expect
                       (third (second (second expansion)))
                       :to-equal
                       '(:copier nil))
                      (cl-weave:expect (car (third expansion)) :to-be 'defun)
                      (cl-weave:expect (second (third expansion))
                                       :to-be
                                       'make-network-interface)
                      (cl-weave:expect (car (fourth (third expansion)))
                                       :to-be
                                       'ntop.shared.domain::validate-typed-slots)))
                   (cl-weave:it
                    "includes the shared record in a sample type"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-sample-type
                              network-sample
                              (interfaces nil)))))
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
                       '(:constructor make-network-sample-raw))
                      (cl-weave:expect
                       (fourth (second (second expansion)))
                       :to-equal
                       '(:copier nil))
                      (cl-weave:expect (car (third expansion)) :to-be 'defun)
                      (cl-weave:expect (second (third expansion))
                                       :to-be
                                       'make-network-sample)
                      (cl-weave:expect (car (fourth (third expansion)))
                                       :to-be
                                       'ntop.shared.domain::validate-typed-slots)))
                   (cl-weave:it
                    "constructs a sample with an interface and timestamp"
                    (let* ((interface
                            (ntop.network.domain:make-network-interface :name
                                                                        "en0"
                                                                        :rx-bytes
                                                                        4096
                                                                        :tx-packets
                                                                        3))
                           (sample
                            (ntop.network.domain:make-network-sample :sampled-at
                                                                     21.0
                                                                     :interfaces
                                                                     (list
                                                                      interface))))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       21.0)
                      (cl-weave:expect
                       (ntop.network.domain:network-interface-rx-bytes
                        interface)
                       :to-be
                       4096)
                      (cl-weave:expect
                       (ntop.network.domain:network-sample-interfaces sample)
                       :to-equal
                       (list interface))))
                   (cl-weave:it-each ((nil nil) (0 0))
                                     "preserves ~S as optional received bytes"
                                     (value expected)
                                     (let ((interface
                                            (ntop.network.domain:make-network-interface
                                             :rx-bytes
                                             value)))
                                       (cl-weave:expect
                                        (ntop.network.domain:network-interface-rx-bytes
                                         interface)
                                        :to-be
                                        expected))))

(cl-weave:it-each
 ((:name ntop.network.domain:make-network-interface
         ntop.network.domain:network-interface-name
         nil
         nil
         nil
         "en0"
         :invalid)
  (:rx-bytes ntop.network.domain:make-network-interface
             ntop.network.domain:network-interface-rx-bytes
             nil
             nil
             nil
             4096
             -1)
  (:tx-bytes ntop.network.domain:make-network-interface
             ntop.network.domain:network-interface-tx-bytes
             nil
             nil
             nil
             8192
             -1)
  (:rx-packets ntop.network.domain:make-network-interface
               ntop.network.domain:network-interface-rx-packets
               nil
               nil
               nil
               2
               -1)
  (:tx-packets ntop.network.domain:make-network-interface
               ntop.network.domain:network-interface-tx-packets
               nil
               nil
               nil
               3
               -1)
  (:interfaces ntop.network.domain:make-network-sample
               ntop.network.domain:network-sample-interfaces
               (:sampled-at 0.0)
               nil
               nil
               (:interface)
               :invalid))
 "checks typed network slot ~S"
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
