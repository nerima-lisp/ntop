(in-package #:ntop.test)

(cl-weave:describe "GPU domain types"
                   (cl-weave:it
                    "expands a value type into a copier-free defstruct"
                    (let ((expansion
                           (macroexpand-1
                            '(ntop.shared.domain:define-value-type
                              gpu-device
                              (name nil)))))
                      (cl-weave:expect (car expansion) :to-be 'progn)
                      (let ((defstruct-form (second expansion))
                            (constructor-form (third expansion))
                            (struct-spec (second (second expansion))))
                        (cl-weave:expect (car defstruct-form)
                                         :to-be
                                         'defstruct)
                        (cl-weave:expect (second struct-spec)
                                         :to-equal
                                         '(:constructor make-gpu-device-raw))
                        (cl-weave:expect (third struct-spec)
                                         :to-equal
                                         '(:copier nil))
                        (cl-weave:expect (third defstruct-form)
                                         :to-equal
                                         '(name nil :read-only t))
                        (cl-weave:expect (car constructor-form)
                                         :to-be
                                         'defun)
                        (cl-weave:expect (second constructor-form)
                                         :to-be
                                         'make-gpu-device)
                        (cl-weave:expect (car (fourth constructor-form))
                                         :to-be
                                         'ntop.shared.domain::validate-typed-slots)
                        (cl-weave:expect (car (fifth constructor-form))
                                         :to-be
                                         'make-gpu-device-raw))))
                   (cl-weave:it "includes the shared record in a sample type"
                                (let ((expansion
                                       (macroexpand-1
                                        '(ntop.shared.domain:define-sample-type
                                          gpu-sample
                                          (devices nil)))))
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
                                                     '(:constructor make-gpu-sample-raw))
                                    (cl-weave:expect (fourth struct-spec)
                                                     :to-equal
                                                     '(:copier nil))
                                    (cl-weave:expect (third defstruct-form)
                                                     :to-equal
                                                     '(devices nil :read-only t))
                                    (cl-weave:expect (car constructor-form)
                                                     :to-be
                                                     'defun)
                                    (cl-weave:expect (second constructor-form)
                                                     :to-be
                                                     'make-gpu-sample)
                                    (cl-weave:expect (car (fourth constructor-form))
                                                     :to-be
                                                     'ntop.shared.domain::validate-typed-slots)
                                    (cl-weave:expect (car (fifth constructor-form))
                                                     :to-be
                                                     'make-gpu-sample-raw))))
                   (cl-weave:it
                    "constructs a sample with a GPU device and timestamp"
                    (let* ((device
                            (ntop.gpu.domain:make-gpu-device :name
                                                             "GPU 0"
                                                             :usage-percent
                                                             42.0
                                                             :memory-used-bytes
                                                             2048))
                           (sample
                            (ntop.gpu.domain:make-gpu-sample :sampled-at
                                                             18.0
                                                             :devices
                                                             (list device))))
                      (cl-weave:expect
                       (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be
                       18.0)
                      (cl-weave:expect
                       (ntop.gpu.domain:gpu-device-usage-percent
                        (first (ntop.gpu.domain:gpu-sample-devices sample)))
                       :to-be
                       42.0)
                      (cl-weave:expect
                       (ntop.gpu.domain:gpu-device-memory-used-bytes device)
                       :to-be
                       2048)))
                   (cl-weave:it-each ((nil nil) (0.0 0.0))
                                     "preserves ~S as optional GPU usage"
                                     (value expected)
                                     (let ((device
                                            (ntop.gpu.domain:make-gpu-device
                                             :usage-percent
                                             value)))
                                       (cl-weave:expect
                                        (ntop.gpu.domain:gpu-device-usage-percent
                                         device)
                                        :to-be
                                        expected))))

(cl-weave:it-each
 ((:name ntop.gpu.domain:make-gpu-device
         ntop.gpu.domain:gpu-device-name
         nil
         nil
         nil
         "GPU 0"
         :invalid)
  (:usage-percent ntop.gpu.domain:make-gpu-device
                  ntop.gpu.domain:gpu-device-usage-percent
                  nil
                  nil
                  nil
                  42.0
                  :invalid)
  (:memory-used-bytes ntop.gpu.domain:make-gpu-device
                      ntop.gpu.domain:gpu-device-memory-used-bytes
                      nil
                      nil
                      nil
                      2048
                      -1)
  (:memory-total-bytes ntop.gpu.domain:make-gpu-device
                       ntop.gpu.domain:gpu-device-memory-total-bytes
                       nil
                       nil
                       nil
                       4096
                       -1)
  (:temperature-celsius ntop.gpu.domain:make-gpu-device
                        ntop.gpu.domain:gpu-device-temperature-celsius
                        nil
                        nil
                        nil
                        51.5
                        :invalid)
  (:devices ntop.gpu.domain:make-gpu-sample
            ntop.gpu.domain:gpu-sample-devices
            (:sampled-at 0.0)
            nil
            nil
            (:device)
            :invalid))
 "checks typed GPU slot ~S"
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
