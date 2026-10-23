(in-package #:ntop.test)

(cl-weave:describe "CPU domain types"
  (cl-weave:it "expands a value type into a copier-free defstruct"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-value-type cpu-core-value
                (core-index nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect (second (second expansion)) :to-equal '(:copier nil))))
  (cl-weave:it "includes the shared record in a sample type"
    (let ((expansion
            (macroexpand-1
             '(ntop.shared.domain:define-sample-type cpu-sample
                (average-usage-percent nil)))))
      (cl-weave:expect (car expansion) :to-be 'defstruct)
      (cl-weave:expect
       (second (second expansion))
       :to-equal '(:include ntop.shared.domain:sample-record))))
  (cl-weave:it "constructs a sample with core values and its timestamp"
    (let* ((core (ntop.cpu.domain:make-cpu-core-value
                  :core-index 0
                  :user-ticks 120
                  :frequency-mhz 1200.0))
           (sample (ntop.cpu.domain:make-cpu-sample
                    :sampled-at 6.5
                    :core-values (list core)
                    :load-average-1 1.25)))
      (cl-weave:expect (ntop.shared.domain:sample-record-sampled-at sample)
                       :to-be 6.5)
      (cl-weave:expect (ntop.cpu.domain:cpu-sample-load-average-1 sample)
                       :to-be 1.25)
      (cl-weave:expect
       (ntop.cpu.domain:cpu-core-value-frequency-mhz
        (first (ntop.cpu.domain:cpu-sample-core-values sample)))
       :to-be 1200.0)))
  (cl-weave:it-each
    ((nil nil) (0.0 0.0))
    "preserves ~S as an optional average"
    (value expected)
    (let ((sample (ntop.cpu.domain:make-cpu-sample
                   :sampled-at 0.0
                   :average-usage-percent value)))
      (cl-weave:expect
       (ntop.cpu.domain:cpu-sample-average-usage-percent sample)
       :to-be expected))))
