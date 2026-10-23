(in-package #:ntop.test)

(defun %wp4-network-metrics-interface
    (rx-bytes tx-bytes rx-packets tx-packets &optional (name "en0"))
  (ntop.network.domain:make-network-interface
   :name name
   :rx-bytes rx-bytes
   :tx-bytes tx-bytes
   :rx-packets rx-packets
   :tx-packets tx-packets))

(defun %wp4-network-metrics-sample (sampled-at &rest interfaces)
  (ntop.network.domain:make-network-sample
   :sampled-at sampled-at
   :interfaces interfaces))

(cl-weave:describe "network domain metrics"
  (cl-weave:it-each
      ((:rx :rx 1300 25)
       (:tx :tx 2500 50))
      "sums all ~A interface counters"
      (case-name direction expected-bytes expected-packets)
    (let ((sample
            (%wp4-network-metrics-sample
             10
             (%wp4-network-metrics-interface 1000 2000 20 40 "en0")
             (%wp4-network-metrics-interface 300 500 5 10 "lo"))))
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes sample direction)
       :to-be expected-bytes)
      (cl-weave:expect
       (ntop.network.domain:network-total-packets sample direction)
       :to-be expected-packets)))

  (cl-weave:it "computes byte totals relative to the selected start sample"
    (let* ((baseline
             (%wp4-network-metrics-sample
              10 (%wp4-network-metrics-interface 1000 2000 10 20)))
           (current
             (%wp4-network-metrics-sample
              15 (%wp4-network-metrics-interface 1450 2500 15 25)))
           (reset
             (%wp4-network-metrics-sample
              20 (%wp4-network-metrics-interface 800 1990 20 30))))
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes
        baseline :rx :start-zeroed-p t)
       :to-be 0)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes current :rx)
       :to-be 1450)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes
        current :rx :baseline-sample baseline :start-zeroed-p t)
       :to-be 450)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes
        reset :tx :baseline-sample baseline :start-zeroed-p t)
       :to-be 0)))

  (cl-weave:it "uses sample timestamps for bit and byte rates"
    (let* ((previous
             (%wp4-network-metrics-sample
              10 (%wp4-network-metrics-interface 100 200 10 20)))
           (current
             (%wp4-network-metrics-sample
              12.5 (%wp4-network-metrics-interface 600 450 30 25)))
           (rx-bits
             (ntop.network.domain:network-rate previous current :rx))
           (tx-bytes
             (ntop.network.domain:network-rate
              previous current :tx :unit :byte)))
      (cl-weave:expect
       (= (ntop.shared.domain:quantity-value rx-bits) 1600)
       :to-be t)
      (cl-weave:expect
       (ntop.shared.domain:quantity-unit rx-bits)
       :to-be :bit)
      (cl-weave:expect
       (= (ntop.shared.domain:quantity-value tx-bytes) 100)
       :to-be t)
      (cl-weave:expect
       (ntop.shared.domain:quantity-unit tx-bytes)
       :to-be :byte)))

  (cl-weave:it "returns zero for a decreasing aggregate counter"
    (let ((previous
            (%wp4-network-metrics-sample
             10 (%wp4-network-metrics-interface 1000 2000 10 20)))
          (current
            (%wp4-network-metrics-sample
             12 (%wp4-network-metrics-interface 900 1900 30 40))))
      (cl-weave:expect
       (ntop.shared.domain:quantity-value
        (ntop.network.domain:network-rate previous current :rx))
       :to-be 0)
      (cl-weave:expect
       (ntop.shared.domain:quantity-value
        (ntop.network.domain:network-rate previous current :tx))
       :to-be 0)))

  (cl-weave:it "keeps unavailable counters distinct from zero"
    (let ((missing
            (%wp4-network-metrics-sample
             10 (%wp4-network-metrics-interface nil nil nil nil)))
          (partial
            (%wp4-network-metrics-sample
             11
             (%wp4-network-metrics-interface 30 40 3 4 "en0")
             (%wp4-network-metrics-interface nil 50 5 6 "lo")))
          (zero
            (%wp4-network-metrics-sample
             12 (%wp4-network-metrics-interface 0 0 0 0)))
          (empty (%wp4-network-metrics-sample 13)))
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes missing :rx)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-total-packets missing :rx)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes zero :rx)
       :to-be 0)
      (cl-weave:expect
       (ntop.network.domain:network-total-packets zero :rx)
       :to-be 0)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes partial :rx)
       :to-be nil)
      (cl-weave:expect (ntop.network.domain:network-total-packets partial :rx) :to-be 8)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes
        zero :rx :baseline-sample partial :start-zeroed-p t)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes nil :rx)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-total-bytes empty :rx)
       :to-be 0)
      (cl-weave:expect
       (ntop.network.domain:network-total-packets empty :tx)
       :to-be 0)
      (cl-weave:expect
       (ntop.network.domain:network-rate missing zero :rx)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-rate nil zero :rx)
       :to-be nil)
      (cl-weave:expect
       (ntop.network.domain:network-average-packet-size missing zero :rx)
       :to-be nil)))

  (cl-weave:it "computes packet rates and average packet sizes"
    (let* ((previous
             (%wp4-network-metrics-sample
              10 (%wp4-network-metrics-interface 100 200 10 20)))
           (current
             (%wp4-network-metrics-sample
              12.5 (%wp4-network-metrics-interface 600 450 30 25)))
           (rx-average
             (ntop.network.domain:network-average-packet-size
              previous current :rx))
           (tx-average
             (ntop.network.domain:network-average-packet-size
              previous current :tx)))
      (cl-weave:expect
       (ntop.network.domain:network-packet-rate-per-second
        previous current :rx)
       :to-be 8.0)
      (cl-weave:expect
       (ntop.network.domain:network-packet-rate-per-second
        previous current :tx)
       :to-be 2.0)
      (cl-weave:expect
       (= (ntop.shared.domain:quantity-value rx-average) 25)
       :to-be t)
      (cl-weave:expect
       (ntop.shared.domain:quantity-unit rx-average)
       :to-be :byte)
      (cl-weave:expect
       (= (ntop.shared.domain:quantity-value tx-average) 50)
       :to-be t)))
  (cl-weave:it "returns zero average size when packets do not increase"
    (let* ((previous
             (%wp4-network-metrics-sample
              10 (%wp4-network-metrics-interface 100 100 10 10)))
           (current
             (%wp4-network-metrics-sample
              12 (%wp4-network-metrics-interface 300 300 10 10)))
           (average
             (ntop.network.domain:network-average-packet-size
              previous current :rx)))
      (cl-weave:expect
       (ntop.network.domain:network-packet-rate-per-second
        previous current :rx)
       :to-be 0)
      (cl-weave:expect
       (ntop.shared.domain:quantity-value average)
       :to-be 0)
      (cl-weave:expect
       (ntop.shared.domain:quantity-unit average)
       :to-be :byte))))
