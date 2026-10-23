(in-package #:ntop.test)

(cl-weave:describe "network unit selection"
  (cl-weave:it-each
      ((:decimal-kilo 1500 :bit nil :kilo 1.5 "kb")
       (:decimal-boundary 1000 :byte nil :kilo 1 "kB")
       (:decimal-mega 1000000 :bit nil :mega 1 "Mb")
       (:decimal-giga 1000000000 :byte nil :giga 1 "GB")
       (:decimal-tera 1000000000000 :bit nil :tera 1 "Tb")
       (:decimal-over-tera 2500000000000 :byte nil :tera 2.5 "TB")
       (:binary-kibi 1536 :byte t :kibi 1.5 "KiB")
       (:binary-boundary 1024 :bit t :kibi 1 "Kib")
       (:binary-mebi 1048576 :byte t :mebi 1 "MiB")
       (:binary-gibi 1073741824 :bit t :gibi 1 "Gib")
       (:binary-tebi 1099511627776 :byte t :tebi 1 "TiB")
       (:binary-over-tebi 2199023255552 :bit t :tebi 2 "Tib")
       (:zero 0 :byte nil :none 0 "B"))
      "selects the ~A quantity prefix"
      (case-name value unit binary-prefix-p expected-prefix expected-value symbol)
    (let ((scaled
            (ntop.network.domain:network-scale-quantity
             (ntop.shared.domain:make-quantity value unit)
             binary-prefix-p)))
      (cl-weave:expect
       (ntop.shared.domain:scaled-quantity-prefix scaled)
       :to-be expected-prefix)
      (cl-weave:expect
       (= (ntop.shared.domain:scaled-quantity-value scaled) expected-value)
       :to-be t)
      (cl-weave:expect
       (ntop.shared.domain:scaled-quantity-symbol scaled)
       :to-equal symbol)))

  (cl-weave:it "preserves an unavailable quantity"
    (cl-weave:expect
     (ntop.network.domain:network-scale-quantity nil nil)
     :to-be nil))

  (cl-weave:it-each
      ((:linear nil nil :linear)
       (:linear-binary nil t :linear)
       (:decimal-log t nil :log10)
       (:binary-log t t :log2))
      "chooses the ~A y-axis scale"
      (case-name logarithmic-p binary-prefix-p expected)
    (cl-weave:expect
     (ntop.network.domain:network-axis-scaling
      logarithmic-p binary-prefix-p)
     :to-be expected)))
