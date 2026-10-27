(in-package #:ntop.test)

(cl-weave:describe "quantities use unit and prefix tables"
                   (cl-weave:it-each
                    ((:bit "b" :information 1) (:byte "B" :information 8)
                     (:second "s" :time 1)
                     (:millisecond "ms" :time 1/1000)
                     (:nanosecond "ns" :time 1/1000000000)
                     (:percent "%" :ratio 1)
                     (:watt "W" :power 1)
                     (:celsius "°C" :temperature 1)
                     (:hertz "Hz" :frequency 1)
                     (:count "" :count 1))
                    "defines the ~A unit"
                    (name symbol dimension factor)
                    (cl-weave:expect
                     (equal (ntop.shared.domain:unit-definition name)
                            (list name symbol dimension factor))
                     :to-be
                     t))
                   (cl-weave:it-each
                    ((:byte 2 :bit 16) (:bit 16 :byte 2)
                     (:second 2 :millisecond 2000)
                     (:millisecond 2000 :second 2)
                     (:nanosecond 1000000 :millisecond 1))
                    "converts ~A quantities to ~A"
                    (source-unit value target-unit expected)
                    (let ((converted
                           (ntop.shared.domain:convert-quantity
                            (ntop.shared.domain:make-quantity value source-unit)
                            target-unit)))
                      (cl-weave:expect (= (ntop.shared.domain:quantity-value converted) expected)
                                       :to-be
                                       t)
                      (cl-weave:expect (ntop.shared.domain:quantity-unit converted)
                                       :to-be
                                       target-unit)))
                   (cl-weave:it "rejects incompatible units and unknown unit prefixes"
  (cl-weave:expect
   (handler-case (progn
                   (ntop.shared.domain:convert-quantity
                    (ntop.shared.domain:make-quantity 1 :byte)
                    :second)
                   :returned)
     (simple-error () :signaled))
   :to-be :signaled)
  (cl-weave:expect
   (handler-case (progn
                   (ntop.shared.domain:unit-definition :fixture-unit)
                   :returned)
     (simple-error () :signaled))
   :to-be :signaled)
  (cl-weave:expect
   (handler-case (progn
                   (ntop.shared.domain:scale-quantity
                    (ntop.shared.domain:make-quantity 1 :byte)
                    :decimal
                    :fixture-prefix)
                   :returned)
     (simple-error () :signaled))
   :to-be :signaled)
  (cl-weave:expect
   (handler-case (progn
                   (ntop.shared.domain:scale-quantity
                    (ntop.shared.domain:make-quantity 1 :byte)
                    :fixture-system
                    :none)
                   :returned)
     (type-error () :signaled))
   :to-be :signaled))
                   (cl-weave:it-each
                    ((:decimal :kilo 2500 5/2 "kB")
                     (:decimal :mega 2000000 2 "MB")
                     (:decimal :none 12345 12345 "B")
                     (:decimal :giga 2000000000 2 "GB")
                     (:decimal :tera 3000000000000 3 "TB")
                     (:binary :kibi 2048 2 "KiB")
                     (:binary :mebi 2097152 2 "MiB")
                     (:binary :none 2048 2048 "B")
                     (:binary :gibi 2147483648 2 "GiB")
                     (:binary :tebi 3298534883328 3 "TiB"))
                    "scales a quantity using ~A prefixes"
                    (family prefix value expected symbol)
                    (let ((scaled
                           (ntop.shared.domain:scale-quantity
                            (ntop.shared.domain:make-quantity value :byte)
                            family
                            prefix)))
                      (cl-weave:expect
                       (= (ntop.shared.domain:scaled-quantity-value scaled) expected)
                       :to-be
                       t)
                      (cl-weave:expect (ntop.shared.domain:scaled-quantity-symbol scaled)
                                       :to-equal
                                       symbol)))
                   (cl-weave:it-property "bit and byte conversions are reversible"
                                         ((bytes (cl-weave:gen-integer :min 0 :max 1000000)))
                                         (let* ((byte-quantity
                                                 (ntop.shared.domain:make-quantity bytes :byte))
                                                (bits
                                                 (ntop.shared.domain:convert-quantity byte-quantity
                                                                                      :bit))
                                                (round-trip
                                                 (ntop.shared.domain:convert-quantity bits :byte)))
                                           (cl-weave:expect
                                            (ntop.shared.domain:quantity-value round-trip)
                                            :to-be
                                            bytes))))
