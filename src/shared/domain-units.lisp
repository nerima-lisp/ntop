(in-package #:ntop.shared.domain)

(defparameter +unit-definitions+
  '((:bit "b" :information 1)
    (:byte "B" :information 8)
    (:second "s" :time 1)
    (:millisecond "ms" :time 1/1000)
    (:nanosecond "ns" :time 1/1000000000)
    (:percent "%" :ratio 1)
    (:watt "W" :power 1)
    (:celsius "°C" :temperature 1)
    (:hertz "Hz" :frequency 1)
    (:count "" :count 1)))

(defparameter +decimal-prefixes+
  '((:none "" 1)
    (:kilo "k" 1000)
    (:mega "M" 1000000)
    (:giga "G" 1000000000)
    (:tera "T" 1000000000000)))

(defparameter +binary-prefixes+
  '((:none "" 1)
    (:kibi "Ki" 1024)
    (:mebi "Mi" 1048576)
    (:gibi "Gi" 1073741824)
    (:tebi "Ti" 1099511627776)))

(defstruct (quantity (:constructor make-quantity (value unit)) (:copier nil))
  (value 0 :type real)
  (unit :count :type symbol))

(defstruct (scaled-quantity
            (:constructor make-scaled-quantity (value unit prefix symbol))
            (:copier nil))
  (value 0 :type real)
  (unit :count :type symbol)
  (prefix :none :type symbol)
  (symbol "" :type string))

(defun unit-definition (unit)
  (or (assoc unit +unit-definitions+)
      (error "Unknown unit ~S." unit)))

(defun convert-quantity (quantity target-unit)
  (let* ((source (unit-definition (quantity-unit quantity)))
         (target (unit-definition target-unit)))
    (unless (eq (third source) (third target))
      (error "Cannot convert ~S to ~S." (quantity-unit quantity) target-unit))
    (make-quantity
     (* (quantity-value quantity) (/ (fourth source) (fourth target)))
     target-unit)))

(defun scale-quantity (quantity prefix-system prefix)
  (let* ((prefixes (ecase prefix-system
                     (:decimal +decimal-prefixes+)
                     (:binary +binary-prefixes+)))
         (prefix-definition
           (or (assoc prefix prefixes)
               (error "Unknown ~A prefix ~S." prefix-system prefix)))
         (unit-definition (unit-definition (quantity-unit quantity)))
         (prefix-symbol (second prefix-definition))
         (unit-symbol (second unit-definition)))
    (make-scaled-quantity
     (/ (quantity-value quantity) (third prefix-definition))
     (quantity-unit quantity)
     prefix
     (concatenate 'string prefix-symbol unit-symbol))))
