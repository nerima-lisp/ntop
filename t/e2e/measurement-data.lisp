(in-package #:ntop.e2e)

(defparameter *measurement-rss-cadence-seconds*
  30d0)

(defparameter *measurement-frame-quiet-seconds*
  0.25d0)

(defparameter *measurement-first-frame-factor*
  3d0)

(defparameter *measurement-interaction-samples*
  20)

(defparameter *measurement-key-octets*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x3f))

(defparameter *measurement-n6-query*
  (format nil "~c[?25$p" #\Escape))
