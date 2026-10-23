(in-package #:ntop.e2e)

(defparameter *measurement-rss-cadence-seconds*
  30d0)

(defparameter *measurement-frame-quiet-seconds*
  0.25d0)

(defparameter *measurement-first-frame-factor*
  3d0)

(defparameter *measurement-interaction-samples*
  20)

(defmacro define-measurement-key-cycle (&body keys)
  `(defparameter *measurement-key-cycle*
     (vector
      ,@(mapcar
         (lambda (key)
           `(list :key
                  ,(first key)
                  :octets
                  (make-array 1
                              :element-type
                              '(unsigned-byte 8)
                              :initial-element
                              ,(second key))))
         keys))))

(define-measurement-key-cycle (#\? #x3f) (#\Escape #x1b))

(defparameter *measurement-n6-query*
  (format nil "~c[?25$p" #\Escape))

(defparameter *measurement-n6-reply-octets*
  (make-array 9
              :element-type
              '(unsigned-byte 8)
              :initial-contents
              '(#x1b #x5b #x3f #x32 #x35 #x3b #x31 #x24 #x79)))

(defparameter *measurement-n6-exit-key-octets*
  (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x71))

(defmacro define-n6-reply-status-table (&body cases)
  `(defparameter *n6-reply-status-table*
     ',(mapcar
        (lambda (case)
          (cons (second case) (first case)))
        cases)))

(define-n6-reply-status-table (0 #x30) (1 #x31) (2 #x32) (3 #x33) (4 #x34))
