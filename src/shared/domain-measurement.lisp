(in-package #:ntop.shared.domain)

(deftype measurement-value () '(or null real))
(deftype measurement-counter () '(integer 0 *))
(deftype optional-counter () '(or null measurement-counter))
(deftype monotonic-seconds () 'real)

(defun measurement-available-p (value)
  (not (null value)))

(defstruct (sample-record
            (:constructor make-sample-record
                (&key (sampled-at
                       (error "Sample timestamp is required."))))
            (:copier nil))
  (sampled-at (error "Sample timestamp is required.")
              :type monotonic-seconds))

(defmacro define-value-type (name &body slots)
  `(defstruct (,name (:copier nil)) ,@slots))

(defmacro define-sample-type (name &body slots)
  `(defstruct (,name (:include sample-record) (:copier nil)) ,@slots))
