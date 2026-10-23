;;; Samples, snapshots, and value records are immutable after construction. Collectors build
;;; a fresh sample each cycle; UI code must not change any struct slots.
;;; Read-only slots do not prevent changes to lists or vectors stored in those slots.
;;; Handoff passes samples directly without copying.
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
              :type monotonic-seconds
              :read-only t))

(defmacro define-value-type (name &body slots)
  `(defstruct (,name (:copier nil))
     ,@(mapcar (lambda (slot)
                 (if (symbolp slot)
                     `(,slot nil :read-only t)
                     (append slot '(:read-only t))))
               slots)))

(defmacro define-sample-type (name &body slots)
  `(defstruct (,name (:include sample-record) (:copier nil))
     ,@(mapcar (lambda (slot)
                 (if (symbolp slot)
                     `(,slot nil :read-only t)
                     (append slot '(:read-only t))))
               slots)))
