(in-package #:ntop.test)

(defvar *clock-result*)

(defclass fixed-clock () ())
(defclass unavailable-clock () ())

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock fixed-clock) on-time on-unavailable)
  (declare (ignore clock on-unavailable))
  (funcall on-time 123.5))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock unavailable-clock) on-time on-unavailable)
  (declare (ignore clock on-time))
  (funcall on-unavailable :clock-unavailable))

(cl-weave:describe "monotonic time is delivered through continuations"
  (cl-weave:before-each
    (setf *clock-result* nil))

  (cl-weave:it "delivers the clock value to the success continuation"
    (ntop.shared.domain:read-monotonic-time
     (make-instance 'fixed-clock)
     (lambda (value) (setf *clock-result* value))
     (lambda (reason) (declare (ignore reason))))
    (cl-weave:expect *clock-result* :to-be 123.5))

  (cl-weave:it "delivers an unavailable reason to its failure continuation"
    (ntop.shared.domain:read-monotonic-time
     (make-instance 'unavailable-clock)
     (lambda (value) (declare (ignore value)))
     (lambda (reason) (setf *clock-result* reason)))
    (cl-weave:expect *clock-result* :to-be :clock-unavailable)))
