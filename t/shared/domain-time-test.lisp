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

(cl-weave:describe "monotonic and wall-clock time continuations"
  (cl-weave:before-each
    (setf *clock-result* nil))

  (cl-weave:it "delivers the monotonic clock value to the success continuation"
    (ntop.shared.domain:read-monotonic-time
     (make-instance 'fixed-clock)
     (lambda (value) (setf *clock-result* value))
     (lambda (reason) (declare (ignore reason))))
    (cl-weave:expect *clock-result* :to-be 123.5))

  (cl-weave:it "delivers an unavailable monotonic reason to its failure continuation"
    (ntop.shared.domain:read-monotonic-time
     (make-instance 'unavailable-clock)
     (lambda (value) (declare (ignore value)))
     (lambda (reason) (setf *clock-result* reason)))
    (cl-weave:expect *clock-result* :to-be :clock-unavailable))

  (cl-weave:it "delivers fixed Unix seconds through the wall-clock port"
    (let ((clock (make-instance '%test-fixture-clock
                                :time 0d0
                                :unix-time 1700000000)))
      (ntop.shared.domain:read-unix-time
       clock
       (lambda (value) (setf *clock-result* value))
       (lambda (reason) (declare (ignore reason))))
      (cl-weave:expect *clock-result* :to-be 1700000000)
      (cl-weave:expect (%test-fixture-clock-unix-time-reads clock)
                       :to-be 1)))

  (cl-weave:it "delivers an unavailable wall-clock reason to its failure continuation"
    (ntop.shared.domain:read-unix-time
     (make-instance '%test-fixture-clock :time 0d0 :failure :clock-unavailable)
     (lambda (value) (declare (ignore value)))
     (lambda (reason) (setf *clock-result* reason)))
    (cl-weave:expect *clock-result* :to-be :clock-unavailable)))
