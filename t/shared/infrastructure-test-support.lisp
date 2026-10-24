(in-package #:ntop.test)

(defclass %test-fixture-clock ()
  ((time :initarg :time :accessor %test-fixture-clock-time)
   (failure :initarg :failure :initform nil :reader %test-fixture-clock-failure)
   (reads :initform 0 :accessor %test-fixture-clock-reads)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock %test-fixture-clock) on-time on-unavailable)
  (incf (%test-fixture-clock-reads clock))
  (if (%test-fixture-clock-failure clock)
      (funcall on-unavailable (%test-fixture-clock-failure clock))
      (funcall on-time (%test-fixture-clock-time clock))))
