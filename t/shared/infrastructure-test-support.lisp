(in-package #:ntop.test)

(defclass %test-fixture-clock ()
  ((time :initarg :time :accessor %test-fixture-clock-time)
   (unix-time :initarg :unix-time :initform 1700000000
              :accessor %test-fixture-clock-unix-time)
   (failure :initarg :failure :initform nil :reader %test-fixture-clock-failure)
   (reads :initform 0 :accessor %test-fixture-clock-reads)
   (unix-time-reads :initform 0 :accessor %test-fixture-clock-unix-time-reads)))

(defmethod ntop.shared.domain:read-monotonic-time
    ((clock %test-fixture-clock) on-time on-unavailable)
  (incf (%test-fixture-clock-reads clock))
  (if (%test-fixture-clock-failure clock)
      (funcall on-unavailable (%test-fixture-clock-failure clock))
      (funcall on-time (%test-fixture-clock-time clock))))
(defmethod ntop.shared.domain:read-unix-time
    ((clock %test-fixture-clock) on-time on-unavailable)
  (incf (%test-fixture-clock-unix-time-reads clock))
  (if (%test-fixture-clock-failure clock)
      (funcall on-unavailable (%test-fixture-clock-failure clock))
      (funcall on-time (%test-fixture-clock-unix-time clock))))
