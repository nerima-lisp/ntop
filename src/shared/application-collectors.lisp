(in-package #:ntop.shared.application)

(deftype collector-unavailability-reason ()
  '(member :not-present :disabled :permission-denied :unsupported-platform
           :private-api-unavailable :read-failed))
;; ON-UNAVAILABLE receives exactly one keyword of type COLLECTOR-UNAVAILABILITY-REASON.
(defmacro define-collector (name)
  `(defgeneric ,name (collector on-sample on-unavailable)))

(define-collector collect-cpu-sample)

(define-collector collect-memory-sample)

(define-collector collect-gpu-sample)

(define-collector collect-network-sample)

(define-collector collect-disk-sample)

(define-collector collect-temperature-sample)

(define-collector collect-battery-sample)

(define-collector collect-process-sample)

(defun normalize-collector-unavailability-reason (reason)
  (if (typep reason (quote collector-unavailability-reason))
      reason
      :read-failed))
