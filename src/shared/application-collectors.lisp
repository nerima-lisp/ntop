(in-package #:ntop.shared.application)

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
