;; Keep the org-required CL-only use list uniform across project packages.
(defmacro define-ntop-package (name &body clauses)
  `(defpackage ,name (:use #:cl) ,@clauses))

(define-ntop-package #:ntop.shared.domain (:documentation "Shared measurement values, time, history and units.")
  (:export
   ;; WP and F labels identify the sole owner of each export section.
   ;; Export lists start empty; each owner adds public symbols in its section.
   ;; WP0-3: measurement values and units.
   ;; WP2: retained time-series history.
   ))

(define-ntop-package #:ntop.shared.application (:documentation "Shared configuration values and collection contracts.")
  (:export
   ;; WP0-4: configuration and collection ports.
   ))

(define-ntop-package #:ntop.platform.darwin (:documentation "Darwin system-call bindings and operating-system data.")
  (:export
   ;; WP0-7, infrastructure-base.lisp: sysctl and mach bindings.
   ;; WP0-7, infrastructure-cf.lisp: CoreFoundation and IOKit bindings.
   ;; F1, infrastructure-ioreport.lisp: IOReport bindings.
   ;; F3, infrastructure-ioaccel.lisp: IOAccelerate bindings.
   ;; F4, infrastructure-ifmib.lisp: interface MIB bindings.
   ;; F5, infrastructure-fs.lisp: filesystem bindings.
   ;; F6, infrastructure-iohid.lisp: IOHID bindings.
   ;; F7, infrastructure-power.lisp: power bindings.
   ;; F8a, infrastructure-libproc.lisp: libproc bindings.
   ))

(define-ntop-package #:ntop.platform.linux (:documentation "Linux procfs and sysfs boundary operations.")
  (:export
   ;; WP9: Linux platform bindings.
   ))

(define-ntop-package #:ntop.ui (:documentation "Shared rendering and input protocols.")
  (:export
   ;; WP0-4: widget and input protocols.
   ;; WP1: formatting and themes.
   ;; WP5: layout.
   ;; WP6: charts and gauges.
   ;; WP7: tables.
   ;; WP10: input events.
   ))

(define-ntop-package #:ntop.cpu.domain (:documentation "CPU domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F1: feature domain behavior.
   ))

(define-ntop-package #:ntop.cpu.infrastructure (:documentation "CPU operating-system data access.")
  (:export
   ;; F1: feature data collection.
   ))

(define-ntop-package #:ntop.cpu.presentation (:documentation "CPU widget rendering.")
  (:export
   ;; F1: feature presentation.
   ))

(define-ntop-package #:ntop.memory.domain (:documentation "Memory domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F2: feature domain behavior.
   ))

(define-ntop-package #:ntop.memory.infrastructure (:documentation "Memory operating-system data access.")
  (:export
   ;; F2: feature data collection.
   ))

(define-ntop-package #:ntop.memory.presentation (:documentation "Memory widget rendering.")
  (:export
   ;; F2: feature presentation.
   ))

(define-ntop-package #:ntop.gpu.domain (:documentation "GPU domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F3: feature domain behavior.
   ))

(define-ntop-package #:ntop.gpu.infrastructure (:documentation "GPU operating-system data access.")
  (:export
   ;; F3: feature data collection.
   ))

(define-ntop-package #:ntop.gpu.presentation (:documentation "GPU widget rendering.")
  (:export
   ;; F3: feature presentation.
   ))

(define-ntop-package #:ntop.network.domain (:documentation "Network domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F4: feature domain behavior.
   ))

(define-ntop-package #:ntop.network.infrastructure (:documentation "Network operating-system data access.")
  (:export
   ;; F4: feature data collection.
   ))

(define-ntop-package #:ntop.network.presentation (:documentation "Network widget rendering.")
  (:export
   ;; F4: feature presentation.
   ))

(define-ntop-package #:ntop.disk.domain (:documentation "Disk domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F5: feature domain behavior.
   ))

(define-ntop-package #:ntop.disk.infrastructure (:documentation "Disk operating-system data access.")
  (:export
   ;; F5: feature data collection.
   ))

(define-ntop-package #:ntop.disk.presentation (:documentation "Disk widget rendering.")
  (:export
   ;; F5: feature presentation.
   ))

(define-ntop-package #:ntop.temperature.domain (:documentation "Temperature domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F6: feature domain behavior.
   ))

(define-ntop-package #:ntop.temperature.infrastructure (:documentation "Temperature operating-system data access.")
  (:export
   ;; F6: feature data collection.
   ))

(define-ntop-package #:ntop.temperature.presentation (:documentation "Temperature widget rendering.")
  (:export
   ;; F6: feature presentation.
   ))

(define-ntop-package #:ntop.battery.domain (:documentation "Battery domain values and calculations.")
  (:export
   ;; WP0-4: agreed feature types.
   ;; F7: feature domain behavior.
   ))

(define-ntop-package #:ntop.battery.infrastructure (:documentation "Battery operating-system data access.")
  (:export
   ;; F7: feature data collection.
   ))

(define-ntop-package #:ntop.battery.presentation (:documentation "Battery widget rendering.")
  (:export
   ;; F7: feature presentation.
   ))

(define-ntop-package #:ntop.process.domain (:documentation "Process values, search language and process operations.")
  (:export
   ;; WP0-4: agreed process types.
   ;; WP3: process search language.
   ;; WP4: process domain behavior.
   ))

(define-ntop-package #:ntop.process.application (:documentation "Process search and termination use cases.")
  (:export
   ;; WP0-4: agreed use-case contracts.
   ;; WP4: process use cases.
   ))

(define-ntop-package #:ntop.process.infrastructure (:documentation "Process data access and operating-system operations.")
  (:export
   ;; F8a: process data collection.
   ))

(define-ntop-package #:ntop.process.presentation (:documentation "Process table, search and termination dialog rendering.")
  (:export
   ;; F8b: process presentation.
   ))

(define-ntop-package #:ntop.app.domain (:documentation "Application state and bottom 0.14.9 rendering compatibility data.")
  (:export
   ;; WP0-4: application state and actions.
   ;; WP0-5: btm parity data.
   ))

(define-ntop-package #:ntop.app.application (:documentation "Application event handling and composition.")
  (:export
   ;; WP10: key bindings.
   ;; WP12a: event loop and sampler.
   ;; WP12b: application composition.
   ))

(define-ntop-package #:ntop.app.infrastructure (:documentation "Terminal session, signal and sampler infrastructure.")
  (:export
   ;; WP12a: terminal and collection infrastructure.
   ))

(define-ntop-package #:ntop.app.presentation (:documentation "Whole-screen layout and application views.")
  (:export
   ;; WP12b: whole-screen presentation.
   ))

(define-ntop-package #:ntop.cli (:documentation "Command-line flags and process entry point.")
  (:export
   ;; WP13: command-line interface.
   ))

(define-ntop-package #:ntop.test (:documentation "Test definitions for ntop.")
  (:export
   ;; WP0-1: foundation smoke tests.
   ))
