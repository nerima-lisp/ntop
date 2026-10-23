(in-package #:asdf-user)

(asdf:defsystem "ntop"
  :description "A terminal system monitor."
  :author "ntop contributors"
  :maintainer "ntop contributors"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/ntop"
  :bug-tracker "https://github.com/nerima-lisp/ntop/issues"
  :source-control (:git "https://github.com/nerima-lisp/ntop.git")
  :depends-on ("cl-tty-kit" "cl-cli" "cl-regex-kit" "cl-host-kit"
               "cl-concurrent-kit" "cl-date-kit" "cl-parser-kit" "sb-posix")
  :pathname "."
  :serial t
  :components
  (;; WP0-1: project packages.
   (:file "src/package")
   ;; WP0-3: shared domain types and rules.
   (:file "src/shared/domain-measurement")
   (:file "src/shared/domain-rate")
   (:file "src/shared/domain-time")
   (:file "src/shared/domain-units")
   ;; WP0-4: shared configuration, collector ports and UI protocols.
   (:file "src/shared/application-config")
   (:file "src/shared/application-collectors")
   (:file "src/ui/presentation-protocol")
   (:file "src/ui/presentation-input")
   (:file "src/feature/battery/domain-types")
   (:file "src/feature/cpu/domain-types")
   (:file "src/feature/disk/domain-types")
   (:file "src/feature/gpu/domain-types")
   (:file "src/feature/memory/domain-types")
   (:file "src/feature/network/domain-types")
   (:file "src/feature/process/domain-search-protocol")
   (:file "src/feature/process/domain-types")
   (:file "src/feature/temperature/domain-types")
   (:file "src/app/domain-state")
   ;; WP0-5: btm compatibility tables and rules.
   (:file "src/app/domain-parity")
   (:file "src/app/domain-parity-keys")
   (:file "src/app/domain-parity-input")
   (:file "src/app/domain-parity-flags")
   (:file "src/app/domain-parity-process")
   (:file "src/app/domain-parity-reader")
   ;; WP10: semantic data and context-aware bindings from the typed parity catalog.
   (:file "src/app/application-keymap-semantics")
   ;; WP0-7: Darwin system and CoreFoundation bindings.
   (:file "src/platform/darwin/infrastructure-base")
   (:file "src/platform/darwin/infrastructure-cf")
   (:file "src/app/application-keymap")
   ;; WP1: shared formatting and themes.
   (:file "src/ui/presentation-format-rules")
   (:file "src/ui/presentation-format-numeric")
   (:file "src/ui/presentation-format-time")
   (:file "src/ui/presentation-theme")
   ;; WP2: time-series history.
   (:file "src/shared/domain-history")
   ;; WP6: charts and resource gauges.
   (:file "src/ui/presentation-chart-model")
   (:file "src/ui/presentation-chart")
   (:file "src/ui/presentation-gauge")
   ;; WP3: process search language.
   (:file "src/feature/process/domain-query")
   (:file "src/feature/process/domain-query-parser")
   (:file "src/feature/process/domain-query-print")
   (:file "src/feature/process/domain-query-compile")
   (:file "src/feature/process/domain-metrics")
   (:file "src/feature/process/domain-grouping")
   (:file "src/feature/process/domain-sorting")
   (:file "src/feature/process/domain-tree")
   (:file "src/feature/process/application-state")
   (:file "src/feature/process/application-selection")
   (:file "src/feature/process/application-termination")
   ;; WP4: process domain.
   ;; WP5, WP6 and WP7: UI layout, charts, gauges and tables.
   (:file "src/ui/presentation-layout")
   ;; WP7: table components.
   (:file "src/ui/presentation-table-layout")
   (:file "src/ui/presentation-table")
   ;; WP9: Linux procfs and sysfs boundary.
   (:file "src/platform/linux/infrastructure-base")
   (:file "src/platform/linux/infrastructure-proc-disk")
   (:file "src/platform/linux/infrastructure-proc-process")
   (:file "src/platform/linux/infrastructure-proc-system")
   (:file "src/platform/linux/infrastructure-sysfs-device")
   (:file "src/platform/linux/infrastructure-sysfs-network")
   ;; F2: memory feature components.
   (:file "src/feature/memory/domain-calculations")
   (:file "src/feature/memory/infrastructure")
   (:file "src/feature/memory/presentation")
   (:file "src/feature/gpu/domain-metrics")
   (:file "src/feature/gpu/infrastructure-collector")
   (:file "src/platform/darwin/infrastructure-ioaccel")
   (:file "src/feature/gpu/infrastructure-darwin")
   (:file "src/feature/gpu/presentation")
   ;; WP12a: application loop and infrastructure.
   )
  :build-operation "program-op"
  :build-pathname "ntop"
  :entry-point "ntop.cli:main"
  :perform (asdf:program-op (operation system)
             (sb-ext:save-lisp-and-die
              (asdf:output-file operation system)
              :executable t
              :compression t
              :save-runtime-options t
              :toplevel (uiop:ensure-function
                         (asdf/system:component-entry-point system))))
  :in-order-to ((asdf:test-op (asdf:test-op "ntop/test"))))

(asdf:defsystem "ntop/test"
  :description "Test system for ntop."
  :author "ntop contributors"
  :maintainer "ntop contributors"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/ntop"
  :bug-tracker "https://github.com/nerima-lisp/ntop/issues"
  :source-control (:git "https://github.com/nerima-lisp/ntop.git")
  :depends-on ("ntop" "cl-weave" "cl-cli" "cl-host-kit" "cl-tty-kit")
  :pathname "."
  :serial t
  :components
  (;; WP0-1: test package and smoke tests.
   (:file "t/package")
   (:file "t/smoke-test")
   ;; WP0-3: shared domain tests.
   (:file "t/shared/domain-measurement-test")
   (:file "t/shared/domain-rate-test")
   (:file "t/shared/domain-time-test")
   (:file "t/shared/domain-units-test")
   ;; WP0-4: application, UI protocol and feature type tests.
   (:file "t/shared/application-config-test")
   (:file "t/shared/application-collectors-test")
   (:file "t/app/domain-state-test")
   (:file "t/ui/presentation-protocol-test")
   (:file "t/ui/presentation-format-axes-test")
   (:file "t/ui/presentation-format-time-test")
   (:file "t/ui/presentation-format-values-test")
   (:file "t/ui/presentation-theme-test")
   (:file "t/feature/battery/domain-types-test")
   (:file "t/feature/cpu/domain-types-test")
   (:file "t/feature/disk/domain-types-test")
   (:file "t/feature/gpu/domain-types-test")
   (:file "t/feature/memory/domain-types-test")
   (:file "t/feature/network/domain-types-test")
   (:file "t/feature/process/domain-search-protocol-test")
   (:file "t/feature/process/domain-query-test")
   (:file "t/feature/process/domain-metrics-test")
   (:file "t/feature/process/domain-grouping-test")
   (:file "t/feature/process/domain-sorting-test")
   (:file "t/feature/process/domain-tree-test")
   (:file "t/feature/process/application-state-test")
   (:file "t/feature/process/application-selection-test")
   (:file "t/feature/process/application-termination-test")
   (:file "t/feature/process/domain-types-test")
   (:file "t/feature/temperature/domain-types-test")
   ;; WP0-5: btm compatibility tests.
   (:file "t/app/domain-parity-test")
   ;; WP0-7: Darwin base and CoreFoundation tests.
   (:file "t/platform/darwin/infrastructure-cf-test")
   (:file "t/platform/darwin/infrastructure-base-test")
   (:file "t/platform/darwin/infrastructure-mach-test")
   ;; WP5: layout tests.
   (:file "t/ui/presentation-layout-test")
   ;; WP6: chart and gauge tests.
   (:file "t/ui/presentation-chart-test-support")
   (:file "t/ui/presentation-chart-model-test")
   (:file "t/ui/presentation-chart-legend-test")
   (:file "t/ui/presentation-chart-axis-test")
   (:file "t/ui/presentation-chart-drawing-test")
   (:file "t/ui/presentation-gauge-test")
   ;; WP7: table tests.
   (:file "t/ui/presentation-table-layout-test")
   ;; WP10: terminal input translation tests.
   (:file "t/ui/presentation-input-test")
   (:file "t/ui/presentation-table-test")
   ;; WP2: history tests.
   (:file "t/shared/domain-history-test")
   ;; WP2: history tests.
   (:file "t/shared/domain-history-stress-test")
   ;; WP3 and WP4: process search and domain tests.
   ;; WP9: Linux boundary tests.
   (:file "t/platform/linux/infrastructure-base-test")
   (:file "t/platform/linux/infrastructure-proc-disk-test")
   (:file "t/platform/linux/infrastructure-proc-process-test")
   (:file "t/platform/linux/infrastructure-proc-system-test")
   (:file "t/platform/linux/infrastructure-sysfs-device-test")
   (:file "t/platform/linux/infrastructure-sysfs-network-test")
   ;; F2: memory feature tests.
   (:file "t/feature/memory/domain-calculations-test")
   (:file "t/feature/memory/infrastructure-test")
   (:file "t/feature/memory/infrastructure-darwin-test")
   (:file "t/feature/memory/presentation-test")
   ;; F3: GPU feature tests.
   (:file "t/feature/gpu/domain-metrics-test")
   (:file "t/platform/darwin/infrastructure-ioaccel-test")
   (:file "t/feature/gpu/infrastructure-darwin-test")
   (:file "t/feature/gpu/presentation-test")
   ;; WP10: application keymap tests.
   (:file "t/app/application-keymap-test-support")
   (:file "t/app/application-keymap-test")
   (:file "t/app/application-keymap-sequences-test")
   ;; WP12a: application loop and collection tests.
   )
  :perform (asdf:test-op (operation system)
  (declare (ignore operation system))
  (unless
      (uiop:symbol-call "CL-WEAVE" "RUN-ALL"
                        :reporter :spec
                        :timeout-ms 40000
                        :max-workers 1
                        :pass-with-no-tests nil)
    (error "ntop/test reported failed tests."))))
