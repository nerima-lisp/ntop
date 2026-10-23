(in-package #:asdf-user)

(asdf:defsystem "ntop"
  :description "A terminal system monitor."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/ntop"
  :bug-tracker "https://github.com/nerima-lisp/ntop/issues"
  :source-control (:git "https://github.com/nerima-lisp/ntop.git")
  :depends-on ()
  :pathname "."
  :serial t
  :components
  (;; WP and F labels identify the owner of each reserved component section.
   ;; List files in dependency order within their section.
   ;; WP0-1: package declarations.
   (:file "src/package")
   ;; WP0-3: src/shared/domain-*.lisp.
   ;; WP0-4: shared application, UI protocols, app state and feature types.
   ;; WP0-5: src/app/domain-parity.lisp.
   ;; WP0-7: Darwin base and CoreFoundation bindings.
   ;; WP1: shared UI formatting and themes.
   ;; WP2: shared time-series history.
   ;; WP3: process search language in src/feature/process/domain-query*.lisp.
   ;; WP4: process domain and application components.
   ;; WP5: shared UI layout components.
   ;; WP6: shared UI chart and gauge components.
   ;; WP7: shared UI table components.
   ;; WP9: Linux platform components.
   ;; WP10: application key bindings and UI input events.
   ;; WP12a: application loop, sampler and infrastructure.
   ;; WP12b: application composition and presentation.
   ;; WP13: command-line components.
   ;; F1: CPU feature components and Darwin IOReport bindings.
   ;; F2: memory feature components.
   ;; F3: GPU feature components and Darwin IOAccelerate bindings.
   ;; F4: network feature components and Darwin interface MIB bindings.
   ;; F5: disk feature components and Darwin filesystem bindings.
   ;; F6: temperature feature components and Darwin IOHID bindings.
   ;; F7: battery feature components and Darwin power bindings.
   ;; F8a: process infrastructure and Darwin libproc bindings.
   ;; F8b: process presentation components.
   )
  :in-order-to ((asdf:test-op (asdf:test-op "ntop/test"))))

(asdf:defsystem "ntop/test"
  :description "Test system for ntop."
  :author "takeokunn <bararararatty@gmail.com>"
  :maintainer "takeokunn <bararararatty@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :homepage "https://github.com/nerima-lisp/ntop"
  :bug-tracker "https://github.com/nerima-lisp/ntop/issues"
  :source-control (:git "https://github.com/nerima-lisp/ntop.git")
  :depends-on ("ntop" "cl-weave")
  :pathname "."
  :serial t
  :components
  (;; WP0-1: test package and foundation smoke test.
   (:file "t/package")
   (:file "t/smoke-test")
   ;; WP0-3: shared domain tests.
   ;; WP0-4: application, UI protocol and feature type tests.
   ;; WP0-5: btm parity tests.
   ;; WP0-6: E2E harness and golden tests.
   ;; WP0-7: Darwin base and CoreFoundation tests.
   ;; WP1: formatting and theme tests.
   ;; WP2: history tests.
   ;; WP3: process query tests.
   ;; WP4: process domain and use-case tests.
   ;; WP5: layout tests.
   ;; WP6: chart and gauge tests.
   ;; WP7: table tests.
   ;; WP9: Linux platform tests.
   ;; WP10: input and key binding tests.
   ;; WP12a: event loop, sampler and infrastructure tests.
   ;; WP12b: application composition tests.
   ;; WP13: CLI tests.
   ;; F1: CPU tests.
   ;; F2: memory tests.
   ;; F3: GPU tests.
   ;; F4: network tests.
   ;; F5: disk tests.
   ;; F6: temperature tests.
   ;; F7: battery tests.
   ;; F8a: process infrastructure tests.
   ;; F8b: process presentation tests.
   )
  :perform (asdf:test-op (operation system)
             (declare (ignore operation system))
             (unless (uiop:symbol-call '#:cl-weave '#:run-all
                                       :reporter :spec
                                       :pass-with-no-tests nil)
               (error "ntop/test reported a failure."))))
