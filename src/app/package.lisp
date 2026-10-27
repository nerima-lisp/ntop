(define-ntop-package #:ntop.app.domain (:documentation "Application state and parity data.")
  (:export
 ;; WP0-4: application state and actions.
 #:app-state #:make-app-state #:app-state-selected-widget #:app-state-expanded-p
 #:app-state-basic-mode-p #:app-state-paused-p #:app-state-zoom-seconds
 #:app-state-scroll-offset #:app-state-active-dialog #:app-state-widget-v-states
 #:input-context #:make-input-context #:input-context-selected-widget
 #:input-context-active-dialog #:input-context-basic-mode-p
 #:input-context-search-mode-p #:app-action #:make-app-action #:app-action-kind
 #:app-action-value #:*app-action-kinds*
 #:collection-snapshot #:collection-snapshot-p #:make-collection-snapshot
 #:collection-snapshot-sampled-at #:collection-snapshot-cpu-sample
 #:collection-snapshot-memory-sample #:collection-snapshot-gpu-sample
 #:collection-snapshot-network-sample #:collection-snapshot-disk-sample
 #:collection-snapshot-temperature-sample #:collection-snapshot-battery-sample
 #:collection-snapshot-process-sample
 ;; WP0-5: typed btm parity data.
 #:btm-parity-catalog #:parity-entry-kind
 #:parity-key #:parity-key-action #:parity-key-keys
 #:parity-key-contexts #:parity-key-behavior #:parity-key-source
 #:parity-mouse #:parity-mouse-action #:parity-mouse-gesture
 #:parity-mouse-contexts #:parity-mouse-behavior #:parity-mouse-condition
 #:parity-mouse-source
 #:parity-flag #:parity-flag-snake-case #:parity-flag-short
 #:parity-flag-aliases #:parity-flag-value-type #:parity-flag-values
 #:parity-flag-condition #:parity-flag-source #:parity-flag-kebab-case
 #:parity-column #:parity-column-header
 #:parity-column-field #:parity-column-aliases #:parity-column-condition
 #:parity-column-default #:parity-column-slot #:parity-column-source
 #:parity-search-field #:parity-search-field-field
 #:parity-search-field-tokens #:parity-search-field-condition
 #:parity-search-field-implicit #:parity-search-field-source
 #:parity-extension #:parity-extension-action
 #:parity-extension-key #:parity-extension-contexts #:parity-extension-behavior
 #:parity-extension-relation #:parity-extension-source))

(define-ntop-package #:ntop.app.application
    (:documentation "Application event handling.")
  (:export
   ;; WP10: key bindings.
   #:application-keymap-state #:application-keymap-state-p
   #:make-application-keymap-state #:dispatch-application-input
   ;; WP12a: event loop and sampler.
   #:collect-application-snapshot #:collect-application-result
   #:application-collection-result #:application-collection-result-p
   #:application-collection-result-snapshot
   #:application-collection-result-unavailability-reasons
   #:application-unavailability-reasons #:application-unavailability-reasons-p
   #:application-unavailability-reason
   #:run-fixed-rate-sampler
   #:run-application-loop
   ;; WP12b: application composition.
   #:make-application-composition
   #:application-composition-history-stores
   #:application-composition-on-action
   #:application-composition-on-snapshot
   #:application-composition-render
   #:next-widget-view))

(define-ntop-package #:ntop.app.infrastructure (:documentation "Terminal and sampler lifecycle.")
  (:export
   ;; WP12a: terminal and collection infrastructure.
   #:system-clock #:make-system-clock
   #:start-collection-worker #:collection-worker-channel
   #:stop-collection-worker #:call-with-terminal-session
   #:run-application
   ;; WP13: application startup wiring.
   #:make-platform-collectors
   ))

(define-ntop-package #:ntop.app.presentation (:documentation "Screen layout and widgets.")
                     (:export
                      ;; WP12b: whole-screen presentation.
                      #:make-help-state #:help-state-p #:help-state-selected-widget
                      #:help-state-query #:help-state-search-active-p
                      #:help-state-sectioned-p #:help-state-scroll-offset
                      #:help-state-transition
                      #:draw-application-screen))
