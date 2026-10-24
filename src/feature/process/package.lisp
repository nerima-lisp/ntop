(define-ntop-package #:ntop.process.domain
    (:documentation "Process records and search language.")
  (:export
   ;; WP0-4: process types and search contract.
   #:process-thread #:make-process-thread #:process-thread-thread-id #:process-thread-name
   #:process-thread-cpu-time-ns #:process-thread-state #:process-thread-priority
   #:process-thread-nice #:process-record #:make-process-record #:process-record-pid
   #:process-record-ppid #:process-record-name #:process-record-command
   #:process-record-cpu-time-ns #:process-record-resident-memory-bytes
   #:process-record-virtual-memory-bytes #:process-record-read-bytes
   #:process-record-write-bytes #:process-record-uid #:process-record-user-name
   #:process-record-state #:process-record-start-time-unix-seconds
   #:process-record-priority #:process-record-nice #:process-record-kernel-thread-p
   #:process-record-threads #:process-record-gpu-usage-percent
   #:process-record-gpu-memory-bytes #:process-record-ps-fallback-cpu-percent
   #:process-signal #:make-process-signal #:process-signal-operating-system
   #:process-signal-name #:process-signal-number #:process-sample
   #:make-process-sample #:process-sample-processes #:process-sample-signals
   #:process-row #:make-process-row #:process-row-p
   #:process-row-count #:process-row-pid #:process-row-ppid #:process-row-name #:process-row-command
   #:process-row-cpu-percent #:process-row-memory-percent
   #:process-row-memory-bytes #:process-row-virtual-memory-bytes
   #:process-row-read-bytes-per-second #:process-row-write-bytes-per-second
   #:process-row-total-read-bytes #:process-row-total-write-bytes
   #:process-row-user-name #:process-row-state #:process-row-elapsed-seconds
   #:process-row-priority #:process-row-nice #:process-row-gpu-usage-percent
   #:process-row-gpu-memory-bytes #:process-row-gpu-memory-percent
   ;; WP3: process search language.
   #:process-search-syntax-error #:make-process-search-syntax-error
   #:process-search-syntax-error-start-offset
   #:process-search-syntax-error-end-offset
   #:process-search-syntax-error-message #:compile-process-search
   ;; WP4: process domain behavior.
   #:process-read-bytes-per-second #:process-write-bytes-per-second
   #:process-cpu-percent #:process-row-from-samples
   #:group-process-domain-rows
   #:process-sort-state #:make-process-sort-state
   #:process-sort-state-key #:process-sort-state-order
   #:process-sort-state-memory-mode #:process-sort-key-from-character
   #:process-sort-select-key #:process-sort-invert #:sort-process-domain-rows
   #:process-tree-state #:make-process-tree-state
   #:process-tree-state-collapsed-p #:process-tree-state-set-collapsed
   #:process-tree-state-toggle #:build-process-tree
   #:process-tree-entry #:process-tree-entry-p #:process-tree-entry-row
   #:process-tree-entry-depth #:process-tree-entry-last-child-p
   #:process-tree-entry-has-children-p #:process-tree-entry-collapsed-p
   #:process-tree-visible-rows))

(define-ntop-package #:ntop.process.application
    (:documentation "Process search and termination.")
  (:export
   ;; WP0-4: agreed use-case contracts.
   ;; WP4: process use cases.
   #:process-search-state #:make-process-search-state #:process-search-state-p
   #:process-search-state-query #:process-search-state-predicate
   #:process-search-state-syntax-error #:compile-process-search-state
   #:find-process-index-by-pid #:find-process-group-index-by-key
   #:process-termination-state #:process-termination-state-p
   #:make-process-termination-state
   #:process-termination-state-selected-signal
   #:process-termination-state-target-pids
   #:process-termination-state-send-status
   #:update-process-termination-targets #:select-process-signal
   #:send-process-termination))

(define-ntop-package #:ntop.process.infrastructure
    (:documentation "Process data and signals.")
  (:export
   ;; F8a: process data collection.
   #:process-collector #:make-process-collector #:send-process-signal))

(define-ntop-package #:ntop.process.presentation (:documentation "Process tables and dialogs.")
  (:export
   ;; F8b: process presentation.
   #:make-process-table-config
   #:process-table-config-sort-state
   #:process-table-config-grouped-p
   #:process-table-config-grouped-by-tab-p
   #:process-table-config-show-command-p
   #:process-table-config-memory-mode
   #:process-table-config-gpu-available-p
   #:process-table-config-focused-p
   #:process-table-config-show-current-entry-when-unfocused-p
   #:process-table-config-show-scroll-position-p
   #:process-table-config-column-ids
   #:process-table-visible-columns #:process-table-sort-options
   #:make-process-search-field-state
   #:process-search-field-state-application-state
   #:process-search-field-state-regex-p
   #:process-search-field-state-case-sensitive-p
   #:process-search-field-state-whole-word-p
   #:transition-process-search-field
   #:make-process-sort-menu-state #:move-process-sort-menu-selection
   #:select-process-sort-menu-choice #:apply-process-sort-menu-choice
   #:open-process-termination-dialog
   #:move-process-termination-dialog-selection
   #:select-process-termination-dialog-choice
   #:confirm-process-termination-dialog
   #:copy-process-termination-dialog-state))
