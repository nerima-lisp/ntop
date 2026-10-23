(in-package #:ntop.app.domain)

(defstruct app-state
  (selected-widget :process)
  expanded-p
  basic-mode-p
  paused-p
  (zoom-seconds 15)
  (scroll-offset 0)
  active-dialog
  (widget-v-states (make-hash-table :test #'eq)))

(defstruct input-context
  selected-widget
  active-dialog
  basic-mode-p
  search-mode-p)

(defstruct app-action
  kind
  value)

(defparameter *app-action-kinds*
  '(:navigation
    :select
    :toggle-widget-v
    :expand
    :basic
    :pause
    :zoom
    :history-scroll
    :return-to-now
    :help
    :search
    :dialog
    :toggle-process-grouping
    :sort-processes
    :reverse-process-sort
    :toggle-process-tree
    :toggle-process-tree-branch
    :set-process-display-mode
    :process-search
    :process-terminate
    :quit))
(defstruct (collection-snapshot (:copier nil))
  (sampled-at (error "Snapshot timestamp is required.")
              :type ntop.shared.domain:monotonic-seconds
              :read-only t)
  (cpu-sample nil :type (or null ntop.cpu.domain:cpu-sample) :read-only t)
  (memory-sample nil :type (or null ntop.memory.domain:memory-sample) :read-only t)
  (gpu-sample nil :type (or null ntop.gpu.domain:gpu-sample) :read-only t)
  (network-sample nil :type (or null ntop.network.domain:network-sample) :read-only t)
  (disk-sample nil :type (or null ntop.disk.domain:disk-sample) :read-only t)
  (temperature-sample nil :type (or null ntop.temperature.domain:temperature-sample) :read-only t)
  (battery-sample nil :type (or null ntop.battery.domain:battery-sample) :read-only t)
  (process-sample nil :type (or null ntop.process.domain:process-sample) :read-only t))
