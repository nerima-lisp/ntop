(in-package #:ntop.app.domain)

(progn
  (defstruct (app-state (:constructor make-app-state-raw))
    selected-widget
    expanded-p
    basic-mode-p
    paused-p
    zoom-seconds
    scroll-offset
    active-dialog
    widget-v-states)
  (defun make-app-state (&key (selected-widget :process)
                              expanded-p
                              basic-mode-p
                              paused-p
                              (zoom-seconds 15)
                              (scroll-offset 0)
                              active-dialog
                              (widget-v-states (make-hash-table :test #'eq)))
    (make-app-state-raw :selected-widget
                        selected-widget
                        :expanded-p
                        expanded-p
                        :basic-mode-p
                        basic-mode-p
                        :paused-p
                        paused-p
                        :zoom-seconds
                        zoom-seconds
                        :scroll-offset
                        scroll-offset
                        :active-dialog
                        active-dialog
                        :widget-v-states
                        widget-v-states)))

(defstruct input-context
  selected-widget
  active-dialog
  basic-mode-p
  search-mode-p)

(defstruct app-action
  kind
  value)

(defparameter *app-action-kinds*
  '(:navigation :select
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

(ntop.shared.domain:define-value-type collection-snapshot
  (sampled-at (error "Snapshot timestamp is required.")
              :type ntop.shared.domain:monotonic-seconds)
  (cpu-sample nil
              :type (or null ntop.cpu.domain:cpu-sample))
  (memory-sample nil
                  :type (or null ntop.memory.domain:memory-sample))
  (gpu-sample nil
              :type (or null ntop.gpu.domain:gpu-sample))
  (network-sample nil
                  :type (or null ntop.network.domain:network-sample))
  (disk-sample nil
               :type (or null ntop.disk.domain:disk-sample))
  (temperature-sample nil
                      :type (or null
                                ntop.temperature.domain:temperature-sample))
  (battery-sample nil
                  :type (or null ntop.battery.domain:battery-sample))
  (process-sample nil
                  :type (or null ntop.process.domain:process-sample)))
