(in-package #:ntop.app.domain)

(defun validate-typed-slots (instance specifications)
  (dolist (specification specifications instance)
    (destructuring-bind (accessor expected-type) specification
      (let ((value (funcall (symbol-function accessor) instance)))
        (if (typep value expected-type)
            nil
            (error (quote type-error) :datum value :expected-type expected-type))))))

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

(progn
  (defstruct
      (collection-snapshot (:constructor make-collection-snapshot-raw)
                           (:copier nil))
    (sampled-at (error "Snapshot timestamp is required.") :read-only t)
    (cpu-sample nil :read-only t)
    (memory-sample nil :read-only t)
    (gpu-sample nil :read-only t)
    (network-sample nil :read-only t)
    (disk-sample nil :read-only t)
    (temperature-sample nil :read-only t)
    (battery-sample nil :read-only t)
    (process-sample nil :read-only t))
  (defun make-collection-snapshot (&rest initargs)
    (let ((instance (apply #'make-collection-snapshot-raw initargs)))
      (validate-typed-slots instance
                            '((collection-snapshot-sampled-at
                               ntop.shared.domain:monotonic-seconds)
                              (collection-snapshot-cpu-sample
                               (or null ntop.cpu.domain:cpu-sample))
                              (collection-snapshot-memory-sample
                               (or null ntop.memory.domain:memory-sample))
                              (collection-snapshot-gpu-sample
                               (or null ntop.gpu.domain:gpu-sample))
                              (collection-snapshot-network-sample
                               (or null ntop.network.domain:network-sample))
                              (collection-snapshot-disk-sample
                               (or null ntop.disk.domain:disk-sample))
                              (collection-snapshot-temperature-sample
                               (or null
                                   ntop.temperature.domain:temperature-sample))
                              (collection-snapshot-battery-sample
                               (or null ntop.battery.domain:battery-sample))
                              (collection-snapshot-process-sample
                               (or null ntop.process.domain:process-sample))))
      instance)))
