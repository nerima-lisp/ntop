(in-package #:ntop.app.presentation)

(defun %screen-layout-mode (state config)
  (let ((basic-p
          (or (ntop.app.domain:app-state-basic-mode-p state)
              (ntop.shared.application:application-config-basic-p config)))
        (expanded-p
          (or (ntop.app.domain:app-state-expanded-p state)
              (ntop.shared.application:application-config-expanded-p config))))
    (cond
      (basic-p :basic)
      (expanded-p :expanded)
      (t :default))))

(defun %screen-view-in-cycle-p (widget view)
  (or (null view)
      (loop with current-view = nil
            for next-view = (ntop.app.application:next-widget-view widget
                                                                   current-view)
            while next-view
            thereis (eq next-view view)
            do (setf current-view next-view))))

(defun %screen-effective-widget (widget state)
  (let* ((view
          (gethash widget (ntop.app.domain:app-state-widget-v-states state)))
         (view (and (%screen-view-in-cycle-p widget view) view)))
    (case widget
      (:memory (or view :memory))
      (:temperature (or view :temperature))
      (:disks (or view :disks))
      (otherwise widget))))

(defun %screen-layout-widget (widget state)
  (case (%screen-effective-widget widget state)
    (:cpu :cpu)
    (:memory :memory)
    (:gpu :gpu)
    (:network :network)
    (:disks :disk)
    (:disk-io-graph :disk-io-graph)
    (:temperature :temperature)
    (:temperature-graph :temperature-graph)
    (:battery :battery)
    (:process :process-table)
    (:basic-table-arrows :basic-table-arrows)))

(defun %screen-widget-base (widget)
  (case widget
    ((:memory :gpu) :memory)
    ((:disks :disk :disk-io-graph) :disks)
    ((:temperature :temperature-graph :battery) :temperature)
    ((:process :process-table) :process)
    (otherwise widget)))

(defun %screen-rectangle-union (left right)
  (let* ((x (min (ntop.ui:rectangle-x left) (ntop.ui:rectangle-x right)))
         (y (min (ntop.ui:rectangle-y left) (ntop.ui:rectangle-y right)))
         (right-edge
          (max (+ (ntop.ui:rectangle-x left) (ntop.ui:rectangle-width left))
               (+ (ntop.ui:rectangle-x right) (ntop.ui:rectangle-width right))))
         (bottom-edge
          (max (+ (ntop.ui:rectangle-y left) (ntop.ui:rectangle-height left))
               (+ (ntop.ui:rectangle-y right) (ntop.ui:rectangle-height right)))))
    (ntop.ui:make-rectangle :x
                            x
                            :y
                            y
                            :width
                            (- right-edge x)
                            :height
                            (- bottom-edge y))))

(defun %screen-layout-placements (columns rows state mode)
  (let* ((selected (ntop.app.domain:app-state-selected-widget state))
         (expanded-widget
           (and (eq mode :expanded) (%screen-effective-widget selected state)))
         (placements
           (ntop.ui:layout-widgets columns rows
                                   :mode mode
                                   :selected-widget expanded-widget
                                   :cpu-height 1
                                   :memory-rows 2
                                   :table-widget :process))
         (cpu (assoc :cpu placements :test (function eq)))
         (cpu-legend (assoc :cpu-legend placements :test (function eq))))
    (when (and cpu cpu-legend)
      (setf (cdr cpu) (%screen-rectangle-union (cdr cpu) (cdr cpu-legend))))
    (remove :cpu-legend placements :key (function car) :test (function eq))))

(defun %screen-config (config mode)
  (let ((copy (ntop.shared.application::copy-application-config config)))
    (setf (ntop.shared.application:application-config-basic-p copy) (eq mode
                                                                        :basic)
          (ntop.shared.application:application-config-expanded-p copy) (eq mode
                                                                           :expanded))
    copy))

(defun %screen-inner-rectangle (rectangle)
  (ntop.ui:make-rectangle :x
                          (1+ (ntop.ui:rectangle-x rectangle))
                          :y
                          (1+ (ntop.ui:rectangle-y rectangle))
                          :width
                          (max 0 (- (ntop.ui:rectangle-width rectangle) 2))
                          :height
                          (max 0 (- (ntop.ui:rectangle-height rectangle) 2))))

(defvar *screen-selection-active-p* nil)

(defun %screen-draw-frame (target rectangle config selected-p)
  (when
      (and (plusp (ntop.ui:rectangle-width rectangle))
           (plusp (ntop.ui:rectangle-height rectangle)))
    (cl-tty-kit:screen-draw-box target
                                (ntop.ui:rectangle-x rectangle)
                                (ntop.ui:rectangle-y rectangle)
                                (ntop.ui:rectangle-width rectangle)
                                (ntop.ui:rectangle-height rectangle)
                                :border
                                :single
                                :style
                                (ntop.ui:presentation-theme-style
                                 (ntop.shared.application:application-config-theme
                                  config)
                                 (if selected-p
                                     :highlighted-border
                                     :border)))))
(defvar *screen-table-theme* :default)
(defmethod ntop.ui:draw-table-text ((target cl-tty-kit:screen) x y text role)
  (cl-tty-kit:screen-write-string target
                                  x
                                  y
                                  text
                                  :style
                                  (ntop.ui:presentation-theme-style
                                   *screen-table-theme*
                                   (case role
                                     (:normal :text)
                                     (:header :table-header)
                                     (:selected :selected-text)
                                     (:title :widget-title)
                                     (otherwise :text)))))

(defun %screen-series (series-by-widget widget)
  (cdr (assoc widget series-by-widget :test #'eq)))

(defun %screen-cpu-series (series snapshot)
  (let ((sample
         (and snapshot
              (ntop.app.domain:collection-snapshot-cpu-sample snapshot)))
        (result (copy-list series)))
    (when sample
      (setf (getf result :sample) sample))
    result))

(defun %screen-memory-series (series snapshot)
  (let ((sample
         (and snapshot
              (ntop.app.domain:collection-snapshot-memory-sample snapshot)))
        (result (copy-list series)))
    (when sample
      (setf (getf result :ram-used-bytes) (ntop.memory.domain:memory-sample-ram-used-bytes
                                           sample)
            (getf result :ram-total-bytes) (ntop.memory.domain:memory-sample-ram-total-bytes
                                            sample)
            (getf result :swap-used-bytes) (ntop.memory.domain:memory-sample-swap-used-bytes
                                            sample)
            (getf result :swap-total-bytes) (ntop.memory.domain:memory-sample-swap-total-bytes
                                             sample)
            (getf result :cache-used-bytes) (ntop.memory.domain:memory-sample-cache-used-bytes
                                             sample)
            (getf result :cache-total-bytes) (ntop.memory.domain:memory-sample-cache-total-bytes
                                              sample)))
    result))

(defun %screen-network-series (series snapshot)
  (or series
      (let ((sample
             (and snapshot
                  (ntop.app.domain:collection-snapshot-network-sample snapshot))))
        (and sample (list sample)))))

(defun %screen-disk-rows (series snapshot)
  (or series
      (let ((sample
             (and snapshot
                  (ntop.app.domain:collection-snapshot-disk-sample snapshot))))
        (ntop.disk.domain:disk-sample-rows nil sample))))

(defun %screen-state-for-widget (state widget)
  (let ((candidate
          (gethash widget (ntop.app.domain:app-state-widget-v-states state))))
    (if (typep candidate 'ntop.ui:table-state)
        candidate
        (ntop.ui:make-table-state))))

(defun %screen-draw-widget (target widget
                                   rectangle
                                   selected-p
                                   state
                                   config
                                   series-by-widget
                                   snapshot
                                   unavailability-reasons)
  (let* ((basic-p (ntop.shared.application:application-config-basic-p config))
         (widget-rectangle
          (if basic-p
              rectangle
              (%screen-inner-rectangle rectangle)))
         (clickable-areas nil))
    (multiple-value-bind (widget-state widget-config widget-series)
        (%screen-widget-inputs widget
                               series-by-widget
                               snapshot
                               state
                               config
                               unavailability-reasons)
      (when (and (plusp (ntop.ui:rectangle-width widget-rectangle))
                 (plusp (ntop.ui:rectangle-height widget-rectangle)))
        (setf clickable-areas
              (ntop.ui:draw-widget widget
                                   target
                                   widget-rectangle
                                   widget-state
                                   widget-config
                                   widget-series)))
      (unless basic-p
        (%screen-draw-frame target rectangle config
                            (and selected-p *screen-selection-active-p*)))
      clickable-areas)))

(defun draw-application-screen (target state
                                       config
                                       snapshot
                                       columns
                                       rows
                                       unavailability-reasons
                                       &optional
                                       series-by-widget)
  (let* ((mode (%screen-layout-mode state config))
         (render-config (%screen-config config mode))
         (*screen-table-theme*
          (ntop.shared.application:application-config-theme render-config))
         (selected
          (%screen-widget-base
           (ntop.app.domain:app-state-selected-widget state)))
         (placements (%screen-layout-placements columns rows state mode))
         (clickable-areas nil))
    (dolist (placement placements)
      (let* ((layout-widget (car placement))
             (rectangle (cdr placement))
             (widget (%screen-layout-widget layout-widget state)))
        (when widget
          (setf clickable-areas
                (nconc clickable-areas
                       (%screen-draw-widget target
                                            widget
                                            rectangle
                                            (eq (%screen-widget-base layout-widget)
                                                selected)
                                            state
                                            render-config
                                            series-by-widget
                                            snapshot
                                            unavailability-reasons))))))
    (setf clickable-areas
          (nconc clickable-areas
                 (%screen-draw-active-process-modal target
                                                    state
                                                    render-config
                                                    snapshot
                                                    columns
                                                    rows)))
    clickable-areas))

(ntop.ui:define-widget :basic-table-arrows
                       (target rectangle state config series)
                       (declare (ignore state))
                       (let ((width (ntop.ui:rectangle-width rectangle))
                             (height (ntop.ui:rectangle-height rectangle)))
                         (when (and (plusp width) (plusp height))
                           (let* ((text
                                   (ntop.ui:truncate-table-text
                                    (or series "◄ Process ►")
                                    width))
                                  (text-width (ntop.ui:measure-table-text text))
                                  (x
                                   (+ (ntop.ui:rectangle-x rectangle)
                                      (floor (- width text-width) 2)))
                                  (y (ntop.ui:rectangle-y rectangle))
                                  (theme
                                   (ntop.shared.application:application-config-theme
                                    config))
                                  (style
                                   (ntop.ui:presentation-theme-style
                                    theme
                                    :widget-title)))
                             (cl-tty-kit:screen-write-string target
                                                             x
                                                             y
                                                             text
                                                             :style
                                                             style)))
                         nil))

(defun %screen-widget-inputs (widget series-by-widget
                                     snapshot
                                     state
                                     config
                                     unavailability-reasons)
  (case widget
    (:cpu
     (values (%screen-state-for-widget state :cpu)
             config
             (%screen-cpu-series (%screen-series series-by-widget :cpu)
                                 snapshot)))
    (:memory
     (values (%screen-state-for-widget state :memory)
             config
             (%screen-memory-series (%screen-series series-by-widget :memory)
                                    snapshot)))
    (:gpu
     (let* ((series (copy-list (%screen-series series-by-widget :gpu)))
            (unavailability-reason
             (ntop.app.application:application-unavailability-reason
              unavailability-reasons
              :gpu)))
       (setf (getf series :unavailability-reason) unavailability-reason)
       (values nil config series)))
    (:basic-table-arrows (values nil config "◄ Process ►"))
    (:network
     (values (ntop.network.presentation:make-network-presentation-state)
             config
             (%screen-network-series (%screen-series series-by-widget :network)
                                     snapshot)))
    (:disk
     (values (%screen-state-for-widget state :disks)
             (ntop.disk.presentation:make-disk-table-config)
             (%screen-disk-rows (%screen-series series-by-widget :disks)
                                snapshot)))
    (:disk-io-graph
     (values nil config (%screen-series series-by-widget :disk-io-graph)))
    (:temperature
     (values (%screen-state-for-widget state :temperature)
             (ntop.temperature.presentation:make-temperature-table-config
              :temperature-unit
              (ntop.shared.application:application-config-temperature-unit
               config))
             (and snapshot
                  (ntop.app.domain:collection-snapshot-temperature-sample
                   snapshot))))
    (:temperature-graph
     (values nil config
             (or (%screen-series series-by-widget :temperature-graph)
                 (%screen-series series-by-widget :temperature))))
    (:battery
     (values (%screen-state-for-widget state :battery)
             config
             (or (%screen-series series-by-widget :battery)
                 (and snapshot
                      (ntop.app.domain:collection-snapshot-battery-sample
                       snapshot)))))
    (:process-table
     (let* ((sort-state (%screen-process-sort-state config state))
            (rows
             (%screen-process-rows snapshot
                                   state
                                   config
                                   sort-state
                                   (%screen-series series-by-widget :process))))
       (multiple-value-bind (table-state table-config)
           (%screen-process-widget-config snapshot
                                          state
                                          config
                                          (eq
                                           (ntop.app.domain:app-state-selected-widget
                                            state)
                                           :process)
                                          sort-state)
         (values table-state table-config rows))))))
