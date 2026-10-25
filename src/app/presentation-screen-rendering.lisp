(in-package #:ntop.app.presentation)

(defun %screen-widget-inputs (widget series-by-widget
                                     snapshot
                                     state
                                     config
                                     unavailability-reasons
                                     &optional current-time-unix-seconds selected-p)
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
    (:basic-table-arrows
     (values nil config (%screen-basic-table-heading state)))
    (:network
     (values (ntop.network.presentation:make-network-presentation-state)
             config
             (%screen-network-series (%screen-series series-by-widget :network)
                                     snapshot)))
    (:disk
     (let ((basic-p
             (ntop.shared.application:application-config-basic-p config)))
       (values (%screen-state-for-widget state :disks)
               (ntop.disk.presentation:make-disk-table-config
                :title (unless basic-p " Disks ")
                :show-header-p (not basic-p)
                :empty-message "No data"
                :focused-p selected-p)
               (%screen-disk-rows (%screen-series series-by-widget :disks)
                                  snapshot))))
    (:disk-io-graph
     (values nil config (%screen-series series-by-widget :disk-io-graph)))
    (:temperature
     (let ((basic-p
             (ntop.shared.application:application-config-basic-p config)))
       (values (%screen-state-for-widget state :temperature)
               (ntop.temperature.presentation:make-temperature-table-config
                :temperature-unit
                (ntop.shared.application:application-config-temperature-unit config)
                :title (unless basic-p " Temperatures ")
                :show-header-p (not basic-p)
                :empty-message "No data"
                :focused-p selected-p)
               (and snapshot
                    (ntop.app.domain:collection-snapshot-temperature-sample
                     snapshot)))))
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
                                   (%screen-series series-by-widget :process)
                                   current-time-unix-seconds)))
       (multiple-value-bind (table-state table-config)
           (%screen-process-widget-config
            snapshot state config
            (eq (if (eq (%screen-layout-mode state config) :basic)
                    (%screen-basic-table-selection state)
                    (ntop.app.domain:app-state-selected-widget state))
                :process)
            sort-state)
         (values table-state table-config rows))))))

(defun %screen-draw-widget (target widget
                                   rectangle
                                   selected-p
                                   state
                                   config
                                   series-by-widget
                                   snapshot
                                   unavailability-reasons
                                   &optional cpu-frame-rectangles current-time-unix-seconds)
  (let* ((basic-p (ntop.shared.application:application-config-basic-p config))
         (basic-table-frame-p
           (and basic-p selected-p
                (member widget
                        (list :disk :temperature :process-table)
                        :test (function eq))))
         (process-search-p
           (and (eq widget :process-table)
                (eq (ntop.app.domain:app-state-active-dialog state)
                    :process-search)))
         (search-body-rectangle
           (when process-search-p
             (nth-value 0
                        (%screen-process-search-layout rectangle config))))
         (widget-rectangle
           (if process-search-p
               (if (or (not basic-p) basic-table-frame-p)
                   (%screen-inner-rectangle search-body-rectangle
                                            basic-table-frame-p)
                   search-body-rectangle)
               (if (or (not basic-p) basic-table-frame-p)
                   (%screen-inner-rectangle rectangle basic-table-frame-p)
                   rectangle)))
         (frame-rectangles
           (cond
             ((and process-search-p (not basic-p))
              (list search-body-rectangle))
             ((or (not basic-p) basic-table-frame-p)
              (list rectangle))
             (t nil)))
         (clickable-areas nil))
    (multiple-value-bind (widget-state widget-config widget-series)
        (%screen-widget-inputs widget
                               series-by-widget
                               snapshot
                               state
                               config
                               unavailability-reasons
                               current-time-unix-seconds
                               selected-p)
      (dolist (frame-rectangle frame-rectangles)
        (%screen-draw-frame target frame-rectangle config
                            selected-p
                            nil (and basic-p basic-table-frame-p)))
      (when (and (plusp (ntop.ui:rectangle-width widget-rectangle))
                 (plusp (ntop.ui:rectangle-height widget-rectangle)))
        (setf clickable-areas
              (if (and (eq widget :cpu) cpu-frame-rectangles)
                  (let ((ntop.cpu.presentation::*cpu-legend-external-frame-p* t))
                    (ntop.ui:draw-widget widget
                                         target
                                         widget-rectangle
                                         widget-state
                                         widget-config
                                         widget-series))
                  (ntop.ui:draw-widget widget
                                       target
                                       widget-rectangle
                                       widget-state
                                       widget-config
                                       widget-series))))
      (when (and (not basic-p) cpu-frame-rectangles)
        (%screen-draw-frame target (first cpu-frame-rectangles) config
                            selected-p t)
        (%screen-draw-frame target (second cpu-frame-rectangles) config
                            selected-p))
      clickable-areas)))

(defun draw-application-screen
  (target state config snapshot columns rows unavailability-reasons
   &optional series-by-widget current-time-unix-seconds)
  (flet ((draw-freeze-indicator (height)
           (when (plusp height)
             (ntop.ui:draw-table-text target
                                      1
                                      (1- height)
                                      "Frozen, press 'f' to unfreeze"
                                      :selected))))
    (let* ((mode (%screen-layout-mode state config))
           (render-config (%screen-config config mode))
           (*screen-table-theme*
             (ntop.shared.application:application-config-theme render-config))
           (selected
             (%screen-widget-base
              (if (eq mode :basic)
                  (%screen-basic-table-selection state)
                  (ntop.app.domain:app-state-selected-widget state))))
           (paused-p (ntop.app.domain:app-state-paused-p state))
           (content-rows (if paused-p (max 0 (1- rows)) rows))
           (layout-results
             (multiple-value-list
              (%screen-layout-placements columns
                                         content-rows
                                         state
                                         mode
                                         snapshot
                                         render-config)))
           (placements (first layout-results))
           (cpu-frames (second layout-results))
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
                                              unavailability-reasons
                                              (and (eq layout-widget :cpu)
                                                   cpu-frames)
                                              current-time-unix-seconds))))))
      (setf clickable-areas
            (nconc clickable-areas
                   (%screen-draw-active-process-modal target
                                                      state
                                                      render-config
                                                      snapshot
                                                      columns
                                                      content-rows)))
      (when paused-p
        (draw-freeze-indicator rows))
      clickable-areas)))
