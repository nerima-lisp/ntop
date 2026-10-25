(in-package #:ntop.app.presentation)
(declaim (ftype function ntop.app.application::%reconcile-process-row-selection))

(defun %screen-process-sort-state (config &optional state)
  (let ((stored
          (and state
               (gethash :process-sort-state
                        (ntop.app.domain:app-state-widget-v-states state)))))
    (if (typep stored (quote ntop.process.domain:process-sort-state))
        stored
        (let* ((key
                (ntop.shared.application:application-config-process-sort-column
                 config))
               (order
                (ntop.shared.application:application-config-process-sort-order
                 config))
               (memory-mode
                (if
                 (ntop.shared.application:application-config-process-default-memory-value-p
                  config)
                 :bytes
                 :percent)))
          (ntop.process.domain:make-process-sort-state
           :key key
           :order order
           :memory-mode memory-mode)))))
(defun %screen-process-search-layout (rectangle config)
  (let* ((basic-p
           (ntop.shared.application:application-config-basic-p config))
         (x (ntop.ui:rectangle-x rectangle))
         (y (ntop.ui:rectangle-y rectangle))
         (width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle))
         (search-height (min height (if basic-p 3 5)))
         (body-height (- height search-height))
         (search-x (+ x (if basic-p 1 0)))
         (search-width (max 0 (- width (if basic-p 2 0)))))
    (values
     (ntop.ui:make-rectangle :x x :y y :width width :height body-height)
     (ntop.ui:make-rectangle :x search-x :y (+ y body-height)
                             :width search-width :height search-height))))
(defun %screen-process-sort-layout (rectangle config)
  (let* ((basic-p
           (ntop.shared.application:application-config-basic-p config))
         (x (ntop.ui:rectangle-x rectangle))
         (y (ntop.ui:rectangle-y rectangle))
         (width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle))
         (menu-width (min 11 width))
         (menu-pane
           (ntop.ui:make-rectangle :x x :y y
                                   :width menu-width :height height))
         (table-pane
           (ntop.ui:make-rectangle :x (+ x menu-width) :y y
                                   :width (- width menu-width) :height height))
         (table-rectangle
           (if basic-p table-pane (%screen-inner-rectangle table-pane)))
         (menu-rectangle
           (if basic-p menu-pane
               (ntop.ui:make-rectangle
                :x (1+ x) :y (1+ y)
                :width (max 0 (- menu-width 2))
                :height (max 0 (- height 2))))))
    (values table-rectangle menu-rectangle table-pane menu-pane)))

(defun %screen-process-widget-config (snapshot state config selected-p sort-state)
  (let* ((gpu-sample
          (and snapshot
               (ntop.app.domain:collection-snapshot-gpu-sample snapshot)))
         (gpu-devices
          (and gpu-sample (ntop.gpu.domain:gpu-sample-devices gpu-sample)))
         (gpu-available-p
          (and
           (not
            (ntop.shared.application:application-config-disable-gpu-p config))
           gpu-devices))
         (process-state
          (gethash :process (ntop.app.domain:app-state-widget-v-states state)))
         (table-state
          (if (typep process-state (quote ntop.ui:table-state))
              process-state
              (ntop.ui:make-table-state
               :selected-index 0
               :scroll-offset (ntop.app.domain:app-state-scroll-offset state))))
         (tree-p
          (ntop.shared.application:application-config-process-default-tree-p
           config))
         (grouped-p
          (and (not tree-p)
               (ntop.shared.application:application-config-process-default-grouped-p
                config)))
         (show-command-p
          (ntop.shared.application:application-config-process-show-command-p
           config))
         (memory-mode
          (if (ntop.shared.application:application-config-process-default-memory-value-p
               config)
              :bytes
              :percent))
         (show-scroll-position-p
          (ntop.shared.application:application-config-show-table-scroll-position-p
           config))
         (column-ids
          (ntop.shared.application:application-config-process-columns config)))
    (values table-state
            (ntop.process.presentation:make-process-table-config
             :sort-state sort-state
             :grouped-p grouped-p
             :show-command-p show-command-p
             :memory-mode memory-mode
             :gpu-available-p (not (null gpu-available-p))
             :focused-p selected-p
             :show-current-entry-when-unfocused-p t
             :show-scroll-position-p show-scroll-position-p
             :basic-p
             (ntop.shared.application:application-config-basic-p config)
             :column-ids column-ids))))

(defun %screen-process-rows (snapshot state config sort-state process-series
                                      &optional current-time-unix-seconds)
  (let* ((sample
          (and snapshot
               (ntop.app.domain:collection-snapshot-process-sample snapshot)))
         (previous-sample (getf process-series :previous-sample))
         (records
          (and sample (ntop.process.domain:process-sample-processes sample)))
         (cpu-sample
          (and snapshot
               (ntop.app.domain:collection-snapshot-cpu-sample snapshot)))
         (cpu-capacity
          (max 1
               (length
                (and cpu-sample
                     (ntop.cpu.domain:cpu-sample-core-values cpu-sample)))))
         (memory-sample
          (and snapshot
               (ntop.app.domain:collection-snapshot-memory-sample snapshot)))
         (memory-total
          (and memory-sample
               (ntop.memory.domain:memory-sample-ram-total-bytes memory-sample)))
         (gpu-memory-total (%screen-process-gpu-memory-total snapshot))
         (raw-rows
          (loop for record in records
                for pid = (ntop.process.domain:process-record-pid record)
                for row = (and pid
                               (ntop.process.domain:process-row-from-samples
                                previous-sample
                                sample
                                pid
                                :cpu-capacity
                                cpu-capacity
                                :memory-total-bytes
                                memory-total
                                :gpu-memory-total-bytes
                                gpu-memory-total
                                :current-time-unix-seconds
                                current-time-unix-seconds
                                :unnormalized-p
                                (ntop.shared.application:application-config-unnormalized-cpu-p
                                 config)))
                when row
                collect row))
         (tree-p
          (ntop.shared.application:application-config-process-default-tree-p
           config))
         (grouped-p
          (and (not tree-p)
               (ntop.shared.application:application-config-process-default-grouped-p
                config)))
         (group-key
          (if (ntop.shared.application:application-config-process-show-command-p
               config)
              :command
            :name))
         (grouped-rows
          (if grouped-p
              (ntop.process.domain:group-process-domain-rows raw-rows group-key)
            raw-rows))
         (sorted-rows
          (ntop.process.domain:sort-process-domain-rows grouped-rows sort-state))
         (search-field
          (gethash :process-search
                   (ntop.app.domain:app-state-widget-v-states state)))
         (search-application-state
          (and (typep search-field
                      (quote ntop.process.presentation::process-search-field-state))
               (ntop.process.presentation:process-search-field-state-application-state
                search-field)))
         (search-predicate
          (and search-application-state
               (ntop.process.application:process-search-state-predicate
                search-application-state)))
         (filtered-rows
          (if search-predicate
              (remove-if-not
               (lambda (row) (funcall search-predicate row))
               sorted-rows)
            sorted-rows)))
    (let ((displayed-rows
           (if tree-p
               (let ((default-collapsed-p
                      (ntop.shared.application:application-config-process-tree-collapse-p
                       config)))
                 (ntop.process.domain:process-tree-visible-rows
                  (ntop.process.domain:build-process-tree filtered-rows sort-state)
                  (ntop.process.domain:make-process-tree-state
                   :default-collapsed-p default-collapsed-p)))
             filtered-rows)))
      (multiple-value-bind (table-state table-config)
          (%screen-process-widget-config
           snapshot
           state
           config
           (eq (ntop.app.domain:app-state-selected-widget state) :process)
           sort-state)
        (declare (ignore table-state))
        (ntop.app.application::%reconcile-process-row-selection
         state displayed-rows table-config))
      (values displayed-rows raw-rows))))

(defun %screen-process-gpu-memory-total (snapshot)
  (let* ((sample
          (and snapshot
               (ntop.app.domain:collection-snapshot-gpu-sample snapshot)))
         (devices (and sample (ntop.gpu.domain:gpu-sample-devices sample)))
         (total
          (loop for device in devices
                for bytes = (ntop.gpu.domain:gpu-device-memory-total-bytes
                             device)
                when bytes
                  sum bytes)))
    (and (plusp total) total)))
(defun resolve-selected-process-targets
    (state config snapshot previous-process-sample)
  (let ((sort-state (%screen-process-sort-state config state)))
    (multiple-value-bind (displayed-rows raw-rows)
        (%screen-process-rows
         snapshot
         state
         config
         sort-state
         (list :previous-sample previous-process-sample) nil)
      (multiple-value-bind (table-state table-config)
          (%screen-process-widget-config snapshot
                                         state
                                         config
                                         t
                                         sort-state)
        (let* ((selected-index
                (ntop.ui:table-state-selected-index table-state))
               (selected
                (nth selected-index displayed-rows))
               (row
                (if (and selected
                         (ntop.process.domain:process-tree-entry-p selected))
                    (ntop.process.domain:process-tree-entry-row selected)
                    selected))
               (grouped-p
                (ntop.process.presentation:process-table-config-grouped-p
                 table-config))
               (show-command-p
                (ntop.process.presentation:process-table-config-show-command-p
                 table-config))
               (group-value
                (and row
                     (if show-command-p
                         (ntop.process.domain:process-row-command row)
                         (ntop.process.domain:process-row-name row))))
               (pids
                (when row
                  (if grouped-p
                      (loop for candidate in raw-rows
                            for candidate-key =
                              (if show-command-p
                                  (ntop.process.domain:process-row-command
                                   candidate)
                                  (ntop.process.domain:process-row-name
                                   candidate))
                            when (equal candidate-key group-value)
                              collect
                                (ntop.process.domain:process-row-pid
                                 candidate))
                      (list (ntop.process.domain:process-row-pid row)))))
               (name
                (and row (ntop.process.domain:process-row-name row))))
          (values pids name))))))
(defun %screen-draw-active-process-modal
    (target state config snapshot columns rows)
  (let ((dialog (ntop.app.domain:app-state-active-dialog state))
        (widget-states (ntop.app.domain:app-state-widget-v-states state)))
    (case dialog
      (:process-search
       (let* ((mode (%screen-layout-mode state config))
              (placements
                (nth-value 0
                           (%screen-layout-placements
                            columns rows state mode)))
              (process-placement (assoc :process placements :test #'eq)))
         (when process-placement
           (multiple-value-bind (body-rectangle search-rectangle)
               (%screen-process-search-layout (cdr process-placement) config)
             (declare (ignore body-rectangle))
             (ntop.ui:draw-widget
              :process-search
              target
              search-rectangle
              (gethash :process-search widget-states)
              config
              nil)))))
      (:process-sort
       (let* ((mode (%screen-layout-mode state config))
              (placements
                (nth-value 0
                           (%screen-layout-placements
                            columns rows state mode)))
              (process-placement (assoc :process placements :test #'eq)))
         (when process-placement
           (multiple-value-bind (table-rectangle menu-rectangle)
               (%screen-process-sort-layout (cdr process-placement) config)
             (declare (ignore table-rectangle))
             (let* ((sort-state (%screen-process-sort-state config state)))
               (multiple-value-bind (table-state process-config)
                   (%screen-process-widget-config snapshot
                                                  state
                                                  config
                                                  nil
                                                  sort-state)
                 (declare (ignore table-state))
                 (let* ((stored-state (gethash :process-sort widget-states))
                        (menu-state
                          (if (typep stored-state 'ntop.ui:table-state)
                              stored-state
                              (ntop.process.presentation:make-process-sort-menu-state
                               process-config))))
                   (ntop.ui:draw-widget :process-sort-menu
                                        target
                                        menu-rectangle
                                        menu-state
                                        process-config
                                        nil))))))))
      (:process-kill
       (let ((dialog-state (gethash :process-kill widget-states)))
         (when dialog-state
           (ntop.ui:draw-widget
            :process-termination-dialog
            target
            (ntop.ui:make-rectangle :x 0
                                    :y 0
                                    :width columns
                                    :height rows)
            dialog-state
            config
            (and snapshot
                 (ntop.app.domain:collection-snapshot-process-sample
                  snapshot))))))
      (otherwise nil))))
