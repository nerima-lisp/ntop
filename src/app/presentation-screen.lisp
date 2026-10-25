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

(defun %screen-layout-placements (columns rows state mode
                                         &optional snapshot config)
  (let* ((selected (ntop.app.domain:app-state-selected-widget state))
         (table-widget
           (if (eq mode :basic)
               (%screen-basic-table-selection state)
               :disks))
         (expanded-widget
           (and (eq mode :expanded)
                (%screen-effective-widget selected state)))
         (cpu-height
           (if (and (eq mode :basic) snapshot)
               (let* ((cpu-sample
                        (ntop.app.domain:collection-snapshot-cpu-sample
                         snapshot))
                      (core-count
                        (if cpu-sample
                            (length
                             (ntop.cpu.domain:cpu-sample-core-values cpu-sample))
                            0))
                      (average-row-p
                        (and cpu-sample
                             config
                             (not
                              (ntop.shared.application:application-config-hide-average-cpu-p
                               config))
                             (realp
                              (ntop.cpu.domain:cpu-sample-average-usage-percent
                               cpu-sample)))))
                 (max 1 (ceiling (+ core-count (if average-row-p 1 0)) 4)))
               1))
         (memory-rows
           (if (and (eq mode :basic) snapshot)
               (let ((memory-sample
                       (ntop.app.domain:collection-snapshot-memory-sample
                        snapshot)))
                 (max
                  (+ 1
                     (if (and memory-sample
                              (realp
                               (ntop.memory.domain:memory-sample-swap-used-bytes
                                memory-sample))
                              (realp
                               (ntop.memory.domain:memory-sample-swap-total-bytes
                                memory-sample)))
                         1
                         0)
                     (if (and memory-sample
                              (realp
                               (ntop.memory.domain:memory-sample-cache-used-bytes
                                memory-sample))
                              (realp
                               (ntop.memory.domain:memory-sample-cache-total-bytes
                                memory-sample)))
                         1
                         0)
                     (if (and memory-sample
                              (realp
                               (ntop.memory.domain:memory-sample-arc-used-bytes
                                memory-sample))
                              (realp
                               (ntop.memory.domain:memory-sample-arc-total-bytes
                                memory-sample)))
                         1
                         0))
                  (if (and config
                           (ntop.shared.application:application-config-network-show-packets-p
                            config))
                      4
                      2)))
               2))
         (placements
           (let ((cpu-left-legend-p
                   (and config
                        (ntop.shared.application:application-config-cpu-left-legend-p
                         config))))
             (ntop.ui:layout-widgets columns
                                     rows
                                     :mode mode
                                     :selected-widget expanded-widget
                                     :cpu-height cpu-height
                                     :memory-rows memory-rows
                                     :table-widget table-widget
                                     :cpu-left-legend-p cpu-left-legend-p)))
         (cpu (assoc :cpu placements :test (function eq)))
         (cpu-legend (assoc :cpu-legend placements :test (function eq)))
         (cpu-frames
           (and cpu cpu-legend (list (cdr cpu) (cdr cpu-legend)))))
    (when (and cpu cpu-legend)
      (setf (cdr cpu) (%screen-rectangle-union (cdr cpu) (cdr cpu-legend))))
    (values
     (remove :cpu-legend placements :key (function car) :test (function eq))
     cpu-frames)))


(defun %screen-config (config mode)
  (let ((copy (ntop.shared.application::copy-application-config config)))
    (setf (ntop.shared.application:application-config-basic-p copy) (eq mode
                                                                        :basic)
          (ntop.shared.application:application-config-expanded-p copy) (eq mode
                                                                           :expanded))
    copy))

(defun %screen-inner-rectangle (rectangle &optional side-borders-only-p)
  (ntop.ui:make-rectangle :x
                          (1+ (ntop.ui:rectangle-x rectangle))
                          :y
                          (ntop.ui:rectangle-y rectangle)
                          :width
                          (max 0 (- (ntop.ui:rectangle-width rectangle) 2))
                          :height
                          (if side-borders-only-p
                              (ntop.ui:rectangle-height rectangle)
                              (max 0 (1- (ntop.ui:rectangle-height rectangle))))))

(defvar *screen-selection-active-p* nil)

(defun %screen-draw-frame (target rectangle config selected-p
                                  &optional right-edge-only-p side-borders-only-p
                                    left-edge-only-p)
  (when (and (plusp (ntop.ui:rectangle-width rectangle))
             (plusp (ntop.ui:rectangle-height rectangle)))
    (let* ((theme (ntop.shared.application:application-config-theme config))
           (style
             (ntop.ui:presentation-theme-style
              theme
              (if selected-p :highlighted-border :border))))
      (cond
        (side-borders-only-p
         (let ((left (ntop.ui:rectangle-x rectangle))
               (right (+ (ntop.ui:rectangle-x rectangle)
                         (1- (ntop.ui:rectangle-width rectangle)))))
           (loop for y from (ntop.ui:rectangle-y rectangle)
                 below (+ (ntop.ui:rectangle-y rectangle)
                          (ntop.ui:rectangle-height rectangle))
                 do (cl-tty-kit:screen-write-string target left y "│" :style style)
                    (cl-tty-kit:screen-write-string target right y "│" :style style))))
        (right-edge-only-p
         (let* ((x (+ (ntop.ui:rectangle-x rectangle)
                      (1- (ntop.ui:rectangle-width rectangle))))
                (top (ntop.ui:rectangle-y rectangle))
                (bottom (+ top (1- (ntop.ui:rectangle-height rectangle)))))
           (cl-tty-kit:screen-write-string target x top "┐" :style style)
           (loop for y from (1+ top) below bottom
                 do (cl-tty-kit:screen-write-string target x y "│" :style style))
           (cl-tty-kit:screen-write-string target x bottom "┘" :style style)))
        (left-edge-only-p
         (let* ((x (ntop.ui:rectangle-x rectangle))
                (top (ntop.ui:rectangle-y rectangle))
                (bottom (+ top (1- (ntop.ui:rectangle-height rectangle)))))
           (cl-tty-kit:screen-write-string target x top "┌" :style style)
           (loop for y from (1+ top) below bottom
                 do (cl-tty-kit:screen-write-string target x y "│" :style style))
           (cl-tty-kit:screen-write-string target x bottom "└" :style style)))
        (t
         (cl-tty-kit:screen-draw-box target
                                     (ntop.ui:rectangle-x rectangle)
                                     (ntop.ui:rectangle-y rectangle)
                                     (ntop.ui:rectangle-width rectangle)
                                     (ntop.ui:rectangle-height rectangle)
                                     :border
                                     :single
                                     :style
                                     style))))))

(defvar *screen-table-theme* :default)
(progn
  (defun %screen-no-color-p (&optional (value (uiop:getenv "NO_COLOR")))
    (and value (plusp (length value))))
  (defun %screen-style (theme role &optional (no-color-p (%screen-no-color-p)))
    (unless no-color-p
      (ntop.ui:presentation-theme-style theme role)))
  (defun %screen-table-style (theme role &optional (no-color-p (%screen-no-color-p)))
    (%screen-style
     theme
     (case role
       (:normal :text)
       (:header :table-header)
       (:selected :selected-text)
       (:title :widget-title)
       (otherwise :text))
     no-color-p))
  (defmethod ntop.ui:draw-table-text ((target cl-tty-kit:screen) x y text role)
    (cl-tty-kit:screen-write-string target
                                    x
                                    y
                                    text
                                    :style
                                    (%screen-table-style
                                     *screen-table-theme*
                                     role))))

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
      (setf (getf result :ram-used-bytes)
            (ntop.memory.domain:memory-sample-ram-used-bytes sample)
            (getf result :ram-total-bytes)
            (ntop.memory.domain:memory-sample-ram-total-bytes sample)
            (getf result :swap-used-bytes)
            (ntop.memory.domain:memory-sample-swap-used-bytes sample)
            (getf result :swap-total-bytes)
            (ntop.memory.domain:memory-sample-swap-total-bytes sample)
            (getf result :cache-used-bytes)
            (ntop.memory.domain:memory-sample-cache-used-bytes sample)
            (getf result :cache-total-bytes)
            (ntop.memory.domain:memory-sample-cache-total-bytes sample)
            (getf result :arc-used-bytes)
            (ntop.memory.domain:memory-sample-arc-used-bytes sample)
            (getf result :arc-total-bytes)
            (ntop.memory.domain:memory-sample-arc-total-bytes sample)))
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
