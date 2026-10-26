(in-package #:ntop.cpu.presentation)

(defstruct
    (%cpu-legend-target
     (:constructor %make-cpu-legend-target
                   (target theme row-styles body-y body-row-offset scroll-offset)))
  target
  theme
  row-styles
  body-y
  body-row-offset
  scroll-offset)

(defvar *cpu-legend-external-frame-p* nil)

(defmethod ntop.ui:draw-table-text ((target %cpu-legend-target) x y text role)
  (let* ((row-index
          (+ (%cpu-legend-target-scroll-offset target)
             (- y (%cpu-legend-target-body-y target))))
         (output-y
          (+ y
             (if (member role '(:normal :selected) :test #'eq)
                 (%cpu-legend-target-body-row-offset target)
                 0)))
         (style
          (case role
            (:header
             (ntop.ui:presentation-theme-style (%cpu-legend-target-theme target)
                                               :table-header))
            (:title
             (ntop.ui:presentation-theme-style (%cpu-legend-target-theme target)
                                               :widget-title))
            (:selected
             (ntop.ui:presentation-theme-style (%cpu-legend-target-theme target)
                                               :selected-text))
            (:normal
             (or (nth row-index (%cpu-legend-target-row-styles target))
                 (ntop.ui:presentation-theme-style
                  (%cpu-legend-target-theme target)
                  :text)))
            (otherwise
             (ntop.ui:presentation-theme-style (%cpu-legend-target-theme target)
                                               :text)))))
    (cl-tty-kit:screen-write-string (%cpu-legend-target-target target)
                                    x
                                    output-y
                                    text
                                    :style
                                    style)))
(defmethod ntop.ui::draw-table-background
           ((target %cpu-legend-target) rectangle role)
  (ntop.ui::draw-table-background (%cpu-legend-target-target target)
                                  rectangle
                                  role))

(defun %cpu-sample (series)
  (let ((sample (getf series :sample)))
    (when (and sample (not (typep sample 'ntop.cpu.domain:cpu-sample)))
      (error "CPU presentation expects a CPU sample, got ~S." sample))
    sample))

(defun %cpu-cores (sample)
  (when sample
    (stable-sort (copy-list (ntop.cpu.domain:cpu-sample-core-values sample))
                 #'<
                 :key
                 (lambda (core)
                   (or (ntop.cpu.domain:cpu-core-value-core-index core)
                       most-positive-fixnum)))))

(defun %cpu-usage-text (usage)
  (if (realp usage)
      (string-trim " " (ntop.ui:format-cpu-basic-percent usage nil))
      "N/A"))

(defun %cpu-frequency-text (frequency)
  (if (and (realp frequency) (not (minusp frequency)))
      (format nil "~D" (round frequency))
      "N/A"))

(defun %cpu-legend-label (row width)
  (ntop.ui:cpu-legend-label
   (case (getf row :kind)
     (:all :all)
     (:average :average)
     (:cpu (getf row :index)))
   width))

(defun %cpu-legend-rows (sample config label-width)
  (append
   (list
    (list :kind
          :all
          :label
          (%cpu-legend-label (list :kind :all) label-width)
          :usage
          ""
          :frequency
          ""))
   (unless
       (ntop.shared.application:application-config-hide-average-cpu-p config)
     (list
      (list :kind
            :average
            :label
            (%cpu-legend-label (list :kind :average) label-width)
            :usage
            (if sample
                (%cpu-usage-text
                 (ntop.cpu.domain:cpu-sample-average-usage-percent sample))
                "No data")
            :frequency
            "N/A")))
   (loop for core in (%cpu-cores sample)
         for position from 0
         for index = (or (ntop.cpu.domain:cpu-core-value-core-index core)
                         position)
         collect (list :kind
                       :cpu
                       :index
                       index
                       :label
                       (%cpu-legend-label (list :kind :cpu :index index)
                                          label-width)
                       :usage
                       (%cpu-usage-text
                        (ntop.cpu.domain:cpu-core-value-usage-percent core))
                       :frequency
                       (%cpu-frequency-text
                        (ntop.cpu.domain:cpu-core-value-frequency-mhz core))))))

(defun %cpu-column (id header width max-percent value-key)
  (ntop.ui:make-table-column :id
                             id
                             :header
                             header
                             :width-kind
                             :soft
                             :width
                             width
                             :max-percent
                             max-percent
                             :value-function
                             (lambda (row)
                               (getf row value-key))))

(defun %cpu-legend-columns (expanded-p)
  (append
   (list (%cpu-column :cpu "CPU" 1 1/2 :label)
         (%cpu-column :use "Use" 3 1/2 :usage))
   (when expanded-p
     (list (%cpu-column :mhz "MHz" 4 1/3 :frequency)))))

(defun %cpu-freeze-legend-columns (columns width-layout)
  ;; BTM returns bare widths, then zips them with the original column list.
  (loop for source in columns
        for entry in width-layout
        collect (ntop.ui:make-table-column :id
                                           (ntop.ui:table-column-id source)
                                           :header
                                           (ntop.ui:table-column-header source)
                                           :width-kind
                                           :hard
                                           :width
                                           (ntop.ui:table-column-layout-width entry)
                                           :value-function
                                           (ntop.ui:table-column-value-function
                                            source))))

(defun %cpu-legend-view (sample config width)
  (let* ((columns
          (%cpu-legend-columns
           (ntop.shared.application:application-config-expanded-p config)))
         ;; BTM supplies data widths 1 and 3; the three-cell headers raise both to 3.
         (width-layout (ntop.ui:compute-table-layout columns
                                                     width
                                                     :right-to-left-p t))
         ;; The returned widths are zipped onto the original CPU, Use column order.
         (label-width (if width-layout
                          (ntop.ui:table-column-layout-width (first width-layout))
                          1))
         (rows (%cpu-legend-rows sample config label-width)))
    (values (%cpu-freeze-legend-columns columns width-layout) rows)))

(defun %cpu-row-style (row theme)
  (case (getf row :kind)
    (:all (ntop.ui:presentation-theme-style theme :all-cpu))
    (:average (ntop.ui:presentation-theme-style theme :avg-cpu))
    (:cpu
     (let ((colors (ntop.ui:presentation-theme-style theme :cpu-colors)))
       (nth (mod (getf row :index) (length colors)) colors)))))

(defun %draw-cpu-legend (target rectangle state sample config theme)
  (let* ((external-frame-p *cpu-legend-external-frame-p*)
         (outer-height
          (if external-frame-p
              (1+ (ntop.ui:rectangle-height rectangle))
              (ntop.ui:rectangle-height rectangle)))
         (inner-height
          (if external-frame-p
              (max 0 (1- (ntop.ui:rectangle-height rectangle)))
              (ntop.ui:rectangle-height rectangle)))
         ;; BTM reserves a gap only when a populated table is at least seven rows tall.
         (body-row-offset
          (if (and sample (> inner-height 1) (>= outer-height 7)) 1 0))
         (inner-rectangle
          (ntop.ui:make-rectangle
           :x (+ (ntop.ui:rectangle-x rectangle)
                 (if external-frame-p 1 0))
           :y (+ (ntop.ui:rectangle-y rectangle)
                 (if external-frame-p 1 0))
           :width (max 0 (- (ntop.ui:rectangle-width rectangle)
                            (if external-frame-p 2 0)))
           :height (max 0 (- inner-height body-row-offset))))
         (width (ntop.ui:rectangle-width inner-rectangle)))
    (multiple-value-bind (columns rows)
        (%cpu-legend-view sample config width)
      (when columns
        (let* ((title-height 0)
               (header-p (and sample (> (ntop.ui:rectangle-height inner-rectangle) 1)))
               (body-y
                (+ (ntop.ui:rectangle-y inner-rectangle)
                   title-height
                   (if header-p 1 0)))
               (body-height
                (max 0
                     (- (ntop.ui:rectangle-height inner-rectangle)
                        title-height
                        (if header-p 1 0))))
               (row-styles
                (mapcar (lambda (row) (%cpu-row-style row theme)) rows)))
          (ntop.ui:table-visible-window state (length rows) body-height)
          (let ((areas
                  (ntop.ui:draw-table
                   (%make-cpu-legend-target
                    target
                    theme
                    row-styles
                    body-y
                    body-row-offset
                    (ntop.ui:table-state-scroll-offset state))
                   inner-rectangle
                   state
                   (ntop.ui:make-table-config :columns columns
                                              :selectable-p t
                                              :focused-p t
                                              :show-current-entry-when-unfocused-p t
                                              :empty-message (unless sample "No data"))
                   rows
                   :table-gap-p nil)))
            (if (plusp body-row-offset)
                (mapcar
                 (lambda (area)
                   (if (eq (ntop.ui:clickable-area-action area) :select-row)
                       (let* ((area-rectangle
                               (ntop.ui:clickable-area-rectangle area))
                              (shifted-rectangle
                               (ntop.ui:make-rectangle
                                :x (ntop.ui:rectangle-x area-rectangle)
                                :y (+ (ntop.ui:rectangle-y area-rectangle)
                                      body-row-offset)
                                :width (ntop.ui:rectangle-width area-rectangle)
                                :height (ntop.ui:rectangle-height area-rectangle))))
                         (ntop.ui:make-clickable-area
                          :rectangle shifted-rectangle
                          :action (ntop.ui:clickable-area-action area)
                          :value (ntop.ui:clickable-area-value area)))
                       area))
                 areas)
                areas)))))))

(defun %cpu-series-kind (series)
  (let ((name (ntop.ui:chart-series-name series)))
    (cond
      ((and (stringp name) (string-equal name "AVG")) (values :average nil))
      ((and (stringp name)
            (> (length name) 3)
            (string-equal name "CPU" :end1 3 :end2 3)
            (every
             (lambda (digit)
               (digit-char-p digit))
             (subseq name 3))) (values :cpu (parse-integer name :start 3)))
      (t (values nil nil)))))

(defun %cpu-row-series-p (row series)
  (multiple-value-bind (series-kind series-index) (%cpu-series-kind series)
    (case (getf row :kind)
      (:all t)
      (:average (eq series-kind :average))
      (:cpu (and (eq series-kind :cpu) (eql series-index (getf row :index)))))))

(defun %cpu-chart-series-style (series ordinal theme)
  (multiple-value-bind (kind index) (%cpu-series-kind series)
    (or (ntop.ui:chart-series-style series)
        (case kind
          (:average (ntop.ui:presentation-theme-style theme :avg-cpu))
          (:cpu
           (let ((colors (ntop.ui:presentation-theme-style theme :cpu-colors)))
             (nth (mod index (length colors)) colors)))
          (otherwise
           (let ((colors (ntop.ui:presentation-theme-style theme :cpu-colors)))
             (nth (mod ordinal (length colors)) colors)))))))

(defun %cpu-selected-chart-series (series rows state theme)
  (let* ((selected-index
          (min (max 0 (ntop.ui:table-state-selected-index state))
               (1- (length rows))))
         (row (nth selected-index rows))
         (items (coerce (or series #()) 'list))
         (visible
          (remove-if-not
           (lambda (item)
             (%cpu-row-series-p row item))
           items)))
    (when (eq (getf row :kind) :all)
      (setf visible (reverse visible)))
    (loop for item in visible
          for ordinal from 0
          collect (ntop.ui:make-chart-series :name
                                             (ntop.ui:chart-series-name item)
                                             :style
                                             (%cpu-chart-series-style item
                                                                      ordinal
                                                                      theme)
                                             :samples
                                             (ntop.ui:chart-series-samples item)))))


(defun %cpu-chart-config (theme config series &optional show-time-labels-p)
  (let* ((latest-time
           (loop for item in series
                 for samples = (ntop.ui:chart-series-samples item)
                 when (plusp (length samples))
                   maximize
                   (ntop.shared.domain:history-sample-sampled-at
                    (aref samples (1- (length samples))))))
         (window
           (ntop.shared.application:application-config-time-window-seconds
            config)))
    (ntop.ui:make-chart-config
     :x-start (and latest-time (- latest-time window))
     :x-end latest-time
     :y-min 0d0
     :y-max 100.5d0
     :y-labels (list "  0%" "100%")
     :x-labels (when show-time-labels-p
                 (list (format nil "~Ds" (round window)) "0s"))
     :theme-name theme
     :legend-position nil
     :dot-marker-p
     (ntop.shared.application:application-config-dot-marker-p config))))

(defun %cpu-load-text (value)
  (if (realp value)
      (format nil "~,2F" value)
      "N/A"))

(defun %cpu-title (sample)
  (if sample
      (let ((loads
             (list (ntop.cpu.domain:cpu-sample-load-average-1 sample)
                   (ntop.cpu.domain:cpu-sample-load-average-5 sample)
                   (ntop.cpu.domain:cpu-sample-load-average-15 sample))))
        (if (every (function realp) loads)
            (apply (function ntop.ui:format-cpu-title) loads)
            (format nil
                    " CPU ─ ~A ~A ~A "
                    (%cpu-load-text (first loads))
                    (%cpu-load-text (second loads))
                    (%cpu-load-text (third loads)))))
      " CPU ─ No data "))

(defun %draw-cpu-title (target rectangle sample theme)
  (let ((width (ntop.ui:rectangle-width rectangle))
        (height (ntop.ui:rectangle-height rectangle)))
    (when (and (plusp width) (plusp height))
      (cl-tty-kit:screen-write-string target
                                      (ntop.ui:rectangle-x rectangle)
                                      (ntop.ui:rectangle-y rectangle)
                                      (ntop.ui:truncate-table-text
                                       (%cpu-title sample)
                                       width)
                                      :style
                                      (ntop.ui:presentation-theme-style theme
                                                                        :widget-title)))))


(defun %draw-cpu-chart (target rectangle state sample config rows series theme)
  (%draw-cpu-title target rectangle sample theme)
  (let ((height (ntop.ui:rectangle-height rectangle)))
    (when (> height 2)
      (let ((selected-series
              (%cpu-selected-chart-series (getf series :usage)
                                          rows
                                          state
                                          theme)))
        (ntop.ui:draw-time-chart
         target
         (ntop.ui:make-rectangle
          :x (ntop.ui:rectangle-x rectangle)
          :y (1+ (ntop.ui:rectangle-y rectangle))
          :width (ntop.ui:rectangle-width rectangle)
          :height (max 0 (1- height)))
         (%cpu-chart-config
          theme
          config
          selected-series
          (and (>= height 7)
               (not (ntop.shared.application:application-config-hide-time-p
                     config))
               (not (ntop.shared.application:application-config-auto-hide-time-p
                     config))))
         selected-series)))))

(defun %cpu-state (state config rows)
  (let ((table-state
         (or state
             (ntop.ui:make-table-state
              :selected-index
              (if
               (and
                (eq
                 (ntop.shared.application:application-config-default-cpu-entry
                  config)
                 :average)
                (> (length rows) 1)
                (eq
                 (getf (second rows) :kind)
                 :average))
               1
               0)))))
    (unless (typep table-state (quote ntop.ui:table-state))
      (error "CPU presentation state must be a table-state."))
    (setf (ntop.ui:table-state-selected-index table-state) (min
                                                            (max 0
                                                                 (ntop.ui:table-state-selected-index
                                                                  table-state))
                                                            (1- (length rows))))
    table-state))

(defun %draw-cpu-graph (target rectangle state sample config series theme)
  (let* ((left-p
           (ntop.shared.application:application-config-cpu-left-legend-p config))
         (rows (%cpu-legend-rows sample config 5))
         (state (%cpu-state state config rows))
         (external-frame-p *cpu-legend-external-frame-p*)
         (layout-x
           (if external-frame-p
               (1- (ntop.ui:rectangle-x rectangle))
               (ntop.ui:rectangle-x rectangle)))
         (layout-width
           (if external-frame-p
               (+ (ntop.ui:rectangle-width rectangle) 2)
               (ntop.ui:rectangle-width rectangle)))
         (graph-left-inset
           (if external-frame-p 1 0))
         (graph-right-inset (if external-frame-p 1 0)))
    (multiple-value-bind (legend-offset legend-width graph-offset graph-width)
        (ntop.ui:cpu-legend-region layout-width :left-p left-p)
      (%draw-cpu-chart target
                       (ntop.ui:make-rectangle
                        :x (+ layout-x graph-offset graph-left-inset)
                        :y (ntop.ui:rectangle-y rectangle)
                        :width
                        (max 0 (- graph-width graph-left-inset graph-right-inset))
                        :height (ntop.ui:rectangle-height rectangle))
                       state sample config rows series theme)
      (when (plusp legend-width)
        (%draw-cpu-legend
         target
         (ntop.ui:make-rectangle :x (+ layout-x legend-offset)
                                 :y (ntop.ui:rectangle-y rectangle)
                                 :width legend-width
                                 :height (ntop.ui:rectangle-height rectangle))
         state sample config theme)))))

