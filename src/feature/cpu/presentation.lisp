(in-package #:ntop.cpu.presentation)

(defstruct
    (%cpu-legend-target
     (:constructor %make-cpu-legend-target
                   (target theme row-styles body-y scroll-offset)))
  target
  theme
  row-styles
  body-y
  scroll-offset)

(defmethod ntop.ui:draw-table-text ((target %cpu-legend-target) x y text role)
  (let* ((row-index
          (+ (%cpu-legend-target-scroll-offset target)
             (- y (%cpu-legend-target-body-y target))))
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
                                    y
                                    text
                                    :style
                                    style)))

(defun %cpu-theme-name (config)
  (string-downcase
   (string (ntop.shared.application:application-config-theme config))))

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
            (%cpu-usage-text
             (and sample
                  (ntop.cpu.domain:cpu-sample-average-usage-percent sample)))
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
   (list (%cpu-column :cpu "CPU" 1 1/4 :label)
         (%cpu-column :use "Use" 3 1/2 :usage))
   (when expanded-p
     (list (%cpu-column :mhz "MHz" 4 1/3 :frequency)))))

(defun %cpu-freeze-legend-columns (columns layout)
  (loop for entry in layout
        for source = (find (ntop.ui:table-column-layout-id entry)
                           columns
                           :key
                           #'ntop.ui:table-column-id
                           :test
                           #'eql)
        collect (ntop.ui:make-table-column :id
                                           (ntop.ui:table-column-id source)
                                           :header
                                           (ntop.ui:table-column-header source)
                                           :width-kind
                                           :hard
                                           :width
                                           (ntop.ui:table-column-layout-width
                                            entry)
                                           :value-function
                                           (ntop.ui:table-column-value-function
                                            source))))

(defun %cpu-legend-view (sample config width)
  (let* ((columns
          (%cpu-legend-columns
           (ntop.shared.application:application-config-expanded-p config)))
         (seed-rows (%cpu-legend-rows sample config 5))
         (layout (ntop.ui:compute-table-layout columns width :rows seed-rows))
         (cpu-column
          (find :cpu layout :key #'ntop.ui:table-column-layout-id :test #'eql))
         (label-width
          (if cpu-column
              (ntop.ui:table-column-layout-width cpu-column)
              1))
         (rows (%cpu-legend-rows sample config label-width)))
    (values (%cpu-freeze-legend-columns columns layout) rows)))

(defun %cpu-row-style (row theme)
  (case (getf row :kind)
    (:all (ntop.ui:presentation-theme-style theme :all-cpu))
    (:average (ntop.ui:presentation-theme-style theme :avg-cpu))
    (:cpu
     (let ((colors (ntop.ui:presentation-theme-style theme :cpu-colors)))
       (nth (mod (getf row :index) (length colors)) colors)))))

(defun %draw-cpu-legend (target rectangle state sample config theme)
  (multiple-value-bind (columns rows)
      (%cpu-legend-view sample config (ntop.ui:rectangle-width rectangle))
    (when columns
      (let* ((title-height 0)
             (header-p (> (ntop.ui:rectangle-height rectangle) 1))
             (body-y
              (+ (ntop.ui:rectangle-y rectangle)
                 title-height
                 (if header-p
                     1
                     0)))
             (body-height
              (max 0
                   (- (ntop.ui:rectangle-height rectangle)
                      title-height
                      (if header-p
                          1
                          0))))
             (row-styles
              (mapcar
               (lambda (row)
                 (%cpu-row-style row theme))
               rows)))
        (ntop.ui:table-visible-window state (length rows) body-height)
        (ntop.ui:draw-table
         (%make-cpu-legend-target target
                                  theme
                                  row-styles
                                  body-y
                                  (ntop.ui:table-state-scroll-offset state))
         rectangle
         state
         (ntop.ui:make-table-config :columns
                                    columns
                                    :selectable-p
                                    t
                                    :focused-p
                                    t
                                    :show-current-entry-when-unfocused-p
                                    t)
         rows)))))

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

(defun %cpu-chart-config (theme config)
  (ntop.ui:make-chart-config :y-min
                             0d0
                             :y-max
                             100.5d0
                             :y-labels
                             '("  0%" "100%")
                             :theme-name
                             theme
                             :legend-position
                             nil
                             :dot-marker-p
                             (ntop.shared.application:application-config-dot-marker-p
                              config)))

(defun %cpu-load-text (value)
  (if (realp value)
      (format nil "~,2F" value)
      "N/A"))

(defun %cpu-title (sample)
  (let ((loads
         (and sample
              (list (ntop.cpu.domain:cpu-sample-load-average-1 sample)
                    (ntop.cpu.domain:cpu-sample-load-average-5 sample)
                    (ntop.cpu.domain:cpu-sample-load-average-15 sample)))))
    (if (and loads (every #'realp loads))
        (apply #'ntop.ui:format-cpu-title loads)
        (format nil
                " CPU ─ ~A ~A ~A "
                (%cpu-load-text (first loads))
                (%cpu-load-text (second loads))
                (%cpu-load-text (third loads))))))

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
  (when (> (ntop.ui:rectangle-height rectangle) 1)
    (ntop.ui:draw-time-chart target
                             (ntop.ui:make-rectangle :x
                                                     (ntop.ui:rectangle-x
                                                      rectangle)
                                                     :y
                                                     (1+
                                                      (ntop.ui:rectangle-y
                                                       rectangle))
                                                     :width
                                                     (ntop.ui:rectangle-width
                                                      rectangle)
                                                     :height
                                                     (1-
                                                      (ntop.ui:rectangle-height
                                                       rectangle)))
                             (%cpu-chart-config theme config)
                             (%cpu-selected-chart-series (getf series :usage)
                                                         rows
                                                         state
                                                         theme))))

(defun %cpu-state (state config rows)
  (let ((table-state
         (or state
             (ntop.ui:make-table-state :selected-index
                                       (if (and
                                            (eq
                                             (ntop.shared.application:application-config-default-cpu-entry
                                              config)
                                             :average)
                                            (> (length rows) 1)
                                            (eq (getf (second rows) :kind)
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
         (state (%cpu-state state config rows)))
    (multiple-value-bind (legend-x legend-width graph-x graph-width)
        (ntop.ui:cpu-legend-region (ntop.ui:rectangle-width rectangle)
                                   :left-p
                                   left-p)
      (%draw-cpu-chart target
                       (ntop.ui:make-rectangle :x
                                               (+
                                                (ntop.ui:rectangle-x rectangle)
                                                graph-x)
                                               :y
                                               (ntop.ui:rectangle-y rectangle)
                                               :width
                                               graph-width
                                               :height
                                               (ntop.ui:rectangle-height
                                                rectangle))
                       state
                       sample
                       config
                       rows
                       series
                       theme)
      (when (plusp legend-width)
        (%draw-cpu-legend target
                          (ntop.ui:make-rectangle :x
                                                  (+
                                                   (ntop.ui:rectangle-x
                                                    rectangle)
                                                   legend-x)
                                                  :y
                                                  (ntop.ui:rectangle-y
                                                   rectangle)
                                                  :width
                                                  legend-width
                                                  :height
                                                  (ntop.ui:rectangle-height
                                                   rectangle))
                          state
                          sample
                          config
                          theme)))))

