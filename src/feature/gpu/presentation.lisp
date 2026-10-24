(in-package #:ntop.gpu.presentation)

(defun %gpu-chart-series (series theme short-names-p)
  (let* ((items (coerce series 'list))
         (count (length items))
         (colors (ntop.ui:presentation-theme-style theme :gpu-colors)))
    (loop for item in items
          for index from 0
          collect
          (ntop.ui:make-chart-series
           :name (ntop.gpu.domain:gpu-display-name
                  (ntop.ui:chart-series-name item)
                  index
                  count
                  short-names-p)
           :style (or (ntop.ui:chart-series-style item)
                      (nth (mod index (length colors)) colors))
           :samples (ntop.ui:chart-series-samples item)))))

(defun %gpu-chart-config (theme config)
  (ntop.ui:make-chart-config
   :y-min 0d0
   :y-max 100d0
   :y-labels '("0%" "50%" "100%")
   :theme-name theme
   :legend-width-ratio 3/4
   :legend-height-ratio 1/2
   :dot-marker-p
   (ntop.shared.application:application-config-dot-marker-p config)))

(defun %draw-gpu-title (target rectangle y title theme)
  (let* ((width (ntop.ui:rectangle-width rectangle))
         (visible (subseq title 0 (min width (length title)))))
    (when (plusp width)
      (cl-tty-kit:screen-write-string
       target
       (ntop.ui:rectangle-x rectangle)
       y
       visible
       :style (ntop.ui:presentation-theme-style theme :widget-title)))))

(defun %draw-gpu-chart-section (target rectangle y height title chart-config series theme)
  (when (plusp height)
    (%draw-gpu-title target rectangle y title theme)
    (ntop.ui:draw-time-chart
     target
     (ntop.ui:make-rectangle
      :x (ntop.ui:rectangle-x rectangle)
      :y (1+ y)
      :width (ntop.ui:rectangle-width rectangle)
      :height (max 0 (1- height)))
     chart-config
     series)))

(defun %draw-gpu-empty-state (target rectangle theme label)
  (let ((width (ntop.ui:rectangle-width rectangle))
        (height (ntop.ui:rectangle-height rectangle)))
    (when (and (plusp width) (plusp height))
      (cl-tty-kit:screen-fill-rect
       target
       (ntop.ui:rectangle-x rectangle)
       (ntop.ui:rectangle-y rectangle)
       width
       height
       #\Space
       :style (ntop.ui:presentation-theme-style theme :general-widget))
      (let* ((x (+ (ntop.ui:rectangle-x rectangle)
                   (floor (max 0 (- width (length label))) 2)))
             (y (+ (ntop.ui:rectangle-y rectangle) (floor height 2))))
        (cl-tty-kit:screen-write-string
         target
         x
         y
         label
         :style (ntop.ui:presentation-theme-style theme :widget-title))))))

(ntop.ui:define-widget :gpu (target rectangle state config series)
  (declare (ignore state))
  (let* ((usage (coerce (or (getf series :usage) #()) 'list))
         (memory (coerce (or (getf series :memory) #()) 'list))
         (reason (getf series :unavailability-reason))
         (theme (string-downcase
                 (string
                  (ntop.shared.application:application-config-theme config)))))
    (if (and (null usage) (null memory))
        (%draw-gpu-empty-state
         target rectangle theme
         (if (eq reason :not-present) "No GPU" "N/A"))
        (let* ((chart-config (%gpu-chart-config theme config))
               (short-names-p
                 (ntop.shared.application:application-config-short-gpu-names-p
                  config))
               (usage-series (%gpu-chart-series usage theme short-names-p))
               (memory-series (%gpu-chart-series memory theme short-names-p))
               (height (ntop.ui:rectangle-height rectangle))
               (usage-height (ceiling height 2))
               (memory-height (- height usage-height))
               (usage-y (ntop.ui:rectangle-y rectangle))
               (memory-y (+ usage-y usage-height)))
          (%draw-gpu-chart-section target rectangle usage-y usage-height
                                   "GPU utilization (%)" chart-config
                                   usage-series theme)
          (%draw-gpu-chart-section target rectangle memory-y memory-height
                                   "GPU memory (%)" chart-config
                                   memory-series theme))))
  nil)
