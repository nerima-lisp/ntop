(in-package #:ntop.memory.presentation)

(defun %memory-counter-p (value)
  (and (integerp value) (not (minusp value))))

(defun %memory-series-entry (series samples-key used-key total-key name style)
  (let ((samples (getf series samples-key))
        (used (getf series used-key))
        (total (getf series total-key)))
    (when (and (vectorp samples)
               (plusp (array-total-size samples))
               (%memory-counter-p used)
               (%memory-counter-p total)
               (plusp total))
      (list (ntop.ui:make-chart-series
             :name (ntop.ui:format-memory-legend name used total)
             :style style
             :samples samples)
            total))))

(defun %memory-chart-series (series theme include-cache-p)
  (let* ((entries
           (remove nil
                   (list (%memory-series-entry
                          series :ram-samples :ram-used-bytes :ram-total-bytes
                          "RAM" (ntop.ui:presentation-theme-style theme :ram))
                         (%memory-series-entry
                          series :swap-samples :swap-used-bytes :swap-total-bytes
                          "SWP" (ntop.ui:presentation-theme-style theme :swap))
                         (when include-cache-p
                           (%memory-series-entry
                            series :cache-samples :cache-used-bytes
                            :cache-total-bytes "CACHE"
                            (ntop.ui:presentation-theme-style theme :cache))))))
         (gpu-series
           (remove-if-not
            (lambda (item)
              (plusp
               (array-total-size (ntop.ui:chart-series-samples item))))
            (getf series :gpu-memory-series)))
         (gpu-total (getf series :gpu-memory-total-bytes))
         (maximum-total
           (if entries (loop for entry in entries maximize (second entry)) 1d0)))
    (when (and gpu-series
               (not (and (integerp gpu-total) (plusp gpu-total))))
      (error "A positive integer GPU memory total is required for sampled GPU series."))
    (values
     (append (mapcar (function first) entries) gpu-series)
     (float (if gpu-series (max maximum-total gpu-total) maximum-total) 1d0))))

(defun %memory-chart-config (theme config maximum)
  (let ((legend-position
          (ntop.shared.application:application-config-memory-legend-position
           config)))
    (ntop.ui:make-chart-config
     :y-min 0d0
     :y-max (max 1d0 maximum)
     :theme-name theme
     :legend-position (unless (eq legend-position :none) legend-position)
     :legend-width-ratio 3/4
     :legend-height-ratio 3/4
     :dot-marker-p
     (ntop.shared.application:application-config-dot-marker-p config))))

(defun %draw-memory-title (target rectangle theme)
  (let* ((title " Memory ")
         (width (ntop.ui:rectangle-width rectangle))
         (visible (subseq title 0 (min width (length title)))))
    (when (and (plusp width)
               (plusp (ntop.ui:rectangle-height rectangle)))
      (cl-tty-kit:screen-write-string
       target
       (ntop.ui:rectangle-x rectangle)
       (ntop.ui:rectangle-y rectangle)
       visible
       :style (ntop.ui:presentation-theme-style theme :widget-title)))))

(defun %draw-memory-chart (target rectangle config series theme)
  (multiple-value-bind (chart-series maximum)
      (%memory-chart-series
       series
       theme
       (ntop.shared.application:application-config-enable-cache-memory-p
        config))
    (let ((height (ntop.ui:rectangle-height rectangle)))
      (when (plusp height)
        (%draw-memory-title target rectangle theme)
        (ntop.ui:draw-time-chart
         target
         (ntop.ui:make-rectangle
          :x (ntop.ui:rectangle-x rectangle)
          :y (1+ (ntop.ui:rectangle-y rectangle))
          :width (ntop.ui:rectangle-width rectangle)
          :height (max 0 (1- height)))
         (%memory-chart-config theme config maximum)
         chart-series)))))

(defun %draw-memory-gauge (target rectangle series theme)
  (let ((used (getf series :ram-used-bytes))
        (total (getf series :ram-total-bytes)))
    (when (and (%memory-counter-p used) (%memory-counter-p total))
      (ntop.ui:draw-basic-memory-gauge
       target
       rectangle
       used
       total
       (ntop.ui:format-memory-fraction used total)
       (ntop.ui:presentation-theme-style theme :ram)))))

(ntop.ui:define-widget :memory (target rectangle state config series)
  (declare (ignore state))
  (let ((theme (string-downcase
                (string
                 (ntop.shared.application:application-config-theme config)))))
    (if (ntop.shared.application:application-config-basic-p config)
        (%draw-memory-gauge target rectangle series theme)
        (%draw-memory-chart target rectangle config series theme)))
  nil)
