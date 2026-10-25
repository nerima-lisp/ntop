(in-package #:ntop.e2e)

;; Ambiguous border styles stay manual instead of guessing which frame is selected.
(defun %route-key-context-state (route subcase initial grid stable)
  (labels ((frame-style (grid frame)
             (let ((corner
                     (grid-cell-at grid (first frame) (third frame))))
               (list (criterion-color-vector (cell-foreground corner))
                     (criterion-color-vector (cell-background corner))
                     (cell-bold-p corner))))
           (selected-frame (grid)
             (let* ((frames (criterion-projection-frames grid))
                    (styles (mapcar (lambda (frame) (frame-style grid frame))
                                    frames))
                    (most-common
                      (and styles
                           (apply (function max)
                                  (mapcar (lambda (style)
                                            (count style styles :test (function equalp)))
                                          styles))))
                    (outliers
                      (loop for frame in frames
                            for style in styles
                            when (< (count style styles :test (function equalp))
                                    most-common)
                              collect frame)))
               (and most-common
                    (> most-common 1)
                    (= (length outliers) 1)
                    (first outliers))))
           (frame-axis-position (frame direction)
             (case direction
               (:right (+ (third frame) (fourth frame)))
               ((:up :down) (+ (first frame) (second frame)))))
           (moved-in-direction-p (direction before after)
             (let ((before-position (frame-axis-position before direction))
                   (after-position (frame-axis-position after direction)))
               (case direction
                 (:right (> after-position before-position))
                 (:up (< after-position before-position))
                 (:down (> after-position before-position))))))
    (let ((id (route-id route))
          (context (or (getf subcase :context) "")))
      (cond
        ((member id (list "key-9" "key-10" "key-11") :test (function string=))
         (let* ((action (route-action route))
                (direction
                  (cond ((search "right" action :test (function char-equal)) :right)
                        ((search "up" action :test (function char-equal)) :up)
                        ((search "down" action :test (function char-equal)) :down)))
                (before (selected-frame initial))
                (after (selected-frame grid)))
           (if (and direction before after)
               (moved-in-direction-p direction before after)
               :manual)))
        ((and (string-equal context "help")
              (member id (list "key-12" "key-13" "key-14" "key-15"
                               "key-16" "key-17" "key-18" "key-19")
                      :test (function string=)))
         (%route-help-navigation-state-p route subcase initial grid))
        ((member id (list "key-35" "key-36") :test (function string=))
         (%route-modal-action-state-p route subcase initial grid))
        (t
         (or (%route-search-toggle-state route subcase initial grid stable)
             :manual))))))