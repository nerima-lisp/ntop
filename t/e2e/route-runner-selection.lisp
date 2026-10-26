(in-package #:ntop.e2e)

;; A selection is observed only when one frame has a distinct border style.
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
               ((:left :right) (+ (third frame) (fourth frame)))
               ((:up :down) (+ (first frame) (second frame)))))
           (moved-in-direction-p (direction before after)
  (let ((aligned-p
          (case direction
            ((:left :right)
             (= (frame-axis-position before :up)
                (frame-axis-position after :up)))
            ((:up :down)
             (= (frame-axis-position before :left)
                (frame-axis-position after :left))))))
    (and aligned-p
         (let ((before-position (frame-axis-position before direction))
               (after-position (frame-axis-position after direction)))
           (case direction
             (:left (< after-position before-position))
             (:right (> after-position before-position))
             (:up (< after-position before-position))
             (:down (> after-position before-position))))))))
    (let ((id (route-id route))
          (context (or (getf subcase :context) "")))
      (cond
        ((member id (list "key-8" "key-9" "key-10" "key-11")
                 :test (function string=))
         (let* ((action (route-action route))
                (direction
                  (cond ((search "left" action :test (function char-equal)) :left)
                        ((search "right" action :test (function char-equal)) :right)
                        ((search "up" action :test (function char-equal)) :up)
                        ((search "down" action :test (function char-equal)) :down)))
                (before (selected-frame initial))
                (after (selected-frame grid)))
           (if (and direction before after)
               (moved-in-direction-p direction before after)
               nil)))
        ((and (string-equal context "help")
              (member id (list "key-12" "key-13" "key-14" "key-15"
                               "key-16" "key-17" "key-18" "key-19")
                      :test (function string=)))
         (%route-help-navigation-state-p route subcase initial grid))
        ((member id (list "key-35" "key-36") :test (function string=))
         (%route-modal-action-state-p route subcase initial grid))
        (t
         (or (%route-search-toggle-state route subcase initial grid stable)
             nil))))))
