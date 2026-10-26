(in-package #:ntop.e2e)

;; A selection is observed only when one frame has a distinct border style.
(defun %route-key-context-state (route subcase initial grid stable)
  (labels ((frame-style (grid frame)
             (let ((corner (grid-cell-at grid (first frame) (third frame))))
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
                                  (mapcar
                                   (lambda (style)
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
                    (let ((before-position
                            (frame-axis-position before direction))
                          (after-position
                            (frame-axis-position after direction)))
                      (case direction
                        (:left (< after-position before-position))
                        (:right (> after-position before-position))
                        (:up (< after-position before-position))
                        (:down (> after-position before-position))))))))
    (let* ((action (route-action route))
           (context (or (getf subcase :context) ""))
           (probe (getf subcase :probe))
           (direction
             (cond
               ((and (eq probe :selection)
                     (string= action "Move widget selection left")) :left)
               ((and (eq probe :selection)
                     (string= action "Move widget selection right")) :right)
               ((and (eq probe :selection)
                     (string= action "Move widget selection up")) :up)
               ((and (eq probe :selection)
                     (string= action "Move widget selection down")) :down)
               ((and (eq probe :tree)
                     (string= action "Move left within widget")) :left)
               ((and (eq probe :tree)
                     (string= action "Move right within widget")) :right))))
      (cond
        ((and direction initial grid)
         (let ((before (selected-frame initial))
               (after (selected-frame grid)))
           (and before after
                (moved-in-direction-p direction before after))))
        ((and (eq probe :navigation)
              (string-equal context "help"))
         (%route-help-navigation-state-p route subcase initial grid))
        ((and (eq probe :modal)
              (getf subcase :modal-action))
         (%route-modal-action-state-p route subcase initial grid))
        (t
         (or (%route-search-toggle-state route subcase initial grid stable)
             nil))))))
