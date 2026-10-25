(in-package #:ntop.app.application)

  (defun %layout-widgets-for-state (state config columns rows)
    (ntop.app.presentation::%screen-layout-placements
     columns
     rows
     state
     (ntop.app.presentation::%screen-layout-mode state config)))

  (defun %layout-widget-base (widget)
    (case widget ((:cpu :memory :network :disks :temperature :process) widget)
      (:disk :disks)
      (:process-table :process)
      (:gpu :memory)
      ((:temperature-graph :battery) :temperature)
      (:disk-io-graph :disks) (:cpu-container :cpu)))

  (defun %widget-center-x (rectangle)
    (+ (ntop.ui:rectangle-x rectangle) (/ (ntop.ui:rectangle-width rectangle) 2)))

  (defun %widget-center-y (rectangle)
    (+ (ntop.ui:rectangle-y rectangle) (/ (ntop.ui:rectangle-height rectangle) 2)))

  (defun %navigation-score (source target direction)
    (let ((dx (- (%widget-center-x (cdr target)) (%widget-center-x (cdr source))))
          (dy (- (%widget-center-y (cdr target)) (%widget-center-y (cdr source)))))
      (case direction
        (:left
         (when (minusp dx)
           (list (abs dy) (- dx))))
        (:right
         (when (plusp dx)
           (list (abs dy) dx)))
        (:up
         (when (minusp dy)
           (list (abs dx) (- dy))))
        (:down
         (when (plusp dy)
           (list (abs dx) dy))))))

  (defun %navigation-score-before-p (left right)
    (or (< (first left) (first right))
        (and (= (first left) (first right)) (< (second left) (second right)))))

  (defun %select-layout-neighbor (state config columns rows direction)
    (let* ((layout (%layout-widgets-for-state state config columns rows))
           (widgets
            (remove-if-not
             (lambda (entry)
               (%layout-widget-base (car entry)))
             layout))
           (selected
            (%layout-widget-base (ntop.app.domain:app-state-selected-widget state)))
           (source
             (find selected
                   widgets
                   :key (lambda (entry)
                          (%layout-widget-base (car entry)))
                   :test #'eq))
           (candidate nil)
           (candidate-score nil))
      (when source
        (dolist (entry widgets)
          (unless (eq entry source)
            (let ((score (%navigation-score source entry direction)))
              (when
                  (and score
                       (or (null candidate-score)
                           (%navigation-score-before-p score candidate-score)))
                (setf candidate entry
                      candidate-score score)))))
        (when candidate
          (setf (ntop.app.domain:app-state-selected-widget state) (%layout-widget-base
                                                                   (car candidate)))))
      state))

  (defun %apply-layout-navigation (state action config columns rows)
    (when (and (null (ntop.app.domain:app-state-active-dialog state))
               (eq (ntop.app.domain:app-action-kind action) :navigation))
      (let* ((direction (ntop.app.domain:app-action-value action))
             (basic-p
               (or (ntop.app.domain:app-state-basic-mode-p state)
                   (ntop.shared.application:application-config-basic-p config))))
        (if basic-p
            (when (member direction '(:left :right) :test (function eq))
              (let* ((tables '(:disks :process :temperature))
                     (selection
                       (ntop.app.presentation::%screen-basic-table-selection state))
                     (index (position selection tables :test (function eq)))
                     (offset (if (eq direction :left) -1 1)))
                (setf (gethash :basic-table-selection
                               (ntop.app.domain:app-state-widget-v-states state))
                      (nth (mod (+ index offset) (length tables)) tables))))
            (when (member direction '(:left :right :up :down)
                           :test (function eq))
              (%select-layout-neighbor state config columns rows direction)))))
    state)
