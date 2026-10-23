(in-package #:ntop.process.presentation)

(defun make-process-sort-menu-state (config)
  (let* ((sort-state (process-table-config-sort-state config))
         (choices (process-table-sort-options config))
         (selected-index
          (or
           (position (ntop.process.domain:process-sort-state-key sort-state)
                     choices
                     :key
                     #'car
                     :test
                     #'eq)
           0)))
    (ntop.ui:make-table-state :selected-index selected-index)))

(defun move-process-sort-menu-selection (state amount choice-count viewport-height)
  (ntop.ui:move-table-selection state amount choice-count viewport-height))

(defun select-process-sort-menu-choice (state index choice-count)
  (unless (and (integerp index) (<= 0 index) (< index choice-count))
    (return-from select-process-sort-menu-choice
      (values state :unknown-choice)))
  (setf (ntop.ui:table-state-selected-index state) index)
  (values state :selected))

(defun apply-process-sort-menu-choice (config state)
  (let* ((sort-state (process-table-config-sort-state config))
         (choices (process-table-sort-options config))
         (choice (nth (ntop.ui:table-state-selected-index state) choices)))
    (if (null choice)
        (values sort-state :no-choice)
        (let ((key (car choice)))
          (if (eq key (ntop.process.domain:process-sort-state-key sort-state))
              (values sort-state :selected)
              (values (ntop.process.domain:process-sort-select-key sort-state key)
                      :selected))))))

(defun %draw-process-sort-choice (target x y width choice selected-p)
  (let* ((label (cdr choice))
         (visible (ntop.ui:truncate-table-text label width)))
    (ntop.ui:draw-table-text target
                             x
                             y
                             visible
                             (if selected-p
                                 :selected
                                 :normal))))

(ntop.ui:define-widget :process-sort-menu
                       (target rectangle state config series)
                       (declare (ignore series))
                       (let* ((choice-list (process-table-sort-options config))
                              (width (ntop.ui:rectangle-width rectangle))
                              (height (ntop.ui:rectangle-height rectangle))
                              (x (ntop.ui:rectangle-x rectangle))
                              (y (ntop.ui:rectangle-y rectangle))
                              (body-height (max 0 (1- height)))
                              (areas nil))
                         (when (and (plusp width) (plusp height))
                           (ntop.ui:draw-table-text target
                                                    x
                                                    y
                                                    (ntop.ui:truncate-table-text
                                                     "Sort By"
                                                     width)
                                                    :header)
                           (multiple-value-bind (start end)
                               (ntop.ui:table-visible-window state
                                                             (length choice-list)
                                                             body-height)
                             (loop for index from start below
                                   end
                                   for choice in (nthcdr start choice-list)
                                   for row-y from (1+ y)
                                   do (%draw-process-sort-choice target
                                                                 x
                                                                 row-y
                                                                 width
                                                                 choice
                                                                 (= index
                                                                    (ntop.ui:table-state-selected-index
                                                                     state))) (push
                                                                               (ntop.ui:make-clickable-area
                                                                                :rectangle
                                                                                (ntop.ui:make-rectangle
                                                                                 :x
                                                                                 x
                                                                                 :y
                                                                                 row-y
                                                                                 :width
                                                                                 width
                                                                                 :height
                                                                                 1)
                                                                                :action
                                                                                :sort-choice
                                                                                :value
                                                                                (car
                                                                                 choice))
                                                                               areas))))
                         (nreverse areas)))
