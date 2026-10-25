(in-package #:ntop.app.application)
(defun %process-row-for-selection (item)
  (if (ntop.process.domain:process-tree-entry-p item)
      (ntop.process.domain:process-tree-entry-row item)
      item))
(defun %process-row-selection-key (item table-config)
  (let ((row (%process-row-for-selection item)))
    (when row
      (if (ntop.process.presentation:process-table-config-grouped-p
           table-config)
          (let ((key-kind
                  (if
                   (ntop.process.presentation:process-table-config-show-command-p
                    table-config)
                   :command
                   :name)))
            (list :group
                  key-kind
                  (if (eq key-kind :command)
                      (ntop.process.domain:process-row-command row)
                      (ntop.process.domain:process-row-name row))))
          (list :pid (ntop.process.domain:process-row-pid row))))))
(defun %process-row-key-index (key rows table-config)
  (position key
            rows
            :key (lambda (row)
                   (%process-row-selection-key row table-config))
            :test (function equal)))
(defun %clamp-process-row-index (index row-count)
  (if (zerop row-count)
      0
      (min (max 0 (if (integerp index) index 0)) (1- row-count))))
(defun %reconcile-process-row-selection (state rows table-config)
  (let* ((widget-states
           (ntop.app.domain:app-state-widget-v-states state))
         (stored-state (gethash :process widget-states))
         (table-state
           (if (typep stored-state (quote ntop.ui:table-state))
               (copy-structure stored-state)
               (ntop.ui:make-table-state)))
         (stored-key (gethash :process-selected-key widget-states))
         (matched-index
           (and stored-key
                (%process-row-key-index stored-key rows table-config)))
         (selected-index
           (if matched-index
               matched-index
               (%clamp-process-row-index
                (ntop.ui:table-state-selected-index table-state)
                (length rows)))))
    (setf (ntop.ui:table-state-selected-index table-state) selected-index
          (gethash :process widget-states) table-state)
    (let ((key
            (%process-row-selection-key (nth selected-index rows)
                                        table-config)))
      (if key
          (setf (gethash :process-selected-key widget-states) key)
          (remhash :process-selected-key widget-states)))
    table-state))
(defun %process-row-navigation-delta (value viewport-height)
  (case value
    ((:previous-row :scroll-up) -1)
    ((:next-row :scroll-down) 1)
    (:first-row :first)
    (:last-row :last)
    (:page-up (- viewport-height))
    (:page-down viewport-height)
    (:half-page-up (- (max 1 (floor viewport-height 2))))
    (:half-page-down (max 1 (floor viewport-height 2)))))
(defun %process-row-navigation-viewport-height (composition state config)
  (let* ((rows (or (application-composition-screen-rows composition) 24))
         (columns (or (application-composition-screen-columns composition) 80))
         (mode (ntop.app.presentation::%screen-layout-mode state config))
         (placement
           (assoc :process
                  (ntop.app.presentation::%screen-layout-placements
                   columns rows state mode)
                  :test (function eq)))
         (outer-height
           (if placement
               (ntop.ui:rectangle-height (cdr placement))
               rows))
         (content-height
           (if (eq mode :basic)
               outer-height
               (max 0 (- outer-height 2)))))
    (max 1 (- content-height 2))))
(defun %apply-process-row-navigation
    (state action rows table-config viewport-height)
  (let* ((value (ntop.app.domain:app-action-value action))
         (viewport-height (max 1 viewport-height))
         (movement (%process-row-navigation-delta value viewport-height)))
    (when movement
      (%reconcile-process-row-selection state rows table-config)
      (let* ((widget-states
               (ntop.app.domain:app-state-widget-v-states state))
             (table-state
               (copy-structure (gethash :process widget-states)))
             (row-count (length rows)))
        (cond
          ((eq movement :first)
           (setf (ntop.ui:table-state-selected-index table-state) 0))
          ((eq movement :last)
           (setf (ntop.ui:table-state-selected-index table-state)
                 (max 0 (1- row-count))))
          (t
           (ntop.ui:move-table-selection table-state
                                          movement
                                          row-count
                                          viewport-height)))
        (ntop.ui:table-visible-window table-state row-count viewport-height)
        (setf (gethash :process widget-states) table-state)
        (let ((key
                (%process-row-selection-key
                 (nth (ntop.ui:table-state-selected-index table-state) rows)
                 table-config)))
          (if key
              (setf (gethash :process-selected-key widget-states) key)
              (remhash :process-selected-key widget-states)))
        t))))
(defun %process-row-navigation-action (composition state action)
  (let ((value (ntop.app.domain:app-action-value action)))
    (when
        (and (eq (ntop.app.domain:app-action-kind action) :navigation)
             (null (ntop.app.domain:app-state-active-dialog state))
             (eq (ntop.app.domain:app-state-selected-widget state) :process)
             (member value
                     (quote (:previous-row :next-row :first-row :last-row
                              :page-up :page-down :half-page-up
                              :half-page-down :scroll-up :scroll-down))
                     :test (function eq)))
      (let* ((config (application-composition-config composition))
             (snapshot (application-composition-latest-snapshot composition))
             (sort-state
               (ntop.app.presentation::%screen-process-sort-state
                config state)))
        (multiple-value-bind (rows raw-rows)
            (ntop.app.presentation::%screen-process-rows
             snapshot
             state
             config
             sort-state
             (list :previous-sample
                   (application-composition-previous-process-sample
                    composition)))
          (declare (ignore raw-rows))
          (multiple-value-bind (table-state table-config)
              (ntop.app.presentation::%screen-process-widget-config
               snapshot state config t sort-state)
            (declare (ignore table-state))
            (%apply-process-row-navigation
             state
             action
             rows
             table-config
             (%process-row-navigation-viewport-height
              composition state config))))))))

(defun %confirm-process-kill-dialog (composition dialog sample)
  (nth-value
   1
   (ntop.process.presentation:confirm-process-termination-dialog
    dialog sample
    (application-composition-config composition)
    (application-composition-process-signal-sender composition))))
