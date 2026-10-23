(in-package #:ntop.ui)

(defgeneric draw-table-text (target x y text role)
  (:documentation
   "Draw TEXT at absolute 0-based terminal-cell X/Y; roles :NORMAL, :HEADER, :SELECTED, :TITLE."))

(defstruct
    (table-config
     (:constructor make-table-config
                   (&key columns
                         title
                         (show-scroll-position-p nil)
                         (right-to-left-p nil)
                         sort-column
                         (sort-order :ascending)
                         (selectable-p t)
                         (focused-p t)
                         (show-current-entry-when-unfocused-p t))))
  columns
  title
  show-scroll-position-p
  right-to-left-p
  sort-column
  sort-order
  selectable-p
  focused-p
  show-current-entry-when-unfocused-p)

(defstruct
    (table-state
     (:constructor make-table-state (&key (selected-index 0) (scroll-offset 0))))
  selected-index
  scroll-offset)

(defun format-table-title (title selected-index item-count width show-scroll-p)
  (when title
    (check-type title string)
    (check-type selected-index (integer 0 *))
    (check-type item-count (integer 0 *))
    (check-type width (integer 0 *))
    (if show-scroll-p
        (let ((with-position
               (format nil "~A(~D of ~D) " title (1+ selected-index) item-count)))
          (if (<= (+ (measure-table-text with-position) 2) width)
              with-position
              title))
        title)))

(defun table-visible-window (state item-count viewport-height)
  (check-type item-count (integer 0 *))
  (check-type viewport-height (integer 0 *))
  (let* ((selected-index
          (if (zerop item-count)
              0
              (min (max 0 (table-state-selected-index state)) (1- item-count))))
         (offset (min (max 0 (table-state-scroll-offset state)) item-count)))
    (setf (table-state-selected-index state) selected-index)
    (unless (zerop viewport-height)
      (when (< selected-index offset)
        (setf offset selected-index))
      (when (>= selected-index (+ offset viewport-height))
        (setf offset (1+ (- selected-index viewport-height))))
      (setf offset (min offset (max 0 (- item-count viewport-height)))))
    (setf (table-state-scroll-offset state) offset)
    (values offset (min item-count (+ offset viewport-height)))))

(defun move-table-selection (state amount item-count viewport-height)
  (check-type amount integer)
  (check-type item-count (integer 0 *))
  (setf (table-state-selected-index state) (if (zerop item-count)
                                               0
                                               (min
                                                (max 0
                                                     (+ (table-state-selected-index state)
                                                        amount))
                                                (1- item-count))))
  (table-visible-window state item-count viewport-height)
  (table-state-selected-index state))

(defun table-cell-within-width (text width)
  (let* ((truncated (truncate-table-text text width))
         (padding (- width (measure-table-text truncated))))
    (if (plusp padding)
        (concatenate 'string truncated (make-string padding :initial-element #\Space))
        truncated)))

(defun table-header-text (column config)
  (let ((header (table-column-header column)))
    (if (and (table-column-sortable-p column)
             (eql (table-column-id column) (table-config-sort-column config)))
        (concatenate 'string
                     header
                     (if (eq (table-config-sort-order config) :descending)
                         "▼"
                         "▲"))
        header)))

(defun table-click-area (x y width value action)
  (when (plusp width)
    (make-clickable-area :rectangle
                         (make-rectangle :x x :y y :width width :height 1)
                         :action
                         action
                         :value
                         value)))

(defun draw-table (target rectangle state config rows)
  (check-type state table-state)
  (check-type config table-config)
  (let* ((row-list (coerce rows 'list))
         (columns (coerce (table-config-columns config) 'list))
         (title-row-p (not (null (table-config-title config))))
         (title-height
          (if title-row-p
              1
              0))
         (header-row-p (> (- (rectangle-height rectangle) title-height) 1))
         (header-height
          (if header-row-p
              1
              0))
         (body-y (+ (rectangle-y rectangle) title-height header-height))
         (body-height (max 0 (- (rectangle-height rectangle) title-height header-height)))
         (selected-index
          (progn
            (table-visible-window state (length row-list) body-height)
            (table-state-selected-index state)))
         (layout
          (compute-table-layout columns
                                (rectangle-width rectangle)
                                :right-to-left-p
                                (table-config-right-to-left-p config)
                                :rows
                                row-list))
         (areas nil))
    (when title-row-p
      (let ((title
             (format-table-title (table-config-title config)
                                 selected-index
                                 (length row-list)
                                 (rectangle-width rectangle)
                                 (table-config-show-scroll-position-p config))))
        (draw-table-text target
                         (rectangle-x rectangle)
                         (rectangle-y rectangle)
                         (truncate-table-text title (rectangle-width rectangle))
                         :title)))
    (when header-row-p
      (let ((header-y (+ (rectangle-y rectangle) title-height)))
        (dolist (column-layout layout)
          (let* ((column
                  (find (table-column-layout-id column-layout)
                        columns
                        :key
                        #'table-column-id
                        :test
                        #'eql))
                 (x (+ (rectangle-x rectangle) (table-column-layout-x column-layout)))
                 (width (table-column-layout-width column-layout)))
            (draw-table-text target
                             x
                             header-y
                             (truncate-table-text (table-header-text column config) width)
                             :header)
            (when (table-column-sortable-p column)
              (let* ((right-edge (+ (rectangle-x rectangle) (rectangle-width rectangle)))
                     (click-width (min (1+ width) (- right-edge x))))
                (push
                 (table-click-area x
                                   header-y
                                   click-width
                                   (table-column-id column)
                                   :sort-column)
                 areas)))))))
    (multiple-value-bind (start-index end-index)
        (table-visible-window state (length row-list) body-height)
      (loop for row-index from start-index below end-index
            for row in (nthcdr start-index row-list)
            for y from body-y
            do (let ((selected-p
                      (and (table-config-selectable-p config)
                           (= row-index (table-state-selected-index state))
                           (or (table-config-focused-p config)
                               (table-config-show-current-entry-when-unfocused-p config)))))
                 (dolist (column-layout layout)
                   (let* ((column
                           (find (table-column-layout-id column-layout)
                                 columns
                                 :key
                                 #'table-column-id
                                 :test
                                 #'eql))
                          (text (table-column-cell-text column row))
                          (x
                           (+ (rectangle-x rectangle)
                              (table-column-layout-x column-layout)))
                          (width (table-column-layout-width column-layout)))
                     (draw-table-text target
                                      x
                                      y
                                      (table-cell-within-width text width)
                                      (if selected-p
                                          :selected
                                          :normal))))
                 (when (table-config-selectable-p config)
                   (push
                    (table-click-area (rectangle-x rectangle)
                                      y
                                      (rectangle-width rectangle)
                                      row-index
                                      :select-row)
                    areas)))))
    (nreverse areas)))

(define-widget :table
               (target rectangle state config series)
               (draw-table target rectangle state config series))
