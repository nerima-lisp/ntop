(in-package #:ntop.ui)

(defgeneric draw-table-text (target x y text role)
  (:documentation
   "Draw TEXT at absolute 0-based terminal-cell X/Y; roles :NORMAL, :HEADER, :SELECTED, :TITLE."))
(defgeneric draw-table-background (target rectangle role))
(defmethod draw-table-background (target rectangle role)
  (let* ((left (max 0 (rectangle-x rectangle)))
         (top (max 0 (rectangle-y rectangle)))
         (right (max left
                     (+ (rectangle-x rectangle)
                        (max 0 (rectangle-width rectangle)))))
         (bottom (max top
                      (+ (rectangle-y rectangle)
                         (max 0 (rectangle-height rectangle)))))
         (blank-line (make-string (- right left) :initial-element #\Space)))
    (loop for y from top below bottom
          do (draw-table-text target left y blank-line role))))

(defstruct
    (table-config
     (:constructor make-table-config
                   (&key columns
                         title
                         (show-header-p t)
                         empty-message
                         (show-scroll-position-p nil)
                         (right-to-left-p nil)
                         sort-column
                         (sort-order :ascending)
                         (selectable-p t)
                         (focused-p t)
                         (show-current-entry-when-unfocused-p t))))
  columns
  title
  show-header-p
  empty-message
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

(defun table-cell-within-width (text width &optional (pad-p t))
  (check-type text string)
  (check-type width (integer 0 *))
  (let* ((text-width
          (loop for character across text
                sum (cl-tty-kit:char-width character)))
         (truncated
          (if (<= text-width width)
              text
              (if (zerop width)
                  ""
                  (concatenate 'string
                               (truncate-table-text text (1- width))
                               "…"))))
         (visible-width
          (loop for character across truncated
                sum (cl-tty-kit:char-width character)))
         (padding (- width visible-width)))
    (if (and pad-p (plusp padding))
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

(defun draw-table (target rectangle state config rows &key (table-gap-p t))
  (check-type state table-state)
  (check-type config table-config)
  (let* ((row-list (coerce rows (quote list)))
         (columns (coerce (table-config-columns config) (quote list)))
         (title-row-p (not (null (table-config-title config))))
         (title-height (if title-row-p 1 0))
         (empty-message-p (and (null row-list) (table-config-empty-message config)))
         (header-row-p
           (and (table-config-show-header-p config)
                (> (- (rectangle-height rectangle) title-height) 1)
                (not empty-message-p)))
         (header-height (if header-row-p 1 0))
         (table-gap-height
           (if (and table-gap-p header-row-p (>= (rectangle-height rectangle) 7)) 1 0))
         (body-y (+ (rectangle-y rectangle)
                    title-height
                    header-height
                    table-gap-height))
         (body-height
           (max 0 (- (rectangle-height rectangle)
                     title-height
                     header-height
                     table-gap-height)))
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
    (draw-table-background target
                           (make-rectangle :x (rectangle-x rectangle)
                                           :y (+ (rectangle-y rectangle) title-height)
                                           :width (rectangle-width rectangle)
                                           :height (max 0 (- (rectangle-height rectangle)
                                                             title-height)))
                           :normal)
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
        (draw-table-background
         target
         (make-rectangle :x (rectangle-x rectangle)
                         :y header-y
                         :width (rectangle-width rectangle)
                         :height 1)
         :header)
        (dolist (column-layout layout)
          (let* ((column
                   (find (table-column-layout-id column-layout)
                         columns
                         :key
                         (function table-column-id)
                         :test
                         (function eql)))
                 (x (+ (rectangle-x rectangle) (table-column-layout-x column-layout)))
                 (width (table-column-layout-width column-layout)))
            (draw-table-text target
                             x
                             header-y
                             (table-cell-within-width (table-header-text column config) width nil)
                             :header)
            (when (table-column-sortable-p column)
              (let* ((right-edge (+ (rectangle-x rectangle) (rectangle-width rectangle)))
                     (click-width (min (1+ width) (- right-edge x))))
                (push (table-click-area x
                                        header-y
                                        click-width
                                        (table-column-id column)
                                        :sort-column)
                      areas)))))))
    (when (and empty-message-p (plusp body-height))
      (draw-table-text target
                       (rectangle-x rectangle)
                       body-y
                       (truncate-table-text empty-message-p
                                            (rectangle-width rectangle))
                       :normal))
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
                 (when selected-p
                   (draw-table-background
                    target
                    (make-rectangle :x (rectangle-x rectangle)
                                    :y y
                                    :width (rectangle-width rectangle)
                                    :height 1)
                    :selected))
                 (dolist (column-layout layout)
                   (let* ((column
                            (find (table-column-layout-id column-layout)
                                  columns
                                  :key
                                  (function table-column-id)
                                  :test
                                  (function eql)))
                          (text (table-column-cell-text column row))
                          (x (+ (rectangle-x rectangle)
                                (table-column-layout-x column-layout)))
                          (width (table-column-layout-width column-layout)))
                     (draw-table-text target
                                      x
                                      y
                                      (table-cell-within-width text width)
                                      (if selected-p :selected :normal))))
                 (when (table-config-selectable-p config)
                   (push (table-click-area (rectangle-x rectangle)
                                           y
                                           (rectangle-width rectangle)
                                           row-index
                                           :select-row)
                         areas)))))
    (nreverse areas)))

(define-widget :table
               (target rectangle state config series)
               (draw-table target rectangle state config series))
