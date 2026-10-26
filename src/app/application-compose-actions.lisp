(in-package #:ntop.app.application)

(defparameter *widget-view-cycles*
  '((:memory nil :gpu)
    (:temperature nil :temperature-graph :battery)
    (:disks nil :disk-io-graph)))

(defun next-widget-view (widget current-view)
  (let ((cycle (assoc widget *widget-view-cycles* :test (function eq))))
    (when cycle
      (let ((position (position current-view (cdr cycle) :test (function eq))))
        (when position
          (nth (mod (1+ position) (length (cdr cycle))) (cdr cycle)))))))

(defun %copy-widget-view-states (states)
  (let ((copy (make-hash-table :test (hash-table-test states)
                              :size (max 1 (hash-table-count states)))))
    (maphash (lambda (widget view)
               (setf (gethash widget copy) view))
             states)
    copy))

(defun %copy-application-state (state)
  (let ((copy (ntop.app.domain::copy-app-state state)))
    (setf (ntop.app.domain:app-state-widget-v-states copy)
          (%copy-widget-view-states
           (ntop.app.domain:app-state-widget-v-states state)))
    copy))

(defun %forward-transition (state action)
  (values state action nil))

(defun %selectable-widget-p (widget)
  (member widget '(:cpu :memory :gpu :network :disks :temperature :battery
                   :process :temperature-graph :disk-io-graph)
          :test (function eq)))

(defun %selected-widget-from-action (value)
  (cond
    ((%selectable-widget-p value) value)
    ((and (consp value)
          (eq (first value) :widget)
          (%selectable-widget-p (second value)))
     (second value))))
(defun %selection-pointer-coordinates (value marker)
  (when (and (listp value)
             (= (length value) 3)
             (eq (first value) marker)
             (integerp (second value))
             (integerp (third value)))
    (values (second value) (third value) t)))

(defun %composition-clickable-area-at (composition action x y)
  (find-if
   (lambda (area)
     (let ((rectangle (ntop.ui:clickable-area-rectangle area)))
       (and (eq (ntop.ui:clickable-area-action area) action)
            (<= (ntop.ui:rectangle-x rectangle) x)
            (< x (+ (ntop.ui:rectangle-x rectangle)
                    (ntop.ui:rectangle-width rectangle)))
            (<= (ntop.ui:rectangle-y rectangle) y)
            (< y (+ (ntop.ui:rectangle-y rectangle)
                    (ntop.ui:rectangle-height rectangle))))))
   (reverse (application-composition-clickable-areas composition))))

(defun %screen-selection-pointer-action (composition state action)
  (unless (eq (ntop.app.domain:app-action-kind action) :select)
    (return-from %screen-selection-pointer-action action))
  (let* ((value (ntop.app.domain:app-action-value action))
         (states (ntop.app.domain:app-state-widget-v-states state)))
    (multiple-value-bind (x y pointer-p)
        (%selection-pointer-coordinates value :widget)
      (when pointer-p
        (let ((area (%composition-clickable-area-at
                     composition :screen-widget x y)))
          (when area
            (let* ((selection-id (ntop.ui:clickable-area-value area))
                   (widget (if (eq selection-id :cpu-legend)
                               :cpu
                             selection-id)))
              (setf (gethash :screen-selection-id states) selection-id)
              (return-from %screen-selection-pointer-action
                (ntop.app.domain:make-app-action :kind :select :value widget)))))))
    (multiple-value-bind (x y pointer-p)
        (%selection-pointer-coordinates value :cpu-series)
      (when pointer-p
        (let ((widget-area (%composition-clickable-area-at
                            composition :screen-widget x y))
              (row-area (%composition-clickable-area-at
                         composition :select-row x y)))
          (when (and widget-area
                     (member (ntop.ui:clickable-area-value widget-area)
                             '(:cpu :cpu-legend)
                             :test (function eq))
                     row-area)
            (let* ((current-state (gethash :cpu states))
                   (table-state
                    (if (typep current-state (quote ntop.ui:table-state))
                        (copy-structure current-state)
                      (ntop.ui:make-table-state)))
                   (row-index (ntop.ui:clickable-area-value row-area)))
              (when (integerp row-index)
                (setf (ntop.ui:table-state-selected-index table-state)
                      row-index
                      (gethash :cpu states) table-state
                      (gethash :screen-selection-id states) :cpu-legend)
                (return-from %screen-selection-pointer-action
                  (ntop.app.domain:make-app-action :kind :select :value :cpu))))))))
    (let ((widget (%selected-widget-from-action value)))
      (when widget
        (setf (gethash :screen-selection-id states) widget)))
    action))

(defun %transition-select (state action)
  (let ((widget
          (%selected-widget-from-action
           (ntop.app.domain:app-action-value action))))
    (if widget
        (progn
          (setf (ntop.app.domain:app-state-selected-widget state) widget)
          (values state nil nil))
        (values state action nil))))

(defun %transition-widget-view (state action)
  (unless (ntop.app.domain:app-state-basic-mode-p state)
    (let* ((widget (or (ntop.app.domain:app-action-value action)
                       (ntop.app.domain:app-state-selected-widget state)))
           (cycle (assoc widget *widget-view-cycles* :test (function eq))))
      (when cycle
        (let* ((states (ntop.app.domain:app-state-widget-v-states state))
               (next-view (next-widget-view widget (gethash widget states))))
          (if next-view
              (setf (gethash widget states) next-view)
              (remhash widget states))))))
  (values state nil nil))

(defun %set-or-toggle (current value)
  (cond
    ((null value) (not current))
    ((eq value t) t)
    ((eq value :toggle) (not current))
    (t current)))

(defun %transition-expand (state action)
  (unless (ntop.app.domain:app-state-basic-mode-p state)
    (setf (ntop.app.domain:app-state-expanded-p state)
          (%set-or-toggle (ntop.app.domain:app-state-expanded-p state)
                          (ntop.app.domain:app-action-value action))))
  (values state nil nil))

(defun %transition-basic-mode (state action)
  (let ((value (ntop.app.domain:app-action-value action)))
    (if (or (null value) (eq value t) (eq value :toggle))
        (setf (ntop.app.domain:app-state-basic-mode-p state)
              (%set-or-toggle
               (ntop.app.domain:app-state-basic-mode-p state)
               value))
        (return-from %transition-basic-mode (values state action nil))))
  (values state nil nil))

(defun %transition-pause (state action)
  (setf (ntop.app.domain:app-state-paused-p state)
        (%set-or-toggle (ntop.app.domain:app-state-paused-p state)
                        (ntop.app.domain:app-action-value action)))
  (values state nil
          (list :pause (ntop.app.domain:app-state-paused-p state))))

(defun %transition-history (state action)
  (values state nil
          (list :history
                (ntop.app.domain:app-action-kind action)
                (ntop.app.domain:app-action-value action))))

(defun %transition-help (state action)
  (declare (ignore action))
  (setf (ntop.app.domain:app-state-active-dialog state) :help)
  (values state nil nil))

(defun %transition-search (state action)
  (case (ntop.app.domain:app-action-value action)
    (:help
     (setf (ntop.app.domain:app-state-active-dialog state) :help-search)
     (values state nil nil))
    (:process
     (let ((states (ntop.app.domain:app-state-widget-v-states state)))
       (cond
         ((eq (ntop.app.domain:app-state-active-dialog state) :process-sort)
          (setf (gethash :process-sort-background-p states) t))
         ((not (eq (ntop.app.domain:app-state-active-dialog state)
                   :process-search))
          (remhash :process-sort-background-p states)))
       (setf (ntop.app.domain:app-state-active-dialog state) :process-search)
       (values state nil nil)))
    (otherwise (values state action nil))))

(defun %transition-dialog (state action)
  (let ((value (ntop.app.domain:app-action-value action)))
    (cond
      ((eq value :clear-expanded)
       (setf (ntop.app.domain:app-state-expanded-p state) nil)
       (values state nil nil))
      ((and (consp value) (eq (first value) :close))
       (when (eq (second value)
                 (ntop.app.domain:app-state-active-dialog state))
         (setf (ntop.app.domain:app-state-active-dialog state)
               (if (eq (second value) :help-search) :help nil)))
       (values state nil nil))
      ((member value '(:process-kill :process-sort :help :help-search)
               :test (function eq))
       (setf (ntop.app.domain:app-state-active-dialog state) value)
       (values state nil nil))
      (t (values state action nil)))))

(defparameter *application-action-rules*
  '((:navigation . %forward-transition)
    (:select . %transition-select)
    (:toggle-widget-v . %transition-widget-view)
    (:expand . %transition-expand)
    (:basic . %transition-basic-mode)
    (:pause . %transition-pause)
    (:zoom . %transition-history)
    (:history-scroll . %transition-history)
    (:return-to-now . %transition-history)
    (:help . %transition-help)
    (:search . %transition-search)
    (:dialog . %transition-dialog)
    (:toggle-process-grouping . %forward-transition)
    (:sort-processes . %forward-transition)
    (:reverse-process-sort . %forward-transition)
    (:toggle-process-tree . %forward-transition)
    (:toggle-process-tree-branch . %forward-transition)
    (:set-process-display-mode . %forward-transition)
    (:process-search . %forward-transition)
    (:process-terminate . %forward-transition)
    (:quit . %forward-transition)))

(defun transition-application-state (state action)
  (let* ((next-state (%copy-application-state state))
         (rule (assoc (ntop.app.domain:app-action-kind action)
                      *application-action-rules*
                      :test (function eq)))
         (handler (if rule
                      (symbol-function (cdr rule))
                      (symbol-function '%forward-transition))))
    (funcall handler next-state action)))
