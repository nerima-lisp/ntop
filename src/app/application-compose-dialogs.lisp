(in-package #:ntop.app.application)
(defparameter *process-search-action-rules*
  '((:toggle-case . :toggle-case-sensitive)
    (:toggle-whole-word . :toggle-whole-word)
    (:toggle-regex . :toggle-regex)
    (:cursor-start . :move-home)
    (:cursor-end . :move-end)
    (:clear . :clear)
    (:delete-word-backward . :delete-previous-word)
    (:delete-backward . :backspace)
    (:delete-forward . :delete-forward)))
(defparameter *process-dialog-navigation-rules*
  '((:up . -1)
    (:down . 1)
    (:previous-row . -1)
    (:next-row . 1)
    (:scroll-up . -1)
    (:scroll-down . 1)
    (:page-up . (:page -1))
    (:page-down . (:page 1))
    (:half-page-up . (:half-page -1))
    (:half-page-down . (:half-page 1))))
(defparameter *process-sort-action-rules*
  '((:cpu . :cpu-percent)
    (:memory . :memory)
    (:gpu-utilization . :gpu-usage-percent)
    (:gpu-memory . :gpu-memory)))

(defun %process-dialog-pointer-coordinates (value marker)
  (when (and (listp value)
             (= (length value) 4)
             (eq (first value) :pointer)
             (eq (second value) marker)
             (integerp (third value))
             (integerp (fourth value)))
    (values (third value) (fourth value) t)))

(defun %process-dialog-area-at (composition action x y)
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
(defun %copy-composition-table-state (state)
  (if (typep state 'ntop.ui:table-state)
      (copy-structure state)
      (ntop.ui:make-table-state)))
(defun %process-sort-state (state config)
  (let ((stored
          (gethash :process-sort-state
                   (ntop.app.domain:app-state-widget-v-states state))))
    (if (typep stored 'ntop.process.domain:process-sort-state)
        stored
        (ntop.process.domain:make-process-sort-state
         :key
         (ntop.shared.application:application-config-process-sort-column config)
         :order
         (ntop.shared.application:application-config-process-sort-order config)
         :memory-mode
         (if
          (ntop.shared.application:application-config-process-default-memory-value-p
           config)
          :bytes
          :percent)))))
(defun %process-dialog-table-config (config snapshot sort-state)
  (let* ((gpu-sample
           (and snapshot
                (ntop.app.domain:collection-snapshot-gpu-sample snapshot)))
         (gpu-devices
           (and gpu-sample (ntop.gpu.domain:gpu-sample-devices gpu-sample)))
         (tree-p
           (ntop.shared.application:application-config-process-default-tree-p
            config))
         (memory-mode
           (if
            (ntop.shared.application:application-config-process-default-memory-value-p
             config)
            :bytes
            :percent)))
    (ntop.process.presentation:make-process-table-config
     :sort-state sort-state
     :grouped-p
     (and
      (not tree-p)
      (ntop.shared.application:application-config-process-default-grouped-p
       config))
     :show-command-p
     (ntop.shared.application:application-config-process-show-command-p config)
     :memory-mode memory-mode
     :gpu-available-p
     (and gpu-devices
          (not
           (ntop.shared.application:application-config-disable-gpu-p config)))
     :focused-p t
     :show-current-entry-when-unfocused-p t
     :show-scroll-position-p
     (ntop.shared.application:application-config-show-table-scroll-position-p
      config)
     :column-ids
     (ntop.shared.application:application-config-process-columns config))))

(defun %ensure-process-search-field (state config)
  (let* ((states (ntop.app.domain:app-state-widget-v-states state))
         (stored (gethash :process-search states)))
    (unless
        (typep stored
               'ntop.process.presentation::process-search-field-state)
      (setf stored
            (ntop.process.presentation:make-process-search-field-state
             :config config)
            (gethash :process-search states) stored))
    stored))
(defun %copy-process-search-field (field)
  (let* ((copy (copy-structure field))
         (application-state
           (copy-structure
            (ntop.process.presentation:process-search-field-state-application-state
             field))))
    (setf
     (ntop.process.presentation:process-search-field-state-application-state
      copy)
     application-state)
    copy))

(defun %blur-process-search-field (state field)
  (let ((copy (%copy-process-search-field field)))
    (ntop.process.presentation:transition-process-search-field copy :blur)
    (setf (gethash :process-search
                   (ntop.app.domain:app-state-widget-v-states state))
          copy)
    copy))
(defun %process-search-transition-action (state action config)
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (field (%ensure-process-search-field state config)))
    (cond
      ((and (eq kind :search) (eq value :submit))
       (%blur-process-search-field state field)
       (setf (ntop.app.domain:app-state-active-dialog state) nil)
       t)
      ((and (eq kind :search)
            (or (stringp value)
                (assoc value *process-search-action-rules* :test #'eq)))
       (let* ((copy (%copy-process-search-field field))
              (rule (assoc value *process-search-action-rules* :test #'eq)))
         (ntop.process.presentation:transition-process-search-field
          copy
          (if (stringp value) :insert-text (cdr rule))
          (and (stringp value) value))
         (setf (gethash :process-search
                        (ntop.app.domain:app-state-widget-v-states state))
               copy)
         t))
      ((and (eq kind :navigation)
            (member value '(:left :right) :test #'eq))
       (let ((copy (%copy-process-search-field field)))
         (ntop.process.presentation:transition-process-search-field
          copy
          (if (eq value :left) :move-left :move-right))
         (setf (gethash :process-search
                        (ntop.app.domain:app-state-widget-v-states state))
               copy)
         t))
      ((and (eq kind :dialog)
            (consp value)
            (eq (first value) :close)
            (eq (second value) :process-search))
       (%blur-process-search-field state field)
       t)
      (t nil))))

(defun %process-dialog-navigation-delta (value viewport-height)
  (let ((rule (assoc value *process-dialog-navigation-rules* :test #'eq)))
    (when rule
      (let ((movement (cdr rule)))
        (if (listp movement)
            (* (second movement)
               (max 1
                    (if (eq (first movement) :half-page)
                        (floor viewport-height 2)
                        viewport-height)))
            movement)))))

(defun %process-sort-dialog-state (state sort-config)
  (let* ((states (ntop.app.domain:app-state-widget-v-states state))
         (stored (gethash :process-sort states)))
    (if (typep stored 'ntop.ui:table-state)
        stored
        (setf (gethash :process-sort states)
              (ntop.process.presentation:make-process-sort-menu-state
               sort-config)))))

(defun %store-process-sort-state (state sort-state)
  (setf (gethash :process-sort-state
                 (ntop.app.domain:app-state-widget-v-states state))
        sort-state)
  sort-state)

(defun %process-sort-action-key (value sort-config)
  (or
   (cdr (assoc value *process-sort-action-rules* :test #'eq))
   (case value
     (:pid-or-count
      (if (ntop.process.presentation:process-table-config-grouped-p sort-config)
          :count
          :pid))
     (:name
      (if (ntop.process.presentation:process-table-config-show-command-p
           sort-config)
          :command
          :name))
     (otherwise
      (let* ((menu (ntop.process.presentation:process-table-sort-options
                    sort-config))
             (choice (and (member value menu :key #'car :test #'eq) value)))
        choice)))))

(defun %apply-process-sort-choice (state config snapshot index)
  (let* ((sort-state (%process-sort-state state config))
         (sort-config
           (%process-dialog-table-config config snapshot sort-state))
         (choices
           (ntop.process.presentation:process-table-sort-options sort-config))
         (menu-state
           (%copy-composition-table-state
            (%process-sort-dialog-state state sort-config))))
    (when (and (integerp index) (<= 0 index) (< index (length choices)))
      (ntop.process.presentation:select-process-sort-menu-choice
       menu-state index (length choices))
      (multiple-value-bind (next-sort-state result)
          (ntop.process.presentation:apply-process-sort-menu-choice
           sort-config menu-state)
        (when (eq result :selected)
          (%store-process-sort-state state next-sort-state))
        (setf (gethash :process-sort
                       (ntop.app.domain:app-state-widget-v-states state))
              menu-state)
        (setf (ntop.app.domain:app-state-active-dialog state) nil)
        t))))

(defun %open-process-sort-dialog (state config snapshot)
  (let* ((sort-state (%process-sort-state state config))
         (sort-config
           (%process-dialog-table-config config snapshot sort-state)))
    (setf (gethash :process-sort
                   (ntop.app.domain:app-state-widget-v-states state))
          (ntop.process.presentation:make-process-sort-menu-state sort-config))
    t))

(defun %open-process-kill-dialog (composition state config)
  (let* ((snapshot (application-composition-latest-snapshot composition))
         (sample
           (and snapshot
                (ntop.app.domain:collection-snapshot-process-sample snapshot))))
    (multiple-value-bind (pids process-name)
        (if snapshot
            (ntop.app.presentation::resolve-selected-process-targets
             state config snapshot
             (application-composition-previous-process-sample composition))
            (values nil nil))
      (multiple-value-bind (dialog result)
          (ntop.process.presentation:open-process-termination-dialog
           pids process-name sample config)
        (if (eq result :opened)
            (setf (gethash :process-kill
                           (ntop.app.domain:app-state-widget-v-states state))
                  dialog)
            (progn
              (remhash :process-kill
                       (ntop.app.domain:app-state-widget-v-states state))
              (setf (ntop.app.domain:app-state-active-dialog state) nil)))
        t))))

(defun %move-process-kill-selection
    (composition state dialog amount viewport-height)
  (when dialog
    (let ((copy (ntop.process.presentation:copy-process-termination-dialog-state
                 dialog)))
      (ntop.process.presentation:move-process-termination-dialog-selection
       copy
       (application-composition-latest-process-sample composition)
       amount
       viewport-height)
      (setf (gethash :process-kill
                     (ntop.app.domain:app-state-widget-v-states state))
            copy))))

(defun %select-process-kill-choice (composition state dialog index)
  (when dialog
    (let* ((copy (ntop.process.presentation:copy-process-termination-dialog-state
                  dialog))
           (sample (application-composition-latest-process-sample composition))
           (selection-index
            (if (eq index most-positive-fixnum) 0 index)))
      (ntop.process.presentation:select-process-termination-dialog-choice
       copy sample selection-index)
      (setf (gethash :process-kill
                     (ntop.app.domain:app-state-widget-v-states state))
            copy))))

(defun %process-sort-dialog-action (composition state action config)
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (snapshot (application-composition-latest-snapshot composition))
         (sort-state (%process-sort-state state config))
         (sort-config
           (%process-dialog-table-config config snapshot sort-state))
         (menu-state
           (%process-sort-dialog-state state sort-config))
         (choices
           (ntop.process.presentation:process-table-sort-options sort-config))
         (rows (or (application-composition-screen-rows composition) 24)))
    (cond
      ((eq kind :sort-processes)
       (if (eq value :apply)
           (%apply-process-sort-choice
            state config snapshot
            (ntop.ui:table-state-selected-index menu-state))
           (multiple-value-bind (x y pointer-p)
               (%process-dialog-pointer-coordinates value :sort-choice)
             (if pointer-p
                 (let ((area (%process-dialog-area-at
                              composition :sort-choice x y)))
                   (let ((index
                           (and area
                                (position
                                 (ntop.ui:clickable-area-value area)
                                 choices :key #'car :test #'eq))))
                     (or (%apply-process-sort-choice state config snapshot index)
                         t)))
                 t))))
      ((eq kind :navigation)
       (cond
         ((eq value :first-row)
          (%apply-process-sort-choice state config snapshot 0)
          t)
         ((eq value :last-row)
          (%apply-process-sort-choice state config snapshot
                                      (max 0 (1- (length choices))))
          t)
         (t
          (let ((movement (%process-dialog-navigation-delta
                           value (max 0 (1- rows)))))
            (when movement
              (let ((copy (%copy-composition-table-state menu-state)))
                (ntop.process.presentation:move-process-sort-menu-selection
                 copy movement (length choices) (max 0 (1- rows)))
                (setf (gethash :process-sort
                               (ntop.app.domain:app-state-widget-v-states state))
                      copy))
              t)))))
      ((and (eq kind :dialog)
            (member value '(:choice-up :choice-down) :test #'eq))
       (let ((movement (if (eq value :choice-up) -1 1))
             (copy (%copy-composition-table-state menu-state)))
         (ntop.process.presentation:move-process-sort-menu-selection
          copy movement (length choices) (max 0 (1- rows)))
         (setf (gethash :process-sort
                        (ntop.app.domain:app-state-widget-v-states state))
               copy)
         t))
      (t nil))))

(defun %process-direct-sort-action (state action config)
  (let* ((value (ntop.app.domain:app-action-value action))
         (sort-state (%process-sort-state state config))
         (sort-config (%process-dialog-table-config config nil sort-state))
         (key (%process-sort-action-key value sort-config)))
    (when key
      (%store-process-sort-state
       state
       (ntop.process.domain:process-sort-select-key sort-state key))
      t)))

(defun %process-row-pointer-action (composition state action)
  (multiple-value-bind (x y pointer-p)
      (%process-dialog-pointer-coordinates
       (ntop.app.domain:app-action-value action) :process-row)
    (when pointer-p
      (let ((area (%process-dialog-area-at composition :select-row x y)))
        (when area
          (let* ((states (ntop.app.domain:app-state-widget-v-states state))
                 (table-state
                   (%copy-composition-table-state (gethash :process states))))
            (setf (ntop.ui:table-state-selected-index table-state)
                  (ntop.ui:clickable-area-value area)
                  (gethash :process states) table-state)
            (remhash :process-selected-key states)))
        t))))

