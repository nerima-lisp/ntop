(in-package #:ntop.app.application)

(defun %process-kill-dialog-action (composition state action)
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (states (ntop.app.domain:app-state-widget-v-states state))
         (dialog (gethash :process-kill states))
         (sample (application-composition-latest-process-sample composition))
         (rows (or (application-composition-screen-rows composition) 24)))
    (cond
      ((and (eq kind :dialog)
            (consp value)
            (eq (first value) :close)
            (eq (second value) :process-kill))
       (remhash :process-kill states)
       t)
      ((and (eq kind :process-terminate) (eq value :confirm))
       (when dialog
         (if (ntop.process.presentation:process-termination-dialog-state-error-message
              dialog)
             (progn
               (remhash :process-kill states)
               (setf (ntop.app.domain:app-state-active-dialog state) nil))
           (let ((result (%confirm-process-kill-dialog composition dialog sample)))
             (cond
               ((eq result :sent)
                (remhash :process-kill states)
                (setf (ntop.app.domain:app-state-active-dialog state) nil))
               ((or (eq result :send-failed) (typep result (quote condition)))
                (let ((copy
                        (ntop.process.presentation:copy-process-termination-dialog-state
                         dialog)))
                  (setf
                   (ntop.process.presentation:process-termination-dialog-state-error-message
                    copy)
                   (if (typep result (quote condition))
                       (princ-to-string result)
                     "The process signal could not be sent."))
                  (setf (gethash :process-kill states) copy))
                (setf (ntop.app.domain:app-state-active-dialog state) :process-kill))
               (t
                (remhash :process-kill states)
                (setf (ntop.app.domain:app-state-active-dialog state) nil))))))
       t)
      ((eq kind :dialog)
       (let ((movement (case value
                         (:choice-up -1)
                         (:choice-down 1))))
         (when (and movement dialog)
           (let ((copy (ntop.process.presentation:copy-process-termination-dialog-state
                        dialog)))
             (ntop.process.presentation:move-process-termination-dialog-selection
              copy sample movement (max 0 (- rows 3)))
             (setf (gethash :process-kill states) copy))
           t)))
      ((eq kind :navigation)
       (let* ((viewport-height (max 0 (- rows 3)))
              (movement (%process-dialog-navigation-delta value viewport-height)))
         (cond
           ((eq value :first-row)
            (%select-process-kill-choice composition state dialog 0)
            t)
           ((eq value :last-row)
            (%move-process-kill-selection
             composition state dialog most-positive-fixnum viewport-height)
            t)
           ((and movement dialog)
            (let ((copy (ntop.process.presentation:copy-process-termination-dialog-state
                         dialog)))
              (ntop.process.presentation:move-process-termination-dialog-selection
               copy sample movement viewport-height)
              (setf (gethash :process-kill states) copy))
            t))))
      ((and (eq kind :process-terminate)
            (multiple-value-bind (x y pointer-p)
                (%process-dialog-pointer-coordinates value :kill-choice)
              (when pointer-p
                (let ((area (%process-dialog-area-at
                             composition :kill-choice x y)))
                  (when (and area dialog)
                    (%select-process-kill-choice
                     composition state dialog
                     (ntop.ui:clickable-area-value area)))
                  t))))
       t)
      (t nil))))

(defun %update-process-dialog-state (composition state action)
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (active (ntop.app.domain:app-state-active-dialog state))
         (config (application-composition-config composition))
         (snapshot (%composition-current-snapshot composition)))
    (cond
      ((and (eq kind :search) (eq value :process))
       (%ensure-process-search-field state config)
       t)
      ((and (eq kind :dialog) (eq value :process-sort))
       (%open-process-sort-dialog state config snapshot))
      ((and (eq kind :dialog) (eq value :process-kill))
       (%open-process-kill-dialog composition state config))
      ((and (eq kind :dialog)
            (consp value)
            (eq (first value) :close)
            (eq (second value) :process-search))
       (%process-search-transition-action state action config))
      ((and (eq kind :dialog)
            (consp value)
            (eq (first value) :close)
            (eq (second value) :process-kill))
       (%process-kill-dialog-action composition state action))
      ((eq active :process-search)
       (%process-search-transition-action state action config))
      ((eq active :process-sort)
       (%process-sort-dialog-action composition state action config))
      ((eq active :process-kill)
       (%process-kill-dialog-action composition state action))
      ((%process-row-navigation-action composition state action)
       t)
      ((and (null active) (eq kind :select))
       (%process-row-pointer-action composition state action))
      ((and (null active) (eq (ntop.app.domain:app-state-selected-widget state) :process)
            (eq kind :sort-processes)
            (not (member value '(:apply :temperature-name :temperature-value)
                         :test #'eq)))
       (%process-direct-sort-action state action config))
      ((and (null active) (eq kind :reverse-process-sort)
            (eq (ntop.app.domain:app-state-selected-widget state) :process))
       (%store-process-sort-state state
        (ntop.process.domain:process-sort-invert
         (%process-sort-state state config)))
       t)
      (t nil))))
