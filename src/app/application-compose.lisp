(in-package #:ntop.app.application)

(defun %composition-series-by-widget (composition snapshot reasons state)
  (let* ((cpu-sample
           (and snapshot (ntop.app.domain:collection-snapshot-cpu-sample snapshot)))
         (memory-sample
           (and snapshot (ntop.app.domain:collection-snapshot-memory-sample snapshot)))
         (battery-sample
           (and snapshot (ntop.app.domain:collection-snapshot-battery-sample snapshot)))
         (gpu-memory-data (%gpu-memory-series-data composition))
         (gpu-view-p
           (eq (gethash :memory
                        (ntop.app.domain:app-state-widget-v-states state))
               :gpu))
         (memory-window-group (if gpu-view-p :gpu :memory))
         (temperature-series (%history-series-for composition :temperature :sensor))
         (memory-series
           (append
            (list :ram-samples
                  (%history-samples-for composition (quote (:memory :ram)))
                  :ram-used-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-ram-used-bytes memory-sample))
                  :ram-total-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-ram-total-bytes memory-sample))
                  :swap-samples
                  (%history-samples-for composition (quote (:memory :swap)))
                  :swap-used-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-swap-used-bytes memory-sample))
                  :swap-total-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-swap-total-bytes memory-sample))
                  :cache-samples
                  (%history-samples-for composition (quote (:memory :cache)))
                  :cache-used-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-cache-used-bytes memory-sample))
                  :cache-total-bytes
                  (and memory-sample
                       (ntop.memory.domain:memory-sample-cache-total-bytes memory-sample)))
            gpu-memory-data)))
    (list
     (cons :process
           (list :previous-sample
                 (application-composition-previous-process-sample composition)))
     (cons :cpu
           (list :sample
                 cpu-sample
                 :usage
                 (%history-series-for composition :cpu :usage)))
     (cons :memory memory-series)
     (cons :gpu
           (list :usage
                 (%history-series-for composition :gpu :usage)
                 :memory
                 (%history-series-for composition :gpu :memory)
                 :unavailability-reason
                 (ntop.app.application:application-unavailability-reason reasons :gpu)))
     (cons :network (%network-visible-samples composition))
     (cons :disks (application-composition-disk-rows composition))
     (cons :disk-io-graph
           (list :read
                 (%history-series-for composition :disk-io :read)
                 :write
                 (%history-series-for composition :disk-io :write)))
     (cons :temperature temperature-series)
     (cons :temperature-graph temperature-series)
     (cons :battery battery-sample)
     (cons :graph-window-widths
           (list (cons :cpu
                       (%composition-history-window-width composition :cpu))
                 (cons :memory
                       (%composition-history-window-width composition memory-window-group))
                 (cons :gpu
                       (%composition-history-window-width composition :gpu))
                 (cons :network
                       (%composition-history-window-width composition :network))
                 (cons :disk-io-graph
                       (%composition-history-window-width composition :disk-io))
                 (cons :temperature-graph
                       (%composition-history-window-width composition
                                                          :temperature)))))))
  (defun %network-visible-samples (composition)
    (let* ((store
            (gethash '(:network :window)
                     (application-composition-history-index composition)))
           (samples (application-composition-network-samples composition)))
      (if store
          (multiple-value-bind (start end)
              (ntop.shared.domain:history-current-window store)
            (if start
                (loop for sample across samples
                      for sampled-at = (ntop.shared.domain:sample-record-sampled-at
                                        sample)
                      when (<= start sampled-at end)
                        collect sample)
                (coerce samples 'list)))
          (coerce samples 'list))))
  (defun %copy-transition-state-into (target source)
    (let ((target-views (ntop.app.domain:app-state-widget-v-states target))
          (source-views (ntop.app.domain:app-state-widget-v-states source)))
      (setf
       (ntop.app.domain:app-state-selected-widget target)
       (ntop.app.domain:app-state-selected-widget source)
       (ntop.app.domain:app-state-expanded-p target)
       (ntop.app.domain:app-state-expanded-p source)
       (ntop.app.domain:app-state-basic-mode-p target)
       (ntop.app.domain:app-state-basic-mode-p source)
       (ntop.app.domain:app-state-paused-p target)
       (ntop.app.domain:app-state-paused-p source)
       (ntop.app.domain:app-state-zoom-seconds target)
       (ntop.app.domain:app-state-zoom-seconds source)
       (ntop.app.domain:app-state-scroll-offset target)
       (ntop.app.domain:app-state-scroll-offset source)
       (ntop.app.domain:app-state-active-dialog target)
       (ntop.app.domain:app-state-active-dialog source))
      (clrhash target-views)
      (maphash
       (lambda (widget view)
         (setf (gethash widget target-views) view))
       source-views)
      target))
(defun %update-help-state (composition state action)
  (let* ((states (ntop.app.domain:app-state-widget-v-states state))
         (active-p (%help-dialog-p state))
         (help-state (gethash :help states))
         (kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action)))
    (when (and active-p (null help-state))
      (setf help-state
            (ntop.app.presentation:make-help-state
             :selected-widget (ntop.app.domain:app-state-selected-widget state)
             :sectioned-p t)))
    (when
        (and help-state
             (or (and (eq kind :dialog)
                      (consp value)
                      (eq (first value) :close)
                      (eq (second value) :help-search))
                 (and active-p
                      (or (eq kind :help)
                          (and (eq kind :dialog)
                               (eq value :help))))))
      (setf help-state
            (ntop.app.presentation:help-state-transition
             help-state
             (ntop.app.domain:make-app-action :kind :search :value :submit)))
      (setf (gethash :help states) help-state))
    (when (and active-p help-state (%help-action-p action))
      (let* ((columns
              (or (application-composition-screen-columns composition) 80))
             (rows (or (application-composition-screen-rows composition) 24))
             (search-row-p
              (and
               (ntop.app.presentation:help-state-search-active-p help-state)
               (> rows 2)))
             (page-size
              (max 1 (- (min rows 27) 2 (if search-row-p 1 0))))
             (line-width (max 1 (- (min columns 96) 3))))
        (setf help-state
              (ntop.app.presentation:help-state-transition
               help-state action :page-size page-size :line-width line-width))
        (when
            (and (eq kind :search)
                 (eq value :submit)
                 (eq (ntop.app.domain:app-state-active-dialog state) :help-search))
          (setf (ntop.app.domain:app-state-active-dialog state) :help))
        (setf (gethash :help states) help-state)
        t))))
  (defun %help-dialog-rectangle (columns rows)
    (let* ((width (min columns 96))
           (height (min rows 27)))
      (ntop.ui:make-rectangle
       :x (floor (- columns width) 2)
       :y (floor (- rows height) 2)
       :width width
       :height height)))
(defun %draw-help-overlay (screen state config columns rows)
  (when (%help-dialog-p state)
    (let* ((states (ntop.app.domain:app-state-widget-v-states state))
           (help-state
            (or (gethash :help states)
                (ntop.app.presentation:make-help-state
                 :selected-widget
                 (ntop.app.domain:app-state-selected-widget state)
                 :sectioned-p t)))
           (rectangle (%help-dialog-rectangle columns rows))
           (theme (ntop.shared.application:application-config-theme config)))
      (setf (gethash :help states) help-state)
      (cl-tty-kit:screen-fill-rect
       screen 0 0 columns rows (code-char 32)
       :style (ntop.app.presentation::%screen-style theme :general-widget))
      (ntop.ui:draw-widget :help screen rectangle help-state config nil))))
  (defun %new-composition-screen (composition columns rows)
    (unless
        (and (eql columns (application-composition-screen-columns composition))
             (eql rows (application-composition-screen-rows composition)))
      (setf (application-composition-previous-screen composition) nil
            (application-composition-screen-columns composition) columns
            (application-composition-screen-rows composition) rows))
    (let ((screen (cl-tty-kit:make-screen columns rows)))
      (setf (application-composition-screen composition) screen)
      screen))
(defun %render-application-composition (composition state
                                                    config
                                                    snapshot
                                                    columns
                                                    rows
                                                    unavailability-reasons
                                                    &optional
                                                    current-time-unix-seconds)
  (check-type state ntop.app.domain:app-state)
  (check-type config ntop.shared.application:application-config)
  (check-type columns (integer 1 *))
  (check-type rows (integer 1 *))
  (let* ((selected-groups (%history-groups-for-state state))
         (selected-width
          (and selected-groups
               (%composition-history-window-width composition
                                                  (first selected-groups)))))
    (setf (ntop.app.domain:app-state-zoom-seconds state)
          (or selected-width
              (application-composition-initial-zoom-seconds composition))))
  (let* ((snapshot
          (if (application-composition-paused-p composition)
              (%composition-current-snapshot composition)
            snapshot))
         (screen (%new-composition-screen composition columns rows))
         (help-active-p (%help-dialog-p state))
         (ntop.app.presentation::*screen-selection-active-p*
          (application-composition-selection-active-p composition))
         (widget-areas
          (let ((ntop.app.presentation::*screen-background-only-p* help-active-p))
            (apply (application-composition-draw-screen composition)
                   (append
                    (list screen
                          state
                          config
                          snapshot
                          columns
                          rows
                          unavailability-reasons
                          (%composition-series-by-widget
                           composition snapshot unavailability-reasons state))
                    (when
                        (and current-time-unix-seconds
                             (eq (application-composition-draw-screen composition)
                                 (function
                                  ntop.app.presentation:draw-application-screen)))
                      (list current-time-unix-seconds))))))
         (areas
          (append (unless help-active-p widget-areas)
                  (%draw-help-overlay screen state config columns rows))))
    (setf (application-composition-clickable-areas composition) areas)
    (let ((diff
           (with-output-to-string (output)
             (cl-tty-kit:render-diff
              screen (application-composition-previous-screen composition)
              output))))
      (loop for index from 0 below (length diff)
            for character = (char diff index)
            do (when (and (char= character #\Newline)
                          (or (zerop index)
                              (not (char= (char diff (1- index)) #\Return))))
                 (write-char #\Return *standard-output*))
            (write-char character *standard-output*)))
    (finish-output)
    (setf (application-composition-previous-screen composition)
          (cl-tty-kit:screen-copy screen))
    areas))
(defun %compose-application-action (composition state action)
  (check-type state ntop.app.domain:app-state)
  (check-type action ntop.app.domain:app-action)
  (setf action (%screen-selection-pointer-action composition state action))
  (multiple-value-bind (next-state forwarded-action history-intent)
      (transition-application-state state action)
    (%copy-transition-state-into state next-state)
    (let ((help-handled-p (%update-help-state composition state action))
          (kind (ntop.app.domain:app-action-kind action))
          (value (ntop.app.domain:app-action-value action)))
      (cond
        ((and (null forwarded-action)
              (or (eq kind :select)
                  (member kind
                          (quote (:zoom :history-scroll :return-to-now))
                          :test (function eq))))
         (setf (application-composition-selection-active-p composition) t))
        ((and (eq kind :dialog) (eq value :clear-expanded))
         (setf (application-composition-selection-active-p composition) nil)))
      (let ((process-handled-p
             (%update-process-dialog-state composition state action)))
        (let ((selected-widget-before
               (ntop.app.domain:app-state-selected-widget state)))
          (%apply-layout-navigation state
                                    action
                                    (application-composition-config composition)
                                    (or (application-composition-screen-columns
                                         composition)
                                        80)
                                    (or (application-composition-screen-rows composition)
                                        24))
          (when (and (eq kind :navigation)
                     (member value
                             (quote (:left :right :up :down))
                             :test
                             (function eq))
                     (not (eq selected-widget-before
                              (ntop.app.domain:app-state-selected-widget state))))
            (setf (application-composition-selection-active-p composition) t)))
        (%apply-history-intent composition state history-intent)
        (when (and (eq kind :navigation)
                   (eq value :reset-collection))
          (%reset-composition-history composition state))
        (when (and forwarded-action
                   (not help-handled-p)
                   (not process-handled-p)
                   (application-composition-external-on-action composition))
          (funcall (application-composition-external-on-action composition)
                   state
                   forwarded-action))))
    state))
  (defun make-application-composition
    (config &key on-action
                 (draw-screen
                   (function ntop.app.presentation:draw-application-screen))
                 (process-signal-sender
                   (function ntop.process.infrastructure:send-process-signal)))
  (check-type config ntop.shared.application:application-config)
  (unless (or (null on-action) (functionp on-action))
    (error "ON-ACTION must be a function or NIL."))
  (unless (functionp draw-screen)
    (error "DRAW-SCREEN must be a function."))
  (unless (functionp process-signal-sender)
    (error "PROCESS-SIGNAL-SENDER must be a function."))
  (let* ((initial-zoom-seconds
           (ntop.shared.domain:history-store-window-width-seconds
            (%make-composition-history-store config)))
         (composition
           (%make-application-composition
            :config config
            :zoom-seconds initial-zoom-seconds
            :initial-zoom-seconds initial-zoom-seconds
            :draw-screen draw-screen
            :external-on-action on-action
            :process-signal-sender process-signal-sender)))
    (setf (application-composition-render composition)
          (lambda (state current-config snapshot columns rows reasons
                   &optional current-time-unix-seconds)
            (%render-application-composition composition
                                             state
                                             current-config
                                             snapshot
                                             columns
                                             rows
                                             reasons
                                             current-time-unix-seconds))
          (application-composition-on-snapshot composition)
          (lambda (snapshot)
            (%record-snapshot-history composition snapshot))
          (application-composition-on-action composition)
          (lambda (state action)
            (%compose-application-action composition state action)))
    composition))

(defun %help-dialog-p (state)
    (member (ntop.app.domain:app-state-active-dialog state)
            '(:help :help-search)
            :test
            (function eq)))

(defun %help-action-p (action)
  (let ((kind (ntop.app.domain:app-action-kind action))
        (value (ntop.app.domain:app-action-value action)))
    (case kind
      (:search
       (or (stringp value)
           (member value '(:help :submit :clear :delete-backward :delete-word-backward)
                   :test #'eq)))
      (:navigation
       (or (help-section-value-p value)
           (member value '(:first-row :last-row :previous-row :next-row
                           :page-up :page-down :half-page-up :half-page-down
                           :scroll-up :scroll-down :up :down)
                   :test #'eq)))
      (:select (symbolp value)))))
