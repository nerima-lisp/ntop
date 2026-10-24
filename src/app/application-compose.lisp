(in-package #:ntop.app.application)

(defun %composition-series-by-widget (composition snapshot reasons)
    (let* ((cpu-sample
            (and snapshot (ntop.app.domain:collection-snapshot-cpu-sample snapshot)))
           (memory-sample
            (and snapshot (ntop.app.domain:collection-snapshot-memory-sample snapshot)))
           (battery-sample
            (and snapshot (ntop.app.domain:collection-snapshot-battery-sample snapshot)))
           (gpu-memory-data (%gpu-memory-series-data composition))
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
                        (ntop.memory.domain:memory-sample-swap-total-bytes
                         memory-sample))
                   :cache-samples
                   (%history-samples-for composition (quote (:memory :cache)))
                   :cache-used-bytes
                   (and memory-sample
                        (ntop.memory.domain:memory-sample-cache-used-bytes
                         memory-sample))
                   :cache-total-bytes
                   (and memory-sample
                        (ntop.memory.domain:memory-sample-cache-total-bytes
                         memory-sample)))
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
       (cons :battery battery-sample))))
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
  (defun %history-groups-for-state (state)
    (let* ((selected (ntop.app.domain:app-state-selected-widget state))
           (view (gethash selected (ntop.app.domain:app-state-widget-v-states state))))
      (case selected
        (:cpu (quote (:cpu)))
        (:memory
         (if (eq view :gpu)
             (quote (:gpu :gpu-memory))
             (quote (:memory))))
        (:gpu (quote (:gpu)))
        (:network (quote (:network)))
        (:disks (and (eq view :disk-io-graph) (quote (:disk-io))))
        (:disk-io-graph (quote (:disk-io)))
        (:temperature (and (eq view :temperature-graph) (quote (:temperature))))
        (:temperature-graph (quote (:temperature))))))
  (defun %history-stores-for-group (composition group)
    (loop for key across (application-composition-history-keys composition)
          when (eq (first key) group)
            collect (gethash key (application-composition-history-index composition))))
  (defun %apply-history-operation (store kind value)
    (case kind
      (:zoom
       (case value
         (:in (ntop.shared.domain:history-zoom-in store))
         (:out (ntop.shared.domain:history-zoom-out store))
         (:reset (ntop.shared.domain:history-reset-zoom store))
         (otherwise
          (when (realp value)
            (ntop.shared.domain:history-set-window-width store value)))))
      (:history-scroll
       (case value
         (:backward (ntop.shared.domain:history-scroll-older store))
         (:forward (ntop.shared.domain:history-scroll-newer store))))
      (:return-to-now (ntop.shared.domain:history-go-current store))))
  (defun %apply-history-intent (composition state intent)
    (when intent
      (case (first intent)
        (:pause
         (let ((paused-p (second intent)))
           (setf (application-composition-paused-p composition) paused-p)
           (map nil
                (lambda (store)
                  (if paused-p
                      (ntop.shared.domain:history-pause store)
                      (ntop.shared.domain:history-resume store)))
                (application-composition-history-stores composition))))
        (:history
         (let* ((kind (second intent))
                (value (third intent))
                (groups (%history-groups-for-state state))
                (stores
                  (loop for group in groups
                        append (%history-stores-for-group composition group))))
           (map nil
                (lambda (store)
                  (%apply-history-operation store kind value))
                stores)
           (when (and (eq kind :zoom) stores)
             (setf (ntop.app.domain:app-state-zoom-seconds state)
                   (ntop.shared.domain:history-store-window-width-seconds
                    (first stores)))))))))
  (defun %reset-composition-history (composition state)
  (setf (fill-pointer (application-composition-history-stores composition)) 0
        (fill-pointer (application-composition-history-keys composition)) 0
        (application-composition-network-samples composition)
        (make-array 0 :adjustable t :fill-pointer 0)
        (application-composition-latest-snapshot composition) nil
        (application-composition-previous-disk-sample composition) nil
        (application-composition-previous-process-sample composition) nil
        (application-composition-latest-process-sample composition) nil
        (application-composition-disk-rows composition) nil
        (application-composition-clickable-areas composition) nil
        (application-composition-previous-screen composition) nil
        (ntop.app.domain:app-state-zoom-seconds state)
        (ntop.shared.application:application-config-time-window-seconds
         (application-composition-config composition))
        (ntop.app.domain:app-state-scroll-offset state) 0)
  (clrhash (application-composition-history-index composition))
  (clrhash (application-composition-history-names composition))
  (clrhash (application-composition-gpu-memory-device-totals composition))
  composition)
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
               :selected-widget
               (ntop.app.domain:app-state-selected-widget state))))
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
        (let* ((rows (or (application-composition-screen-rows composition) 24))
               (search-row-p
                 (and
                  (ntop.app.presentation:help-state-search-active-p help-state)
                  (> rows 2)))
               (page-size (max 1 (- rows 2 (if search-row-p 1 0)))))
          (setf help-state
                (ntop.app.presentation:help-state-transition
                 help-state action :page-size page-size))
          (when
              (and (eq kind :search)
                   (eq value :submit)
                   (eq (ntop.app.domain:app-state-active-dialog state) :help-search))
            (setf (ntop.app.domain:app-state-active-dialog state) :help))
          (setf (gethash :help states) help-state)
          t))))
  (defun %layout-widgets-for-state (state config columns rows)
    (let* ((basic-p
            (or (ntop.app.domain:app-state-basic-mode-p state)
                (ntop.shared.application:application-config-basic-p config)))
           (expanded-p
            (or (ntop.app.domain:app-state-expanded-p state)
                (ntop.shared.application:application-config-expanded-p config)))
           (selected (ntop.app.domain:app-state-selected-widget state))
           (view (gethash selected (ntop.app.domain:app-state-widget-v-states state)))
           (view
            (and
             (or (null view)
                 (loop with current-view = nil
                       for next-view = (next-widget-view selected current-view)
                       while next-view
                       thereis (eq next-view view)
                       do (setf current-view next-view)))
             view))
           (effective-widget (or view selected)))
      (cond
        (basic-p
         (ntop.ui:layout-widgets columns
                                 rows
                                 :mode
                                 :basic
                                 :cpu-height
                                 1
                                 :memory-rows
                                 2
                                 :table-widget
                                 :process))
        (expanded-p
         (ntop.ui:layout-widgets columns
                                 rows
                                 :mode
                                 :expanded
                                 :selected-widget
                                 effective-widget))
        (t (ntop.ui:layout-widgets columns rows)))))
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
    (when
        (and (null (ntop.app.domain:app-state-active-dialog state))
             (not (ntop.app.domain:app-state-basic-mode-p state))
             (not (ntop.shared.application:application-config-basic-p config))
             (eq (ntop.app.domain:app-action-kind action) :navigation))
      (let ((direction (ntop.app.domain:app-action-value action)))
        (when (member direction (quote (:left :right :up :down)) :test (function eq))
          (%select-layout-neighbor state config columns rows direction)))))
  (defun %draw-help-overlay (screen state config columns rows)
    (when (%help-dialog-p state)
      (let* ((states (ntop.app.domain:app-state-widget-v-states state))
             (help-state
               (or (gethash :help states)
                   (ntop.app.presentation:make-help-state
                    :selected-widget
                    (ntop.app.domain:app-state-selected-widget state))))
             (rectangle
               (ntop.ui:make-rectangle :x 0 :y 0 :width columns :height rows)))
        (setf (gethash :help states) help-state)
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
                                                      unavailability-reasons)
  (check-type state ntop.app.domain:app-state)
  (check-type config ntop.shared.application:application-config)
  (check-type columns (integer 1 *))
  (check-type rows (integer 1 *))
  (let* ((screen (%new-composition-screen composition columns rows))
         (widget-areas
           (funcall (application-composition-draw-screen composition)
                    screen
                    state
                    config
                    snapshot
                    columns
                    rows
                    unavailability-reasons
                    (%composition-series-by-widget composition
                                                   snapshot
                                                   unavailability-reasons)))
         (areas (append widget-areas
                        (%draw-help-overlay screen
                                            state
                                            config
                                            columns
                                            rows))))
    (setf (application-composition-clickable-areas composition) areas)
    (cl-tty-kit:render-diff screen
                            (application-composition-previous-screen composition))
    (setf (application-composition-previous-screen composition)
          (cl-tty-kit:screen-copy screen))
    areas))
  (defun %compose-application-action (composition state action)
  (check-type state ntop.app.domain:app-state)
  (check-type action ntop.app.domain:app-action)
  (multiple-value-bind (next-state forwarded-action history-intent)
      (transition-application-state state action)
    (%copy-transition-state-into state next-state)
    (let ((help-handled-p (%update-help-state composition state action))
          (kind (ntop.app.domain:app-action-kind action))
          (value (ntop.app.domain:app-action-value action)))
      (let ((process-handled-p
              (%update-process-dialog-state composition state action)))
        (%apply-layout-navigation state
                                  action
                                  (application-composition-config composition)
                                  (or (application-composition-screen-columns
                                       composition)
                                      80)
                                  (or (application-composition-screen-rows composition)
                                      24))
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
  (defun make-application-composition (config &key
                                              on-action
                                              (draw-screen
                                               (function
                                                ntop.app.presentation:draw-application-screen)))
    (check-type config ntop.shared.application:application-config)
    (unless (or (null on-action) (functionp on-action))
      (error "ON-ACTION must be a function or NIL."))
    (unless (functionp draw-screen)
      (error "DRAW-SCREEN must be a function."))
    (let ((composition
           (%make-application-composition :config
                                          config
                                          :draw-screen
                                          draw-screen
                                          :external-on-action
                                          on-action)))
      (setf (application-composition-render composition) (lambda
                                                             (state current-config
                                                                    snapshot
                                                                    columns
                                                                    rows
                                                                    reasons)
                                                           (%render-application-composition
                                                            composition
                                                            state
                                                            current-config
                                                            snapshot
                                                            columns
                                                            rows
                                                            reasons))
            (application-composition-on-snapshot composition) (lambda (snapshot)
                                                                (%record-snapshot-history
                                                                 composition
                                                                 snapshot))
            (application-composition-on-action composition) (lambda (state action)
                                                              (%compose-application-action
                                                               composition
                                                               state
                                                               action)))
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
             (member value
                     '(:help :submit :clear :delete-backward :delete-word-backward)
                     :test
                     (function eq))))
        (:navigation
         (member value
                 '(:first-row :last-row
                              :previous-row
                              :next-row
                              :page-up
                              :page-down
                              :half-page-up
                              :half-page-down
                              :scroll-up
                              :scroll-down
                              :up
                              :down)
                 :test
                 (function eq)))
        (:select (symbolp value)))))
