(in-package #:ntop.test)

(defstruct (%f8b-sort-target (:constructor %f8b-make-sort-target (cells roles)))
  cells
  roles)

(defmethod ntop.ui:draw-table-text ((target %f8b-sort-target) x y text role)
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%f8b-sort-target-cells target) 0))
                  (<= 0 column)
                  (< column (array-dimension (%f8b-sort-target-cells target) 1)))
          do (setf (aref (%f8b-sort-target-cells target) y column) character
                   (aref (%f8b-sort-target-roles target) y column) role)))

(defun %f8b-sort-target (width height)
  (%f8b-make-sort-target (make-array (list height width) :initial-element #\Space)
                         (make-array (list height width) :initial-element nil)))

(defun %f8b-sort-lines (target)
  (loop for row below (array-dimension (%f8b-sort-target-cells target) 0)
        collect (string-right-trim '(#\Space)
                                   (coerce
                                    (loop for column below (array-dimension (%f8b-sort-target-cells target) 1)
                                          collect (aref (%f8b-sort-target-cells target) row column))
                                    'string))))

(defun %f8b-sort-menu-golden-lines ()
  (with-open-file
      (stream (asdf:system-relative-pathname "ntop/test" "t/feature/process/goldens/process-sort-menu-12x6.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun %f8b-sort-config (&key (key :cpu-percent) (order :descending))
  (ntop.process.presentation::make-process-table-config :grouped-p
                                                        nil
                                                        :sort-state
                                                        (ntop.process.domain:make-process-sort-state :key
                                                                                                     key
                                                                                                     :order
                                                                                                     order)))

(cl-weave:describe "Process sort menu presentation"
                   (cl-weave:it "expands the menu widget through define-widget"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.ui:define-widget :process-sort-menu
                                                          (target rectangle state config series)
                                                          :rendered))
                                 :to-equal
                                 '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key (eql :process-sort-menu)) target
                                                                  rectangle
                                                                  state
                                                                  config
                                                                  series)
                                    (declare (ignore ntop.ui::widget-key))
                                    :rendered)))
                   (cl-weave:it "starts selection at the currently sorted visible column"
                                (let* ((config (%f8b-sort-config :key :pid :order :descending))
                                       (choices (ntop.process.presentation::process-table-sort-options config))
                                       (state (ntop.process.presentation::make-process-sort-menu-state config)))
                                  (cl-weave:expect (ntop.ui:table-state-selected-index state)
                                                   :to-be
                                                   (position :pid choices :key #'car :test #'eq))))
                   (cl-weave:it "moves and selects within the visible sort choices"
                                (let* ((config (%f8b-sort-config))
                                       (choices (ntop.process.presentation::process-table-sort-options config))
                                       (state (ntop.ui:make-table-state))
                                       (last-index (1- (length choices))))
                                  (ntop.process.presentation::move-process-sort-menu-selection state
                                                                                               1
                                                                                               (length choices)
                                                                                               2)
                                  (cl-weave:expect (ntop.ui:table-state-selected-index state) :to-be 1)
                                  (cl-weave:expect
                                   (multiple-value-list
                                    (ntop.process.presentation::select-process-sort-menu-choice state
                                                                                                last-index
                                                                                                (length choices)))
                                   :to-equal
                                   (list state :selected))
                                  (cl-weave:expect (ntop.ui:table-state-selected-index state) :to-be last-index)
                                  (cl-weave:expect
                                   (multiple-value-list
                                    (ntop.process.presentation::select-process-sort-menu-choice state
                                                                                                (length choices)
                                                                                                (length choices)))
                                   :to-equal
                                   (list state :unknown-choice))))
                   (cl-weave:it "keeps order for the active choice and uses the domain default for a new choice"
                                (let* ((config (%f8b-sort-config :key :cpu-percent :order :ascending))
                                       (choices (ntop.process.presentation::process-table-sort-options config))
                                       (sort-state (ntop.process.presentation::process-table-config-sort-state config))
                                       (state (ntop.process.presentation::make-process-sort-menu-state config))
                                       (pid-index (position :pid choices :key #'car :test #'eq)))
                                  (multiple-value-bind (same-state result)
                                      (ntop.process.presentation::apply-process-sort-menu-choice config state)
                                    (cl-weave:expect same-state :to-be sort-state)
                                    (cl-weave:expect result :to-be :selected))
                                  (setf (ntop.ui:table-state-selected-index state) pid-index)
                                  (multiple-value-bind (next-state result)
                                      (ntop.process.presentation::apply-process-sort-menu-choice config state)
                                    (cl-weave:expect result :to-be :selected)
                                    (cl-weave:expect (ntop.process.domain:process-sort-state-key next-state)
                                                     :to-be
                                                     :pid)
                                    (cl-weave:expect (ntop.process.domain:process-sort-state-order next-state)
                                                     :to-be
                                                     :ascending))))
                   (cl-weave:it "returns choice-specific clickable regions"
                                (let* ((config (%f8b-sort-config :key :name))
                                       (choices (ntop.process.presentation::process-table-sort-options config))
                                       (target (%f8b-sort-target 12 4))
                                       (areas
                                        (ntop.ui:draw-widget :process-sort-menu
                                                             target
                                                             (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 4)
                                                             (ntop.process.presentation::make-process-sort-menu-state
                                                              config)
                                                             config
                                                             nil)))
                                  (cl-weave:expect (mapcar #'ntop.ui:clickable-area-action areas)
                                                   :to-equal
                                                   (make-list (min 3 (length choices)) :initial-element :sort-choice))
                                  (cl-weave:expect (mapcar #'ntop.ui:clickable-area-value areas)
                                                   :to-equal
                                                   (mapcar #'car (subseq choices 0 (min 3 (length choices)))))
                                  (cl-weave:expect (aref (%f8b-sort-target-roles target) 2 0) :to-be :selected)))
                   (cl-weave:it "matches the deterministic visible-column golden"
                                (let* ((config (%f8b-sort-config :key :memory))
                                       (target (%f8b-sort-target 12 6))
                                       (state (ntop.process.presentation::make-process-sort-menu-state config)))
                                  (ntop.ui:draw-widget :process-sort-menu
                                                       target
                                                       (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 6)
                                                       state
                                                       config
                                                       nil)
                                  (cl-weave:expect (%f8b-sort-lines target) :to-equal (%f8b-sort-menu-golden-lines))))
                   (cl-weave:it "handles invalid choices, missing keys, and empty menu bounds"
                                (let* ((config (%f8b-sort-config :key :pid))
                                       (choices (ntop.process.presentation::process-table-sort-options config))
                                       (state (ntop.ui:make-table-state))
                                       (sort-state (ntop.process.presentation::process-table-config-sort-state config)))
                                  (dolist (index (list -1 1.5d0))
                                    (cl-weave:expect
                                     (nth-value 1
                                                (ntop.process.presentation::select-process-sort-menu-choice state
                                                                                                            index
                                                                                                            (length
                                                                                                             choices)))
                                     :to-be
                                     :unknown-choice))
                                  (setf (ntop.ui:table-state-selected-index state) (length choices))
                                  (cl-weave:expect
                                   (multiple-value-list
                                    (ntop.process.presentation::apply-process-sort-menu-choice config state))
                                   :to-equal
                                   (list sort-state :no-choice))
                                  (let* ((unmatched-config
                                          (ntop.process.presentation::make-process-table-config :grouped-p
                                                                                                nil
                                                                                                :column-ids
                                                                                                '(:name-or-command)
                                                                                                :sort-state
                                                                                                (ntop.process.domain:make-process-sort-state
                                                                                                 :key
                                                                                                 :pid)))
                                         (unmatched-state
                                          (ntop.process.presentation::make-process-sort-menu-state unmatched-config)))
                                    (cl-weave:expect (ntop.ui:table-state-selected-index unmatched-state) :to-be 0))
                                  (loop for (width height) in '((0 3) (12 0))
                                        for target = (%f8b-sort-target width height)
                                        for rectangle = (ntop.ui:make-rectangle :x 0 :y 0 :width width :height height)
                                        for areas = (ntop.ui:draw-widget :process-sort-menu
                                                                         target
                                                                         rectangle
                                                                         state
                                                                         config
                                                                         nil)
                                        do (cl-weave:expect areas :to-be nil)))))
