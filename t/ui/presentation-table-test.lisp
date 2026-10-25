(in-package #:ntop.test)

(defstruct wp7-fake-table-target
  calls)

(defmethod ntop.ui:draw-table-text ((target wp7-fake-table-target) x y text role)
  (push (list x y text role) (wp7-fake-table-target-calls target)))

(ntop.ui:define-table-columns *wp7-render-columns*
                              (:name "Name" :hard :width 6 :sortable-p t)
                              (:pid "PID" :hard :width 3 :sortable-p t))

(defun %wp7-table-calls (target)
  (reverse (wp7-fake-table-target-calls target)))

(defun %wp7-table-call (target role)
  (find role (%wp7-table-calls target) :key #'fourth))

(defun %wp7-unfocused-config ()
  (ntop.ui:make-table-config :columns
                             *wp7-render-columns*
                             :focused-p
                             nil
                             :show-current-entry-when-unfocused-p
                             nil))

(defun %wp7-click-values (areas)
  (mapcar #'ntop.ui:clickable-area-value areas))

(cl-weave:describe "table rendering and target adapter"
                   (cl-weave:it "writes header and body text through the target adapter"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                4
                                                                :y
                                                                8
                                                                :width
                                                                16
                                                                :height
                                                                3)))
                                  (ntop.ui:draw-widget :table
                                                       target
                                                       rectangle
                                                       (ntop.ui:make-table-state)
                                                       config
                                                       '((:name "Alpha" :pid 10)))
                                  (cl-weave:expect
                                   (mapcar
                                    (lambda (call)
                                      (list (second call) (third call) (fourth call)))
                                    (%wp7-table-calls target))
                                   :to-equal
                                   '((8 "Name" :header) (8 "PID" :header)
                                     (9 "Alpha    " :selected)
                                     (9 "10   " :selected)))))
                   (cl-weave:it
                    "matches BTM sortable.rs:107-127 sorted-header truncation"
                    (let* ((target (make-wp7-fake-table-target))
                           (config
                            (ntop.ui:make-table-config :columns
                                                       *wp7-render-columns*
                                                       :sort-column
                                                       :name
                                                       :sort-order
                                                       :ascending))
                           (rectangle (ntop.ui:make-rectangle :x 0 :y 0
                                                              :width 16 :height 2)))
                      (ntop.ui:draw-table target rectangle
                                          (ntop.ui:make-table-state) config nil)
                      (cl-weave:expect (third (%wp7-table-call target :header))
                                       :to-equal
                                       "Name▲")
                      (let* ((narrow-column
                              (ntop.ui:make-table-column :id :name
                                                         :header "Name(n)"
                                                         :width-kind :hard
                                                         :width 2
                                                         :sortable-p t))
                             (narrow-target (make-wp7-fake-table-target))
                             (narrow-config
                              (ntop.ui:make-table-config :columns (list narrow-column)
                                                         :sort-column :name
                                                         :sort-order :ascending))
                             (narrow-rectangle
                              (ntop.ui:make-rectangle :x 0 :y 0 :width 2 :height 2)))
                        (ntop.ui:draw-table narrow-target narrow-rectangle
                                            (ntop.ui:make-table-state)
                                            narrow-config nil)
                        (cl-weave:expect
                         (third (%wp7-table-call narrow-target :header))
                         :to-equal
                         "N…"))))
                   (cl-weave:it "renders a descending sort mark in the selected header"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*
                                                                   :sort-column
                                                                   :name
                                                                   :sort-order
                                                                   :descending))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                16
                                                                :height
                                                                2)))
                                  (ntop.ui:draw-table target
                                                      rectangle
                                                      (ntop.ui:make-table-state)
                                                      config
                                                      nil)
                                  (cl-weave:expect
                                   (third (%wp7-table-call target :header))
                                   :to-equal
                                   "Name▼")))
                   (cl-weave:it
                    "falls back to the plain title when the scroll suffix does not fit"
                    (cl-weave:expect (ntop.ui:format-table-title "Long" 0 1 14 t)
                                     :to-equal
                                     "Long"))
                   (cl-weave:it
                    "includes the scroll position when the title and margin fit"
                    (cl-weave:expect (ntop.ui:format-table-title "Long" 0 1 15 t)
                                     :to-equal
                                     "Long(1 of 1) "))
                   (cl-weave:it
                    "draws a title with its title role at absolute coordinates"
                    (let* ((target (make-wp7-fake-table-target))
                           (config
                            (ntop.ui:make-table-config :columns
                                                       nil
                                                       :title
                                                       "Processes"
                                                       :show-scroll-position-p
                                                       t))
                           (rectangle
                            (ntop.ui:make-rectangle :x 7 :y 9 :width 30 :height 2)))
                      (ntop.ui:draw-table target
                                          rectangle
                                          (ntop.ui:make-table-state :selected-index 5)
                                          config
                                          (quote ((:name "one"))))
                      (cl-weave:expect (%wp7-table-call target :title)
                                       :to-equal
                                       '(7 9 "Processes(1 of 1) " :title))))
                   (cl-weave:it "adds header sort and row selection click areas"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*
                                                                   :sort-column
                                                                   :pid))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                5
                                                                :y
                                                                7
                                                                :width
                                                                12
                                                                :height
                                                                3))
                                       (areas
                                        (ntop.ui:draw-table target
                                                            rectangle
                                                            (ntop.ui:make-table-state)
                                                            config
                                                            '((:name "one" :pid 1))))
                                       (area-rectangles
                                        (mapcar #'ntop.ui:clickable-area-rectangle areas)))
                                  (cl-weave:expect (%wp7-click-values areas)
                                                   :to-equal
                                                   '(:name :pid 0))
                                  (cl-weave:expect
                                   (mapcar
                                    (lambda (area)
                                      (list (ntop.ui:rectangle-x area)
                                            (ntop.ui:rectangle-y area)
                                            (ntop.ui:rectangle-width area)
                                            (ntop.ui:rectangle-height area)))
                                    area-rectangles)
                                   :to-equal
                                   '((5 7 8 1) (13 7 4 1) (5 8 12 1)))
                                  (cl-weave:expect
                                   (mapcar #'ntop.ui:clickable-area-action areas)
                                   :to-equal
                                   '(:sort-column :sort-column :select-row))))
                   (cl-weave:it
                    "keeps header click identities after a hidden column is skipped"
                    (let* ((target (make-wp7-fake-table-target))
                           (columns
                            (list
                             (ntop.ui:make-table-column :id
                                                        :first
                                                        :header
                                                        "A"
                                                        :width-kind
                                                        :hard
                                                        :width
                                                        2
                                                        :sortable-p
                                                        t)
                             (ntop.ui:make-table-column :id
                                                        :hidden
                                                        :header
                                                        "H"
                                                        :width-kind
                                                        :hard
                                                        :width
                                                        6
                                                        :hidden-p
                                                        t
                                                        :sortable-p
                                                        t)
                             (ntop.ui:make-table-column :id
                                                        :last
                                                        :header
                                                        "C"
                                                        :width-kind
                                                        :hard
                                                        :width
                                                        3
                                                        :sortable-p
                                                        t)))
                           (config (ntop.ui:make-table-config :columns columns))
                           (rectangle
                            (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 2))
                           (areas
                            (ntop.ui:draw-table target
                                                rectangle
                                                (ntop.ui:make-table-state)
                                                config
                                                nil)))
                      (cl-weave:expect (%wp7-click-values areas)
                                       :to-equal
                                       '(:first :last))
                      (cl-weave:expect
                       (mapcar
                        (lambda (call)
                          (list (first call) (third call)))
                        (%wp7-table-calls target))
                       :to-equal
                       '((0 "A") (4 "C")))))
                   (cl-weave:it "draws the selected row with the selected role"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                16
                                                                :height
                                                                3)))
                                  (ntop.ui:draw-table target
                                                      rectangle
                                                      (ntop.ui:make-table-state
                                                       :selected-index
                                                       1)
                                                      config
                                                      '((:name "one" :pid 1)
                                                        (:name "two" :pid 2)))
                                  (cl-weave:expect
                                   (mapcar #'fourth (subseq (%wp7-table-calls target) 2))
                                   :to-equal
                                   '(:normal :normal :selected :selected))))
                   (cl-weave:it "keeps the selected row visible after scrolling down"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*))
                                       (state
                                        (ntop.ui:make-table-state :selected-index 4))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                16
                                                                :height
                                                                4))
                                       (rows
                                        '((:name "zero" :pid 0) (:name "one" :pid 1)
                                          (:name "two" :pid 2)
                                          (:name "three" :pid 3)
                                          (:name "four" :pid 4))))
                                  (ntop.ui:draw-table target rectangle state config rows)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-scroll-offset state)
                                   :to-be
                                   2)
                                  (cl-weave:expect
                                   (mapcar #'third (subseq (%wp7-table-calls target) 2))
                                   :to-equal
                                   '("two      " "2    "
                                                 "three    "
                                                 "3    "
                                                 "four     "
                                                 "4    "))))
                   (cl-weave:it "moves selection down and clamps it at the final row"
                                (let ((state (ntop.ui:make-table-state)))
                                  (cl-weave:expect
                                   (ntop.ui:move-table-selection state 5 3 2)
                                   :to-be
                                   2)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-scroll-offset state)
                                   :to-be
                                   1)))
                   (cl-weave:it "moves selection up and clamps it at the first row"
                                (let ((state
                                       (ntop.ui:make-table-state :selected-index
                                                                 2
                                                                 :scroll-offset
                                                                 1)))
                                  (cl-weave:expect
                                   (ntop.ui:move-table-selection state -5 3 2)
                                   :to-be
                                   0)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-scroll-offset state)
                                   :to-be
                                   0)))
                   (cl-weave:it "does not draw a header in a one-row viewport"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                5
                                                                :width
                                                                16
                                                                :height
                                                                1))
                                       (areas
                                        (ntop.ui:draw-table target
                                                            rectangle
                                                            (ntop.ui:make-table-state)
                                                            config
                                                            '((:name "one" :pid 1)))))
                                  (cl-weave:expect
                                   (mapcar #'fourth (%wp7-table-calls target))
                                   :to-equal
                                   '(:selected :selected))
                                  (cl-weave:expect (%wp7-click-values areas)
                                                   :to-equal
                                                   '(0))))
                   (cl-weave:it "can suppress an unfocused current-row highlight"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config (%wp7-unfocused-config))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                16
                                                                :height
                                                                2)))
                                  (ntop.ui:draw-table target
                                                      rectangle
                                                      (ntop.ui:make-table-state)
                                                      config
                                                      '((:name "one" :pid 1)))
                                  (cl-weave:expect
                                   (mapcar #'fourth (subseq (%wp7-table-calls target) 2))
                                   :to-equal
                                   '(:normal :normal))))
                   (cl-weave:it "draws no cells for an empty row list"
                                (let* ((target (make-wp7-fake-table-target))
                                       (config
                                        (ntop.ui:make-table-config :columns
                                                                   *wp7-render-columns*))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                16
                                                                :height
                                                                2)))
                                  (ntop.ui:draw-table target
                                                      rectangle
                                                      (ntop.ui:make-table-state)
                                                      config
                                                      nil)
                                  (cl-weave:expect
                                   (mapcar #'fourth (%wp7-table-calls target))
                                   :to-equal
                                   '(:header :header))))
                   (cl-weave:it "keeps the selection when the viewport height is zero"
                                (let ((state
                                       (ntop.ui:make-table-state :selected-index
                                                                 2
                                                                 :scroll-offset
                                                                 1)))
                                  (multiple-value-bind (start end)
                                      (ntop.ui:table-visible-window state 5 0)
                                    (cl-weave:expect (list start end)
                                                     :to-equal
                                                     (list 1 1)))
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index state)
                                   :to-be
                                   2)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-scroll-offset state)
                                   :to-be
                                   1)))
                   (cl-weave:it "moves selection to zero for an empty table"
                                (let ((state (ntop.ui:make-table-state :selected-index 3)))
                                  (cl-weave:expect
                                   (ntop.ui:move-table-selection state 1 0 0)
                                   :to-be
                                   0)
                                  (cl-weave:expect
                                   (ntop.ui:table-state-selected-index state)
                                   :to-be
                                   0)))
                   (cl-weave:it "renders empty messages in basic and normal panes without headers"
  (let* ((basic-target (make-wp7-fake-table-target))
         (normal-target (make-wp7-fake-table-target))
         (basic-config
           (ntop.ui:make-table-config :columns *wp7-render-columns*
                                      :title nil
                                      :show-header-p nil
                                      :empty-message "No data"))
         (normal-config
           (ntop.ui:make-table-config :columns *wp7-render-columns*
                                      :title " Processes "
                                      :empty-message "No data"))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 5 :width 16 :height 3)))
    (ntop.ui:draw-table basic-target rectangle (ntop.ui:make-table-state)
                        basic-config nil)
    (ntop.ui:draw-table normal-target rectangle (ntop.ui:make-table-state)
                        normal-config nil)
    (cl-weave:expect (%wp7-table-calls basic-target)
                     :to-equal
                     (quote ((0 5 "No data" :normal))))
    (cl-weave:expect (%wp7-table-calls normal-target)
                     :to-equal
                     (quote ((0 5 " Processes " :title)
                             (0 6 "No data" :normal)))))))
