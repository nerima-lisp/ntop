(in-package #:ntop.test)

(cl-weave:describe "application screen composition"
                   (cl-weave:it
                    "matches the specified FR-001 layout at each required terminal size"
                    (dolist
                        (case '((80 24 "application-80x24.txt")
                                (160 48 "application-160x48.txt")
                                (40 12 "application-40x12.txt")))
                      (destructuring-bind (columns rows golden) case
                        (multiple-value-bind (areas screen output)
                            (%wp12b-screen-render columns rows)
                          (declare (ignore areas))
                          (cl-weave:expect (list (cl-tty-kit:screen-width screen)
                                                 (cl-tty-kit:screen-height screen))
                                           :to-equal (list columns rows))
                          (cl-weave:expect (%wp12b-screen-bytes output) :to-equal
                                           (%wp12b-screen-golden-bytes golden))))))
                   (cl-weave:it
                    "renders the Memory legend within its configured pane"
                    (multiple-value-bind (areas screen output)
                        (%wp12b-screen-render 80 24)
                      (declare (ignore areas output))
                      (cl-weave:expect
                       (%wp12b-screen-memory-legend-summary 80 24 screen)
                       :to-be t))
                    (multiple-value-bind (areas screen output)
                        (%wp12b-screen-render 160 48)
                      (declare (ignore areas output))
                      (let ((rows
                             (%wp12b-screen-memory-legend-summary
                              160 48 screen)))
                        (cl-weave:expect
                         (first rows)
                         :to-equal "│RAM: 62%   5.0GiB/8.0GiB│")
                        (cl-weave:expect
                         (second rows)
                         :to-equal "│SWP: 50%   1.0GiB/2.0GiB│"))))
                   (cl-weave:it
                    "renders only startup widgets and draws the CPU parent once"
                    (let ((state (ntop.app.domain:make-app-state))
                          (*wp12b-screen-widget-calls* nil)
                          (*wp12b-screen-recording-p* t))
                      (%wp12b-screen-render 80 24 :state state)
                      (setf *wp12b-screen-widget-calls*
                            (nreverse *wp12b-screen-widget-calls*))
                      (cl-weave:expect
                       (mapcar (function first) *wp12b-screen-widget-calls*)
                       :to-equal
                       (quote (:cpu :memory :network :disk :temperature :process-table)))
                      (cl-weave:expect
                       (loop for widget in (quote (:disk :temperature))
                             for key in (quote (:disks :temperature))
                             for call = (find widget *wp12b-screen-widget-calls*
                                              :key (function first))
                             always (and (typep (fourth call) (quote ntop.ui:table-state))
                                         (not (nth-value
                                               1
                                               (gethash
                                                key
                                                (ntop.app.domain:app-state-widget-v-states
                                                 state))))))
                       :to-be t)
                      (progn
                        (cl-weave:expect
                         (count :cpu *wp12b-screen-widget-calls* :key (function first))
                         :to-equal 1)
                        (let* ((placements
                                (ntop.ui:layout-widgets 80 24
                                                        :cpu-height 1
                                                        :memory-rows 2
                                                        :table-widget :process))
                               (cpu (assoc :cpu placements :test (function eq)))
                               (legend (assoc :cpu-legend placements :test (function eq)))
                               (cpu-call
                                (find :cpu *wp12b-screen-widget-calls* :key (function first)))
                               (cpu-rectangle (second cpu-call)))
                          (cl-weave:expect
                           (ntop.ui:rectangle-width cpu-rectangle)
                           :to-equal
                           (- (+ (ntop.ui:rectangle-width (cdr cpu))
                                 (ntop.ui:rectangle-width (cdr legend)))
                              2))))))
                   (cl-weave:it
                    "renders the basic selector and honors no-color table roles"
                    (let* ((state (ntop.app.domain:make-app-state :basic-mode-p t))
                           (roles '((:normal :text) (:header :table-header)
                                    (:selected :selected-text) (:title :widget-title)
                                    (:custom :text)))
                           (ntop.app.presentation::*screen-table-theme* :gruvbox)
                           (*wp12b-screen-widget-calls* nil)
                           (*wp12b-screen-recording-p* t))
                      (multiple-value-bind (areas screen output)
                          (%wp12b-screen-render 80 24 :state state)
                        (declare (ignore areas))
                        (cl-weave:expect
                         (and
                          (not (null (member :basic-table-arrows
                                             *wp12b-screen-widget-calls*
                                             :key (function first)
                                             :test (function eq))))
                          (and (search "◄ Disks" output)
                               (search "Temperature ►" output))
                          (loop for (role theme-role) in roles
                                do (ntop.ui:draw-table-text screen 0 1 "x" role)
                                always
                                (and
                                 (equal (cl-tty-kit:cell-style
                                         (cl-tty-kit:screen-cell screen 0 1))
                                        (ntop.app.presentation::%screen-table-style
                                         :gruvbox role))
                                 (equal (ntop.app.presentation::%screen-table-style
                                         :gruvbox role nil)
                                        (ntop.ui:presentation-theme-style
                                         :gruvbox theme-role))))
                          (and
                            (not (ntop.app.presentation::%screen-no-color-p nil))
                            (not (ntop.app.presentation::%screen-no-color-p ""))
                            (ntop.app.presentation::%screen-no-color-p "1")
                            (and
                             (null (ntop.app.presentation::%screen-style
                                    :default :border t))
                             (let ((ntop.ui::*presentation-no-color-p* t))
                               (let* ((cpu-colors
                                        (ntop.ui:presentation-theme-style
                                         :default :cpu-colors))
                                      (gpu-colors
                                        (ntop.ui:presentation-theme-style
                                         :default :gpu-colors))
                                      (default-cpu-colors
                                        (let ((ntop.ui::*presentation-no-color-p*
                                               nil))
                                          (ntop.ui:presentation-theme-style
                                           :default :cpu-colors)))
                                      (default-gpu-colors
                                        (let ((ntop.ui::*presentation-no-color-p*
                                               nil))
                                          (ntop.ui:presentation-theme-style
                                           :default :gpu-colors))))
                                 (and
                                  (null (ntop.ui:presentation-theme-style
                                         :default :text))
                                  (null (ntop.ui:presentation-theme-style
                                         :default :table-header))
                                  (null (ntop.ui:presentation-theme-style
                                         :default :selected-text))
                                  (= (length cpu-colors)
                                     (length default-cpu-colors))
                                  (every #'null cpu-colors)
                                  (= (length gpu-colors)
                                     (length default-gpu-colors))
                                  (every #'null gpu-colors)))))
                            (equal (ntop.app.presentation::%screen-style
                                    :default :border nil)
                                   (ntop.ui:presentation-theme-style
                                    :default :border)))
                          (null
                                  (ntop.app.presentation::%screen-table-style
                                   :default :header t))
                          (equal (ntop.app.presentation::%screen-table-style
                                  :default :header nil)
                                 (ntop.ui:presentation-theme-style
                                  :default :table-header))
                          (let ((selector-position
                                  (loop for y below (cl-tty-kit:screen-height screen)
                                        thereis
                                        (loop for x below (cl-tty-kit:screen-width screen)
                                              when (char= #\◄
                                                          (cl-tty-kit:cell-char
                                                           (cl-tty-kit:screen-cell
                                                            screen x y)))
                                                return (list x y)))))
                            (and selector-position
                                 (equal
                                  (cl-tty-kit:cell-style
                                   (apply #'cl-tty-kit:screen-cell
                                          screen selector-position))
                                  (ntop.ui:presentation-theme-style
                                   (ntop.shared.application:application-config-theme
                                    (%wp12b-screen-config))
                                   :highlighted-border)))))
                         :to-be
                         t))))
                   (cl-weave:it
                    "shows the basic Process header when the table pane has room"
                    (let ((state (ntop.app.domain:make-app-state :basic-mode-p t)))
                      (multiple-value-bind (areas screen output)
                          (%wp12b-screen-render 80 24 :state state)
                        (declare (ignore areas screen))
                        (cl-weave:expect
                         (and (search "Count" output)
                              (not (search "Processes" output)))
                         :to-be
                         t))))
                   (cl-weave:it
                    "computes process CPU and I/O rates from the previous sample"
                    (let ((*wp12b-screen-widget-calls* nil) (*wp12b-screen-recording-p* t))
                      (%wp12b-screen-render 80 24
                                            :config (%wp12b-screen-config))
                      (let* ((process-call
                              (find :process-table
                                    *wp12b-screen-widget-calls*
                                    :key
                                    #'first
                                    :test
                                    #'eq))
                             (rows (third process-call))
                             (alpha
                              (find 100
                                    rows
                                    :key
                                    #'ntop.process.domain:process-row-pid
                                    :test
                                    #'eql)))
                        (cl-weave:expect (not (null alpha)) :to-be t)
                        (cl-weave:expect
                         (list
                          (ntop.process.domain:process-row-cpu-percent alpha)
                          (ntop.process.domain:process-row-read-bytes-per-second
                           alpha)
                          (ntop.process.domain:process-row-write-bytes-per-second
                           alpha))
                         :to-equal
                         '(5.0d0 2000.0d0 500.0d0)))))
                   (cl-weave:it
                    "keeps selected table focus without a default frame highlight"
                    (dolist (selected-widget '(:process :disks :temperature))
                      (let* ((state (ntop.app.domain:make-app-state))
                             (*wp12b-screen-widget-calls* nil)
                             (*wp12b-screen-recording-p* t))
                        (setf (ntop.app.domain:app-state-selected-widget state)
                              selected-widget)
                        (multiple-value-bind (areas screen)
                            (%wp12b-screen-render 80 24 :state state)
                          (declare (ignore areas))
                          (let ((focused-p
                                 (loop for widget in '(:disk :temperature :process-table)
                                       for call = (find widget
                                                        *wp12b-screen-widget-calls*
                                                        :key #'first)
                                       collect
                                       (if (eq widget :process-table)
                                           (ntop.process.presentation:process-table-config-focused-p
                                            (fifth call))
                                         (ntop.ui:table-config-focused-p
                                          (fifth call))))))
                            (cl-weave:expect
                             focused-p
                             :to-equal
                             (list (eq selected-widget :disks)
                                   (eq selected-widget :temperature)
                                   (eq selected-widget :process))))
                          (when (eq selected-widget :process)
                            (let* ((process-call (find :process-table
                                                       *wp12b-screen-widget-calls*
                                                       :key #'first))
                                   (rectangle (second process-call)))
                              (cl-weave:expect
                               (cl-tty-kit:cell-style
                                (cl-tty-kit:screen-cell
                                 screen
                                 (- (ntop.ui:rectangle-x rectangle) 1)
                                 (ntop.ui:rectangle-y rectangle)))
                               :to-equal
                               (ntop.ui:presentation-theme-style
                                :default
                                :border))))))))
                   (cl-weave:it "highlights CPU graph and legend frames by their separate IDs"
  (let* ((ntop.app.presentation::*screen-selection-active-p* t)
         (state (ntop.app.domain:make-app-state :selected-widget :cpu))
         (normal-style
           (ntop.ui:presentation-theme-style :default :border))
         (selected-style
           (ntop.ui:presentation-theme-style :default :highlighted-border))
         (expanded-config (%wp12b-screen-default-config)))
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24 :state state)
      (cl-weave:expect
       (and
        (not
         (null
          (find-if (lambda (area)
                     (and (eq (ntop.ui:clickable-area-action area) :screen-widget)
                          (eq (ntop.ui:clickable-area-value area) :cpu)))
                   areas)))
        (not
         (null
          (find-if (lambda (area)
                     (and (eq (ntop.ui:clickable-area-action area) :screen-widget)
                          (eq (ntop.ui:clickable-area-value area) :cpu-legend)))
                   areas))))
       :to-be t)
      (cl-weave:expect
       (list (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
             (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 46 0)))
       :to-equal
       (list selected-style normal-style)))
    (setf (gethash :screen-selection-id
                   (ntop.app.domain:app-state-widget-v-states state))
          :cpu-legend)
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24 :state state)
      (declare (ignore areas))
      (cl-weave:expect
       (list (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
             (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 46 0)))
       :to-equal
       (list normal-style selected-style)))
    (setf (ntop.shared.application:application-config-expanded-p expanded-config) t
          (gethash :screen-selection-id
                   (ntop.app.domain:app-state-widget-v-states state))
          :cpu)
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24 :state state :config expanded-config)
      (declare (ignore areas))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
       :to-equal selected-style))
    (setf (gethash :screen-selection-id
                   (ntop.app.domain:app-state-widget-v-states state))
          :cpu-legend)
    (multiple-value-bind (areas screen)
        (%wp12b-screen-render 80 24 :state state :config expanded-config)
      (declare (ignore areas))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
       :to-equal normal-style)))))
