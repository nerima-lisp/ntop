(in-package #:ntop.test)

(defun %wp12b-help-entry-action (entry)
  (case (ntop.app.domain:parity-entry-kind entry)
    (:key (ntop.app.domain:parity-key-action entry))
    (:extension (ntop.app.domain:parity-extension-action entry))))

(defun %wp12b-help-action-present-p (entries action)
  (some
   (lambda (entry)
     (string= action (%wp12b-help-entry-action entry)))
   entries))

(defun %wp12b-app-action (kind value)
  (ntop.app.domain:make-app-action :kind kind :value value))

(cl-weave:describe "application help presentation"
                   (cl-weave:it
                    "builds independent search and navigation states from application actions"
                    (let* ((state
                            (ntop.app.presentation::make-help-state
                             :selected-widget
                             :memory))
                           (opened
                            (ntop.app.presentation::help-state-transition state
                                                                          (%wp12b-app-action
                                                                           :search
                                                                           :help)))
                           (typed
                            (ntop.app.presentation::help-state-transition opened
                                                                          (%wp12b-app-action
                                                                           :search
                                                                           "gpu")))
                           (scrolled
                            (ntop.app.presentation::help-state-transition opened
                                                                          (%wp12b-app-action
                                                                           :navigation
                                                                           :down)
                                                                          :page-size
                                                                          1))
                           (deleted
                            (ntop.app.presentation::help-state-transition typed
                                                                          (%wp12b-app-action
                                                                           :search
                                                                           :delete-backward)))
                           (submitted
                            (ntop.app.presentation::help-state-transition typed
                                                                          (%wp12b-app-action
                                                                           :search
                                                                           :submit))))
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-query state)
                       :to-equal
                       "")
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-search-active-p state)
                       :to-be
                       nil)
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-search-active-p
                        opened)
                       :to-be
                       t)
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-query typed)
                       :to-equal
                       "gpu")
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-scroll-offset
                        scrolled)
                       :to-equal
                       1)
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-query deleted)
                       :to-equal
                       "gp")
                      (cl-weave:expect
                       (ntop.app.presentation::help-state-search-active-p
                        submitted)
                       :to-be
                       nil)))
                   (cl-weave:it
                    "shows keys from the selected widget context, help, and extensions"
                    (let* ((process
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :process)))
                           (process-search
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :process
                              :search-active-p
                              t)))
                           (memory
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :memory)))
                           (disk
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :disk)))
                           (network
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :network))))
                      (cl-weave:expect
                       (%wp12b-help-action-present-p process
                                                     "Sort process by CPU")
                       :to-be
                       t)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p process
                                                     "Documented horizontal movement")
                       :to-be
                       t)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p process
                                                     "Enter search text")
                       :to-be
                       nil)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p process-search
                                                     "Enter search text")
                       :to-be
                       t)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p memory
                                                     "Cycle selected widget view")
                       :to-be
                       t)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p disk
                                                     "Select disk column 0")
                       :to-be
                       t)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p network
                                                     "Cycle selected widget view")
                       :to-be
                       nil)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p network
                                                     "Scroll history backward")
                       :to-be
                       t)))
                   (cl-weave:it
                    "filters contextual keys case-insensitively by action and behavior"
                    (let* ((state
                            (ntop.app.presentation::make-help-state
                             :selected-widget
                             :memory
                             :query
                             "gpu"))
                           (entries
                            (ntop.app.presentation::help-state-visible-entries
                             state))
                           (empty
                            (ntop.app.presentation::help-state-visible-entries
                             (ntop.app.presentation::make-help-state
                              :selected-widget
                              :memory
                              :query
                              "no such key"))))
                      (cl-weave:expect (length entries) :to-equal 1)
                      (cl-weave:expect
                       (%wp12b-help-action-present-p entries
                                                     "Cycle selected widget view")
                       :to-be
                       t)
                      (cl-weave:expect empty :to-equal nil)))
                   (cl-weave:it "declares the help widget through define-widget"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.ui:define-widget :help
                                                          (target rectangle
                                                                  state
                                                                  config
                                                                  series)
                                                          :rendered))
                                 :to-equal
                                 '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key
                                                                   (eql :help))
                                                                  target
                                                                  rectangle
                                                                  state
                                                                  config
                                                                  series)
                                    (declare (ignore ntop.ui::widget-key))
                                    :rendered)))
                   (cl-weave:it
                    "renders a contextual help panel with theme styles and search highlights"
                    (let* ((config
                            (ntop.shared.application:make-application-config
                             :theme
                             :gruvbox))
                           (screen (cl-tty-kit:make-screen 60 8))
                           (rectangle
                            (ntop.ui:make-rectangle :x
                                                    0
                                                    :y
                                                    0
                                                    :width
                                                    60
                                                    :height
                                                    8))
                           (state
                            (ntop.app.presentation::make-help-state
                             :selected-widget
                             :memory
                             :query
                             "GPU"
                             :search-active-p
                             t)))
                      (ntop.ui:draw-widget :help
                                           screen
                                           rectangle
                                           state
                                           config
                                           nil)
                      (cl-weave:expect
                       (search "Help: Memory"
                               (cl-tty-kit:screen-row-string screen 0))
                       :to-be
                       3)
                      (cl-weave:expect
                       (search "Cycle selected widget view"
                               (cl-tty-kit:screen-row-string screen 1))
                       :to-be
                       4)
                      (cl-weave:expect
                       (search "Search: GPU|"
                               (cl-tty-kit:screen-row-string screen 6))
                       :to-be
                       1)
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 0 0))
                       :to-equal
                       (ntop.ui:presentation-theme-style :gruvbox :border))
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 2 0))
                       :to-equal
                       (ntop.ui:presentation-theme-style :gruvbox :widget-title))
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 1 1))
                       :to-equal
                       (ntop.ui:presentation-theme-style :gruvbox :text))
                      (let* ((row (cl-tty-kit:screen-row-string screen 1))
                             (match (search "GPU" row :test #'char-equal)))
                        (cl-weave:expect (not (null match)) :to-be t)
                        (cl-weave:expect
                         (cl-tty-kit:cell-style
                          (cl-tty-kit:screen-cell screen (1+ match) 1))
                         :to-equal
                         (ntop.ui:presentation-theme-style :gruvbox
                                                           :selected-text)))))
                   (cl-weave:it "handles short and one-cell help rectangles"
                                (let* ((screen (cl-tty-kit:make-screen 3 3))
                                       (rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                3
                                                                :height
                                                                3))
                                       (one-column-rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                1
                                                                :height
                                                                3))
                                       (zero-width-rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                0
                                                                :height
                                                                3))
                                       (zero-height-rectangle
                                        (ntop.ui:make-rectangle :x
                                                                0
                                                                :y
                                                                0
                                                                :width
                                                                3
                                                                :height
                                                                0))
                                       (config
                                        (ntop.shared.application:make-application-config
                                         :theme
                                         :default)))
                                  (ntop.ui:draw-widget :help
                                                       screen
                                                       rectangle
                                                       (ntop.app.presentation::make-help-state)
                                                       config
                                                       nil)
                                  (ntop.ui:draw-widget :help
                                                       screen
                                                       one-column-rectangle
                                                       (ntop.app.presentation::make-help-state)
                                                       config
                                                       nil)
                                  (ntop.ui:draw-widget :help
                                                       screen
                                                       zero-width-rectangle
                                                       (ntop.app.presentation::make-help-state)
                                                       config
                                                       nil)
                                  (ntop.ui:draw-widget :help
                                                       screen
                                                       zero-height-rectangle
                                                       (ntop.app.presentation::make-help-state)
                                                       config
                                                       nil)
                                  (cl-weave:expect
                                   (cl-tty-kit:screen-height screen)
                                   :to-be
                                   3)
                                  (cl-weave:expect
                                   (cl-tty-kit:cell-style
                                    (cl-tty-kit:screen-cell screen 0 0))
                                   :to-equal
                                   (ntop.ui:presentation-theme-style :default
                                                                     :border))
                                  (cl-weave:expect
                                   (cl-tty-kit:cell-style
                                    (cl-tty-kit:screen-cell screen 0 1))
                                   :to-equal
                                   (ntop.ui:presentation-theme-style :default
                                                                     :border)))))
