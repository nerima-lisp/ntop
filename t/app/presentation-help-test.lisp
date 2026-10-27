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
                           (invalid-section
                            (ntop.app.presentation::help-state-transition
                             opened
                             (%wp12b-app-action
                              :navigation
                              (list :help-section "1" :extra))))
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
                       (list
                        (ntop.app.presentation::help-state-scroll-offset invalid-section)
                        (ntop.app.presentation::help-state-sectioned-p invalid-section))
                       :to-equal
                       (list 0 nil))
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
                       5)
                      (cl-weave:expect
                       (search "Search: GPU|"
                               (cl-tty-kit:screen-row-string screen 6))
                       :to-be
                       1)
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 0 0))
                       :to-equal
                       (ntop.app.presentation::%screen-style :gruvbox :border))
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 2 0))
                       :to-equal
                       (ntop.app.presentation::%screen-style :gruvbox :widget-title))
                      (cl-weave:expect
                       (cl-tty-kit:cell-style
                        (cl-tty-kit:screen-cell screen 1 1))
                       :to-equal
                       (ntop.app.presentation::%screen-style :gruvbox :text))
                      (let* ((row (cl-tty-kit:screen-row-string screen 1))
                             (match (search "GPU" row :test #'char-equal)))
                        (cl-weave:expect (not (null match)) :to-be t)
                        (cl-weave:expect
                         (cl-tty-kit:cell-style
                          (cl-tty-kit:screen-cell screen (1+ match) 1))
                         :to-equal
                         (ntop.app.presentation::%screen-style :gruvbox :selected-text)))))
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
                                   (ntop.app.presentation::%screen-style :default :border))
                                  (cl-weave:expect
                                   (cl-tty-kit:cell-style
                                    (cl-tty-kit:screen-cell screen 0 1))
                                   :to-equal
                                   (ntop.app.presentation::%screen-style :default :border)))))

(cl-weave:describe
 "application help presentation boundary behavior"
 (cl-weave:it
  "handles word deletion, unchanged queries, and search mode transitions"
  (flet ((transition (state value)
           (ntop.app.presentation::help-state-transition
            state (%wp12b-app-action :search value)))
         (query (state) (ntop.app.presentation::help-state-query state))
         (offset (state) (ntop.app.presentation::help-state-scroll-offset state))
         (active-p (state)
           (ntop.app.presentation::help-state-search-active-p state)))
    (let* ((query-text (concatenate 'string "gpu  memory" (string #\Tab) " "))
           (state (ntop.app.presentation::make-help-state
                   :query query-text :scroll-offset 9))
           (deleted (transition state :delete-word-backward))
           (inserted (transition state "x"))
           (cleared (transition state :clear))
           (empty-delete
             (transition (ntop.app.presentation::make-help-state
                          :scroll-offset 7)
                         :delete-backward))
           (unchanged (transition state :unknown))
           (opened (transition
                    (ntop.app.presentation::make-help-state :scroll-offset 6)
                    :help))
           (submitted (transition
                       (ntop.app.presentation::make-help-state
                        :query "cpu" :search-active-p t)
                       :submit)))
      (cl-weave:expect (query deleted) :to-equal "gpu  ")
      (cl-weave:expect (offset deleted) :to-equal 0)
      (cl-weave:expect (query inserted) :to-equal (concatenate 'string query-text "x"))
      (cl-weave:expect (offset inserted) :to-equal 0)
      (cl-weave:expect (query cleared) :to-equal "")
      (cl-weave:expect (query empty-delete) :to-equal "")
      (cl-weave:expect (offset empty-delete) :to-equal 7)
      (cl-weave:expect (query unchanged) :to-equal query-text)
      (cl-weave:expect (offset unchanged) :to-equal 9)
      (cl-weave:expect (offset opened) :to-equal 0)
      (cl-weave:expect (active-p opened) :to-be t)
      (cl-weave:expect (query submitted) :to-equal "cpu")
      (cl-weave:expect (active-p submitted) :to-be nil))))
 (cl-weave:it
  "applies navigation actions at their scroll boundaries"
  (flet ((move (state kind value page-size)
           (ntop.app.presentation::help-state-transition
            state (%wp12b-app-action kind value) :page-size page-size))
         (offset (state) (ntop.app.presentation::help-state-scroll-offset state)))
    (let* ((state (ntop.app.presentation::make-help-state
                   :selected-widget :process))
           (last-offset
             (max 0 (- (length (ntop.app.presentation::%help-visible-lines state)) 2)))
           (cases
             (list (list :navigation :first-row 4 2 0)
                   (list :navigation :last-row 0 2 :end)
                   (list :navigation :up 2 2 1)
                   (list :navigation :scroll-up 0 2 0)
                   (list :navigation :previous-row :end 2
                         (max 0 (1- last-offset)))
                   (list :navigation :down 2 2 (min last-offset 3))
                   (list :navigation :scroll-down last-offset 2 last-offset)
                   (list :navigation :next-row -1 2 (min last-offset 1))
                   (list :navigation :page-up 1 4 0)
                   (list :navigation :page-down 1 4 (min last-offset 5))
                   (list :navigation :half-page-up 1 4 0)
                   (list :navigation :half-page-down 1 4 (min last-offset 3))
                   (list :navigation :half-page-down 0 1 (min last-offset 1))
                   (list :navigation :unknown 5 2 5)
                   (list :pause nil 4 2 4))))
      (dolist (test-case cases)
        (destructuring-bind (kind value start page-size expected) test-case
          (let ((input (ntop.app.presentation::make-help-state
                        :selected-widget :process :scroll-offset start)))
            (cl-weave:expect
             (offset (move input kind value page-size))
             :to-equal expected)))))))
 (cl-weave:it
  "maps widget aliases and ignores non-string catalog fields"
  (dolist (case
           '((:process ("tables" "process table" "normal process table"))
             (:process-table ("tables" "process table" "normal process table"))
             (:process-tree ("tables" "process table" "process tree"))
             (:disk ("tables" "disk table" "Disks"))
             (:disks ("tables" "disk table" "Disks"))
             (:temperature ("tables" "temperature table" "Temperatures"))
             (:battery ("battery tabs" "battery widget"))
             (:cpu ("graphs" "CPU legend")) (:memory ("graphs" "Memory"))
             (:network ("graphs")) (:gpu ("graphs"))
             (:temperature-graph ("graphs")) (:disk-io-graph ("graphs"))
             (:unknown nil)))
    (cl-weave:expect
     (ntop.app.presentation::%help-widget-contexts (first case))
     :to-equal (second case)))
  (let ((entry (ntop.app.domain::make-parity-key
                :keys '(17) :action nil :contexts nil :behavior nil)))
    (cl-weave:expect
     (ntop.app.presentation::%help-entry-matches-query-p entry "cpu")
     :to-be nil)))
 (cl-weave:it
  "renders help without a query, short panels, and the final scrolled line"
  (let* ((config (ntop.shared.application:make-application-config :theme :default))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 50 :height 4))
         (empty-screen (cl-tty-kit:make-screen 50 4))
         (empty-state (ntop.app.presentation::make-help-state
                       :selected-widget :unknown))
         (no-match-screen (cl-tty-kit:make-screen 50 4))
         (no-match-state (ntop.app.presentation::make-help-state
                          :selected-widget :unknown
                          :query "not-present")))
    (ntop.ui:draw-widget :help empty-screen rectangle empty-state config nil)
    (ntop.ui:draw-widget :help no-match-screen rectangle no-match-state config nil)
    (cl-weave:expect
     (not (null (search "Quit" (cl-tty-kit:screen-row-string empty-screen 1))))
     :to-be t)
    (cl-weave:expect (search "No matching key bindings."
                             (cl-tty-kit:screen-row-string no-match-screen 1))
                     :to-be 1)
    (setf (ntop.app.presentation::help-state-sectioned-p no-match-state) t)
    (ntop.ui:draw-widget :help no-match-screen rectangle no-match-state config nil)
    (cl-weave:expect (search "No matching key bindings."
                             (cl-tty-kit:screen-row-string no-match-screen 1))
                     :to-be 1)
    (dolist (height (list 1 2))
      (let* ((screen (cl-tty-kit:make-screen 20 height))
             (short-rectangle
               (ntop.ui:make-rectangle :x 0 :y 0 :width 20 :height height)))
        (ntop.ui:draw-widget :help screen short-rectangle empty-state config nil)
        (cl-weave:expect (search "Help:" (cl-tty-kit:screen-row-string screen 0))
                         :to-be 3)))
    (let* ((state (ntop.app.presentation::make-help-state
                   :selected-widget :process :scroll-offset :end))
           (lines (ntop.app.presentation::%help-visible-lines state))
           (screen (cl-tty-kit:make-screen 100 4))
           (wide-rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 100 :height 4)))
      (ntop.ui:draw-widget :help screen wide-rectangle state config nil)
      (let* ((visible
              (ntop.ui:truncate-table-text (car (last lines)) 98))
             (row (cl-tty-kit:screen-row-string screen 2))
             (body (subseq row 1 (1- (length row))))
             (padding
              (make-string (- 98 (ntop.ui:measure-table-text visible))
                           :initial-element #\Space)))
        (cl-weave:expect body
                         :to-equal
                         (concatenate (quote string) visible padding))))
    (cl-weave:expect (ntop.app.presentation::%help-border-line 0 #\┌ #\┐)
                     :to-equal "")
    (cl-weave:expect (ntop.app.presentation::%help-border-line 1 #\│ #\│)
                     :to-equal "│")
    (let ((before (cl-tty-kit:screen-row-string empty-screen 0)))
      (ntop.app.presentation::%help-write empty-screen 0 0 "" nil)
      (ntop.app.presentation::%help-write-line
       empty-screen 0 0 0 "ignored" nil "" nil)
      (cl-weave:expect (cl-tty-kit:screen-row-string empty-screen 0)
                       :to-equal before))))
)
