(in-package #:ntop.test)

(defun %wp12b-app-action (kind value)
  (ntop.app.domain:make-app-action :kind kind :value value))

(cl-weave:describe "application help presentation"
  (cl-weave:it "transitions query and section state from application actions"
    (let* ((state (ntop.app.presentation::make-help-state :selected-widget :memory))
           (opened (ntop.app.presentation::help-state-transition
                    state (%wp12b-app-action :search :help)))
           (typed (ntop.app.presentation::help-state-transition
                   opened (%wp12b-app-action :search "gpu")))
           (deleted (ntop.app.presentation::help-state-transition
                     typed (%wp12b-app-action :search :delete-backward)))
           (submitted (ntop.app.presentation::help-state-transition
                       typed (%wp12b-app-action :search :submit)))
           (invalid (ntop.app.presentation::help-state-transition
                     opened (%wp12b-app-action
                             :navigation '(:help-section "1" :extra)))))
      (cl-weave:expect (ntop.app.presentation::help-state-query state) :to-equal "")
      (cl-weave:expect (ntop.app.presentation::help-state-search-active-p opened)
                       :to-be t)
      (cl-weave:expect (ntop.app.presentation::help-state-query typed)
                       :to-equal "gpu")
      (cl-weave:expect (ntop.app.presentation::help-state-query deleted)
                       :to-equal "gp")
      (cl-weave:expect (ntop.app.presentation::help-state-search-active-p submitted)
                       :to-be nil)
      (cl-weave:expect
       (list (ntop.app.presentation::help-state-scroll-offset invalid)
             (ntop.app.presentation::help-state-sectioned-p invalid))
       :to-equal '(0 nil))))
  (cl-weave:it "stores BTM's complete text in one ordered section table"
    (let* ((state (ntop.app.presentation::make-help-state))
           (lines (ntop.app.presentation::%help-visible-lines state)))
      (cl-weave:expect (length ntop.app.presentation::*help-sections*) :to-equal 10)
      (cl-weave:expect (length lines) :to-equal 143)
      (cl-weave:expect (first lines)
                       :to-equal
                       (concatenate 'string "Scroll to browse or press the number key to go to the "
                              "corresponding help menu section:"))
      (cl-weave:expect
       (not (null (search "5 - Process sort widget" (format nil "~{~A~%~}" lines))))
       :to-be t)
      (cl-weave:expect
       (not (null (member "5 - Sort widget" lines :test #'string=)))
       :to-be t)
      (cl-weave:expect
       (not
        (null
         (member
          (concatenate 'string
                       "Mouse click             Selects the clicked "
                       "widget, table entry, dialog option, or tab")
          lines
          :test #'string=)))
       :to-be t)
      (cl-weave:expect
       (not
        (null
         (member "M                       Sort by GPU memory usage, press again to reverse"
                 lines :test #'string=)))
       :to-be t)))
    (cl-weave:it "filters matching BTM sections and wraps to the requested width"
    (let* ((state (ntop.app.presentation::make-help-state :query "gPu"))
           (matches (ntop.app.presentation::%help-visible-lines state))
           (trimmed
             (ntop.app.presentation::%help-visible-lines
              (ntop.app.presentation::make-help-state :query "  gPu  ")))
           (whitespace-only
             (ntop.app.presentation::%help-visible-lines
              (ntop.app.presentation::make-help-state :query "   ")))
           (all-lines
             (ntop.app.presentation::%help-visible-lines
              (ntop.app.presentation::make-help-state)))
           (wrapped
             (ntop.app.presentation::%help-visible-lines
              (ntop.app.presentation::make-help-state) :width 28))
           (empty
             (ntop.app.presentation::%help-visible-lines
              (ntop.app.presentation::make-help-state :query "not-present"))))
      (cl-weave:expect matches
                       :to-equal
                       '("3 - Process widget"
                         "C                       Sort by GPU usage, press again to reverse"
                         "M                       Sort by GPU memory usage, press again to reverse"
                         ""
                         "4 - Process search widget"
                         "gpu%                    ex: gpu% < 4.2"))
      (cl-weave:expect trimmed :to-equal matches)
      (cl-weave:expect whitespace-only :to-equal all-lines)
      (cl-weave:expect empty :to-equal nil)
      (cl-weave:expect (> (length wrapped) (length all-lines)) :to-be t)
      (cl-weave:expect
       (every (lambda (line) (<= (ntop.ui:measure-table-text line) 28)) wrapped)
       :to-be t)))
  (cl-weave:it "jumps to the body occurrence of every numbered section"
    (let ((state (ntop.app.presentation::make-help-state))
          (sections '((1 "1 - General")
                      (2 "2 - CPU widget")
                      (3 "3 - Process widget")
                      (4 "4 - Process search widget")
                      (5 "5 - Sort widget")
                      (6 "6 - Temperature widget")
                      (7 "7 - Disk widget")
                      (8 "8 - Battery widget")
                      (9 "9 - Basic memory widget"))))
      (dolist (section sections)
        (destructuring-bind (number heading) section
          (let* ((next
                   (ntop.app.presentation::help-state-transition
                    state
                    (%wp12b-app-action
                     :navigation
                     (list :help-section (format nil "~D" number)))
                    :page-size 25 :line-width 77))
                 (lines
                   (ntop.app.presentation::%help-visible-lines state :width 77))
                 (body-position (position heading lines :test #'string= :from-end t))
                 (last-offset (max 0 (- (length lines) 25)))
                 (offset (ntop.app.presentation::help-state-scroll-offset next))
                 (visible
                   (subseq lines offset (min (length lines) (+ offset 25)))))
            (cl-weave:expect
             (ntop.app.presentation::help-state-sectioned-p next) :to-be t)
            (cl-weave:expect offset :to-equal (min body-position last-offset))
            (cl-weave:expect
             (not (null (position heading visible :test #'string=)))
             :to-be t))))))
  (cl-weave:it "declares the help widget through define-widget"
    (cl-weave:expect
     (macroexpand-1
      '(ntop.ui:define-widget :help (target rectangle state config series) :rendered))
     :to-equal
     '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key (eql :help))
                                     target rectangle state config series)
        (declare (ignore ntop.ui::widget-key))
        :rendered)))
  (cl-weave:it "renders BTM help rows, search, and theme styles"
    (let* ((config (ntop.shared.application:make-application-config :theme :gruvbox))
           (screen (cl-tty-kit:make-screen 60 8))
           (rectangle
             (ntop.ui:make-rectangle :x 0 :y 0 :width 60 :height 8))
           (state
             (ntop.app.presentation::make-help-state
              :query "GPU" :search-active-p t)))
      (ntop.ui:draw-widget :help screen rectangle state config nil)
      (cl-weave:expect (search " Help " (cl-tty-kit:screen-row-string screen 0))
                       :to-be 1)
      (cl-weave:expect (search "3 - Process widget"
                               (cl-tty-kit:screen-row-string screen 1))
                       :to-be 1)
      (cl-weave:expect (search "C                       Sort by GPU usage"
                               (cl-tty-kit:screen-row-string screen 2))
                       :to-be 1)
      (cl-weave:expect
       (search "Search: GPU"
               (cl-tty-kit:screen-row-string screen 7))
       :to-be 0)
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
       :to-equal (ntop.app.presentation::%screen-style :gruvbox :border))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 1 0))
       :to-equal (ntop.app.presentation::%screen-style :gruvbox :widget-title))
      (cl-weave:expect
       (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 1 1))
       :to-equal (ntop.app.presentation::%screen-style :gruvbox :table-header))
      (let* ((row (cl-tty-kit:screen-row-string screen 2))
             (match (search "GPU" row :test #'char-equal)))
        (cl-weave:expect (not (null match)) :to-be t)
        (cl-weave:expect
         (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen match 2))
         :to-equal (ntop.app.presentation::%screen-style :gruvbox :selected-text))))))


(cl-weave:describe "application help presentation boundary behavior"
    (cl-weave:it "handles query editing and search mode transitions"
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
               (transition (ntop.app.presentation::make-help-state :scroll-offset 7)
                           :delete-backward))
             (whitespace-deleted
               (transition
                (ntop.app.presentation::make-help-state :query "   ")
                :delete-word-backward))
             (word-deleted
               (transition
                (ntop.app.presentation::make-help-state :query "gpu")
                :delete-word-backward))
             (unchanged (transition state :unknown))
             (opened
               (transition (ntop.app.presentation::make-help-state :scroll-offset 6)
                           :help))
             (submitted
               (transition (ntop.app.presentation::make-help-state
                            :query "cpu" :search-active-p t)
                           :submit)))
        (cl-weave:expect (query deleted) :to-equal "gpu  ")
        (cl-weave:expect (offset deleted) :to-equal 0)
        (cl-weave:expect (query inserted)
                         :to-equal (concatenate 'string query-text "x"))
        (cl-weave:expect (offset inserted) :to-equal 0)
        (cl-weave:expect (query cleared) :to-equal "")
        (cl-weave:expect (query empty-delete) :to-equal "")
        (cl-weave:expect (offset empty-delete) :to-equal 7)
        (cl-weave:expect (query whitespace-deleted) :to-equal "")
        (cl-weave:expect (query word-deleted) :to-equal "")
        (cl-weave:expect (query unchanged) :to-equal query-text)
        (cl-weave:expect (offset unchanged) :to-equal 9)
        (cl-weave:expect (offset opened) :to-equal 0)
        (cl-weave:expect (active-p opened) :to-be t)
        (cl-weave:expect (query submitted) :to-equal "cpu")
        (cl-weave:expect (active-p submitted) :to-be nil)
        (let* ((search-state
                 (ntop.app.presentation::make-help-state :search-active-p t))
               (lines (ntop.app.presentation::%help-visible-lines search-state))
               (last-offset (max 0 (- (length lines) 22)))
               (before
                 (ntop.app.presentation::make-help-state
                  :search-active-p t :scroll-offset last-offset))
               (next
                 (ntop.app.presentation::help-state-transition
                  before (%wp12b-app-action :navigation :down) :page-size 21)))
          (cl-weave:expect (length lines) :to-equal 143)
          (cl-weave:expect
           (ntop.app.presentation::help-state-scroll-offset next)
           :to-equal last-offset)))))
   (cl-weave:it "applies navigation actions at their scroll boundaries"
    (flet ((move (state kind value page-size)
             (ntop.app.presentation::help-state-transition
              state (%wp12b-app-action kind value) :page-size page-size))
           (offset (state) (ntop.app.presentation::help-state-scroll-offset state)))
      (let* ((state (ntop.app.presentation::make-help-state :selected-widget :process))
             (last-offset
               (max 0 (- (length (ntop.app.presentation::%help-visible-lines state)) 2)))
             (cases
               (list (list :navigation :first-row 4 2 0)
                     (list :navigation :last-row 0 2 :end)
                     (list :navigation :up 2 2 1)
                     (list :navigation :scroll-up 0 2 0)
                     (list :navigation :previous-row :end 2 (max 0 (1- last-offset)))
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
              (cl-weave:expect (offset (move input kind value page-size))
                               :to-equal expected))))
        (let ((invalid-section
                (move (ntop.app.presentation::make-help-state :scroll-offset 7)
                      :navigation '(:not-help-section "1") 2)))
          (cl-weave:expect (offset invalid-section) :to-equal 7)
          (cl-weave:expect
           (ntop.app.presentation::help-state-sectioned-p invalid-section)
           :to-be nil)))))
     (cl-weave:it "renders help viewport boundaries and end scrolling"
    (let* ((config (ntop.shared.application:make-application-config))
           (search-screen (cl-tty-kit:make-screen 60 3))
           (zero-width-search-screen (cl-tty-kit:make-screen 8 1))
           (short-screen (cl-tty-kit:make-screen 22 2))
           (narrow-screen (cl-tty-kit:make-screen 3 5))
           (zero-width-screen (cl-tty-kit:make-screen 8 5))
           (zero-height-screen (cl-tty-kit:make-screen 8 5))
           (end-screen (cl-tty-kit:make-screen 60 8))
           (end-state (ntop.app.presentation::make-help-state :scroll-offset :end))
           (end-lines
             (ntop.app.presentation::%help-visible-lines end-state :width 57)))
      (ntop.app.presentation::%help-draw-search-line
       zero-width-search-screen 0 0 0
       (ntop.app.presentation::make-help-state :search-active-p t)
       nil nil nil)
      (cl-weave:expect
       (cl-tty-kit:screen-row-string zero-width-search-screen 0)
       :to-equal (make-string 8 :initial-element (code-char 32)))
      (ntop.ui:draw-widget
       :help search-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 60 :height 3)
       (ntop.app.presentation::make-help-state :search-active-p t)
       config nil)
      (cl-weave:expect
       (search "Type to search" (cl-tty-kit:screen-row-string search-screen 2))
       :to-be 10)
      (ntop.ui:draw-widget
       :help short-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 22 :height 2)
       (ntop.app.presentation::make-help-state :query "x" :search-active-p t)
       config nil)
      (cl-weave:expect
       (search "Esc to close" (cl-tty-kit:screen-row-string short-screen 0))
       :to-be nil)
      (cl-weave:expect
       (search "Search:" (cl-tty-kit:screen-row-string short-screen 1))
       :to-be nil)
      (ntop.ui:draw-widget
       :help narrow-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 3 :height 5)
       (ntop.app.presentation::make-help-state)
       config nil)
      (cl-weave:expect
       (search "▲" (cl-tty-kit:screen-row-string narrow-screen 1))
       :to-be 1)
      (ntop.ui:draw-widget
       :help zero-width-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 5)
       (ntop.app.presentation::make-help-state)
       config nil)
      (cl-weave:expect
       (cl-tty-kit:screen-row-string zero-width-screen 0)
       :to-equal (make-string 8 :initial-element (code-char 32)))
      (ntop.ui:draw-widget
       :help zero-height-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 0)
       (ntop.app.presentation::make-help-state)
       config nil)
      (cl-weave:expect
       (cl-tty-kit:screen-row-string zero-height-screen 0)
       :to-equal (make-string 8 :initial-element (code-char 32)))
      (ntop.ui:draw-widget
       :help end-screen
       (ntop.ui:make-rectangle :x 0 :y 0 :width 60 :height 8)
       end-state config nil)
      (cl-weave:expect
       (search (first (last end-lines))
               (cl-tty-kit:screen-row-string end-screen 6))
       :to-be 1)))
 (cl-weave:it "keeps a wide help character when it exceeds the wrap width"
    (cl-weave:expect
     (ntop.app.presentation::%help-wrap-line "界x" 1)
     :to-equal (list "界" "x"))))
