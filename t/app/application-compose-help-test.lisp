(in-package #:ntop.test)

(cl-weave:describe "A1 BTM help canvas"
  (cl-weave:it
   "clears the dashboard and matches BTM help title and search frame"
   (let* ((draw-count 0)
          (config (ntop.shared.application:make-application-config))
          (composition
            (ntop.app.application:make-application-composition
             config
             :draw-screen
             (lambda (target draw-state draw-config snapshot columns rows reasons series)
               (declare
                (ignore draw-state draw-config snapshot columns rows reasons series))
               (incf draw-count)
               (cl-tty-kit:screen-write-string target 0 0 "dashboard")
               nil)))
          (render
            (ntop.app.application:application-composition-render composition))
          (state (ntop.app.domain:make-app-state :selected-widget :process)))
     (flet ((render-current ()
              (with-output-to-string (stream)
                (let ((*standard-output* stream))
                  (funcall render state config nil 160 48 nil)))))
       (render-current)
       (setf (ntop.app.domain:app-state-active-dialog state) :help)
       (render-current)
       (let* ((screen
                (ntop.app.application::application-composition-screen composition))
              (top-row (cl-tty-kit:screen-row-string screen 10))
              (dashboard-row (cl-tty-kit:screen-row-string screen 0))
              (bottom-row (cl-tty-kit:screen-row-string screen 36)))
         (progn
           (cl-weave:expect draw-count :to-be 1)
           (cl-weave:expect
            (ntop.app.presentation:help-state-sectioned-p
             (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
            :to-be t)
           (let* ((general-row
                    (loop for row from 11 below 36
                          when
                          (search "1 - General"
                                  (cl-tty-kit:screen-row-string screen row))
                            return row))
                  (general-entry
                    (and general-row
                         (cl-tty-kit:screen-row-string screen
                                                       (1+ general-row)))))
             (cl-weave:expect (not (null general-row)) :to-be t)
             (cl-weave:expect
              (not
               (null
                (and general-entry
                     (search " - " general-entry)
                     (search ": " general-entry))))
              :to-be t))
           (cl-weave:expect
            (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 32 10))
            :to-equal
            (ntop.app.presentation::%screen-style :default :border))
           (cl-weave:expect
            (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 33 10))
            :to-equal
            (ntop.app.presentation::%screen-style :default :widget-title))
           (cl-weave:expect
            (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 32 36))
            :to-equal
            (ntop.app.presentation::%screen-style :default :border)))
         (cl-weave:expect
          (every (lambda (character) (char= character #\Space)) dashboard-row)
          :to-be t)
         (cl-weave:expect (search " Help " top-row) :to-be 33)
         (cl-weave:expect (char-code (char top-row 33)) :to-be 32)
         (cl-weave:expect (search "Esc to close" top-row) :to-be 114)
         (cl-weave:expect (search "Help: Process" top-row) :to-be nil)
         (cl-weave:expect (char bottom-row 32) :to-equal #\└)
         (cl-weave:expect (char bottom-row 127) :to-equal #\┘))
       (progn
        (remhash :help (ntop.app.domain:app-state-widget-v-states state))
        (funcall
         (ntop.app.application:application-composition-on-action composition)
         state
         (ntop.app.domain:make-app-action :kind :help))
        (render-current)
        (cl-weave:expect
          (ntop.app.presentation:help-state-sectioned-p
           (gethash :help (ntop.app.domain:app-state-widget-v-states state)))
          :to-be t)
         (cl-weave:expect
          (not
           (null
            (loop for row from 11 below 36
                  thereis
                  (search "1 - General"
                          (cl-tty-kit:screen-row-string
                           (ntop.app.application::application-composition-screen
                            composition)
                           row)))))
          :to-be t)
        (setf
         (ntop.app.domain:app-state-active-dialog state) :help-search
         (gethash :help (ntop.app.domain:app-state-widget-v-states state))
         (ntop.app.presentation:make-help-state
          :selected-widget :process :query "" :search-active-p t)))
       (render-current)
       (let* ((screen
                (ntop.app.application::application-composition-screen composition))
              (bottom-row (cl-tty-kit:screen-row-string screen 35))
              (search-row (cl-tty-kit:screen-row-string screen 36)))
         (cl-weave:expect draw-count :to-be 1)
         (cl-weave:expect (char bottom-row 32) :to-equal #\└)
         (progn
          (cl-weave:expect (char bottom-row 127) :to-equal (char "┘" 0))
          (cl-weave:expect (char bottom-row 126) :to-equal (char "▼" 0)))
         (cl-weave:expect (char search-row 32) :to-equal #\Space)
         (cl-weave:expect (char search-row 127) :to-equal #\Space)
         (cl-weave:expect (search "Search:" search-row) :to-be 33))))))
(cl-weave:describe "help search overlay retains numbered sections"
  (cl-weave:it
   "renders a matching section after search opens from a numbered jump"
   (let* ((config (ntop.shared.application:make-application-config))
          (composition
            (ntop.app.application:make-application-composition config))
          (render
            (ntop.app.application:application-composition-render composition))
          (state (ntop.app.domain:make-app-state :selected-widget :process))
          (section-state
            (ntop.app.presentation:help-state-transition
             (ntop.app.presentation:make-help-state :selected-widget :process)
             (ntop.app.domain:make-app-action
              :kind :navigation :value '(:help-section "1"))
             :page-size 25))
          (search-open-state
            (ntop.app.presentation:help-state-transition
             section-state
             (ntop.app.domain:make-app-action :kind :search :value :help)
             :page-size 25))
          (search-state
            (ntop.app.presentation:help-state-transition
             search-open-state
             (ntop.app.domain:make-app-action :kind :search :value "CPU")
             :page-size 25)))
     (setf (ntop.app.domain:app-state-active-dialog state) :help-search
           (gethash :help (ntop.app.domain:app-state-widget-v-states state))
           search-state)
     (with-output-to-string (stream)
       (let ((*standard-output* stream))
         (funcall render state config nil 160 48 nil)))
     (let ((screen
             (ntop.app.application::application-composition-screen composition)))
       (cl-weave:expect
        (ntop.app.presentation:help-state-sectioned-p search-state) :to-be t)
       (cl-weave:expect
         (ntop.app.presentation:help-state-query search-state) :to-equal "CPU")
        (let ((matching-entry
                (loop for row from 10 below 36
                      for line = (cl-tty-kit:screen-row-string screen row)
                      when (and (search "CPU" line)
                                (search " - " line)
                                (search ": " line))
                        return line)))
          (cl-weave:expect (not (null matching-entry)) :to-be t))
       (cl-weave:expect
        (not
         (null
          (loop for row from 10 below 36
                thereis
                (search "2 - CPU widget"
                        (cl-tty-kit:screen-row-string screen row)))))
        :to-be t))))
  (cl-weave:it
   "draws neutral dashboard frames behind the help overlay"
   (let* ((config (ntop.shared.application:make-application-config))
          (composition
            (ntop.app.application:make-application-composition config))
          (render
            (ntop.app.application:application-composition-render composition))
          (state (ntop.app.domain:make-app-state :selected-widget :cpu)))
     (setf (ntop.app.application::application-composition-selection-active-p
            composition)
           t
           (ntop.app.domain:app-state-active-dialog state) :help)
     (with-output-to-string (stream)
       (let ((*standard-output* stream))
         (funcall render state config nil 160 48 nil)))
     (let* ((screen
              (ntop.app.application::application-composition-screen composition))
            (top-row (cl-tty-kit:screen-row-string screen 0)))
       (cl-weave:expect
        (char top-row 0) :to-equal #\┌)
       (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
        :to-equal
        (ntop.ui:presentation-theme-style :default :border))
       (cl-weave:expect (char top-row 1) :to-equal #\─)
       (cl-weave:expect (search "CPU" top-row) :to-be nil)
       (cl-weave:expect
        (char (cl-tty-kit:screen-row-string screen 1) 1)
        :to-equal #\Space)))))
