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
         (cl-weave:expect draw-count :to-be 1)
         (cl-weave:expect
          (every (lambda (character) (char= character #\Space)) dashboard-row)
          :to-be t)
         (cl-weave:expect (search " Help " top-row) :to-be 33)
         (cl-weave:expect (char-code (char top-row 33)) :to-be 32)
         (cl-weave:expect (search "Esc to close" top-row) :to-be 114)
         (cl-weave:expect (search "Help: Process" top-row) :to-be nil)
         (cl-weave:expect (char bottom-row 32) :to-equal #\└)
         (cl-weave:expect (char bottom-row 127) :to-equal #\┘))
       (setf
        (ntop.app.domain:app-state-active-dialog state) :help-search
        (gethash :help (ntop.app.domain:app-state-widget-v-states state))
        (ntop.app.presentation:make-help-state
         :selected-widget :process :query "" :search-active-p t))
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
