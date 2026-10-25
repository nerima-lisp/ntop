(in-package #:ntop.test)

(defun %f8b-process-search-screen-lines (screen)
  (loop for row below (cl-tty-kit:screen-height screen)
        collect (string-right-trim '(#\Space #\Return)
                                   (cl-tty-kit:screen-row-string screen
                                                                 row))))

(defun %f8b-process-search-golden-lines ()
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      "t/feature/process/goldens/presentation-search-40x5.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun %f8b-render-process-search-golden ()
  (let* ((screen (cl-tty-kit:make-screen 40 5))
         (rectangle (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 5))
         (config
          (ntop.shared.application:make-application-config :theme :default))
         (state
          (ntop.process.presentation::make-process-search-field-state
           :config
           config)))
    (ntop.process.presentation::transition-process-search-field state
                                                                :insert-text
                                                                "worker")
    (ntop.ui:draw-widget :process-search screen rectangle state config nil)
    (values screen (%f8b-process-search-screen-lines screen))))

(cl-weave:describe
  "Process search field rendering"
  (cl-weave:it
    "declares the search widget through define-widget"
    (cl-weave:expect
      (macroexpand-1
       '(ntop.ui:define-widget :process-search
                               (target rectangle state config series)
                               :rendered))
      :to-equal
      '(defmethod ntop.ui:draw-widget ((ntop.ui::widget-key
                                        (eql :process-search)) target
                                       rectangle
                                       state
                                       config
                                       series)
         (declare (ignore ntop.ui::widget-key))
         :rendered)))
  (cl-weave:it
    "retains a syntax error span and message for rendering"
    (let* ((config
            (ntop.shared.application:make-application-config
             :process-regex-p
             t))
           (state
            (ntop.process.presentation::make-process-search-field-state
             :config
             config))
           (screen (cl-tty-kit:make-screen 40 5))
           (rectangle
            (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 5))
           (theme
            (ntop.ui:presentation-theme-style :default :invalid-query)))
      (ntop.process.presentation::transition-process-search-field state
                                                                  :insert-text
                                                                  "[")
      (let* ((application-state
              (ntop.process.presentation::process-search-field-state-application-state
               state))
             (syntax-error
              (ntop.process.application:process-search-state-syntax-error
               application-state)))
        (cl-weave:expect
          (ntop.process.domain:process-search-syntax-error-start-offset
           syntax-error)
          :to-equal
          1)
        (cl-weave:expect
          (ntop.process.domain:process-search-syntax-error-end-offset
           syntax-error)
          :to-equal
          1)
        (cl-weave:expect
          (ntop.process.domain:process-search-syntax-error-message
           syntax-error)
          :to-equal
          "Invalid regular expression."))
      (ntop.ui:draw-widget :process-search
                           screen
                           rectangle
                           state
                           config
                           nil)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 4 1))
        :to-equal
        theme)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 1 2))
        :to-equal
        theme)))
  (cl-weave:it
    "matches the deterministic search field golden"
    (multiple-value-bind (screen lines) (%f8b-render-process-search-golden)
      (let ((golden (%f8b-process-search-golden-lines)))
        (cl-weave:expect
          (length golden)
          :to-be
          5)
        (cl-weave:expect
          (search "Esc to close" (cl-tty-kit:screen-row-string screen 0))
          :to-equal
          26)
        (cl-weave:expect
          lines
          :to-equal
          golden))))
  (cl-weave:it
    "renders nonzero syntax spans and focused cursor styles"
    (let* ((config
            (ntop.shared.application:make-application-config :theme
                                                             :default))
           (state
            (ntop.process.presentation::make-process-search-field-state
             :config
             config))
           (screen (cl-tty-kit:make-screen 40 5))
           (rectangle
            (ntop.ui:make-rectangle :x 0 :y 0 :width 40 :height 5))
           (invalid-style
            (ntop.ui:presentation-theme-style :default :invalid-query))
           (text-style (ntop.ui:presentation-theme-style :default :text))
           (selected-style
            (ntop.ui:presentation-theme-style :default :selected-text)))
      (ntop.process.presentation::transition-process-search-field state
                                                                  :insert-text
                                                                  "cpu = 1.2.3")
      (let ((syntax-error
             (ntop.process.application:process-search-state-syntax-error
              (ntop.process.presentation::process-search-field-state-application-state
               state))))
        (cl-weave:expect
          (list
           (ntop.process.domain:process-search-syntax-error-start-offset
            syntax-error)
           (ntop.process.domain:process-search-syntax-error-end-offset
            syntax-error))
          :to-equal
          (list 6 11)))
      (ntop.ui:draw-widget :process-search
                           screen
                           rectangle
                           state
                           config
                           nil)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 9 1))
        :to-equal
        invalid-style)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 13 1))
        :to-equal
        invalid-style)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 8 1))
        :to-equal
        text-style)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
        :to-equal
        invalid-style)
      (ntop.process.presentation::transition-process-search-field state
                                                                  :blur)
      (ntop.process.presentation::transition-process-search-field state
                                                                  :clear)
      (ntop.ui:draw-widget :process-search
                           screen
                           rectangle
                           state
                           config
                           nil)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 0 0))
        :to-equal
        (ntop.ui:presentation-theme-style :default :border))
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 8 1))
        :to-equal
        text-style)
      (ntop.process.presentation::transition-process-search-field state
                                                                  :clear)
      (ntop.process.presentation::transition-process-search-field state
                                                                  :insert-text
                                                                  "abcdef")
      (ntop.process.presentation::transition-process-search-field state
                                                                  :set-cursor
                                                                  3)
      (ntop.process.presentation::transition-process-search-field state
                                                                  :focus)
      (ntop.ui:draw-widget :process-search
                           screen
                           rectangle
                           state
                           config
                           nil)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell screen 6 1))
        :to-equal
        selected-style)))
  (cl-weave:it
    "renders narrow fields, truncated errors, and option styles"
    (let* ((basic-config
            (ntop.shared.application:make-application-config :basic-p
                                                             t
                                                             :process-case-sensitive-p
                                                             t
                                                             :process-whole-word-p
                                                             t
                                                             :process-regex-p
                                                             t))
           (wide-screen (cl-tty-kit:make-screen 8 5))
           (wide-rectangle
            (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 5)))
      (ntop.ui:draw-widget :process-search
                           wide-screen
                           wide-rectangle
                           nil
                           basic-config
                           nil)
      (cl-weave:expect
        (search "Search" (cl-tty-kit:screen-to-string wide-screen))
        :to-be
        nil)
      (cl-weave:expect
        (search "Case" (cl-tty-kit:screen-row-string wide-screen 3))
        :to-be
        1)
      (cl-weave:expect
        (cl-tty-kit:cell-style (cl-tty-kit:screen-cell wide-screen 1 3))
        :to-equal
        (ntop.ui:presentation-theme-style :default :selected-text))
      (let* ((three-screen (cl-tty-kit:make-screen 3 3))
             (three-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 3 :height 3))
             (two-screen (cl-tty-kit:make-screen 2 4))
             (two-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 2 :height 4))
             (short-screen (cl-tty-kit:make-screen 8 2))
             (short-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 2))
             (zero-width-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 2))
             (zero-height-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 8 :height 0)))
        (ntop.ui:draw-widget :process-search
                             three-screen
                             three-rectangle
                             nil
                             basic-config
                             nil)
        (cl-weave:expect
          (not
           (null (search ">" (cl-tty-kit:screen-to-string three-screen))))
          :to-be
          t)
        (ntop.ui:draw-widget :process-search
                             two-screen
                             two-rectangle
                             nil
                             basic-config
                             nil)
        (cl-weave:expect
          (search ">" (cl-tty-kit:screen-to-string two-screen))
          :to-be
          nil)
        (ntop.ui:draw-widget :process-search
                             short-screen
                             short-rectangle
                             nil
                             basic-config
                             nil)
        (ntop.ui:draw-widget :process-search
                             short-screen
                             zero-width-rectangle
                             nil
                             basic-config
                             nil)
        (ntop.ui:draw-widget :process-search
                             short-screen
                             zero-height-rectangle
                             nil
                             basic-config
                             nil)
        (cl-weave:expect
          (cl-tty-kit:screen-height short-screen)
          :to-be
          2))
      (let* ((regex-config
              (ntop.shared.application:make-application-config
               :process-regex-p
               t))
             (error-state
              (ntop.process.presentation::make-process-search-field-state
               :config
               regex-config))
             (error-screen (cl-tty-kit:make-screen 12 4))
             (error-rectangle
              (ntop.ui:make-rectangle :x 0 :y 0 :width 12 :height 4)))
        (ntop.process.presentation::transition-process-search-field
         error-state
         :insert-text
         "[")
        (ntop.ui:draw-widget :process-search
                             error-screen
                             error-rectangle
                             error-state
                             regex-config
                             nil)
        (cl-weave:expect
          (not
           (null
            (search "Invalid" (cl-tty-kit:screen-to-string error-screen))))
          :to-be
          t)
        (cl-weave:expect
          (cl-tty-kit:cell-style (cl-tty-kit:screen-cell error-screen 1 2))
          :to-equal
          (ntop.ui:presentation-theme-style :default :invalid-query)))))
  (cl-weave:it
    "keeps visible ranges within cell widths"
    (loop for (query cursor width expected) in (list
                                                (list "Alpha Worker"
                                                      12
                                                      36
                                                      (list 0 12 12))
                                                (list "abcdefghij"
                                                      10
                                                      5
                                                      (list 6 10 4))
                                                (list "abcdefghij"
                                                      3
                                                      5
                                                      (list 0 5 5))
                                                (list "A界B"
                                                      2
                                                      4
                                                      (list 0 3 4))
                                                (list "abcd"
                                                      2
                                                      0
                                                      (list 2 2 0))
                                                (list "abc"
                                                      -5
                                                      5
                                                      (list 0 3 3)))
          do (cl-weave:expect
               (multiple-value-list
                (ntop.process.presentation::%process-search-visible-range
                 query
                 cursor
                 width))
               :to-equal
               expected))))
