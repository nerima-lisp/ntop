(in-package #:ntop.process.presentation)

(defparameter *process-search-option-labels*
  '(("Case" process-search-field-state-case-sensitive-p)
    ("Whole" process-search-field-state-whole-word-p)
    ("Regex" process-search-field-state-regex-p)))

(defun %process-search-visible-range (query cursor width)
  (let* ((length (length query))
         (cursor (min length (max 0 cursor)))
         (offsets (make-array (1+ length) :initial-element 0)))
    (loop
      for
      index
      below
      length
      do
      (setf
        (aref offsets (1+ index))
        (+ (aref offsets index) (cl-tty-kit:char-width (char query index)))))
    (let* ((cursor-cell (aref offsets cursor))
           (start
            (loop
              for
              index
              from
              0
              to
              cursor
              when
              (<= (- cursor-cell (aref offsets index)) (max 0 (1- width)))
              return
              index))
           (visible-width
            (if (and (< cursor length) (plusp width)) width
              (max 0 (1- width))))
           (end start)
           (used 0))
      (loop
        while
        (< end length)
        for
        character
        =
        (char query end)
        for
        character-width
        =
        (cl-tty-kit:char-width character)
        while
        (<= (+ used character-width) visible-width)
        do
        (incf used character-width)
        (incf end))
      (values start end used))))

(defun %process-search-query-style (state theme start end index)
  (cond
    ((and (>= index start) (< index end))
     (ntop.ui:presentation-theme-style
       theme
       :invalid-query))
    ((and (process-search-field-state-focused-p state)
          (= index (process-search-field-state-cursor-index state)))
     (ntop.ui:presentation-theme-style
       theme
       :selected-text))
    (t
     (ntop.ui:presentation-theme-style
       theme
       :text))))

(defun %process-search-write-query (target x y width state theme)
  (let* ((query (%process-search-query state))
         (error
          (ntop.process.application:process-search-state-syntax-error
           (process-search-field-state-application-state state)))
         (cursor (process-search-field-state-cursor-index state))
         (error-start
          (if error
              (min (length query)
                   (ntop.process.domain:process-search-syntax-error-start-offset
                    error))
            -1))
         (error-end
          (if error
              (min (length query)
                   (ntop.process.domain:process-search-syntax-error-end-offset
                    error))
            -1))
         (prefix
          (cond
            ((>= width 4) "> ")
            ((>= width 3) ">")
            (t "")))
         (query-x (+ x (length prefix)))
         (available-width (max 0 (- width 2 (length prefix)))))
    (when
      (plusp (length prefix))
      (cl-tty-kit:screen-write-string target
                                      x
                                      y
                                      prefix
                                      :style
                                      (ntop.ui:presentation-theme-style
                                        theme
                                        (if (process-search-field-state-focused-p
                                             state) :table-header
                                          :text))))
    (multiple-value-bind
      (start end used)
      (%process-search-visible-range query cursor available-width)
      (loop
        for
        index
        from
        start
        below
        end
        for
        character
        =
        (char query index)
        for
        character-width
        =
        (cl-tty-kit:char-width character)
        for
        style
        =
        (%process-search-query-style state
                                     theme
                                     error-start
                                     error-end
                                     index)
        do
        (cl-tty-kit:screen-write-string target
                                        query-x
                                        y
                                        (string character)
                                        :style
                                        style)
        (incf query-x character-width))
      (when
        (and (process-search-field-state-focused-p state)
             (= cursor (length query))
             (< used available-width))
        (cl-tty-kit:screen-write-string target
                                        query-x
                                        y
                                        " "
                                        :style
                                        (ntop.ui:presentation-theme-style
                                          theme
                                          (if (and (>= error-start 0)
                                                   (= error-start
                                                      error-end)
                                                   (= error-start cursor))
                                              :invalid-query
                                            :selected-text)))))))

(defun %process-search-write-options (target x y width state theme)
  (loop
    with
    draw-x
    =
    x
    with
    remaining-width
    =
    width
    for
    (label accessor)
    in
    *process-search-option-labels*
    for
    index
    from
    0
    for
    style
    =
    (ntop.ui:presentation-theme-style
      theme
      (if (funcall (symbol-function accessor) state) :selected-text
        :text))
    for
    visible
    =
    (ntop.ui:truncate-table-text label remaining-width)
    for
    visible-width
    =
    (ntop.ui:measure-table-text visible)
    do
    (when
      (plusp visible-width)
      (cl-tty-kit:screen-write-string target draw-x y visible :style style)
      (incf draw-x visible-width)
      (decf remaining-width visible-width))
    when
    (and (< index (1- (length *process-search-option-labels*)))
         (>= remaining-width 2))
    do
    (cl-tty-kit:screen-write-string target
                                    draw-x
                                    y
                                    "  "
                                    :style
                                    (ntop.ui:presentation-theme-style
                                      theme
                                      :text))
    (incf draw-x 2)
    (decf remaining-width 2)))

(defun %process-search-write-error (target x y width state theme)
  (let* ((error
          (ntop.process.application:process-search-state-syntax-error
           (process-search-field-state-application-state state)))
         (message
          (and error
               (ntop.process.domain:process-search-syntax-error-message
                error)))
         (visible
          (and message (ntop.ui:truncate-table-text message width))))
    (when
      (and visible (plusp (ntop.ui:measure-table-text visible)))
      (cl-tty-kit:screen-write-string target
                                      x
                                      y
                                      visible
                                      :style
                                      (ntop.ui:presentation-theme-style
                                        theme
                                        :invalid-query)))))

(defun %draw-process-search-field (target rectangle state config)
  (let* ((theme (ntop.shared.application:application-config-theme config))
         (error
          (ntop.process.application:process-search-state-syntax-error
           (process-search-field-state-application-state state)))
         (focused-p (process-search-field-state-focused-p state))
         (basic-p
          (ntop.shared.application:application-config-basic-p config))
         (x (ntop.ui:rectangle-x rectangle))
         (y (ntop.ui:rectangle-y rectangle))
         (width (ntop.ui:rectangle-width rectangle))
         (height (ntop.ui:rectangle-height rectangle))
         (border-style
          (ntop.ui:presentation-theme-style
            theme
            (cond
              (error :invalid-query)
              (focused-p :highlighted-border)
              (t :border)))))
    (when
      (and (plusp width) (plusp height))
      (cl-tty-kit:screen-draw-box target
                                  x
                                  y
                                  width
                                  height
                                  :border
                                  :single
                                  :style
                                  border-style)
      (when
        (and (not basic-p) (>= width 12))
        (cl-tty-kit:screen-write-string target
                                        (+ x 2)
                                        y
                                        " Search "
                                        :style
                                        (ntop.ui:presentation-theme-style
                                          theme
                                          :table-header)))
      (when
        (>= height 3)
        (%process-search-write-query target
                                     (1+ x)
                                     (1+ y)
                                     width
                                     state
                                     theme))
      (when
        (>= height 4)
        (%process-search-write-error target
                                     (1+ x)
                                     (+ y 2)
                                     (max 0 (- width 2))
                                     state
                                     theme))
      (when
        (>= height 5)
        (%process-search-write-options target
                                       (1+ x)
                                       (+ y 3)
                                       (max 0 (- width 2))
                                       state
                                       theme)))))

(ntop.ui:define-widget :process-search
                       (target rectangle state config series)
                       (declare (ignore series))
                       (%draw-process-search-field target
                                                   rectangle
                                                   (or state
                                                       (make-process-search-field-state
                                                        :config
                                                        config))
                                                   config)
                       nil)
