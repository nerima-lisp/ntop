(in-package #:ntop.app.presentation)

(defstruct
    (help-state
     (:constructor make-help-state
                   (&key (selected-widget :process)
                         (query "")
                         (search-active-p nil)
                         (scroll-offset 0)
                         (sectioned-p nil))))
  selected-widget
  query
  search-active-p
  scroll-offset
  sectioned-p)

(defun %help-delete-previous-word (query)
  (let ((end (length query)))
    (loop while (and (plusp end) (find (char query (1- end)) '(#\Space #\Tab)))
          do (decf end))
    (let ((start end))
      (loop while (and (plusp start)
                       (not (find (char query (1- start)) '(#\Space #\Tab))))
            do (decf start))
      (subseq query 0 start))))

(defun %help-query-after-action (query value)
  (cond
    ((stringp value) (concatenate 'string query value))
    ((eq value :clear) "")
    ((eq value :delete-backward) (subseq query 0 (max 0 (1- (length query)))))
    ((eq value :delete-word-backward) (%help-delete-previous-word query))
    (t query)))

(defun %help-scroll-offset-after-action
    (state offset value page-size line-width)
  (let* ((section-number (%help-action-section-number value))
         (lines (%help-visible-lines state :width line-width))
         (last-offset (max 0 (- (length lines) page-size)))
         (offset (if (eq offset :end) last-offset (max 0 offset)))
         (half-page (max 1 (floor page-size 2))))
    (if section-number
        (or (let ((position
                    (position (%help-section-header section-number)
                              lines :test #'string= :from-end t)))
              (when position (min last-offset position)))
            offset)
        (case value
          (:first-row 0)
          (:last-row :end)
          ((:up :scroll-up :previous-row) (max 0 (1- offset)))
          ((:down :scroll-down :next-row) (min last-offset (1+ offset)))
          (:page-up (max 0 (- offset page-size)))
          (:page-down (min last-offset (+ offset page-size)))
          (:half-page-up (max 0 (- offset half-page)))
          (:half-page-down (min last-offset (+ offset half-page)))
          (otherwise offset)))))

(defun help-state-transition (state action
                                  &key (page-size 10) line-width)
  (check-type state help-state)
  (check-type action ntop.app.domain:app-action)
  (check-type page-size (integer 1 *))
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (query (help-state-query state))
         (search-active-p (help-state-search-active-p state))
         (scroll-offset (help-state-scroll-offset state))
         (selected-widget (help-state-selected-widget state))
         (sectioned-p (help-state-sectioned-p state)))
    (cond
      ((eq kind :search)
       (cond
         ((eq value :help)
          (setf search-active-p t
                scroll-offset 0))
         ((eq value :submit)
          (setf search-active-p nil))
         (t
          (let ((next-query (%help-query-after-action query value)))
            (unless (string= query next-query)
              (setf scroll-offset 0))
            (setf query next-query)))))
      ((eq kind :navigation)
       (when (%help-action-section-number value)
         (setf sectioned-p t))
       (setf scroll-offset
             (%help-scroll-offset-after-action
              state scroll-offset value page-size line-width))))
    (make-help-state :selected-widget selected-widget
                     :query query
                     :search-active-p search-active-p
                     :scroll-offset scroll-offset
                     :sectioned-p sectioned-p)))

;; Text copied from bottom 0.14.9 src/constants.rs HELP_TEXT.
(defun %help-section-header (number)
    (let ((section (assoc number (cdr *help-sections*))))
      (when section
        (format nil "~D - ~A" (first section) (second section)))))

  (defun %help-action-section-number (value)
    (when (and (consp value)
               (eq (first value) :help-section)
               (null (cddr value))
               (stringp (second value))
               (= (length (second value)) 1))
      (let ((character (char (second value) 0)))
        (when (char<= #\1 character #\9)
          (- (char-code character) (char-code #\0))))))

  (defun %help-line-matches-query-p (line query)
    (search query line :test #'char-equal))

  (defun %help-section-line-records (state)
    (let ((query (string-trim '(#\Space #\Tab #\Newline #\Return)
                              (help-state-query state))))
      (if (zerop (length query))
          (let ((records nil))
            (dolist (section *help-sections*)
              (if (eq (first section) :contents)
                  (dolist (line (rest section))
                    (push (list line :text) records))
                (progn
                  (push (list "" :text) records)
                  (push (list (%help-section-header (first section)) :header)
                        records)
                  (dolist (line (cddr section))
                    (push (list line :text) records)))))
            (nreverse records))
        (let ((records nil)
              (matched-section-p nil))
          (dolist (section (cdr *help-sections*))
            (let* ((header (%help-section-header (first section)))
                   (header-match-p (%help-line-matches-query-p header query))
                   (matching-lines
                    (remove-if-not
                     (lambda (line)
                       (%help-line-matches-query-p line query))
                     (cddr section))))
              (when (or header-match-p matching-lines)
                (when matched-section-p
                  (push (list "" :text) records))
                (push (list header :header) records)
                (dolist (line matching-lines)
                  (push (list line :text) records))
                (setf matched-section-p t))))
          (nreverse records)))))

  (defun %help-wrap-line (text width)
    (if (or (null width)
            (<= (ntop.ui:measure-table-text text) width)
            (zerop (length text)))
        (list text)
      (loop with start = 0
            while (< start (length text))
            for end = (let ((end start)
                            (used 0))
                        (loop while (and (< end (length text))
                                         (<= (+ used
                                                (cl-tty-kit:char-width
                                                 (char text end)))
                                             width))
                              do (incf used
                                       (cl-tty-kit:char-width (char text end)))
                              (incf end))
                        (if (= end start) (1+ start) end))
            for break-at = (and (< end (length text))
                                (position #\Space text
                                          :start start :end end :from-end t))
            for line-end = (if (and break-at (> break-at start))
                               break-at
                             end)
            collect (string-right-trim '(#\Space #\Tab)
                                       (subseq text start line-end))
            do (setf start
                     (if (and break-at (> break-at start))
                         (or (position-if-not
                              (lambda (character)
                                (find character '(#\Space #\Tab)))
                              text :start (1+ break-at))
                             (length text))
                       line-end)))))

  (defun %help-styled-lines (state width)
    (loop for (text style) in (%help-section-line-records state)
          append (mapcar (lambda (line) (list line style))
                         (%help-wrap-line text width))))

  (defun %help-visible-lines (state &key width)
    (mapcar #'first (%help-styled-lines state width)))


(defun %help-write (target x y text style)
  (when (plusp (length text))
    (cl-tty-kit:screen-write-string target x y text :style style)))

(defun %help-write-line (target x y width text style query selected-style)
  (when (plusp width)
    (let* ((visible (ntop.ui:truncate-table-text text width))
           (needle (string-trim '(#\Space #\Tab #\Newline #\Return) query))
           (match (and (plusp (length needle))
                       (search needle visible :test #'char-equal)))
           (prefix (if match (subseq visible 0 match) visible))
           (column (+ x (ntop.ui:measure-table-text prefix))))
      (%help-write target x y prefix style)
      (when match
        (let ((hit (subseq visible match (+ match (length needle)))))
          (%help-write target column y hit selected-style)
          (incf column (ntop.ui:measure-table-text hit))
          (%help-write target
                       column
                       y
                       (subseq visible (+ match (length needle)))
                       style)))
      (%help-write target
                   (+ x (ntop.ui:measure-table-text visible))
                   y
                   (make-string
                    (max 0 (- width (ntop.ui:measure-table-text visible)))
                    :initial-element #\Space)
                   style))))

(defun %help-draw-scrollbar
    (target rectangle max-scroll-index viewport-length position style)
  (let* ((x (+ (ntop.ui:rectangle-x rectangle)
               (ntop.ui:rectangle-width rectangle)
               -2))
         (y (1+ (ntop.ui:rectangle-y rectangle)))
         (height (max 0 (- (ntop.ui:rectangle-height rectangle) 2))))
    (when (and (> max-scroll-index viewport-length) (> height 2))
      (let* ((thumb-length
              (max 1 (floor (* height viewport-length)
                            (+ max-scroll-index viewport-length))))
             (thumb-range (max 0 (- height thumb-length 2)))
             (thumb-offset (floor (* position thumb-range) max-scroll-index)))
        (%help-write target x y "▲" style)
        (%help-write target x (+ y height -1) "▼" style)
        (loop for row from (+ y 1 thumb-offset)
              below (+ y 1 thumb-offset thumb-length)
              do (%help-write target x row "█" style))))))

(defun %help-draw-search-line
    (target x y width state title-style text-style selected-style)
  (when (plusp width)
    (let* ((query (help-state-query state))
           (prefix "Search: ")
           (prefix-width (min width (length prefix)))
           (visible (ntop.ui:truncate-table-text
                     query
                     (max 0 (- width prefix-width 1))))
           (query-width (ntop.ui:measure-table-text visible))
           (cursor-x (+ x prefix-width query-width)))
      (%help-write target x y (subseq prefix 0 prefix-width) title-style)
      (%help-write target (+ x prefix-width) y visible text-style)
      (%help-write target cursor-x y " " selected-style)
      (let ((column (1+ cursor-x)))
        (when (zerop (length query))
          (%help-write target column y " " text-style)
          (incf column)
          (let* ((hint (ntop.ui:truncate-table-text
                        "Type to search, Esc to close"
                        (max 0 (- width (- column x)))))
                 (hint-width (ntop.ui:measure-table-text hint)))
            (%help-write target column y hint (append text-style '(:dim)))
            (incf column hint-width)))
        (%help-write target
                     column
                     y
                     (make-string (max 0 (- width (- column x)))
                                  :initial-element #\Space)
                     text-style)))))

(ntop.ui:define-widget :help
                       (target rectangle state config series)
                       (declare (ignore series))
                       (let* ((state (or state (make-help-state)))
                              (theme
                               (string-downcase
                                (string
                                 (ntop.shared.application:application-config-theme
                                  config))))
                              (border-style
                               (%screen-style theme :border))
                              (title-style
                               (%screen-style theme :widget-title))
                              (text-style
                               (%screen-style theme :text))
                              (header-style
                               (%screen-style theme :table-header))
                              (selected-style
                               (%screen-style theme :selected-text))
                              (x (ntop.ui:rectangle-x rectangle))
                              (y (ntop.ui:rectangle-y rectangle))
                              (width (ntop.ui:rectangle-width rectangle))
                              (height (ntop.ui:rectangle-height rectangle))
                              (search-active-p
                               (help-state-search-active-p state))
                              (content-width (max 0 (- width 3)))
                              (content-height
                               (max 0 (- height 2 (if search-active-p 1 0))))
                              (lines (%help-styled-lines state content-width))
                              (max-scroll-index
                               (max 0 (- (length lines) content-height)))
                              (offset
                               (if (eq (help-state-scroll-offset state) :end)
                                   max-scroll-index
                                   (min max-scroll-index
                                        (max 0 (help-state-scroll-offset state)))))
                              (visible-lines
                               (subseq lines
                                       offset
                                       (min (length lines)
                                            (+ offset content-height))))
                              (left-title " Help ")
                              (right-title " Esc to close "))
                         (when (and (plusp width) (plusp height))
                           (cl-tty-kit:screen-draw-box target
                               x
                               y
                               width
                               height
                               :border :single
                               :style border-style)
                           (%help-write target
                                        (1+ x)
                                        y
                                        (ntop.ui:truncate-table-text
                                         left-title
                                         (max 0 (- width 2)))
                                        title-style)
                           (when (>= width
                                     (+ 3
                                        (length left-title)
                                        (length right-title)))
                             (%help-write target
                                          (- (+ x width) 1 (length right-title))
                                          y
                                          right-title
                                          title-style))
                           (loop for (text style) in visible-lines
                                 for row from (1+ y)
                                 do (%help-write-line
                                     target
                                     (1+ x)
                                     row
                                     content-width
                                     text
                                     (if (eq style :header)
                                         header-style
                                         text-style)
                                     (help-state-query state)
                                     selected-style))
                           (when (and search-active-p (> height 2))
                             (%help-draw-search-line
                              target
                              (1+ x)
                              (+ y height -2)
                              content-width
                              state
                              title-style
                              text-style
                              selected-style))
                           (%help-draw-scrollbar target
                                                 rectangle
                                                 max-scroll-index
                                                 content-height
                                                 offset
                                                 text-style))
                         nil))
