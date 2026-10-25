(in-package #:ntop.app.presentation)

(defstruct
    (help-state
     (:constructor make-help-state
                   (&key (selected-widget :process)
                         (query "")
                         (search-active-p nil)
                         (scroll-offset 0))))
  selected-widget
  query
  search-active-p
  scroll-offset)

(defun %help-entry-fields (entry)
  (case (ntop.app.domain:parity-entry-kind entry)
    (:key
     (values (ntop.app.domain:parity-key-keys entry)
             (ntop.app.domain:parity-key-action entry)
             (ntop.app.domain:parity-key-contexts entry)
             (ntop.app.domain:parity-key-behavior entry)))
    (:extension
     (values (list (ntop.app.domain:parity-extension-key entry))
             (ntop.app.domain:parity-extension-action entry)
             (ntop.app.domain:parity-extension-contexts entry)
             (ntop.app.domain:parity-extension-behavior entry)))))

(defun %help-entry-matches-query-p (entry query)
  (multiple-value-bind (keys action contexts behavior)
      (%help-entry-fields entry)
    (some
     (lambda (text)
       (and (stringp text) (search query text :test #'char-equal)))
     (append keys contexts (list action behavior)))))

(defun %help-filter-entries (entries query)
  (let ((needle (string-trim '(#\Space #\Tab #\Newline #\Return) query)))
    (if (zerop (length needle))
        entries
        (remove-if-not
         (lambda (entry)
           (%help-entry-matches-query-p entry needle))
         entries))))

(defun %help-widget-contexts (widget)
  (case widget
    ((:process :process-table)
     (list "tables" "process table" "normal process table"))
    (:process-tree (list "tables" "process table" "process tree"))
    ((:disk :disks) (list "tables" "disk table" "Disks"))
    (:temperature (list "tables" "temperature table" "Temperatures"))
    (:battery (list "battery tabs" "battery widget"))
    (:cpu (list "graphs" "CPU legend"))
    (:memory (list "graphs" "Memory"))
    ((:network :gpu :temperature-graph :disk-io-graph) (list "graphs"))
    (otherwise nil)))

(defun %help-active-contexts (state)
  (append (list "all" "all widgets" "normal" "help")
          (when (help-state-search-active-p state)
            (list "help search"))
          (%help-widget-contexts (help-state-selected-widget state))))

(defun %help-entry-contextual-p (entry contexts)
  (multiple-value-bind (keys action entry-contexts behavior)
      (%help-entry-fields entry)
    (declare (ignore keys action behavior))
    (some
     (lambda (entry-context)
       (member entry-context contexts :test #'string-equal))
     entry-contexts)))

(defun help-state-visible-entries (state)
  (check-type state help-state)
  (%help-filter-entries
   (remove-if-not
    (lambda (entry)
      (and
       (member (ntop.app.domain:parity-entry-kind entry)
               '(:key :extension)
               :test
               #'eq)
       (%help-entry-contextual-p entry (%help-active-contexts state))))
    (ntop.app.domain:btm-parity-catalog))
   (help-state-query state)))

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

(defun %help-scroll-offset-after-action (state offset value page-size)
  (let* ((last-offset
          (max 0 (- (length (%help-visible-lines state)) page-size)))
         (offset
          (if (eq offset :end)
              last-offset
              (max 0 offset)))
         (half-page (max 1 (floor page-size 2))))
    (case value
      (:first-row 0)
      (:last-row :end)
      ((:up :scroll-up :previous-row) (max 0 (1- offset)))
      ((:down :scroll-down :next-row) (min last-offset (1+ offset)))
      (:page-up (max 0 (- offset page-size)))
      (:page-down (min last-offset (+ offset page-size)))
      (:half-page-up (max 0 (- offset half-page)))
      (:half-page-down (min last-offset (+ offset half-page)))
      (otherwise offset))))

(defun help-state-transition (state action &key (page-size 10))
  (check-type state help-state)
  (check-type action ntop.app.domain:app-action)
  (check-type page-size (integer 1 *))
  (let* ((kind (ntop.app.domain:app-action-kind action))
         (value (ntop.app.domain:app-action-value action))
         (query (help-state-query state))
         (search-active-p (help-state-search-active-p state))
         (scroll-offset (help-state-scroll-offset state))
         (selected-widget (help-state-selected-widget state)))
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
       (setf scroll-offset (%help-scroll-offset-after-action state
                                                             scroll-offset
                                                             value
                                                             page-size))))
    (make-help-state :selected-widget
                     selected-widget
                     :query
                     query
                     :search-active-p
                     search-active-p
                     :scroll-offset
                     scroll-offset)))

(defun %help-widget-label (widget)
  (string-capitalize
   (substitute #\Space #\- (string-downcase (symbol-name widget)))))

(defun %help-entry-line (entry)
  (multiple-value-bind (keys action contexts behavior)
      (%help-entry-fields entry)
    (declare (ignore contexts))
    (format nil "~{~A~^, ~}  ~A: ~A" keys action behavior)))

(defun %help-visible-lines (state)
  (let ((entries (help-state-visible-entries state)))
    (if entries
        (mapcar #'%help-entry-line entries)
        (list
         (if (plusp (length (help-state-query state)))
             "No matching key bindings."
             "No key bindings for this widget.")))))

(defun %help-write (target x y text style)
  (when (plusp (length text))
    (cl-tty-kit:screen-write-string target x y text :style style)))

(defun %help-write-line (target x y width text style query selected-style)
  (when (plusp width)
    (let* ((visible (ntop.ui:truncate-table-text text width))
           (needle (string-trim '(#\Space #\Tab #\Newline #\Return) query))
           (match
            (and (plusp (length needle))
                 (search needle visible :test #'char-equal)))
           (segments
            (if match
                (list (list (subseq visible 0 match) style)
                      (list (subseq visible match (+ match (length needle)))
                            selected-style)
                      (list (subseq visible (+ match (length needle))) style))
                (list (list visible style))))
           (column x))
      (dolist (segment segments)
        (let ((text (first segment)))
          (%help-write target column y text (second segment))
          (incf column (ntop.ui:measure-table-text text))))
      (%help-write target
                   column
                   y
                   (make-string
                    (max 0 (- width (ntop.ui:measure-table-text visible)))
                    :initial-element
                    #\Space)
                   style))))

(defun %help-border-line (width left right)
  (cond
    ((zerop width) "")
    ((= width 1) (string left))
    (t
     (format nil
             "~C~A~C"
             left
             (make-string (- width 2) :initial-element #\─)
             right))))

(defun %help-draw-border (target rectangle style)
  (let ((x (ntop.ui:rectangle-x rectangle))
        (y (ntop.ui:rectangle-y rectangle))
        (width (ntop.ui:rectangle-width rectangle))
        (height (ntop.ui:rectangle-height rectangle)))
    (when (and (plusp width) (plusp height))
      (%help-write target x y (%help-border-line width #\┌ #\┐) style)
      (when (> height 1)
        (%help-write target
                     x
                     (+ y height -1)
                     (%help-border-line width #\└ #\┘)
                     style))
      (loop for row from (1+ y) below (1- (+ y height))
            do (%help-write target x row "│" style) (when (> width 1)
                                                      (%help-write target
                                                                   (+ x
                                                                      width
                                                                      -1)
                                                                   row
                                                                   "│"
                                                                   style))))))

(defun %help-search-prompt (state)
  (concatenate 'string "Search: " (help-state-query state) "|"))

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
                              (selected-style
                               (%screen-style theme :selected-text))
                              (x (ntop.ui:rectangle-x rectangle))
                              (y (ntop.ui:rectangle-y rectangle))
                              (width (ntop.ui:rectangle-width rectangle))
                              (height (ntop.ui:rectangle-height rectangle))
                              (search-active-p
                               (help-state-search-active-p state))
                              (lines (%help-visible-lines state)))
                         (when (and (plusp width) (plusp height))
                           (%help-draw-border target rectangle border-style)
                           (let* ((title
                                   (format nil
                                           " Help: ~A "
                                           (%help-widget-label
                                            (help-state-selected-widget state))))
                                  (title
                                   (ntop.ui:truncate-table-text title
                                                                (max 0
                                                                     (- width 3)))))
                             (%help-write target (+ x 2) y title title-style))
                           (when (> height 2)
                             (let* ((content-x (1+ x))
                                    (content-width (max 0 (- width 2)))
                                    (search-row-p
                                     (and search-active-p (> height 2)))
                                    (content-height
                                     (max 0
                                          (- height
                                             2
                                             (if search-row-p
                                                 1
                                                 0))))
                                    (requested-offset
                                     (help-state-scroll-offset state))
                                    (maximum-offset
                                     (max 0 (- (length lines) content-height)))
                                    (offset
                                     (if (eq requested-offset :end)
                                         maximum-offset
                                         (min maximum-offset
                                              (max 0 requested-offset))))
                                    (visible-lines
                                     (subseq lines
                                             offset
                                             (min (length lines)
                                                  (+ offset content-height)))))
                               (loop for text in visible-lines
                                     for row from (1+ y)
                                     do (%help-write-line target
                                                          content-x
                                                          row
                                                          content-width
                                                          text
                                                          text-style
                                                          (help-state-query
                                                           state)
                                                          selected-style))
                               (when search-row-p
                                 (%help-write-line target
                                                   content-x
                                                   (+ y height -2)
                                                   content-width
                                                   (%help-search-prompt state)
                                                   text-style
                                                   (help-state-query state)
                                                   selected-style)))))
                         nil))
