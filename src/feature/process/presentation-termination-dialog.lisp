(in-package #:ntop.process.presentation)

(defstruct
    (process-termination-dialog-state
     (:constructor %make-process-termination-dialog-state (&key termination-state process-name mode selection-state)))
  termination-state
  process-name
  mode
  (selection-state (ntop.ui:make-table-state) :type ntop.ui:table-state))

(defun %process-termination-mode (config)
  (if (or (ntop.shared.application:application-config-disable-advanced-kill-p config) #-unix t #+unix nil)
      :simple
      :signals))

(defun %process-sample-signals (sample)
  (if sample
      (stable-sort
       (remove-if-not
        (lambda (signal)
          (let ((number (ntop.process.domain:process-signal-number signal))
                (name (ntop.process.domain:process-signal-name signal)))
            (and (integerp number) (plusp number) (stringp name) (plusp (length name)))))
        (copy-list (ntop.process.domain:process-sample-signals sample)))
       (lambda (left right)
         (< (ntop.process.domain:process-signal-number left) (ntop.process.domain:process-signal-number right))))
      nil))

(defun process-termination-dialog-choices (state sample)
  (if (eq (process-termination-dialog-state-mode state) :simple)
      '((:yes . "Yes") (:no . "No"))
      (cons (cons :cancel "0: Cancel")
            (mapcar
             (lambda (signal)
               (cons signal
                     (format nil
                             "~D: ~A"
                             (ntop.process.domain:process-signal-number signal)
                             (ntop.process.domain:process-signal-name signal))))
             (%process-sample-signals sample)))))

(defun open-process-termination-dialog (target-pids process-name sample config)
  (cond
    ((ntop.shared.application:application-config-read-only-p config) (values nil :read-only))
    ((null target-pids) (values nil :no-targets))
    (t
     (let* ((mode (%process-termination-mode config))
            (state
             (%make-process-termination-dialog-state :termination-state
                                                     (ntop.process.application:make-process-termination-state
                                                      :target-pids
                                                      (copy-list target-pids))
                                                     :process-name
                                                     (or process-name "process")
                                                     :mode
                                                     mode
                                                     :selection-state
                                                     (ntop.ui:make-table-state)))
            (choices (process-termination-dialog-choices state sample))
            (default-index
             (if (eq mode :simple)
                 1
                 (or
                  (loop for choice in choices
                        for index from 0
                        for signal = (car choice)
                        when (and (typep signal 'ntop.process.domain:process-signal)
                                  (= 15 (ntop.process.domain:process-signal-number signal)))
                          return index)
                  0))))
       (setf (ntop.ui:table-state-selected-index (process-termination-dialog-state-selection-state state)) default-index)
       (values state :opened)))))

(defun move-process-termination-dialog-selection (state sample amount viewport-height)
  (let* ((choices (process-termination-dialog-choices state sample))
         (selection (process-termination-dialog-state-selection-state state)))
    (ntop.ui:move-table-selection selection amount (length choices) viewport-height)
    state))

(defun select-process-termination-dialog-choice (state sample index)
  (let ((choices (process-termination-dialog-choices state sample)))
    (if (and (integerp index) (<= 0 index) (< index (length choices)))
        (progn
          (setf (ntop.ui:table-state-selected-index (process-termination-dialog-state-selection-state state)) index)
          (values state :selected))
        (values state :unknown-choice))))

(defun %process-termination-dialog-selected-choice (state sample)
  (nth (ntop.ui:table-state-selected-index (process-termination-dialog-state-selection-state state))
       (process-termination-dialog-choices state sample)))

(defun %send-process-termination-signal (termination-state sample signal config sender)
  (multiple-value-bind (selected-state selection-result)
      (ntop.process.application:select-process-signal termination-state
                                                      sample
                                                      (ntop.process.domain:process-signal-name signal))
    (if (eq selection-result :selected)
        (ntop.process.application:send-process-termination selected-state config sender)
        (values selected-state selection-result))))

(defun confirm-process-termination-dialog (state sample config sender)
  (let* ((choice (%process-termination-dialog-selected-choice state sample))
         (termination-state (process-termination-dialog-state-termination-state state)))
    (cond
      ((null choice) (values termination-state :no-choice))
      ((eq (car choice) :cancel) (values termination-state :cancel))
      ((eq (car choice) :no) (values termination-state :cancel))
      ((eq (car choice) :yes)
       (let ((default-signal
              (find 15 (%process-sample-signals sample) :key #'ntop.process.domain:process-signal-number)))
         (if default-signal
             (%send-process-termination-signal termination-state sample default-signal config sender)
             (values termination-state :unknown-signal))))
      ((typep (car choice) (quote ntop.process.domain:process-signal))
       (%send-process-termination-signal termination-state sample (car choice) config sender)))))

(defun %process-termination-message (state)
  (let* ((pids
          (ntop.process.application:process-termination-state-target-pids
           (process-termination-dialog-state-termination-state state)))
         (name (ntop.ui:truncate-table-text (process-termination-dialog-state-process-name state) 20)))
    (if (> (length pids) 1)
        (format nil "Kill ~D processes with the name '~A'? Press ENTER to confirm." (length pids) name)
        (format nil "Kill process '~A' with PID ~D? Press ENTER to confirm." name (first pids)))))

(defun %draw-dialog-line (target x y width text role &optional centered-p)
  (when (plusp width)
    (let* ((visible (ntop.ui:truncate-table-text text width))
           (visible-width (ntop.ui:measure-table-text visible))
           (draw-x
            (if centered-p
                (+ x (floor (- width visible-width) 2))
                x)))
      (ntop.ui:draw-table-text target draw-x y visible role))))

(defun %draw-process-dialog-frame (target x y width height title)
  (when (and (>= width 2) (>= height 2))
    (let ((horizontal (make-string (- width 2) :initial-element #\─)))
      (%draw-dialog-line target x y width (format nil "┌~A┐" horizontal) :normal)
      (loop for row from (1+ y) below (1- (+ y height))
            do (%draw-dialog-line target x row 1 "│" :normal) (%draw-dialog-line target
                                                                                 (+ x width -1)
                                                                                 row
                                                                                 1
                                                                                 "│"
                                                                                 :normal))
      (%draw-dialog-line target x (+ y height -1) width (format nil "└~A┘" horizontal) :normal)
      (%draw-dialog-line target (+ x 1) y (max 0 (- width 2)) title :title)
      (%draw-dialog-line target
                         (+ x (max 1 (- width 1 (ntop.ui:measure-table-text " Esc to close "))))
                         y
                         (min (ntop.ui:measure-table-text " Esc to close ") (max 0 (- width 2)))
                         " Esc to close "
                         :title))))

(defun %draw-process-termination-choice (target x y width choice selected-p)
  (let ((label (cdr choice)))
    (%draw-dialog-line target
                       x
                       y
                       width
                       label
                       (if selected-p
                           :selected
                           :normal))))

(defun %process-termination-dialog-width (rectangle state message choices)
  (let* ((width (ntop.ui:rectangle-width rectangle))
         (longest-choice
          (loop for choice in choices
                maximize (ntop.ui:measure-table-text (cdr choice)) into longest
                finally (return (or longest 0))))
         (content-width
          (max 12
               (+ 2 (ntop.ui:measure-table-text message))
               (+ 2 longest-choice)
               (+ 2
                  (ntop.ui:measure-table-text
                   (if (eq (process-termination-dialog-state-mode state) :simple)
                       " Confirm Kill Process "
                       " Select Signal "))))))
    (min width 100 content-width)))

(defun %draw-simple-process-termination-choices (target x y width state choices)
  (let* ((choice-widths
          (mapcar
           (lambda (choice)
             (ntop.ui:measure-table-text (cdr choice)))
           choices))
         (gap 3)
         (total (+ (reduce #'+ choice-widths :initial-value 0) (* gap (max 0 (1- (length choices))))))
         (start (+ x (max 0 (floor (- width total) 2))))
         (selected-index (ntop.ui:table-state-selected-index (process-termination-dialog-state-selection-state state)))
         (areas nil))
    (loop for choice in choices
          for index from 0
          for choice-width in choice-widths
          for visible-width = (min choice-width (max 0 (- (+ x width) start)))
          do (%draw-process-termination-choice target start y visible-width choice (= index selected-index)) (when
                                                                                                                 (plusp
                                                                                                                  visible-width)
                                                                                                               (push
                                                                                                                (ntop.ui:make-clickable-area
                                                                                                                 :rectangle
                                                                                                                 (ntop.ui:make-rectangle
                                                                                                                  :x
                                                                                                                  start
                                                                                                                  :y
                                                                                                                  y
                                                                                                                  :width
                                                                                                                  visible-width
                                                                                                                  :height
                                                                                                                  1)
                                                                                                                 :action
                                                                                                                 :kill-choice
                                                                                                                 :value
                                                                                                                 index)
                                                                                                                areas)) (incf
                                                                                                                         start
                                                                                                                         (+
                                                                                                                          choice-width
                                                                                                                          gap)))
    areas))

(defun %draw-process-termination-dialog (target rectangle state sample)
  (let* ((choices (process-termination-dialog-choices state sample))
         (message (%process-termination-message state))
         (width (%process-termination-dialog-width rectangle state message choices))
         (height (ntop.ui:rectangle-height rectangle))
         (visible-choice-count (min (length choices) (max 0 (- height 3))))
         (box-height (+ 3 visible-choice-count))
         (x (+ (ntop.ui:rectangle-x rectangle) (max 0 (floor (- (ntop.ui:rectangle-width rectangle) width) 2))))
         (y (+ (ntop.ui:rectangle-y rectangle) (max 0 (floor (- height box-height) 2))))
         (inner-x (1+ x))
         (inner-width (max 0 (- width 2)))
         (selection-state (process-termination-dialog-state-selection-state state))
         (selected-index (ntop.ui:table-state-selected-index selection-state))
         (title
          (if (eq (process-termination-dialog-state-mode state) :simple)
              " Confirm Kill Process "
              " Select Signal "))
         (areas nil))
    (when (and (plusp width) (>= height 3))
      (%draw-process-dialog-frame target x y width box-height title)
      (%draw-dialog-line target inner-x (1+ y) inner-width message :normal t)
      (if (eq (process-termination-dialog-state-mode state) :simple)
          (setf areas (%draw-simple-process-termination-choices target inner-x (+ y 2) inner-width state choices))
          (multiple-value-bind (offset end)
              (ntop.ui:table-visible-window selection-state (length choices) visible-choice-count)
            (loop for index from offset below
                  end
                  for choice in (nthcdr offset choices)
                  for row-y from (+ y 2)
                  do (%draw-process-termination-choice target inner-x row-y inner-width choice (= index selected-index)) (push
                                                                                                                          (ntop.ui:make-clickable-area
                                                                                                                           :rectangle
                                                                                                                           (ntop.ui:make-rectangle
                                                                                                                            :x
                                                                                                                            inner-x
                                                                                                                            :y
                                                                                                                            row-y
                                                                                                                            :width
                                                                                                                            inner-width
                                                                                                                            :height
                                                                                                                            1)
                                                                                                                           :action
                                                                                                                           :kill-choice
                                                                                                                           :value
                                                                                                                           index)
                                                                                                                          areas))))
      (nreverse areas))))

(ntop.ui:define-widget :process-termination-dialog
                       (target rectangle state config sample)
                       (declare (ignore config))
                       (%draw-process-termination-dialog target rectangle state sample))
