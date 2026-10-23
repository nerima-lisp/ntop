(in-package #:ntop.ui)

(defstruct input-meaning
  kind
  key
  text
  gesture
  x
  y
  modifiers)

(defun %canonical-input-modifiers (modifiers)
  (let* ((modifiers
          (remove-duplicates (remove-if-not #'keywordp (copy-list modifiers))
                             :test
                             #'eq))
         (known '(:control :alt :shift)))
    (append
     (loop for modifier in known
           when (member modifier modifiers :test #'eq)
             collect modifier)
     (sort (set-difference modifiers known :test #'eq)
           #'string<
           :key
           #'symbol-name))))

(defun %camel-case-name (name)
  (with-output-to-string (output)
    (let ((capitalize-next-p t))
      (loop for char across name
            do (if (char= char #\-)
                   (setf capitalize-next-p t)
                   (progn
                     (write-char
                      (if capitalize-next-p
                          (char-upcase char)
                          (char-downcase char))
                      output)
                     (setf capitalize-next-p nil)))))))

(defun %modifier-label (modifier)
  (case modifier
    (:control "Ctrl")
    (:alt "Alt")
    (:shift "Shift")
    (otherwise (%camel-case-name (symbol-name modifier)))))

(defun %prefix-key-label (modifiers key-label)
  (if (null modifiers)
      key-label
      (with-output-to-string (output)
        (dolist (modifier modifiers)
          (write-string (%modifier-label modifier) output)
          (write-char #\+ output))
        (write-string key-label output))))

(defun %special-key-label (code)
  (when (keywordp code)
    (let ((name (symbol-name code)))
      (cond
        ((member code
                 '(:unknown-csi :focus-in :focus-out :paste-start :paste-end)
                 :test
                 #'eq) nil)
        ((eq code :escape) "Escape")
        ((eq code :backtab) "Shift+Tab")
        ((and (>= (length name) 8) (string= name "CONTROL-" :end1 8 :end2 8))
         (concatenate 'string "Ctrl+" (%camel-case-name (subseq name 8))))
        ((and (>= (length name) 6) (string= name "SHIFT-" :end1 6 :end2 6))
         (concatenate 'string "Shift+" (%camel-case-name (subseq name 6))))
        ((and (>= (length name) 4) (string= name "ALT-" :end1 4 :end2 4))
         (concatenate 'string "Alt+" (%camel-case-name (subseq name 4))))
        (t (%camel-case-name (symbol-name code)))))))

(defun %character-key-label (event modifiers)
  (let* ((code (cl-tty-kit:key-event-code event))
         (shifted-key (cl-tty-kit:key-event-shifted-key event))
         (shift-p (member :shift modifiers :test #'eq))
         (control-p (member :control modifiers :test #'eq))
         (char
          (cond
            ((and shift-p shifted-key) shifted-key)
            ((and shift-p (characterp code)) (char-upcase code))
            ((and control-p (characterp code)) (char-upcase code))
            ((characterp code) code))))
    (when char
      (if (char= char #\Space)
          "Space"
          (string char)))))

(defun %special-implicit-modifier (code)
  (when (keywordp code)
    (let ((name (symbol-name code)))
      (cond
        ((eq code :backtab) :shift)
        ((and (>= (length name) 8) (string= name "CONTROL-" :end1 8 :end2 8))
         :control)
        ((and (>= (length name) 6) (string= name "SHIFT-" :end1 6 :end2 6))
         :shift)
        ((and (>= (length name) 4) (string= name "ALT-" :end1 4 :end2 4)) :alt)))))

(defun %key-event-label (event)
  (let* ((type (cl-tty-kit:key-event-type event))
         (code (cl-tty-kit:key-event-code event))
         (modifiers
          (%canonical-input-modifiers (cl-tty-kit:key-event-modifiers event)))
         (key-label
          (case type
            (:character (%character-key-label event modifiers))
            (:special (%special-key-label code))))
         (implicit-modifier
          (and (eq type :special) (%special-implicit-modifier code))))
    (when key-label
      (setf modifiers (remove implicit-modifier modifiers :test #'eq))
      (when
          (and (eq type :character)
               (member :shift modifiers :test #'eq)
               (not (member :control modifiers :test #'eq))
               (not (member :alt modifiers :test #'eq)))
        (setf modifiers (remove :shift modifiers :test #'eq)))
      (%prefix-key-label modifiers key-label))))

(defun %key-event-text (event)
  (or (cl-tty-kit:key-event-text event)
      (when (eq :character (cl-tty-kit:key-event-type event))
        (let* ((code (cl-tty-kit:key-event-code event))
               (shift-p
                (member :shift
                        (cl-tty-kit:key-event-modifiers event)
                        :test
                        #'eq))
               (char
                (or (and shift-p (cl-tty-kit:key-event-shifted-key event))
                    (and shift-p (characterp code) (char-upcase code))
                    code)))
          (string char)))))

(defun %key-event-meaning (event)
  (let ((type (cl-tty-kit:key-event-type event))
        (kind (cl-tty-kit:key-event-kind event)))
    (cond
      ((eq type :paste)
       (make-input-meaning :kind
                           :paste
                           :text
                           (cl-tty-kit:key-event-code event)
                           :modifiers
                           (%canonical-input-modifiers
                            (cl-tty-kit:key-event-modifiers event))))
      ((and (member type '(:character :special) :test #'eq)
            (member kind '(:press :repeat) :test #'eq))
       (let ((key (%key-event-label event)))
         (when key
           (make-input-meaning :kind
                               :key
                               :key
                               key
                               :text
                               (%key-event-text event)
                               :modifiers
                               (%canonical-input-modifiers
                                (cl-tty-kit:key-event-modifiers event))))))
      (t nil))))

(defun %mouse-event-gesture (event)
  (let ((button (cl-tty-kit:mouse-event-button event))
        (action (cl-tty-kit:mouse-event-action event)))
    (case action
      (:scroll
       (case button
         (:wheel-up "wheel-up")
         (:wheel-down "wheel-down")
         (:wheel-left "wheel-left")
         (:wheel-right "wheel-right")))
      (:press
       (case button
         (:left "left-button-down")
         (:right "right-button-down")
         (:middle "middle-button-down")
         ((:wheel-up :wheel-down :wheel-left :wheel-right)
          (string-downcase (symbol-name button)))
         (:none "other-button-down")
         (otherwise nil)))
      (:move "move")
      (:drag "drag")
      (:release "release"))))

(defun %mouse-event-meaning (event)
  (let ((gesture (%mouse-event-gesture event)))
    (when gesture
      (make-input-meaning :kind
                          :mouse
                          :gesture
                          gesture
                          :x
                          (cl-tty-kit:mouse-event-x event)
                          :y
                          (cl-tty-kit:mouse-event-y event)
                          :modifiers
                          (%canonical-input-modifiers
                           (cl-tty-kit:mouse-event-modifiers event))))))

(defmethod translate-input ((raw-event cl-tty-kit:key-event) input-context
                            on-meaning
                            on-unhandled)
  (declare (ignore input-context))
  (let ((meaning (%key-event-meaning raw-event)))
    (if meaning
        (funcall on-meaning meaning)
        (funcall on-unhandled raw-event))))

(defmethod translate-input ((raw-event cl-tty-kit:mouse-event) input-context
                            on-meaning
                            on-unhandled)
  (declare (ignore input-context))
  (let ((meaning (%mouse-event-meaning raw-event)))
    (if meaning
        (funcall on-meaning meaning)
        (funcall on-unhandled raw-event))))

(defmethod translate-input ((raw-event t) input-context on-meaning on-unhandled)
  (declare (ignore input-context on-meaning))
  (funcall on-unhandled raw-event))
