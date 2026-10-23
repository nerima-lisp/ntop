(in-package #:ntop.test)

(defun %captured-input-translation (raw-event)
  (let ((calls nil))
    (ntop.ui:translate-input raw-event
                             nil
                             (lambda (meaning)
                               (push (list :meaning meaning) calls))
                             (lambda (event)
                               (push (list :unhandled event) calls)))
    (nreverse calls)))

(defun %captured-input-meaning (raw-event)
  (let ((calls (%captured-input-translation raw-event)))
    (cl-weave:expect (length calls) :to-be 1)
    (cl-weave:expect (caar calls) :to-be :meaning)
    (second (first calls))))

(defun %expect-unhandled-input (raw-event)
  (let ((calls (%captured-input-translation raw-event)))
    (cl-weave:expect (length calls) :to-be 1)
    (cl-weave:expect (caar calls) :to-be :unhandled)
    (cl-weave:expect (cadar calls) :to-be raw-event)))

(defun %sgr-mouse-event (parameters)
  (let ((input (concatenate 'string (string (code-char 27)) "[<" parameters)))
    (multiple-value-bind (event consumed)
        (cl-tty-kit:decode-mouse-sequence input)
      (cl-weave:expect consumed :to-be (length input))
      event)))

(cl-weave:describe "presentation input translation"
                   (cl-weave:it-each
                    ((:f1 "F1") (:f2 "F2")
                     (:f3 "F3")
                     (:f4 "F4")
                     (:f5 "F5")
                     (:f6 "F6")
                     (:f7 "F7")
                     (:f8 "F8")
                     (:f9 "F9")
                     (:f10 "F10")
                     (:f11 "F11")
                     (:f12 "F12")
                     (:f13 "F13")
                     (:f14 "F14")
                     (:f15 "F15")
                     (:f16 "F16")
                     (:f17 "F17")
                     (:f18 "F18")
                     (:f19 "F19")
                     (:f20 "F20"))
                    "labels function key ~A"
                    (code expected)
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-key-event :type
                                                       :special
                                                       :code
                                                       code))))
                      (cl-weave:expect (ntop.ui::input-meaning-kind meaning)
                                       :to-be
                                       :key)
                      (cl-weave:expect (ntop.ui::input-meaning-key meaning)
                                       :to-equal
                                       expected)))
                   (cl-weave:it-each
                    ((:page-up "PageUp") (:page-down "PageDown") (:up "Up") (:left "Left") (:enter "Enter") (:escape "Escape") (:backtab "Shift+Tab") (:control-a "Ctrl+A") (:shift-a "Shift+A") (:alt-a "Alt+A"))
                    "labels special key ~A as ~A"
                    (code expected)
                    (cl-weave:expect
                     (ntop.ui::input-meaning-key
                      (%captured-input-meaning
                       (cl-tty-kit:make-key-event :type :special :code code)))
                     :to-equal
                     expected))
                   (cl-weave:it "uses parity labels for control, alt, and extended modifiers" (let ((control (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 99) :modifiers (list :control)))) (alt (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 99) :modifiers (list :alt)))) (extended (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 99) :modifiers (list :super))))) (cl-weave:expect (ntop.ui::input-meaning-key control) :to-equal "Ctrl+C") (cl-weave:expect (ntop.ui::input-meaning-key alt) :to-equal "Alt+c") (cl-weave:expect (ntop.ui::input-meaning-key extended) :to-equal "Super+c") (cl-weave:expect (ntop.ui::input-meaning-text control) :to-equal "c")))
                   (cl-weave:it
                    "normalizes modifier order for special key labels"
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-key-event :type
                                                       :special
                                                       :code
                                                       :left
                                                       :modifiers
                                                       '(:shift :alt
                                                                :control
                                                                :shift)))))
                      (cl-weave:expect (ntop.ui::input-meaning-key meaning)
                                       :to-equal
                                       "Ctrl+Alt+Shift+Left")
                      (cl-weave:expect
                       (ntop.ui::input-meaning-modifiers meaning)
                       :to-equal
                       '(:control :alt :shift))))
                   (cl-weave:it "uses shifted printable text and normalizes shifted fallback and space keys" (let ((shifted (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 103) :modifiers (list :shift) :shifted-key (code-char 71)))) (shifted-code (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 103) :modifiers (list :shift)))) (space (%captured-input-meaning (cl-tty-kit:make-key-event :type :character :code (code-char 32))))) (cl-weave:expect (ntop.ui::input-meaning-key shifted) :to-equal "G") (cl-weave:expect (ntop.ui::input-meaning-text shifted) :to-equal "G") (cl-weave:expect (ntop.ui::input-meaning-modifiers shifted) :to-equal (list :shift)) (cl-weave:expect (ntop.ui::input-meaning-key shifted-code) :to-equal "G") (cl-weave:expect (ntop.ui::input-meaning-text shifted-code) :to-equal "G") (cl-weave:expect (ntop.ui::input-meaning-key space) :to-equal "Space") (cl-weave:expect (ntop.ui::input-meaning-text space) :to-equal " ")))
                   (cl-weave:it
                    "preserves IME text independently of the key code"
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-key-event :type
                                                       :character
                                                       :code
                                                       #\a
                                                       :text
                                                       "が"))))
                      (cl-weave:expect (ntop.ui::input-meaning-key meaning)
                                       :to-equal
                                       "a")
                      (cl-weave:expect (ntop.ui::input-meaning-text meaning)
                                       :to-equal
                                       "が")))
                   (cl-weave:it "preserves the complete pasted payload"
                                (let ((meaning
                                       (%captured-input-meaning
                                        (cl-tty-kit:make-key-event :type
                                                                   :paste
                                                                   :code
                                                                   (format nil
                                                                           "first~%second 日本語")))))
                                  (cl-weave:expect
                                   (ntop.ui::input-meaning-kind meaning)
                                   :to-be
                                   :paste)
                                  (cl-weave:expect
                                   (ntop.ui::input-meaning-text meaning)
                                   :to-equal
                                   (format nil "first~%second 日本語"))))
                   (cl-weave:it
                    "dispatches repeated key presses through the semantic continuation"
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-key-event :type
                                                       :special
                                                       :code
                                                       :page-up
                                                       :kind
                                                       :repeat))))
                      (cl-weave:expect (ntop.ui::input-meaning-key meaning)
                                       :to-equal
                                       "PageUp")))
                   (cl-weave:it "sends key releases to the raw continuation"
                                (let ((event
                                       (cl-tty-kit:make-key-event :type
                                                                  :character
                                                                  :code
                                                                  #\x
                                                                  :kind
                                                                  :release)))
                                  (%expect-unhandled-input event)))
                   (cl-weave:it-each
                    ((:focus-in) (:focus-out)
                     (:paste-start)
                     (:paste-end)
                     (:unknown-csi))
                    "sends unsupported special event ~A to the raw continuation"
                    (code)
                    (%expect-unhandled-input
                     (cl-tty-kit:make-key-event :type :special :code code)))
                   (cl-weave:it-each
                    ((:press :left "left-button-down") (:press :right "right-button-down") (:press :middle "middle-button-down") (:press :none "other-button-down") (:press :wheel-up "wheel-up") (:press :wheel-down "wheel-down") (:press :wheel-left "wheel-left") (:press :wheel-right "wheel-right") (:scroll :wheel-up "wheel-up") (:scroll :wheel-down "wheel-down") (:scroll :wheel-left "wheel-left") (:scroll :wheel-right "wheel-right") (:move :none "move") (:drag :left "drag") (:release :left "release"))
                    "maps mouse action ~A and button ~A"
                    (action button expected)
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-mouse-event :action
                                                         action
                                                         :button
                                                         button))))
                      (cl-weave:expect (ntop.ui::input-meaning-kind meaning)
                                       :to-be
                                       :mouse)
                      (cl-weave:expect (ntop.ui::input-meaning-gesture meaning)
                                       :to-equal
                                       expected)))
                   (cl-weave:it
                    "retains mouse coordinates and canonical modifiers"
                    (let ((meaning
                           (%captured-input-meaning
                            (cl-tty-kit:make-mouse-event :action
                                                         :press
                                                         :button
                                                         :left
                                                         :x
                                                         6
                                                         :y
                                                         10
                                                         :modifiers
                                                         '(:shift :control :alt)))))
                      (cl-weave:expect (ntop.ui::input-meaning-x meaning)
                                       :to-be
                                       6)
                      (cl-weave:expect (ntop.ui::input-meaning-y meaning)
                                       :to-be
                                       10)
                      (cl-weave:expect
                       (ntop.ui::input-meaning-modifiers meaning)
                       :to-equal
                       '(:control :alt :shift))))
                   (cl-weave:it-each
                    (("0;7;11M" "left-button-down")
                     ("2;7;11M" "right-button-down")
                     ("3;7;11M" "other-button-down")
                     ("64;7;11M" "wheel-up")
                     ("65;7;11M" "wheel-down")
                     ("32;7;11M" "drag")
                     ("35;7;11M" "move")
                     ("0;7;11m" "release"))
                    "translates SGR mouse report ~A"
                    (parameters expected)
                    (let ((meaning
                           (%captured-input-meaning
                            (%sgr-mouse-event parameters))))
                      (cl-weave:expect (ntop.ui::input-meaning-gesture meaning)
                                       :to-equal
                                       expected)
                      (cl-weave:expect (ntop.ui::input-meaning-x meaning)
                                       :to-be
                                       6)
                      (cl-weave:expect (ntop.ui::input-meaning-y meaning)
                                       :to-be
                                       10)))
                   (cl-weave:it "retains SGR mouse modifiers"
                                (let ((meaning
                                       (%captured-input-meaning
                                        (%sgr-mouse-event "28;7;11M"))))
                                  (cl-weave:expect
                                   (ntop.ui::input-meaning-modifiers meaning)
                                   :to-equal
                                   '(:control :alt :shift))))
                   (cl-weave:it
                    "sends unsupported mouse reports and raw event types to the raw continuation"
                    (let ((mouse-event
                           (cl-tty-kit:make-mouse-event :action
                                                        :scroll
                                                        :button
                                                        :none))
                          (raw-event 'unsupported-input-event))
                      (%expect-unhandled-input mouse-event)
                      (%expect-unhandled-input raw-event))))
