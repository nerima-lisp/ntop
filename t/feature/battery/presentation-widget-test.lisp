(in-package #:ntop.test)

(defstruct (%battery-screen-target (:constructor %make-battery-screen-target (cells roles))) cells roles)

(defmethod ntop.ui:draw-table-text ((target %battery-screen-target) x y text role)
  (loop for character across text
        for column from x
        when (and (<= 0 y) (< y (array-dimension (%battery-screen-target-cells target) 0))
                  (<= 0 column) (< column (array-dimension (%battery-screen-target-cells target) 1)))
          do (setf (aref (%battery-screen-target-cells target) y column) character
                   (aref (%battery-screen-target-roles target) y column) role)))

(defun %battery-screen-target (width height)
  (%make-battery-screen-target
   (make-array (list height width) :initial-element #\Space)
   (make-array (list height width) :initial-element :normal)))

(defun %battery-screen-lines (target)
  (loop for row below (array-dimension (%battery-screen-target-cells target) 0)
        collect (string-right-trim
                 '(#\Space)
                 (coerce (loop for column below
                               (array-dimension (%battery-screen-target-cells target) 1)
                               collect (aref (%battery-screen-target-cells target) row column))
                         'string))))

(defun %battery-test-device (&key (charge 50) (rate 1.25) (state :discharging)
                                  (to-full 600) (to-empty 900) (health 92.0))
  (ntop.battery.domain:make-battery-device
   :charge-percent charge :power-watts rate :state state
   :seconds-to-full to-full :seconds-to-empty to-empty :health-percent health))

(defun %battery-test-sample (&optional (devices (list (%battery-test-device))))
  (ntop.battery.domain:make-battery-sample :sampled-at 0 :devices devices))

(defun %battery-fixture-screen ()
  (let* ((target (%battery-screen-target 32 8))
         (rect (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 8))
         (device (%battery-test-device :charge 73 :rate 14.4 :state :charging
                                       :to-full 3723 :health 82.346)))
    (ntop.ui:draw-widget :battery target rect (ntop.ui:make-table-state) nil
                         (%battery-test-sample (list device)))
    (format nil "~{~A~%~}" (%battery-screen-lines target))))

(defun %battery-golden-lines ()
  (with-open-file (stream (asdf:system-relative-pathname
                           "ntop" "t/feature/battery/screen-golden.txt"))
    (loop for line = (read-line stream nil) while line collect line)))

(cl-weave:describe
 "Battery presentation"
 (cl-weave:it-each
  ((-5 10 "[      0%]") (0 10 "[      0%]") (12.5 10 "[|    13%]")
   (25 10 "[|    25%]") (50 10 "[||   50%]") (100 10 "[||||100%]")
   (125 10 "[||||100%]") (nil 10 "[     N/A]") (nil 5 "[N/A]"))
  "formats charge ~A"
  (charge width expected)
  (cl-weave:expect
   (ntop.battery.presentation::%battery-charge-bar charge width)
   :to-equal expected))
 (cl-weave:it-each
  ((nil :normal) (-5 :low-battery) (0 :low-battery) (9.9 :low-battery)
     (10 :medium-battery) (49.9 :medium-battery) (50 :high-battery)
     (100 :high-battery) (120 :high-battery))
  "uses the charge color threshold for ~A"
  (charge expected)
  (cl-weave:expect (ntop.battery.presentation::%battery-charge-color-role charge)
                   :to-be expected))
 (cl-weave:it-each
  ((14.4 "14.40W") (-0.5 "-0.50W") (nil "N/A"))
  "formats rate ~A"
  (watts expected)
  (cl-weave:expect (ntop.battery.presentation::%battery-rate-text watts)
                   :to-equal expected))
 (cl-weave:it-each
  ((82.346 "82.35%") (100 "100.00%") (nil "N/A"))
  "formats health ~A"
  (health expected)
  (cl-weave:expect (ntop.battery.presentation::%battery-health-text health)
                   :to-equal expected))
 (cl-weave:it-each
  ((:charging "Charging" "Time to full")
   (:discharging "Discharging" "Time to empty")
   (:full "Full" nil)
   (:empty "Empty" nil)
   (:unknown "Unknown" nil))
  "renders state ~A with its applicable time"
  (state expected-state expected-time)
  (let* ((target (%battery-screen-target 80 8))
         (screen-rectangle
           (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8))
         (device (%battery-test-device :state state
                                       :to-full 3723
                                       :to-empty 3723)))
    (ntop.ui:draw-widget :battery target screen-rectangle
                         (ntop.ui:make-table-state) nil
                         (%battery-test-sample (list device)))
    (let ((screen (format nil "~{~A~%~}" (%battery-screen-lines target))))
      (cl-weave:expect (not (null (search expected-state screen))) :to-be t)
      (if expected-time
          (cl-weave:expect
           (not (null
                 (and (search expected-time screen)
                      (search "1 hour, 2 minutes, 3 seconds" screen))))
           :to-be t)
          (cl-weave:expect
           (and (not (search "Time to full" screen))
                (not (search "Time to empty" screen)))
           :to-be t)))))
 (cl-weave:it-each
  ((3723 "1 hour, 2 minutes, 3 seconds")
     (3661 "1 hour, 1 minute, 1 second")
     (7322 "2 hours, 2 minutes, 2 seconds")
     (3600 "1 hour, 0 minutes, 0 seconds")
     (123 "2 minutes, 3 seconds") (60 "1 minute, 0 seconds")
     (1 "0 minutes, 1 second") (0 "0 minutes, 0 seconds"))
  "formats long time ~A seconds"
  (seconds expected)
  (cl-weave:expect (ntop.battery.presentation::%battery-long-time seconds)
                   :to-equal expected))
 (cl-weave:it-each
  ((3723 "1h 2m 3s") (3661 "1h 1m 1s") (123 "2m 3s")
     (60 "1m 0s") (1 "0m 1s") (3600 "1h 0m 0s"))
  "formats short time ~A seconds"
  (seconds expected)
  (cl-weave:expect (ntop.battery.presentation::%battery-short-time seconds)
                   :to-equal expected))
 (cl-weave:it-each
  ((:charging 3723 80 "Time to full" "1 hour, 2 minutes, 3 seconds")
     (:charging 3723 32 "To full" "1h 2m 3s")
     (:charging 0 80 "Time to full" "0 minutes, 0 seconds")
     (:discharging 123 80 "Time to empty" "2 minutes, 3 seconds")
     (:discharging 3723 32 "To empty" "1h 2m 3s")
     (:discharging 1 80 "Time to empty" "0 minutes, 1 second"))
  "chooses the time row for ~A"
  (state seconds width label value)
  (cl-weave:expect
   (ntop.battery.presentation::%battery-time-row
    (if (eq state :charging)
        (%battery-test-device :state state :to-full seconds)
        (%battery-test-device :state state :to-empty seconds))
    width)
   :to-equal (list :label label :value value)))
 (cl-weave:it-each
  ((:charging nil) (:charging -1)
   (:discharging nil) (:discharging -1)
   (:full 30) (:empty 30) (:unknown 30))
  "omits time for state ~A and unavailable seconds"
  (state seconds)
  (cl-weave:expect
   (ntop.battery.presentation::%battery-time-row
    (%battery-test-device :state state
                          :to-full seconds
                          :to-empty seconds)
    80)
   :to-be nil))
 (cl-weave:it "uses the table configuration defaults and overrides"
  (let* ((columns ntop.battery.presentation::*battery-value-columns*)
         (defaults
           (ntop.battery.presentation::%battery-table-config columns))
         (tab-config
           (ntop.battery.presentation::%battery-table-config
            columns :selectable-p t :focused-p nil
            :show-current-entry-when-unfocused-p t)))
    (cl-weave:expect (ntop.ui:table-config-columns defaults) :to-be columns)
    (cl-weave:expect (ntop.ui:table-config-selectable-p defaults) :to-be nil)
    (cl-weave:expect (ntop.ui:table-config-focused-p defaults) :to-be t)
    (cl-weave:expect
     (ntop.ui:table-config-show-current-entry-when-unfocused-p defaults)
     :to-be nil)
    (cl-weave:expect (ntop.ui:table-config-selectable-p tab-config) :to-be t)
    (cl-weave:expect (ntop.ui:table-config-focused-p tab-config) :to-be nil)
    (cl-weave:expect
     (ntop.ui:table-config-show-current-entry-when-unfocused-p tab-config)
     :to-be t)))
 (cl-weave:it-each
  ((0 :low-battery) (10 :medium-battery) (50 :high-battery))
  "draws role for charge ~A"
  (charge role)
  (let ((target (%battery-screen-target 24 3)))
    (ntop.ui:draw-widget :battery target
                         (ntop.ui:make-rectangle :x 0 :y 0 :width 24 :height 3)
                         (ntop.ui:make-table-state) nil
                         (%battery-test-sample (list (%battery-test-device :charge charge))))
    (cl-weave:expect (aref (%battery-screen-target-roles target) 1 0) :to-be role)))
 (cl-weave:it "preserves the selected device and returns clickable tab indices"
  (let* ((devices (loop for index below 5
                        collect (%battery-test-device
                                 :charge (+ 10 (* index 10))
                                 :state (if (= index 4) :full :discharging))))
         (state (ntop.ui:make-table-state :selected-index 4))
         (target (%battery-screen-target 80 8))
         (areas (ntop.ui:draw-widget
                 :battery target
                 (ntop.ui:make-rectangle :x 0 :y 0 :width 80 :height 8)
                 state nil (%battery-test-sample devices))))
    (cl-weave:expect (mapcar #'ntop.ui:clickable-area-action areas)
                     :to-equal '(:battery-tab :battery-tab :battery-tab
                                 :battery-tab :battery-tab))
    (cl-weave:expect (mapcar #'ntop.ui:clickable-area-value areas)
                     :to-equal '(0 1 2 3 4))
    (cl-weave:expect (ntop.ui:table-state-selected-index state) :to-be 4)
    (cl-weave:expect (aref (%battery-screen-target-roles target) 0 48)
                     :to-be :selected)
    (cl-weave:expect
     (not (null (search "Full" (nth 5 (%battery-screen-lines target)))))
     :to-be t))
  (dolist (case (list (list -1 3 0)
                      (list 1 3 1)
                      (list 8 3 2)
                      (list 1/2 3 0)
                      (list 1 0 0)))
    (let* ((requested (first case))
           (device-count (second case))
           (expected (third case))
           (state (ntop.ui:make-table-state :selected-index requested)))
      (cl-weave:expect
       (ntop.battery.presentation::%battery-selected-index state device-count)
       :to-be expected)
      (cl-weave:expect (ntop.ui:table-state-selected-index state)
                       :to-be expected)))
  (cl-weave:expect
   (ntop.battery.presentation::%battery-selected-index nil 3)
   :to-be 0))
 (cl-weave:it "keeps selected tabs visible and clips labels and separators"
  (let* ((state (ntop.ui:make-table-state :selected-index 4))
         (target (%battery-screen-target 1 1))
         (areas (ntop.ui:draw-widget
                 :battery target
                 (ntop.ui:make-rectangle :x 0 :y 0 :width 1 :height 1)
                 state nil (%battery-test-sample
                            (loop repeat 5 collect (%battery-test-device))))))
    (cl-weave:expect (length areas) :to-be 1)
    (cl-weave:expect (ntop.ui:clickable-area-value (first areas)) :to-be 4)
    (cl-weave:expect (ntop.ui:table-state-selected-index state) :to-be 4)
    (cl-weave:expect (aref (%battery-screen-target-cells target) 0 0)
                     :to-be #\B))
  (let* ((target (%battery-screen-target 10 1))
         (areas (ntop.ui:draw-widget
                 :battery target
                 (ntop.ui:make-rectangle :x 0 :y 0 :width 10 :height 1)
                 (ntop.ui:make-table-state) nil
                 (%battery-test-sample
                  (loop repeat 3 collect (%battery-test-device))))))
    (cl-weave:expect (length areas) :to-be 1)
    (cl-weave:expect (ntop.ui:clickable-area-value (first areas))
                     :to-be 0)
    (cl-weave:expect (aref (%battery-screen-target-cells target) 0 9)
                     :to-be #\|)))
 (cl-weave:it "handles absent, empty, and zero-sized multi-device data"
  (let* ((target (%battery-screen-target 32 2))
         (rect (ntop.ui:make-rectangle :x 0 :y 0 :width 32 :height 2)))
    (ntop.ui:draw-widget :battery target rect (ntop.ui:make-table-state) nil nil)
    (cl-weave:expect (search "No data found" (first (%battery-screen-lines target)))
                     :to-be 0)
    (setf target (%battery-screen-target 32 2))
    (ntop.ui:draw-widget :battery target rect (ntop.ui:make-table-state) nil
                         (%battery-test-sample nil))
    (cl-weave:expect (search "No data found" (first (%battery-screen-lines target)))
                     :to-be 0)
    (setf target (%battery-screen-target 0 0))
    (cl-weave:expect
     (ntop.ui:draw-widget
      :battery target
      (ntop.ui:make-rectangle :x 0 :y 0 :width 0 :height 0)
      (ntop.ui:make-table-state) nil
      (%battery-test-sample
       (list (%battery-test-device) (%battery-test-device))))
     :to-be nil)))
 (cl-weave:it "matches the deterministic fixture screen golden"
   (let* ((actual (%battery-fixture-screen))
          (expected-lines (%battery-golden-lines))
          (expected (format nil "~{~A~%~}" expected-lines)))
     (cl-weave:expect actual :to-equal expected)
     (cl-weave:expect (length expected-lines) :to-be 8)
     (cl-weave:expect (some (lambda (line) (plusp (length line))) expected-lines)
                      :to-be t))))

