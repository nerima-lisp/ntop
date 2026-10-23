(in-package #:ntop.test)

(defstruct
    (%f8b-kill-target (:constructor %f8b-make-kill-target (cells roles)))
  cells
  roles)

(defmethod ntop.ui:draw-table-text ((target %f8b-kill-target) x
                                    y
                                    text
                                    role)
  (loop for character across text
        for column from x
        when (and (<= 0 y)
                  (< y (array-dimension (%f8b-kill-target-cells target) 0))
                  (<= 0 column)
                  (< column
                     (array-dimension (%f8b-kill-target-cells target) 1)))
          do (setf (aref (%f8b-kill-target-cells target) y column) character
                   (aref (%f8b-kill-target-roles target) y column) role)))

(defun %f8b-kill-target (width height)
  (%f8b-make-kill-target
   (make-array (list height width) :initial-element #\Space)
   (make-array (list height width) :initial-element nil)))

(defun %f8b-kill-lines (target)
  (loop for row below (array-dimension (%f8b-kill-target-cells target) 0)
        collect (string-right-trim '(#\Space)
                                   (coerce
                                    (loop for column below (array-dimension
                                                            (%f8b-kill-target-cells
                                                             target)
                                                            1)
                                          collect (aref
                                                   (%f8b-kill-target-cells
                                                    target)
                                                   row
                                                   column))
                                    'string))))

(defun %f8b-kill-golden-lines (name)
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      (format nil
                                              "t/feature/process/goldens/~A"
                                              name)))
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun %f8b-draw-termination-dialog (target width
                                            height
                                            dialog
                                            config
                                            sample)
  (ntop.ui:draw-widget :process-termination-dialog
                       target
                       (ntop.ui:make-rectangle :x
                                               0
                                               :y
                                               0
                                               :width
                                               width
                                               :height
                                               height)
                       dialog
                       config
                       sample))

(cl-weave:describe
  "Process termination dialog rendering"
  (cl-weave:it
    "returns clickable signal and yes/no choice regions"
    (let* ((sample
            (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)
                                 (%f8b-process-signal "SIGINT" 2)))
           (signal-config
            (ntop.shared.application:make-application-config))
           (simple-config
            (ntop.shared.application:make-application-config
             :disable-advanced-kill-p
             t))
           (signal-dialog nil)
           (simple-dialog nil))
      (setf signal-dialog (%f8b-opened-termination-dialog '(123)
                                                          "fixture-worker"
                                                          sample
                                                          signal-config))
      (setf simple-dialog (%f8b-opened-termination-dialog '(123)
                                                          "fixture-worker"
                                                          sample
                                                          simple-config))
      (let* ((signal-target (%f8b-kill-target 72 12))
             (signal-areas
              (%f8b-draw-termination-dialog signal-target
                                            72
                                            12
                                            signal-dialog
                                            signal-config
                                            sample))
             (simple-target (%f8b-kill-target 72 8))
             (simple-areas
              (%f8b-draw-termination-dialog simple-target
                                            72
                                            8
                                            simple-dialog
                                            simple-config
                                            sample)))
        (cl-weave:expect
          (mapcar #'ntop.ui:clickable-area-action signal-areas)
          :to-equal
          '(:kill-choice :kill-choice :kill-choice))
        (cl-weave:expect
          (mapcar #'ntop.ui:clickable-area-value signal-areas)
          :to-equal
          '(0 1 2))
        (cl-weave:expect
          (mapcar #'ntop.ui:clickable-area-action simple-areas)
          :to-equal
          '(:kill-choice :kill-choice))
        (cl-weave:expect
          (mapcar #'ntop.ui:clickable-area-value simple-areas)
          :to-equal
          '(0 1)))))
  (cl-weave:it
    "matches deterministic signal and yes/no dialog goldens"
    (let* ((sample
            (%f8b-process-sample (%f8b-process-signal "SIGTERM" 15)
                                 (%f8b-process-signal "SIGINT" 2)
                                 (%f8b-process-signal "SIGKILL" 9)))
           (signal-config
            (ntop.shared.application:make-application-config))
           (simple-config
            (ntop.shared.application:make-application-config
             :disable-advanced-kill-p
             t))
           (signal-dialog nil)
           (simple-dialog nil)
           (signal-target (%f8b-kill-target 72 12))
           (simple-target (%f8b-kill-target 72 8)))
      (setf signal-dialog (%f8b-opened-termination-dialog '(123)
                                                          "fixture-worker"
                                                          sample
                                                          signal-config))
      (setf simple-dialog (%f8b-opened-termination-dialog '(123)
                                                          "fixture-worker"
                                                          sample
                                                          simple-config))
      (%f8b-draw-termination-dialog signal-target
                                    72
                                    12
                                    signal-dialog
                                    signal-config
                                    sample)
      (%f8b-draw-termination-dialog simple-target
                                    72
                                    8
                                    simple-dialog
                                    simple-config
                                    sample)
      (cl-weave:expect
        (%f8b-kill-lines signal-target)
        :to-equal
        (%f8b-kill-golden-lines "process-termination-signals-72x12.txt"))
      (cl-weave:expect
        (%f8b-kill-lines simple-target)
        :to-equal
        (%f8b-kill-golden-lines "process-termination-yes-no-72x8.txt"))))
  (cl-weave:it
    "formats grouped target text"
    (let* ((sample (%f8b-process-sample))
           (config
            (ntop.shared.application:make-application-config
             :disable-advanced-kill-p
             t))
           (dialog nil))
      (setf dialog (%f8b-opened-termination-dialog '(123 124)
                                                   nil
                                                   sample
                                                   config))
      (cl-weave:expect
        (ntop.process.presentation::%process-termination-message dialog)
        :to-equal
        "Kill 2 processes with the name 'process'? Press ENTER to confirm.")))
  (cl-weave:it-each
    ((1 4 ("" "" "" "")) (1 2 ("" "")) (0 4 ("" "" "" "")))
    "draws safely in a ~D-by-~D rectangle"
    (width height expected-lines)
    (let* ((sample (%f8b-process-sample))
           (config
            (ntop.shared.application:make-application-config
             :disable-advanced-kill-p
             t))
           (dialog nil)
           (target (%f8b-kill-target width height)))
      (setf dialog (%f8b-opened-termination-dialog '(123 124)
                                                   nil
                                                   sample
                                                   config))
      (cl-weave:expect
        (%f8b-draw-termination-dialog target
                                      width
                                      height
                                      dialog
                                      config
                                      sample)
        :to-be
        nil)
      (cl-weave:expect
        (%f8b-kill-lines target)
        :to-equal
        expected-lines))))
