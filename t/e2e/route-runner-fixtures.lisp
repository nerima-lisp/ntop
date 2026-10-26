(in-package #:ntop.e2e)

(defun %route-fixture-append-evidence (path text)
  (when path
    (with-open-file (stream path :direction :output :if-exists :append :if-does-not-exist :create)
      (write-string text stream)
      (terpri stream)))
  (and path (namestring path)))

(progn
  (defun %route-fixture-side-values (reference candidate &rest keys)
    (remove-duplicates
     (loop for side in (list reference candidate)
           append (loop for key in keys
                        for value =
                          (and side
                               (if (eq key :fixture-evidence)
                                   (or (getf side key)
                                       (let ((path
                                               (getf (getf side :fixture-state)
                                                     :evidence-path)))
                                         (and path (namestring path))))
                                   (getf side key)))
                        when value
                          append (if (member key
                                            '(:fixture-evidence
                                              :powermetrics-evidence :evidence))
                                     (if (listp value) value (list value))
                                     (list value))))
     :test #'equal))

  (defun %route-fixture-na-result (subcase mode reason actual evidence)
    (list :status :na
          :applicability (getf subcase :applicability)
          :mode mode
          :reason reason
          :actual actual
          :evidence evidence))

  (defun %route-fixture-write-capability-evidence (prefix text)
    (let ((path (and prefix (pathname (format nil "~a.fixture.txt" prefix)))))
      (when path
        (%route-fixture-append-evidence path text))
      (and path (list (namestring path)))))
)

(defun %route-fixture-prepare
    (state route subcase session raw rows columns rate timeout evidence-prefix)
  (let* ((path (and evidence-prefix
                    (pathname (format nil "~a.fixture.txt" evidence-prefix))))
         (context (or (getf subcase :context) ""))
         (grid (%route-grid raw rows columns))
         (setup-inputs
           (cond
             ((and (string= (route-id route) "mouse-69")
                   (string-equal context "help"))
              (%route-mouse-action-inputs
               route (list :context "help" :gesture "wheel-down")
               grid rows columns))
             ((and (string= (route-id route) "mouse-70")
                   (string-equal context "graphs"))
              (%route-mouse-action-inputs
               route (list :context "graphs" :gesture "wheel-up")
               grid rows columns))
             ((and (eq (route-kind route) :key)
                   (member (route-action route)
                           (list "Zoom graph out" "Reset graph zoom")
                           :test (function string=))
                   (string-equal context "graphs"))
              (list "+"))
             ((and (eq (route-kind route) :key)
                   (string= (route-action route) "Select help section"))
              (cond
                ((string= (getf subcase :input) "1") (list "2"))
                ((member (getf subcase :input) (list "7" "8" "9")
                         :test (function string=))
                 (list "1")))))))
    (labels ((send-inputs (inputs)
               (dolist (input inputs)
                 (pty-send-input session (%route-octets input)
                                 :timeout-seconds timeout)
                 (multiple-value-bind (output)
                     (%route-read-through-quiet-period
                      session *route-frame-quiet-seconds* timeout
                      (* *route-first-frame-factor* (/ rate 1000d0)))
                   (%append-octets raw output)))))
      (setf (getf state :evidence-path) path)
      (when path
        (%write-text
         path
         (format nil
                 (concatenate 'string
                              "route: ~a~%fixture: ~s~%probe: ~s~%"
                              "applicability: ~s~%system: ~a~%children: ~s~%")
                 (route-id route) (getf state :fixture) (getf state :probe)
                 (getf state :applicability) (software-type)
                 (mapcar (lambda (child)
                           (list (sb-ext:process-pid (getf child :process))
                                 (getf child :child-pids)))
                         (getf state :children)))))
      (when (eq (getf state :applicability) :linux-only)
        (%route-fixture-append-evidence
         path (format nil "linux: ~s~%" (member :linux *features*))))
      (when (eq (getf state :applicability) :mouse-other)
        (%route-fixture-prepare-mouse-capability
         state route subcase raw rows columns)
        (%route-fixture-append-evidence
         path (format nil "mouse: ~s; inputs: ~s; diagnostic: ~a~%"
                      (getf state :mouse-other-capability)
                      (getf state :mouse-other-inputs)
                      (getf state :mouse-other-diagnostic))))
      (when (eq (getf state :applicability) :powermetrics)
        (let ((sample (%route-fixture-powermetrics-sample)))
          (setf (getf state :powermetrics-before) sample)
          (%route-fixture-append-evidence
           path (%route-fixture-powermetrics-text "before route" sample))))
      (when setup-inputs
        (%route-fixture-append-evidence path
                                        (format nil "setup inputs: ~s~%" setup-inputs))
        (send-inputs setup-inputs))
      (when (eq (getf state :probe) :search-cursor)
        (pty-send-input
         session
         (%route-octets
          (concatenate 'string (%route-key-input "Ctrl+U") "abcd"
                       (%route-key-input "Left") (%route-key-input "Left")))
         :timeout-seconds timeout)
        (multiple-value-bind (output)
            (%route-read-through-quiet-period
             session *route-frame-quiet-seconds* timeout
             (* *route-first-frame-factor* (/ rate 1000d0)))
          (%append-octets raw output)))
      (values raw state (and path (namestring path))))))

(defun %route-fixture-store-after-sample (side state)
  (let* ((before (getf state :powermetrics-before))
         (after (%route-fixture-powermetrics-sample))
         (path (getf state :evidence-path)))
    (setf (getf side :powermetrics-before) before
          (getf side :powermetrics-after) after
          (getf side :powermetrics-before-values) (getf before :values)
          (getf side :powermetrics-after-values) (getf after :values)
          (getf side :powermetrics-evidence) (and path (namestring path)))
    (%route-fixture-append-evidence path (%route-fixture-powermetrics-text "after route" after))))

(defun %route-fixture-applicability-result
    (route subcase mode reference candidate &optional evidence-prefix)
  (declare (ignore route))
  (case (getf subcase :applicability)
    (:battery-tabs
     (let* ((reference-grid (or (getf reference :initial-grid) (getf reference :grid)))
            (candidate-grid (or (getf candidate :initial-grid) (getf candidate :grid)))
            (reference-tabs (%route-fixture-battery-tabs reference-grid))
            (candidate-tabs (%route-fixture-battery-tabs candidate-grid)))
       (when (and reference-grid candidate-grid
                  (<= (length reference-tabs) 1)
                  (<= (length candidate-tabs) 1))
         (let ((evidence (%route-fixture-side-values
                          reference candidate :fixture-evidence :evidence)))
           (dolist (side (list reference candidate))
             (%route-fixture-append-evidence
              (getf (getf side :fixture-state) :evidence-path)
              (format nil "battery-tab-labels: ~s~%"
                      (%route-fixture-battery-tabs
                       (or (getf side :initial-grid) (getf side :grid))))))
           (%route-fixture-na-result
            subcase mode
            "Neither side has more than one selectable battery tab."
            (format nil "reference tabs ~s; candidate tabs ~s"
                    reference-tabs candidate-tabs)
            evidence)))))
    (:linux-only
     (let* ((linux-p (or (member :linux *features*)
                         (search "Linux" (software-type) :test (function char-equal))))
            (actual (format nil "software-type ~a; :linux feature ~s"
                            (software-type) (not (null (member :linux *features*))))))
       (unless linux-p
         (let* ((states (%route-fixture-side-values reference candidate :fixture-state))
                (paths (remove nil
                               (mapcar
                                (lambda (state)
                                  (let ((path (getf state :evidence-path)))
                                    (when path
                                      (%route-fixture-append-evidence
                                       path
                                       (format nil "linux-only capability: unavailable~%~a~%"
                                               actual)))
                                    (and path (namestring path))))
                                states)))
                (evidence (or (%route-fixture-side-values
                               reference candidate :fixture-evidence)
                              paths
                              (%route-fixture-write-capability-evidence
                               evidence-prefix
                               (format nil "linux-only capability: unavailable~%~a~%"
                                       actual))
                              (%route-fixture-side-values
                               reference candidate :evidence))))
           (%route-fixture-na-result
            subcase mode
            "The Linux-only flag does not apply on this runtime."
            actual evidence)))))
    (:mouse-other
     (let* ((states (%route-fixture-side-values
                     reference candidate :fixture-state))
            (capabilities (mapcar (lambda (state)
                                    (getf state :mouse-other-capability))
                                  states)))
       (when (and (= 2 (length capabilities))
                  (every (lambda (value) (eq value :unavailable)) capabilities))
         (dolist (state states)
           (%route-fixture-append-evidence
            (getf state :evidence-path)
            (format nil "mouse-other: ~s; input: ~s~%"
                    (getf state :mouse-other-diagnostic)
                    (getf state :mouse-other-inputs))))
         (%route-fixture-na-result
          subcase mode
          "The route input encoder cannot express a distinct Other button event."
          (format nil "reference ~s; candidate ~s"
                  (getf (first states) :mouse-other-diagnostic)
                  (getf (second states) :mouse-other-diagnostic))
          (%route-fixture-side-values
           reference candidate :fixture-evidence :evidence)))))
    (:powermetrics
     (let* ((states (%route-fixture-side-values
                     reference candidate :fixture-state))
            (before (mapcar (lambda (state)
                              (getf state :powermetrics-before))
                            states))
            (capabilities (mapcar (lambda (sample)
                                    (getf sample :capability))
                                  before)))
       (when (and (= 2 (length capabilities))
                  (every (lambda (value) (eq value :unavailable)) capabilities))
         (return-from %route-fixture-applicability-result
  (%route-fixture-na-result
   subcase mode
   "powermetrics CPU power sampling is unavailable on both sides."
   (format nil "reference: ~a; candidate: ~a"
           (or (getf (first before) :reason)
               (getf (first before) :stderr)
               (getf (first before) :exit-code))
           (or (getf (second before) :reason)
               (getf (second before) :stderr)
               (getf (second before) :exit-code)))
   (%route-fixture-side-values
    reference candidate :fixture-evidence :evidence))))
       (when (= 2 (length states))
         (when (eq :available (getf (first before) :capability))
           (%route-fixture-store-after-sample reference (first states)))
         (when (eq :available (getf (second before) :capability))
           (%route-fixture-store-after-sample candidate (second states))))
       nil))
    (otherwise nil)))

(defun %route-mouse-subcase-inputs (route subcase grid rows columns)
  (let ((button (and (string= (route-id route) "mouse-79")
                     (getf subcase :button))))
    (if button
        (let* ((compatible-subcase (copy-list subcase))
               (inputs
                 (progn
                   (setf (getf compatible-subcase :gesture)
                         "right/middle/other button")
                   (%route-mouse-action-inputs
                    route compatible-subcase grid rows columns))))
          (unless (and (consp inputs) (consp (cdr inputs)))
            (error "Could not encode the mouse button input for ~a." button))
          (case button
            (:right (list (first inputs)))
            (:middle (list (second inputs)))
            (:other
             (error "The current mouse input backend cannot encode Other button-down."))
            (otherwise
             (error "Unknown mouse button subcase ~s." button))))
      (%route-mouse-action-inputs route subcase grid rows columns))))

(defun %route-fixture-after-action
    (state session raw rows columns rate timeout)
  (let ((input (getf state :after-action-input)))
    (when input
      (pty-send-input session (%route-octets input) :timeout-seconds timeout)
      (multiple-value-bind (output)
          (%route-read-through-quiet-period
           session *route-frame-quiet-seconds* timeout
           (* *route-first-frame-factor* (/ rate 1000d0)))
        (%append-octets raw output)))
    (values raw (and input (%route-grid raw rows columns)))))
