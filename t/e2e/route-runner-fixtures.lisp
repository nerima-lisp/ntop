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
         (navigation-inputs
           (case (getf subcase :fixture-navigation-start)
             (:first (list (%route-key-input "Home")))
             (:last (list (%route-key-input "End")))
             ((nil) nil)
             (otherwise
              (error "Unknown fixture navigation start: ~s."
                     (getf subcase :fixture-navigation-start)))))
         (tree-focus (getf subcase :fixture-tree-focus))
         (tree-query (getf subcase :fixture-tree-query))
         (tree-inputs
           (when tree-focus
             (unless (and (stringp tree-query) (plusp (length tree-query)))
               (error "A process-tree fixture requires a nonempty query."))
             (list (%route-key-input "/")
                   tree-query
                   (%route-key-input "Escape"))))
         (setup-inputs
           (append
            (cond
              ((and (eq (route-kind route) :mouse)
                    (member (route-action route) (list "Scroll up" "Scroll down")
                            :test (function string=))
                    (member context (list "help" "graphs")
                            :test (function string-equal)))
               (%route-mouse-action-inputs
                route
                (list :context context
                      :gesture (if (string= (route-action route) "Scroll up")
                                   "wheel-down"
                                 "wheel-up"))
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
                  (list "1")))))
            navigation-inputs
            tree-inputs)))
    (labels ((send-inputs (inputs)
               (dolist (input inputs)
                 (let ((expected-query
                         (and tree-focus
                              (string= input tree-query)
                              tree-query)))
                   (pty-send-input session (%route-octets input)
                                   :timeout-seconds timeout)
                   (multiple-value-bind (output)
                       (%route-read-through-quiet-period
                        session *route-frame-quiet-seconds* timeout
                        (* *route-first-frame-factor* (/ rate 1000d0))
                        (when expected-query
                          (lambda (input-output)
                            (multiple-value-bind (query)
                                (%route-observer-prompt-state
                                 (%route-grid-after-output
                                  raw input-output rows columns))
                              (and query
                                   (string= expected-query
                                            (string-trim '(#\Space #\│)
                                                         query)))))))
                     (%append-octets raw output)))))
             (process-rows ()
               (%route-observer-process-rows (%route-grid raw rows columns)))
             (process-row-name-p (text name)
               (search name text :test (function char-equal)))
             (tree-row-index (name required-p)
               (let* ((rows (process-rows))
                      (index
                        (position-if
                         (lambda (text) (process-row-name-p text name))
                         rows)))
                 (when (and required-p (null index))
                   (error
                    "Process-tree fixture process ~s is not visible among ~d rows: ~s."
                    name (length rows)
                    (subseq rows 0 (min 5 (length rows)))))
                 index))
             (select-tree-row (index)
               (unless (and (integerp index) (not (minusp index)))
                 (error "Process-tree fixture row index is invalid: ~s." index))
               (let* ((visible-rows (process-rows))
                      (move-limit (max 1 (* 2 (length visible-rows)))))
                 (labels ((selected-p ()
                            (multiple-value-bind (selected-index)
                                (%route-observer-process-selection-index
                                 (%route-grid raw rows columns))
                              (or (eql selected-index index)
                                  (and (= (length visible-rows) 1)
                                       (zerop index))))))
                   (send-inputs (list (%route-key-input "Home")))
                   (loop repeat move-limit
                         until (selected-p)
                         do (send-inputs (list (%route-key-input "Down"))))
                   (unless (selected-p)
                     (error "Could not select process-tree fixture row ~s."
                            index)))))
             (tree-target-expanded-p ()
               (let ((mapping (getf subcase :fixture-tree-expansion-by-input)))
                 (if mapping
                     (let ((setting
                             (assoc (getf subcase :input) mapping :test #'string=)))
                       (unless setting
                         (error "No tree expansion state is declared for input ~s."
                                (getf subcase :input)))
                       (case (second setting)
                         (:expanded t)
                         (:collapsed nil)
                         (otherwise
                          (error "Unknown tree expansion state: ~s."
                                 (second setting)))))
                   (let ((target
                           (getf subcase :fixture-tree-expanded :missing)))
                     (unless (or (eq target t) (null target))
                       (error "A tree fixture requires an expansion state."))
                     (eq target t)))))
             (prepare-tree ()
               (let* ((pids (getf state :tree-pids))
                      (focus tree-focus)
                      (parent-name (getf subcase :fixture-tree-parent-name))
                      (child-name (getf subcase :fixture-tree-child-name))
                      (parent-pid (first pids))
                      (child-pid (second pids))
                      (target-expanded-p (tree-target-expanded-p)))
                 (unless (and (listp pids) (= (length pids) 2))
                   (error "The process-tree fixture did not return two PIDs."))
                 (unless (and (stringp parent-name) (plusp (length parent-name))
                              (stringp child-name) (plusp (length child-name)))
                   (error "The process-tree fixture must declare parent and child names."))
                 (unless (member focus (list :parent :child))
                   (error "Unknown process-tree fixture focus: ~s." focus))
                 (%route-fixture-stage-progress route subcase :process-tree-filtered)
                 (progn
                         (select-tree-row (tree-row-index parent-name t))
                         (let* ((selected-index
                                  (nth-value
                                   0
                                   (%route-observer-process-selection-index
                                    (%route-grid raw rows columns))))
                                (selected-row
                                  (and selected-index
                                       (nth selected-index (process-rows)))))
                           (unless (and selected-row
                                        (process-row-name-p selected-row parent-name))
                             (error "Could not focus process-tree parent ~s; selected row is ~s."
                                    parent-name selected-row))
                           (setf (getf state :tree-initial-selected-row)
                                 selected-row)))
                 (let* ((child-index (tree-row-index child-name nil))
                        (expanded-p (not (null child-index))))
                   (unless (eq expanded-p target-expanded-p)
                     (send-inputs (list (%route-key-input "Space")))
                     (setf child-index (tree-row-index child-name nil)
                           expanded-p (not (null child-index))))
                   (unless (eq expanded-p target-expanded-p)
                     (error "Could not set the process-tree expansion state; selected row is ~s."
                        (let* ((selected-index
                                 (nth-value
                                  0
                                  (%route-observer-process-selection-index
                                   (%route-grid raw rows columns)))))
                          (and selected-index
                               (nth selected-index (process-rows))))))
                   (let* ((focus-pid (ecase focus
                                       (:parent parent-pid)
                                       (:child child-pid)))
                          (focus-name (ecase focus
                                        (:parent parent-name)
                                        (:child child-name)))
                          (focus-index (tree-row-index focus-name t)))
                     (progn
                     (select-tree-row focus-index)
                     (multiple-value-bind (selected-index)
                         (%route-observer-process-selection-index
                          (%route-grid raw rows columns))
                       (let ((selected-row
                               (and selected-index
                                    (nth selected-index (process-rows)))))
                         (unless (and selected-row
                                      (process-row-name-p selected-row focus-name))
                           (error "Process-tree fixture selected row ~s at index ~s instead of ~s."
                                  selected-row selected-index focus-name))
                         (setf (getf state :tree-focus-row) selected-row))))
                     (setf (getf state :tree-expanded) expanded-p
                           (getf state :tree-focus) focus
                           (getf state :tree-focus-pid) focus-pid))
                   (%route-fixture-stage-progress route subcase :process-tree-ready)))))
      (%route-fixture-stage-progress route subcase :prepare-start)
      (setf (getf state :evidence-path) path)
      (when (getf subcase :query-fixture)
        (setf setup-inputs
              (append setup-inputs
                      (%route-fixture-query-setup-inputs subcase))
              (getf state :after-action-input)
              (%route-fixture-query-after-action-input subcase)))
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
      (when tree-focus
        (prepare-tree)
        (%route-fixture-append-evidence
         path
         (format nil "tree fixture: pids ~s; expanded ~s; focus pid ~d; selected ~s~%"
                 (getf state :tree-pids)
                 (getf state :tree-expanded)
                 (getf state :tree-focus-pid)
                 (getf state :tree-focus-row))))
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
      (%route-fixture-stage-progress route subcase :action-ready)
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
            (reference-frame (%route-observer-frame-with-title reference-grid "Battery"))
            (candidate-frame (%route-observer-frame-with-title candidate-grid "Battery"))
            (reference-tabs (%route-fixture-battery-tabs reference-grid reference-frame))
            (candidate-tabs (%route-fixture-battery-tabs candidate-grid candidate-frame)))
       (when (and reference-frame candidate-frame
                  (<= (length reference-tabs) 1)
                  (<= (length candidate-tabs) 1))
         (let ((evidence (%route-fixture-side-values
                          reference candidate :fixture-evidence :evidence)))
           (dolist (entry (list (list reference reference-frame reference-tabs)
                                (list candidate candidate-frame candidate-tabs)))
             (destructuring-bind (side frame tabs) entry
               (%route-fixture-append-evidence
                (getf (getf side :fixture-state) :evidence-path)
                (format nil "battery-frame: ~s; battery-tab-labels: ~s~%"
                        frame tabs))))
           (%route-fixture-na-result
            subcase mode
            "Neither side has more than one selectable battery tab."
            (format nil "reference frame ~s tabs ~s; candidate frame ~s tabs ~s"
                    reference-frame reference-tabs candidate-frame candidate-tabs)
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
  (let ((button (getf subcase :button)))
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
  (let ((route (getf state :route))
        (subcase (getf state :subcase)))
    (when (and route subcase)
      (%route-fixture-stage-progress route subcase :after-action-start))
    (let ((input (getf state :after-action-input)))
      (when input
        (pty-send-input session (%route-octets input) :timeout-seconds timeout)
        (multiple-value-bind (output)
            (%route-read-through-quiet-period
             session *route-frame-quiet-seconds* timeout
             (* *route-first-frame-factor* (/ rate 1000d0)))
          (%append-octets raw output)))
      (when (and route subcase)
        (%route-fixture-stage-progress route subcase :after-action-complete))
      (values raw (and input (%route-grid raw rows columns))))))
