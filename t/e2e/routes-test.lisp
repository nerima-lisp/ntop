(in-package #:ntop.e2e.test)
(progn
  (defun %route-subcase-variants (route)
    (mapcar (function (lambda (subcase) (getf subcase :variant)))
            (ntop.e2e::route-subcases route)))
  (defun %route-has-subcases-p (route)
    (let ((subcases (ntop.e2e::route-subcases route)))
      (and (consp subcases)
           (every
            (function
             (lambda (subcase)
               (and (listp subcase) (consp subcase))))
            subcases))))
  (defun %route-flag-spellings-p (route)
  (let* ((entry (ntop.e2e::route-entry route))
         (short (ntop.app.domain:parity-flag-short entry))
         (kebab (ntop.app.domain:parity-flag-kebab-case entry))
         (snake (ntop.app.domain:parity-flag-snake-case entry))
         (subcases (ntop.e2e::route-subcases route))
         (variants (%route-subcase-variants route))
         (aliases (ntop.app.domain:parity-flag-aliases entry))
         (actual-aliases
           (loop for subcase in subcases
                 when (eq :alias (getf subcase :variant))
                   collect (getf subcase :alias)))
         (invalid
           (find-if (lambda (subcase)
                      (and (getf subcase :invalid-value)
                           (eq :kebab (getf subcase :variant))))
                    subcases))
         (invalid-count
           (count t subcases
                  :key (lambda (subcase) (getf subcase :invalid-value)))))
    (and (stringp short)
         (stringp kebab)
         (stringp snake)
         (string= kebab (substitute #\- #\_ snake))
         (string= (ntop.e2e::route-action route) snake)
         (member :kebab variants)
         (member :snake variants)
         (eql (not (null (member :short variants)))
              (plusp (length short)))
         (equal aliases actual-aliases)
         (= invalid-count 1)
         invalid)))
  (defun %route-control-flags-p (routes)
    (every
     (function
      (lambda (control)
        (let* ((route
                 (find (first control) routes
                       :key (function ntop.e2e::route-action)
                       :test (function string=)))
               (short
                 (and route
                      (ntop.app.domain:parity-flag-short
                       (ntop.e2e::route-entry route))))
               (variants
                 (and route (%route-subcase-variants route))))
          (and route
               (string= short (second control))
               (member :kebab variants)
               (member :snake variants)
               (member :short variants)))))
     (list (list "help" "-h") (list "version" "-V"))))
  (cl-weave:describe
   "typed route catalog"
   (cl-weave:it
 "counts typed routes and checks expanded subcase metadata"
 (let* ((parity (ntop.e2e::route-parity-catalog))
        (additional (ntop.e2e::route-additional-catalog))
        (all (ntop.e2e::route-catalog))
        (cases
          (loop for route in all
                append
                (loop for subcase in
                       (ntop.e2e::%route-subcases-expanded route)
                      for index from 1
                      collect
                      (cons
                       (format nil "~a-~3,'0d"
                               (ntop.e2e::route-id route) index)
                       subcase))))
        (metadata
          (remove-if-not
           (lambda (entry)
             (some (lambda (key) (getf (cdr entry) key))
                   '(:fixture :probe :applicability)))
           cases))
        (applicable
          (remove-if-not
           (lambda (entry) (getf (cdr entry) :applicability))
           metadata))
        (b-ids
          '("key-16-003" "key-17-003" "key-20-008" "key-21-007"
            "key-21-008" "key-34-001" "key-36-004" "key-48-001"
            "key-48-002" "mouse-69-004" "mouse-78-001" "flag-98-002"
            "flag-99-001" "sleep-child-001" "key-15-002" "flag-113-001"
            "search-field-155-001" "mouse-69-001" "mouse-70-001"))
        (mouse
          (remove-if-not
           (lambda (entry) (search "mouse-79-" (car entry)))
           cases)))
   (cl-weave:expect
    (list (length parity) (length additional) (length all))
    :to-equal (list 173 9 182))
   (cl-weave:expect
    (loop for kind in
          (list :key :mouse :flag :column :search-field :extension)
          collect (cons kind (ntop.e2e::route-kind-count kind parity)))
    :to-equal
    (list (cons :key 68) (cons :mouse 12) (cons :flag 52)
          (cons :column 20) (cons :search-field 17) (cons :extension 4)))
   (cl-weave:expect
    (list (length cases)
          (length metadata)
          (count nil metadata
                 :key (lambda (entry)
                        (getf (cdr entry) :applicability)))
          (length applicable)
          (loop for app in
                (list :battery-tabs :linux-only :mouse-other :powermetrics)
                collect
                (count app applicable
                       :key (lambda (entry)
                              (getf (cdr entry) :applicability)))))
    :to-equal (list 422 139 130 9 (list 5 2 1 1)))
   (cl-weave:expect
    (every
     (lambda (entry)
       (and
        (member (getf (cdr entry) :fixture)
                (list :dashboard :processes :process-tree :graph :search
                      :help :table :battery :capability))
        (member (getf (cdr entry) :probe)
                (list :transition :selection :tree :search-cursor
                      :search-results :query-edit :navigation :modal :sort
                      :table :mouse :column :graph :extension :resize :restore
                      :cpu-frequency))
        (or
         (null (getf (cdr entry) :applicability))
         (member (getf (cdr entry) :applicability)
                 (list :battery-tabs :linux-only :mouse-other :powermetrics)))))
     metadata)
    :to-be t)
   (cl-weave:expect
    (every
     (lambda (id)
       (let ((entry (assoc id cases :test #'string=)))
         (and entry
              (not
               (some (lambda (key) (getf (cdr entry) key))
                     (list :fixture :probe :applicability))))))
     b-ids)
    :to-be t)
   (cl-weave:expect
    (mapcar #'car mouse)
    :to-equal (list "mouse-79-001" "mouse-79-002" "mouse-79-003"))
   (cl-weave:expect
    (mapcar
     (lambda (entry)
       (list (getf (cdr entry) :button)
             (getf (cdr entry) :probe)
             (getf (cdr entry) :applicability)))
     mouse)
    :to-equal
    (list (list :right :transition nil)
          (list :middle :transition nil)
          (list :other :mouse :mouse-other)))
   (cl-weave:expect
    (mapcar
     (lambda (id)
       (let ((route
               (find id all :key #'ntop.e2e::route-id :test #'string=)))
         (list (ntop.e2e::route-classification route)
               (ntop.e2e::route-manual-reason route))))
     (list "mouse-79" "cpu-frequency"))
    :to-equal (list (list :automated nil) (list :automated nil)))))
   (cl-weave:it
 "selects routes and subcases from the 182-route catalog"
 (let* ((routes (ntop.e2e::route-catalog))
        (key-fifteen
          (find "key-15" routes
                :key (function ntop.e2e::route-id)
                :test (function string=)))
        (selected
          (ntop.e2e::%route-selected-routes
           routes (list "key-10" "key-15") nil))
        (expected
          (remove-if-not
           (lambda (route)
             (member (ntop.e2e::route-id route)
                     (list "key-10" "key-15")
                     :test (function string=)))
           routes))
        (catalog-subcases
          (ntop.e2e::%route-subcases-expanded key-fifteen))
        (subcases
          (ntop.e2e::%route-selected-subcases
           key-fifteen "key-15-002")))
   (cl-weave:expect (ntop.e2e::route-count routes) :to-equal 182)
   (cl-weave:expect (every (function %route-has-subcases-p) routes)
                    :to-be t)
   (cl-weave:expect (mapcar (function ntop.e2e::route-id) selected)
                    :to-equal
                    (mapcar (function ntop.e2e::route-id) expected))
   (cl-weave:expect (length subcases) :to-equal 1)
   (cl-weave:expect (caar subcases) :to-equal 2)
   (cl-weave:expect (cdar subcases) :to-equal (second catalog-subcases))
   (cl-weave:expect
    (mapcar (function ntop.e2e::route-id)
            (ntop.e2e::%route-selected-routes
             routes nil "key-15-002"))
    :to-equal
    (list "key-15"))
   (cl-weave:expect
    (handler-case
        (progn
          (ntop.e2e::%route-selected-routes
           routes (list "unknown") nil)
          nil)
      (error () t))
    :to-be
    t)
   (cl-weave:expect
    (handler-case
        (progn
          (ntop.e2e::%route-selected-routes
           routes (list "key-10") "key-15-002")
          nil)
      (error () t))
    :to-be
    t)))
   (cl-weave:it
 "writes a one-subcase N/A report with reason and evidence"
 (let* ((status nil)
        (output
          (with-output-to-string (*standard-output*)
            (setf status
                  (ntop.e2e::run-route-harness
                   :mode "btm-self"
                   :route-ids (list "extension-170")
                   :subcase-id "extension-170-001"
                   :evidence-directory
                   (namestring (ntop.e2e::%make-e2e-temporary-directory))))))
        (prefix "Route report: ")
        (prefix-start (search prefix output))
        (path-start (and prefix-start (+ prefix-start (length prefix))))
        (path-end (and path-start
                       (position (code-char 10) output :start path-start)))
        (report (and path-end
                     (uiop:read-file-string
                      (subseq output path-start path-end)))))
   (cl-weave:expect status :to-be 0)
   (cl-weave:expect (search "1/1 extension-170 N/A" output)
                    :to-be 0)
   (cl-weave:expect
    (search "| ID | Status | Input sequence | Expected | Actual | " report)
    :to-satisfy (function plusp))
   (cl-weave:expect (search "extension-170-001" report)
                    :to-satisfy (function plusp))
   (cl-weave:expect
    (not (null
          (search "The extension action has no btm-self equivalent." report)))
    :to-be t)
   (cl-weave:expect (search "no-btm.md" report) :to-satisfy (function plusp))
   (cl-weave:expect (search "MANUAL" output) :to-be nil)
   (cl-weave:expect (search "Procedure:" report) :to-be nil)
   (cl-weave:expect
    (search "Subcase counts: 1 total, 0 PASS, 0 FAIL, 1 N/A." report)
    :to-satisfy (function plusp))))
   (cl-weave:it
    "records every flag spelling and the help and version controls"
    (let* ((parity (ntop.e2e::route-parity-catalog))
           (flags
             (remove-if-not
              (function (lambda (route)
                          (eq :flag (ntop.e2e::route-kind route))))
              parity)))
      (cl-weave:expect (length flags) :to-equal 52)
      (cl-weave:expect (every (function %route-flag-spellings-p) flags)
                       :to-be t)
      (cl-weave:expect (%route-control-flags-p flags) :to-be t)))
   (cl-weave:it
    "normalizes color environment and matches single-widget route layouts"
    (labels ((route-for-context (routes context)
               (find-if
                (lambda (route)
                  (some
                   (lambda (subcase)
                     (string-equal (or (getf subcase :context) "") context))
                   (ntop.e2e::%route-subcases-expanded route)))
                routes))
             (subcase-for-context (route context)
               (find-if
                (lambda (subcase)
                  (string-equal (or (getf subcase :context) "") context))
                (ntop.e2e::%route-subcases-expanded route))))
      (let* ((environment
               (ntop.e2e::%route-environment
                '("TERM=screen" "HOME=/inherited" "XDG_CONFIG_HOME=/inherited"
                  "NO_COLOR=1" "CLICOLOR=1" "CLICOLOR_FORCE=1"
                  "FORCE_COLOR=1" "COLORTERM=truecolor"
                  "ANSI_COLORS_DISABLED=1" "PATH=/bin")
                "/tmp/route-home/" "/tmp/route-xdg/"))
             (routes (ntop.e2e::route-catalog))
             (battery-route (route-for-context routes "battery tabs"))
             (battery-subcase (subcase-for-context battery-route "battery tabs"))
             (cpu-route (route-for-context routes "CPU legend"))
             (cpu-subcase (subcase-for-context cpu-route "CPU legend"))
             (battery-ntop-args
               (ntop.e2e::%route-base-arguments
                "ntop" #p"/tmp/route.toml" battery-route battery-subcase "/tmp"))
             (battery-btm-args
               (ntop.e2e::%route-base-arguments
                "btm" #p"/tmp/route.toml" battery-route battery-subcase "/tmp"))
             (cpu-ntop-args
               (ntop.e2e::%route-base-arguments
                "ntop" #p"/tmp/route.toml" cpu-route cpu-subcase "/tmp"))
             (cpu-btm-args
               (ntop.e2e::%route-base-arguments
                "btm" #p"/tmp/route.toml" cpu-route cpu-subcase "/tmp")))
        (cl-weave:expect environment :to-equal
                         '("HOME=/tmp/route-home/" "XDG_CONFIG_HOME=/tmp/route-xdg/"
                           "TERM=xterm-256color" "PATH=/bin"))
        (cl-weave:expect
 (not (null (member "--battery" battery-ntop-args :test #'string=)))
 :to-be
 t)
        (cl-weave:expect
 (not (null
       (and (member "--expanded" battery-ntop-args :test #'string=)
            (search '("--default-widget-type" "battery"
                      "--default-widget-count" "1")
                    battery-ntop-args :test #'equal))))
 :to-be
 t)
        (cl-weave:expect (member "--battery" battery-btm-args :test #'string=)
                         :to-be nil)
        (cl-weave:expect (member "--expanded" battery-btm-args :test #'string=)
                         :to-be nil)
        (cl-weave:expect
 (not (null
       (and (member "--expanded" cpu-ntop-args :test #'string=)
            (search '("--default-widget-type" "cpu"
                      "--default-widget-count" "1")
                    cpu-ntop-args :test #'equal))))
 :to-be
 t)
        (cl-weave:expect (member "--expanded" cpu-btm-args :test #'string=)
                         :to-be nil)))))

  (cl-weave:describe
 "BTM self applicability N/A results"
 (cl-weave:it
  "recognizes an empty process table as an unavailable mouse target"
  (let ((subcase (list :context "tables"))
        (empty-grid (criterion-test-grid 12 50 ""))
        (visible-grid (criterion-test-grid 12 50 "sshd")))
    (cl-weave:expect
     (ntop.e2e::%route-mouse-target-unavailable-p subcase empty-grid)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-mouse-target-unavailable-p subcase visible-grid)
     :to-be nil)))
 (cl-weave:it
  "matches special single-widget layouts between BTM and ntop"
  (let ((routes (ntop.e2e::route-catalog)))
    (dolist (case
              (list (list "key-20" 5 "battery tabs" :battery "--battery")
                    (list "key-20" 6 "battery tabs" :battery "--battery")
                    (list "mouse-75" 1 "CPU legend" :cpu nil)
                    (list "mouse-77" 1 "battery widget" :battery "--battery")))
      (destructuring-bind (route-id subcase-index context widget extra-flag)
          case
        (let* ((route (find route-id routes :key #'ntop.e2e::route-id
                            :test #'string=))
               (subcase
                 (nth (1- subcase-index)
                      (ntop.e2e::%route-subcases-expanded route)))
               (layout (ntop.e2e::%route-config-layout route subcase))
               (btm-config
                 (let ((ntop.e2e::*btm-layout* layout))
                   (ntop.e2e:btm-config-string 1000)))
               (ntop-args
                 (ntop.e2e::%route-base-arguments
                  "ntop" #p"/tmp/wp15a-route.toml" route subcase "/tmp")))
          (cl-weave:expect (getf subcase :context) :to-equal context)
          (cl-weave:expect
 layout
 :to-equal
 (list
  (list
   (list (intern (symbol-name widget) "NTOP.E2E")
         nil
         :default))))
          (cl-weave:expect
           (not (null
                 (search
                  (format nil "type = ~s" (string-downcase (symbol-name widget)))
                  btm-config)))
           :to-be t)
          (cl-weave:expect
           (not (null
                 (search
                  (list "--default-widget-type"
                        (string-downcase (symbol-name widget))
                        "--default-widget-count" "1")
                  ntop-args :test #'equal)))
           :to-be t)
          (cl-weave:expect
           (not (null (member "--expanded" ntop-args :test #'string=)))
           :to-be t)
          (when extra-flag
            (cl-weave:expect
             (not (null (member extra-flag ntop-args :test #'string=)))
             :to-be t)))))))
 (cl-weave:it
  "matches starting selection layouts between BTM and ntop"
  (let ((routes (ntop.e2e::route-catalog)))
    (dolist (id (list "key-9" "key-10" "key-11"))
      (let* ((route
               (find id routes :key (function ntop.e2e::route-id)
                     :test (function string=)))
             (subcase
               (find "normal" (ntop.e2e::%route-subcases-expanded route)
                     :key (lambda (entry) (getf entry :context))
                     :test (function string-equal)))
             (layout (ntop.e2e::%route-config-layout route subcase))
             (default-widget
               (loop for row in layout
                     thereis
                     (loop for widget in row
                           when (member :default (cddr widget))
                             return (first widget))))
             (expected-widget
               (if (member id (list "key-9" "key-11")
                           :test (function string=))
                   :cpu
                 :proc))
             (ntop-args
               (ntop.e2e::%route-base-arguments
                "ntop" #p"/tmp/wp15a-route.toml" route subcase "/tmp")))
        (cl-weave:expect
 default-widget
 :to-equal
 (intern (symbol-name expected-widget) "NTOP.E2E"))
        (cl-weave:expect
         (not (null (search
                     (list "--default-widget-type"
                           (string-downcase (symbol-name expected-widget))
                           "--default-widget-count" "1")
                     ntop-args :test (function equal))))
         :to-be t)))))
 (cl-weave:it
  "records mode N/A evidence without a manual procedure"
  (let* ((route
          (find "extension-170" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
         (subcase (first (ntop.e2e::%route-subcases-expanded route)))
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (result
          (ntop.e2e::%route-btm-self-na-result route subcase 1 directory))
         (evidence (first (getf result :evidence)))
         (report (uiop:read-file-string evidence)))
    (unwind-protect
         (progn
           (cl-weave:expect (getf result :status) :to-be :na)
           (cl-weave:expect (getf result :applicability) :to-be :mode)
           (cl-weave:expect (getf result :mode) :to-equal "btm-self")
           (cl-weave:expect (stringp (getf result :reason)) :to-be t)
           (cl-weave:expect (stringp (getf result :actual)) :to-be t)
            (cl-weave:expect (not (null (probe-file evidence))) :to-be t)
           (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                            :to-be :na)
           (cl-weave:expect (search "Reason:" report) :to-satisfy (function plusp))
           (cl-weave:expect (search "Procedure:" report) :to-be nil))
      (uiop:delete-directory-tree directory :validate t))))
))
