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
    "counts each typed record and the nine additions once"
    (let* ((parity (ntop.e2e::route-parity-catalog))
           (additional (ntop.e2e::route-additional-catalog))
           (all (ntop.e2e::route-catalog))
           (kinds (list :key :mouse :flag :column :search-field :extension)))
      (cl-weave:expect (ntop.e2e::route-count parity) :to-equal 173)
      (cl-weave:expect
       (mapcar
        (function
         (lambda (kind)
           (cons kind (ntop.e2e::route-kind-count kind parity))))
        kinds)
       :to-equal
       (list (cons :key 68)
             (cons :mouse 12)
             (cons :flag 52)
             (cons :column 20)
             (cons :search-field 17)
             (cons :extension 4)))
      (cl-weave:expect (ntop.e2e::route-count additional) :to-equal 9)
      (cl-weave:expect (ntop.e2e::route-count all) :to-equal 182)))
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
 "writes a one-subcase report with the full route report format"
 (let* ((status nil)
        (output
          (with-output-to-string (*standard-output*)
            (setf status
                  (ntop.e2e::run-route-harness
                   :mode "btm-self"
                   :route-ids (list "key-15")
                   :subcase-id "key-15-002"
                   :evidence-directory
                   (namestring (uiop:temporary-directory))))))
        (prefix "Route report: ")
        (prefix-start (search prefix output))
        (path-start (and prefix-start
                         (+ prefix-start (length prefix))))
        (path-end (and path-start
                       (position #\Newline output :start path-start)))
        (report (and path-end
                     (uiop:read-file-string
                      (subseq output path-start path-end)))))
   (cl-weave:expect status :to-be 0)
   (cl-weave:expect (search "1/1 key-15 MANUAL" output)
                    :to-equal
                    0)
   (cl-weave:expect
    (search "| ID | Status | Input sequence | Expected | Actual | "
            report)
    :to-satisfy (function plusp))
   (cl-weave:expect (search "| key-15 | MANUAL |" report)
                    :to-satisfy (function plusp))
   (cl-weave:expect
    (search "Subcase counts: 1 total, 0 PASS, 0 FAIL, 1 MANUAL." report)
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
                '("TERM=screen"
                  "NO_COLOR=1"
                  "CLICOLOR=1"
                  "CLICOLOR_FORCE=1"
                  "FORCE_COLOR=1"
                  "COLORTERM=truecolor"
                  "ANSI_COLORS_DISABLED=1"
                  "PATH=/bin")))
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
                         '("TERM=xterm-256color" "PATH=/bin"))
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
 "BTM self subcase manual classifications"
 (cl-weave:it
  "classifies exactly six expanded BTM self subcases"
  (let* ((routes (ntop.e2e::route-catalog))
         (target-ids
           (list "key-15-002" "flag-113-001" "search-field-153-001"
                 "search-field-154-001" "search-field-155-001" "resize-001")))
    (labels ((manual-targets (mode)
               (loop for route in routes
                     append
                     (loop for subcase in
                             (ntop.e2e::%route-subcases-expanded route)
                           for index from 1
                           for spec =
                             (ntop.e2e::%route-btm-self-manual-spec
                              mode route subcase)
                           when spec
                             collect
                             (format nil "~a-~3,'0d"
                                     (ntop.e2e::route-id route) index))))
             (target-route-subcase-counts ()
               (loop for id in '("key-15" "flag-113" "search-field-153"
                                 "search-field-154" "search-field-155" "resize")
                     for route =
                       (find id routes :key #'ntop.e2e::route-id
                             :test #'string=)
                     collect
                     (cons id
                           (length
                            (ntop.e2e::%route-subcases-expanded route)))))
             (manual-specs-have-procedure-p ()
               (every
                (lambda (route)
                  (loop for subcase in
                          (ntop.e2e::%route-subcases-expanded route)
                        for spec =
                          (ntop.e2e::%route-btm-self-manual-spec
                           "btm-self" route subcase)
                        always
                        (or (null spec)
                            (and (stringp (getf spec :reason))
                                 (plusp (length (getf spec :reason)))
                                 (stringp (getf spec :procedure))
                                 (plusp (length (getf spec :procedure)))))))
                routes)))
      (cl-weave:expect (manual-targets "btm-self") :to-equal target-ids)
      (cl-weave:expect (manual-targets "ntop") :to-equal nil)
      (cl-weave:expect
       (target-route-subcase-counts)
       :to-equal
       (list (cons "key-15" 4)
             (cons "flag-113" 4)
             (cons "search-field-153" 1)
             (cons "search-field-154" 2)
             (cons "search-field-155" 1)
             (cons "resize" 1)))
      (cl-weave:expect (manual-specs-have-procedure-p) :to-be t))))
 (cl-weave:it
  "keeps a failed sibling visible when another subcase is manual"
  (let ((route (ntop.e2e::make-route-case :id "test")))
    (cl-weave:expect
     (ntop.e2e::%route-result-status
      route
      (list (list :passed t)
            (list :status :manual :passed nil)
            (list :passed t)))
     :to-be :manual)
    (cl-weave:expect
     (ntop.e2e::%route-result-status
      route
      (list (list :passed t)
            (list :status :manual :passed nil)
            (list :passed nil)))
     :to-be :fail)))
 (cl-weave:it
 "classifies invisible and asymmetric route state observations"
 (let* ((route
          (find "basic-mode" (ntop.e2e::route-catalog)
                :key (function ntop.e2e::route-id)
                :test (function string=)))
        (subcase (first (ntop.e2e::%route-subcases-expanded route)))
        (empty-side (list :projection nil :initial-projection nil
                          :grid nil :initial-grid nil))
        (visible-grid
          (criterion-test-grid
           12 100
           "RAM RX: TX: Total RX: Total TX: Disks Temperature"))
        (visible-side
          (list :projection nil :initial-projection nil
                :grid visible-grid :initial-grid visible-grid))
        (unobserved
          (multiple-value-list
           (ntop.e2e::%route-all-expected-actual
            route "ntop" subcase empty-side empty-side)))
        (asymmetric
          (multiple-value-list
           (ntop.e2e::%route-all-expected-actual
            route "ntop" subcase visible-side empty-side))))
   (cl-weave:expect (fourth unobserved) :to-be :manual)
   (cl-weave:expect
    (search "Both condition-A projections matched" (third unobserved))
    :to-satisfy (function plusp))
   (cl-weave:expect (fourth asymmetric) :to-be nil)
   (cl-weave:expect
    (search "reference, but not the candidate" (third asymmetric))
    :to-satisfy (function plusp))))
 (cl-weave:it
  "uses one status classification for subcase rows and totals"
  (cl-weave:expect
   (mapcar #'ntop.e2e::%route-subresult-status
           (list (list :passed t)
                 (list :status :manual :passed nil)
                 (list :passed nil)))
   :to-equal '(:pass :manual :fail)))
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
 "classifies unverified Help Escape as manual with a human check"
 (let* ((route
         (find "key-36" (ntop.e2e::route-catalog)
               :key #'ntop.e2e::route-id
               :test #'string=))
        (subcase
         (find "help" (ntop.e2e::%route-subcases-expanded route)
               :key (lambda (entry) (getf entry :context))
               :test #'string-equal))
        (reason
         (ntop.e2e::%route-manual-reason route subcase nil nil))
        (procedure
         (ntop.e2e::%route-manual-procedure route subcase "Esc" "help")))
   (cl-weave:expect
 (let ((grid (ntop.e2e:make-terminal-grid :rows 1 :cols 1)))
   (ntop.e2e::%route-modal-action-state-p route subcase grid grid))
 :to-equal
 :manual)
   (cl-weave:expect (not (null (search "Both paired BTM captures" reason))) :to-be t)
   (cl-weave:expect (not (null (search "press Escape once" procedure))) :to-be t)
   (cl-weave:expect (not (null (search "dashboard returns" procedure))) :to-be t)))
))
