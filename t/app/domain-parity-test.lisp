(in-package #:ntop.test)

(defvar *btm-parity-catalog-fixture*
  nil)

(progn
  (defun parity-entry-well-formed-p (entry)
    (labels ((string-list-p (value)
               (and (listp value) (every #'stringp value)))
             (sources-p (value)
               (and (consp value) (every #'stringp value))))
      (typecase entry
        (ntop.app.domain:parity-key
         (and (stringp (ntop.app.domain:parity-key-action entry))
              (string-list-p (ntop.app.domain:parity-key-keys entry))
              (string-list-p (ntop.app.domain:parity-key-contexts entry))
              (stringp (ntop.app.domain:parity-key-behavior entry))
              (sources-p (ntop.app.domain:parity-key-source entry))))
        (ntop.app.domain:parity-mouse
         (and (stringp (ntop.app.domain:parity-mouse-action entry))
              (stringp (ntop.app.domain:parity-mouse-gesture entry))
              (string-list-p (ntop.app.domain:parity-mouse-contexts entry))
              (stringp (ntop.app.domain:parity-mouse-behavior entry))
              (stringp (ntop.app.domain:parity-mouse-condition entry))
              (sources-p (ntop.app.domain:parity-mouse-source entry))))
        (ntop.app.domain:parity-flag
         (and (stringp (ntop.app.domain:parity-flag-snake-case entry))
              (stringp (ntop.app.domain:parity-flag-kebab-case entry))
              (stringp (ntop.app.domain:parity-flag-short entry))
              (string-list-p (ntop.app.domain:parity-flag-aliases entry))
              (stringp (ntop.app.domain:parity-flag-value-type entry))
              (string-list-p (ntop.app.domain:parity-flag-values entry))
              (stringp (ntop.app.domain:parity-flag-condition entry))
              (sources-p (ntop.app.domain:parity-flag-source entry))))
        (ntop.app.domain:parity-column
         (and (stringp (ntop.app.domain:parity-column-header entry))
              (stringp (ntop.app.domain:parity-column-field entry))
              (string-list-p (ntop.app.domain:parity-column-aliases entry))
              (stringp (ntop.app.domain:parity-column-condition entry))
              (stringp (ntop.app.domain:parity-column-default entry))
              (stringp (ntop.app.domain:parity-column-slot entry))
              (sources-p (ntop.app.domain:parity-column-source entry))))
        (ntop.app.domain:parity-search-field
         (and (stringp (ntop.app.domain:parity-search-field-field entry))
              (string-list-p (ntop.app.domain:parity-search-field-tokens entry))
              (stringp (ntop.app.domain:parity-search-field-condition entry))
              (stringp (ntop.app.domain:parity-search-field-implicit entry))
              (sources-p (ntop.app.domain:parity-search-field-source entry))))
        (ntop.app.domain:parity-extension
         (and (stringp (ntop.app.domain:parity-extension-action entry))
              (stringp (ntop.app.domain:parity-extension-key entry))
              (string-list-p (ntop.app.domain:parity-extension-contexts entry))
              (stringp (ntop.app.domain:parity-extension-behavior entry))
              (stringp (ntop.app.domain:parity-extension-relation entry))
              (sources-p (ntop.app.domain:parity-extension-source entry))))
        (t nil))))
  (defun parity-macro-expansion-error-p (form)
    (handler-case
        (progn (macroexpand-1 form) nil)
      (error () t))))

(defun parity-rows (kind)
  (remove-if-not
   (lambda (entry)
     (eql kind (ntop.app.domain:parity-entry-kind entry)))
   *btm-parity-catalog-fixture*))

(cl-weave:describe
 "btm parity catalog"
 (cl-weave:before-all
  (setf *btm-parity-catalog-fixture* (ntop.app.domain:btm-parity-catalog)))
 (cl-weave:after-all
  (setf *btm-parity-catalog-fixture* nil))
 (cl-weave:it-each
  ((:key 68) (:mouse 12) (:flag 52) (:column 20)
   (:search-field 17) (:extension 4))
  "contains the recorded count for ~A entries"
  (kind expected-count)
  (cl-weave:expect (length (parity-rows kind))
                   :to-equal expected-count))
 (cl-weave:it
 "contains all 173 typed records"
 (cl-weave:expect (length *btm-parity-catalog-fixture*)
                  :to-equal 173)
 (cl-weave:expect
  (every
   (lambda (entry)
     (keywordp (ntop.app.domain:parity-entry-kind entry)))
   *btm-parity-catalog-fixture*)
  :to-be t)
 (cl-weave:expect
  (every #'parity-entry-well-formed-p *btm-parity-catalog-fixture*)
  :to-be t))
 (cl-weave:it "preserves representative fields in each parity table"
  (let ((mouse (first (parity-rows :mouse)))
        (key (first (parity-rows :key)))
        (flag (first (parity-rows :flag)))
        (column (first (parity-rows :column)))
        (search-field (first (parity-rows :search-field)))
        (extension (first (parity-rows :extension))))
    (cl-weave:expect
     (list (ntop.app.domain:parity-mouse-action mouse)
           (ntop.app.domain:parity-mouse-gesture mouse)
           (ntop.app.domain:parity-mouse-contexts mouse)
           (ntop.app.domain:parity-mouse-condition mouse))
     :to-equal
     (list "Scroll up" "wheel-up" '("graphs" "tables" "help" "kill dialog")
           "--disable_click disables mouse capture"))
    (cl-weave:expect
     (and (member "src/event.rs:31-34"
                  (ntop.app.domain:parity-mouse-source mouse)
                  :test #'string=)
          (member "src/lib.rs:363-369"
                  (ntop.app.domain:parity-mouse-source mouse)
                  :test #'string=))
     :to-be-truthy)
    (cl-weave:expect
     (list (ntop.app.domain:parity-key-action key)
           (ntop.app.domain:parity-key-keys key)
           (ntop.app.domain:parity-key-contexts key))
     :to-equal
     (list "Quit" '("q" "Q") '("normal")))
    (cl-weave:expect
     (list (ntop.app.domain:parity-flag-snake-case flag)
           (ntop.app.domain:parity-flag-kebab-case flag)
           (ntop.app.domain:parity-flag-short flag)
           (ntop.app.domain:parity-flag-value-type flag)
           (ntop.app.domain:parity-flag-values flag))
     :to-equal
     (list "autohide_time" "autohide-time" "" "boolean" nil))
    (cl-weave:expect
     (list (ntop.app.domain:parity-column-header column)
           (ntop.app.domain:parity-column-field column)
           (ntop.app.domain:parity-column-aliases column)
           (ntop.app.domain:parity-column-default column))
     :to-equal
     (list "CPU%(c)" "CpuPercent" '("cpu%") "yes"))
    (cl-weave:expect
     (list (ntop.app.domain:parity-search-field-field search-field)
           (ntop.app.domain:parity-search-field-tokens search-field)
           (ntop.app.domain:parity-search-field-implicit search-field))
     :to-equal
     (list "PID" '("pid") "no"))
    (cl-weave:expect
     (list (ntop.app.domain:parity-extension-action extension)
           (ntop.app.domain:parity-extension-key extension)
           (ntop.app.domain:parity-extension-relation extension))
     :to-equal
     (list "Cycle selected widget view" "v"
           "Spec addition absent from btm keys."))))
 (cl-weave:it-property
  "derives kebab-case from each flag's snake-case name"
  ((index (cl-weave:gen-integer :min 0 :max 51)))
  (let* ((flag (nth index (parity-rows :flag)))
         (snake-case (ntop.app.domain:parity-flag-snake-case flag)))
    (cl-weave:expect (ntop.app.domain:parity-flag-kebab-case flag)
                     :to-equal (substitute #\- #\_ snake-case))))
 (cl-weave:it
  "expands the common macro into a named structure and table"
  (let ((form
          '(ntop.app.domain::define-parity-table
            *probe-parity-table* :key
            ("Probe" ("x") ("normal") "Probe action" ("test:1")))))
    (multiple-value-bind (expansion expanded-p) (macroexpand-1 form)
      (let* ((structure-form (find 'defstruct (cdr expansion) :key #'car))
             (structure-definition (second structure-form))
             (include-form
               (find :include (cdr structure-definition) :key #'car))
             (table-form (find 'defparameter (cdr expansion) :key #'car))
             (table-value (third table-form))
             (record-form (second table-value)))
        (cl-weave:expect expanded-p :to-be t)
        (cl-weave:expect (car expansion) :to-equal 'progn)
        (cl-weave:expect (first structure-definition)
                         :to-equal 'ntop.app.domain:parity-key)
        (cl-weave:expect (second include-form)
                         :to-equal 'ntop.app.domain::parity-entry)
        (cl-weave:expect (third include-form)
                         :to-equal '(ntop.app.domain::kind :key))
        (cl-weave:expect
         (mapcar #'symbol-name (mapcar #'car (cddr structure-form)))
         :to-equal '("ACTION" "KEYS" "CONTEXTS" "BEHAVIOR" "SOURCE"))
        (cl-weave:expect (second table-form) :to-equal '*probe-parity-table*)
        (cl-weave:expect (car table-value) :to-equal 'list)
        (cl-weave:expect (car record-form)
                         :to-equal 'ntop.app.domain::make-parity-key)
        (cl-weave:expect (second record-form) :to-equal :action)
        (cl-weave:expect (third record-form) :to-equal '(quote "Probe"))
        (cl-weave:expect (fourth record-form) :to-equal :keys)
        (cl-weave:expect (fifth record-form) :to-equal '(quote ("x")))
        (cl-weave:expect
         (notany (lambda (form)
                   (member (car form) '(defgeneric defmethod)))
                 (cdr expansion))
         :to-be t)))))
 (cl-weave:it
  "rejects invalid table names, kinds and rows"
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table "invalid" :key))
   :to-be t)
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table *probe-parity-table* :unknown))
   :to-be t)
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table
      *probe-parity-table* :key ("Probe")))
   :to-be t)
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table
      *probe-parity-table* :key "not-a-row"))
   :to-be t))
 (cl-weave:it
  "returns the defined records without copying them"
  (let* ((first-entry (first (ntop.app.domain:btm-parity-catalog)))
         (repeated-entry (first (ntop.app.domain:btm-parity-catalog)))
         (table-entry
           (first ntop.app.domain::*btm-parity-key-data*)))
    (cl-weave:expect (eq first-entry table-entry) :to-be t)
    (cl-weave:expect (eq first-entry repeated-entry) :to-be t)
    (cl-weave:expect
     (eq (ntop.app.domain:parity-key-keys first-entry)
         (ntop.app.domain:parity-key-keys repeated-entry))
     :to-be t))))
