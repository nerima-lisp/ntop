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
 (cl-weave:it
  "records the mouse condition and its evidence as fields"
  (let ((mouse (first (parity-rows :mouse))))
    (cl-weave:expect (ntop.app.domain:parity-mouse-condition mouse)
                     :to-equal "--disable_click disables mouse capture")
    (cl-weave:expect (not (null
      (member "src/lib.rs:363-369"
              (ntop.app.domain:parity-mouse-source mouse)
              :test #'string=)))
                     :to-be t)))
 (cl-weave:it-property
  "derives kebab-case from each flag's snake-case name"
  ((index (cl-weave:gen-integer :min 0 :max 51)))
  (let* ((flag (nth index (parity-rows :flag)))
         (snake-case (ntop.app.domain:parity-flag-snake-case flag)))
    (cl-weave:expect (ntop.app.domain:parity-flag-kebab-case flag)
                     :to-equal (substitute #\- #\_ snake-case))))
 (cl-weave:it
  "expands the common macro into a typed named table"
  (let ((form
          '(ntop.app.domain::define-parity-table
            *probe-parity-table* :key
            ("Probe" ("x") ("normal") "Probe action" ("test:1")))))
    (multiple-value-bind (expansion expanded-p) (macroexpand-1 form)
      (let* ((structure-form (find 'defstruct (cdr expansion) :key #'car))
             (table-form (find 'defparameter (cdr expansion) :key #'car))
             (table-value (third table-form)))
        (cl-weave:expect expanded-p :to-be t)
        (cl-weave:expect (car expansion) :to-equal 'progn)
        (cl-weave:expect (first (second structure-form))
                         :to-equal 'ntop.app.domain:parity-key)
        (cl-weave:expect
         (mapcar #'symbol-name (mapcar #'car (cddr structure-form)))
         :to-equal '("KIND" "ACTION" "KEYS" "CONTEXTS" "BEHAVIOR" "SOURCE"))
        (cl-weave:expect (second table-form) :to-equal '*probe-parity-table*)
        (cl-weave:expect (car table-value) :to-equal 'list)
        (cl-weave:expect (car (second table-value))
                         :to-equal 'ntop.app.domain::make-parity-key)))))
 (cl-weave:it
  "rejects unknown kinds and rows with the wrong width"
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table *probe-parity-table* :unknown))
   :to-be t)
  (cl-weave:expect
   (parity-macro-expansion-error-p
    '(ntop.app.domain::define-parity-table
      *probe-parity-table* :key ("Probe")))
   :to-be t))
 (cl-weave:it
  "returns independent records and nested values"
  (let* ((first-entry (first (ntop.app.domain:btm-parity-catalog)))
         (second-entry (first (ntop.app.domain:btm-parity-catalog)))
         (keys (ntop.app.domain:parity-key-keys first-entry)))
    (cl-weave:expect (not (eq first-entry second-entry)) :to-be t)
    (setf (char (first keys) 0) #\X
          (car keys) "changed")
    (cl-weave:expect (first (ntop.app.domain:parity-key-keys second-entry))
                     :to-equal "q")
    (cl-weave:expect
     (first
      (ntop.app.domain:parity-key-keys
       (first (ntop.app.domain:btm-parity-catalog))))
     :to-equal "q"))))
