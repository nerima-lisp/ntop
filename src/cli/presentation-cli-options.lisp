(in-package #:ntop.cli)

(defun %flag-records ()
  (remove-if-not
   (lambda (entry)
     (eq (ntop.app.domain:parity-entry-kind entry) :flag))
   (ntop.app.domain:btm-parity-catalog)))

(defun %bare-option-name (name)
  (string-left-trim "-" name))

(defun %option-aliases (entry)
  (let* ((snake (ntop.app.domain:parity-flag-snake-case entry))
         (kebab (ntop.app.domain:parity-flag-kebab-case entry)))
    (remove-duplicates
     (append
      (unless (string= snake kebab)
        (list kebab))
      (mapcar #'%bare-option-name (ntop.app.domain:parity-flag-aliases entry)))
     :test
     #'string-equal)))

(defun %option-short-name (entry)
  (let ((short (ntop.app.domain:parity-flag-short entry)))
    (when (and short (plusp (length short)))
      (%bare-option-name short))))

(defun %option-choices (entry)
  (when (string= (ntop.app.domain:parity-flag-value-type entry) "enum")
    (ntop.app.domain:parity-flag-values entry)))

(defun %make-cli-option (entry)
  (let* ((type (ntop.app.domain:parity-flag-value-type entry))
         (boolean-p (string= type "boolean"))
         (option-type
          (cond
            ((string= type "integer") :integer)
            ((string= type "boolean") nil)
            (t :string)))
         (choices (%option-choices entry))
         (arguments
          (append
           (list :name
                 (ntop.app.domain:parity-flag-snake-case entry)
                 :short
                 (%option-short-name entry)
                 :aliases
                 (%option-aliases entry)
                 :kind
                 (if boolean-p
                     :flag
                     :value))
           (when option-type
             (list :type option-type))
           (when choices
             (list :choices choices)))))
    (apply #'cl-cli:make-option arguments)))

(defun %make-cli-app ()
  (cl-cli:make-app :name
                   "ntop"
                   :version
                   "0.1.0"
                   :global-options
                   (loop for entry in (%flag-records)
                         unless (string= (ntop.app.domain:parity-flag-value-type entry)
                                         "control")
                           collect (%make-cli-option entry))))
