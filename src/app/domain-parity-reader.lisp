(in-package #:ntop.app.domain)

(progn
  (defun parity-flag-kebab-case (entry)
    (substitute #\- #\_ (parity-flag-snake-case entry)))
  (defun btm-parity-catalog ()
    (mapcar #'copy-parity-entry
            (append *btm-parity-key-data*
                    *btm-parity-mouse-data*
                    *btm-parity-flag-data*
                    *btm-parity-column-data*
                    *btm-parity-search-field-data*
                    *btm-parity-extension-data*))))
