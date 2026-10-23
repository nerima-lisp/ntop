(in-package #:ntop.app.domain)

(defun btm-parity-catalog ()
  (copy-tree
   (append *btm-parity-key-data*
           *btm-parity-mouse-data*
           *btm-parity-flag-data*
           *btm-parity-column-data*
           *btm-parity-search-field-data*
           *btm-parity-extension-data*)))
