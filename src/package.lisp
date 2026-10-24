;; Keep the org-required CL-only use list uniform across project packages.
(defmacro define-ntop-package (name &body clauses)
  `(defpackage ,name (:use #:cl) ,@clauses))

