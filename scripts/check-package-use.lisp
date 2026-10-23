(require "asdf")

(let* ((source (merge-pathnames "src/package.lisp"
                                (uiop:ensure-directory-pathname
                                 (uiop:getcwd))))
       (common-lisp-package (find-package '#:cl)))
  (load source)
  (let ((packages
          (sort (remove-if-not
                 (lambda (package)
                   (uiop:string-prefix-p "NTOP." (package-name package)))
                 (list-all-packages))
                #'string<
                :key #'package-name)))
    (unless packages
      (error "No ntop packages were declared."))
    (dolist (package packages)
      (unless (equal (package-use-list package) (list common-lisp-package))
        (error "~A must use only COMMON-LISP." (package-name package))))
    (format t "PACKAGE-USE checked ~D packages.~%" (length packages))))
