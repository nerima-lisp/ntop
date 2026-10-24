(require "asdf")

(let* ((root (uiop:ensure-directory-pathname (uiop:getcwd)))
       (system-definition (merge-pathnames "ntop.asd" root))
       (common-lisp-package (find-package "COMMON-LISP")))
  (asdf:load-asd system-definition)
  (dolist (component
           (asdf:component-children (asdf:find-system "ntop")))
    (when (string=
           (or (pathname-name (asdf:component-pathname component)) "")
           "package")
      (load (asdf:component-pathname component))))
  (load (merge-pathnames "t/package.lisp" root))
  (let ((packages
         (sort (remove-if-not
                (lambda (package)
                  (uiop:string-prefix-p "NTOP." (package-name package)))
                (list-all-packages))
               (function string<)
               :key (function package-name))))
    (unless packages
      (error "No ntop packages were declared."))
    (dolist (package packages)
      (unless (equal (package-use-list package) (list common-lisp-package))
        (error "~A must use only COMMON-LISP." (package-name package))))
    (format t "PACKAGE-USE checked ~D packages.~%" (length packages))))
