(in-package #:ntop.test)

(defmacro define-layer-import-policy (name &body rules)
  `(defparameter ,name
     ',rules))

(define-layer-import-policy +layer-import-policies+
                            (:shared-domain)
                            (:shared-application :shared-domain)
                            (:feature-domain :shared-domain)
                            (:feature-application :shared-domain
                                                  :shared-application
                                                  :same-feature-domain)
                            (:feature-infrastructure :shared-domain
                                                     :shared-application
                                                     :same-feature-domain
                                                     :same-feature-application
                                                     :platform)
                            (:feature-presentation :shared-domain
                                                   :shared-application
                                                   :same-feature-domain
                                                   :same-feature-application
                                                   :ui)
                            (:platform)
                            (:ui :shared-domain)
                            (:composition :all-ntop)
                            (:cli :app :shared-application))

(defun package-designator-name (designator)
  (typecase designator
    (symbol (symbol-name designator))
    (string (string-upcase designator))))

(defparameter +source-file-layer-rules+
  (quote
   (("domain" . "DOMAIN") ("application" . "APPLICATION")
    ("infrastructure" . "INFRASTRUCTURE")
    ("presentation" . "PRESENTATION"))))

(defun package-layer (name)
  (let* ((parts (uiop:split-string name :separator "."))
         (last-part (car (last parts))))
    (cond
      ((string= name "NTOP.SHARED.DOMAIN") :shared-domain)
      ((string= name "NTOP.SHARED.APPLICATION") :shared-application)
      ((string= name "NTOP.UI") :ui)
      ((uiop:string-prefix-p "NTOP.PLATFORM." name) :platform)
      ((uiop:string-prefix-p "NTOP.APP." name) :composition)
      ((string= name "NTOP.CLI") :cli)
      ((member last-part
               '("DOMAIN" "APPLICATION" "INFRASTRUCTURE" "PRESENTATION")
               :test
               #'string=)
       (cond
         ((string= last-part "DOMAIN") :feature-domain)
         ((string= last-part "APPLICATION") :feature-application)
         ((string= last-part "INFRASTRUCTURE") :feature-infrastructure)
         (t :feature-presentation))))))

(defun package-owner (name)
  (let ((position (position #\. name :from-end t)))
    (and position (subseq name 0 position))))

(defun policy-rule-allows-p (rule source target target-layer)
  (case rule
    (:shared-domain (string= target "NTOP.SHARED.DOMAIN"))
    (:shared-application (string= target "NTOP.SHARED.APPLICATION"))
    (:same-feature-domain
     (and (eq target-layer :feature-domain)
          (string= (package-owner source) (package-owner target))))
    (:same-feature-application
     (and (eq target-layer :feature-application)
          (string= (package-owner source) (package-owner target))))
    (:platform (eq target-layer :platform))
    (:ui (eq target-layer :ui))
    (:app (eq target-layer :composition))
    (:all-ntop (uiop:string-prefix-p "NTOP." target))
    (otherwise nil)))

(defun layer-dependency-allowed-p (source target)
  (let* ((source-layer (package-layer source))
         (target-layer (package-layer target))
         (rules (cdr (assoc source-layer +layer-import-policies+))))
    (or (string= source target)
        (not (uiop:string-prefix-p "NTOP." target))
        (some
         (lambda (rule)
           (policy-rule-allows-p rule source target target-layer))
         rules))))

(defun source-file-layer (name)
  (loop for rule in +source-file-layer-rules+
        for stem = (car rule)
        when (or (string= name stem)
                 (uiop:string-prefix-p (concatenate 'string stem "-") name))
          return (cdr rule)))

(defun path-layer-package (pathname)
  (let* ((directories (remove-if-not #'stringp (pathname-directory pathname)))
         (source-position (position "src" directories :test #'string=))
         (area (and source-position (nth (1+ source-position) directories)))
         (feature (and source-position (nth (+ source-position 2) directories)))
         (name (or (pathname-name pathname) ""))
         (layer (source-file-layer name)))
    (cond
      ((null area) nil)
      ((string= area "shared")
       (when layer
         (format nil "NTOP.SHARED.~A" layer)))
      ((string= area "platform")
       (when feature
         (format nil "NTOP.PLATFORM.~A" (string-upcase feature))))
      ((string= area "ui") "NTOP.UI")
      ((string= area "feature")
       (when (and feature layer)
         (format nil "NTOP.~A.~A" (string-upcase feature) layer)))
      ((string= area "app")
       (when layer
         (format nil "NTOP.APP.~A" layer)))
      ((string= area "cli") "NTOP.CLI"))))

(defun read-source-forms (pathname)
  (with-open-file (stream pathname :direction :input)
    (let ((*read-eval* nil)
          (*package* (find-package '#:cl-user)))
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            collect form))))

(defun form-operator-name (form)
  (when (and (consp form) (symbolp (first form)))
    (symbol-name (first form))))

(defun declared-package-name (forms)
  (loop for form in forms
        when (string= (or (form-operator-name form) "") "IN-PACKAGE")
          return (package-designator-name (second form))))

(defun package-declaration-imports (form)
  (when
      (member (form-operator-name form)
              '("DEFPACKAGE" "DEFINE-NTOP-PACKAGE")
              :test
              #'string=)
    (let ((source (package-designator-name (second form))))
      (loop for clause in (cddr form)
            when (and (consp clause)
                      (member (first clause)
                              '(:use :import-from :shadowing-import-from)))
              append (let ((targets
                            (if (eq (first clause) :use)
                                (rest clause)
                                (list (second clause)))))
                       (loop for target in targets
                             for name = (package-designator-name target)
                             when (and name (uiop:string-prefix-p "NTOP." name))
                               collect (cons source name)))))))

(defun collect-package-references (forms)
  (let ((references '()))
    (labels ((visit (object)
               (cond
                 ((symbolp object)
                  (let ((package (symbol-package object)))
                    (when
                        (and package
                             (uiop:string-prefix-p "NTOP."
                                                   (package-name package)))
                      (pushnew (package-name package)
                               references
                               :test
                               #'string=))))
                 ((consp object)
                   (visit (car object))
                   (visit (cdr object)))
                 ((and (vectorp object) (not (stringp object)))
                  (map nil #'visit object)))))
      (map nil #'visit forms))
    references))

(defun source-file-violations (pathname root &optional expected-package)
  (let* ((relative (enough-namestring pathname root))
         (expected
          (or expected-package (path-layer-package (pathname relative))))
         (forms (read-source-forms pathname))
         (actual (declared-package-name forms))
         (references (collect-package-references forms)))
    (append
     (unless expected
       (list (format nil "~A has no package rule." relative)))
     (unless (equal actual expected)
       (list
        (format nil "~A declares ~A; expected ~A." relative actual expected)))
     (loop for target in references
           unless (layer-dependency-allowed-p expected target)
             collect (format nil
                             "~A imports forbidden package ~A."
                             relative
                             target)))))

(defun source-files (directory &optional package-definitions-only)
  (append
   (remove-if-not
    (lambda (pathname)
      (and (string= (or (pathname-type pathname) "") "lisp")
           (if package-definitions-only
               (string= (or (pathname-name pathname) "") "package")
               (not (string= (or (pathname-name pathname) "") "package")))))
    (uiop:directory-files directory))
   (mapcan
    (lambda (subdirectory)
      (source-files subdirectory package-definitions-only))
    (uiop:subdirectories directory))))

(defun check-package-imports (package-forms)
  (loop for form in package-forms
        append (loop for (source . target) in (package-declaration-imports form)
                     unless (layer-dependency-allowed-p source target)
                       collect (format nil
                                       "~A imports forbidden package ~A."
                                       source
                                       target))))

(defun check-source-tree ()
  (let* ((root (asdf:system-source-directory "ntop"))
         (source-root (merge-pathnames "src/" root))
         (package-forms
          (mapcan (function read-source-forms) (source-files source-root t))))
    (append (check-package-imports package-forms)
            (mapcan
             (lambda (pathname)
               (source-file-violations pathname root))
             (source-files source-root)))))
