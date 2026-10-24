(require "asdf")

(defparameter *project-root*
  (truename
   (merge-pathnames "../" (uiop:pathname-directory-pathname *load-truename*))))
(defparameter *org-main-system-names*
  ;; These are main ASDF systems from pinned nerima-lisp flake inputs, plus ntop.
  ;; Source: nerima-lisp/.github@3e82bb552611d14c8a0e67cbac920a4ac02e253d,
  ;; DEPENDENCY_POLICY.md (depth clause, lines 82-94).
  ;; The limit applies to repository edges between main systems.
  (quote ("ntop"
    "cl-tty-kit" "cl-codec-kit" "cl-cli" "cl-regex-kit" "cl-host-kit"
    "cl-concurrent-kit" "cl-boundary-kit" "cl-date-kit" "cl-parser-kit"
    "cl-json-kit" "cl-weave")))

(asdf:load-asd (merge-pathnames "ntop.asd" *project-root*))

(defun dependency-names (dependency)
  (cond
    ((null dependency) nil)
    ((stringp dependency) (list (string-downcase dependency)))
    ((symbolp dependency)
     (list (string-downcase (symbol-name dependency))))
    ((consp dependency)
     (case (first dependency)
       (:require
        (mapcan (function dependency-names) (rest dependency)))
       (:version
        (dependency-names (second dependency)))
       (:feature
        (when (uiop:featurep (second dependency))
          (dependency-names (third dependency))))
       (otherwise
        (error "Unsupported ASDF dependency form: ~S." dependency))))
    (t
     (error "Unsupported ASDF dependency value: ~S." dependency))))

(defun system-dependency-names (system)
  (sort
   (remove-if-not
    (lambda (name)
      (member name *org-main-system-names* :test (function string=)))
    (remove-duplicates
     (mapcan (lambda (dependency)
               (dependency-names dependency))
             (asdf:system-depends-on system))
     :test (function string=)))
   (function string<)))

(defun walk-main-dependencies (system-name prefix graph on-leaf on-unresolved)
  (when (member system-name prefix :test (function string=))
    (error "Dependency cycle: ~{~A~^ -> ~}."
           (append prefix (list system-name))))
  (let* ((path (append prefix (list system-name)))
         (system (asdf:find-system system-name nil)))
    (unless system
      (funcall on-unresolved (or (car (last prefix)) "<root>") system-name)
      (funcall on-leaf path)
      (return-from walk-main-dependencies nil))
    (let ((dependencies (system-dependency-names system)))
      (setf (gethash system-name graph) dependencies)
      (if dependencies
          (dolist (dependency dependencies)
            (walk-main-dependencies
             dependency path graph on-leaf on-unresolved))
          (funcall on-leaf path)))))

(defun collect-dependency-paths (root graph)
  (let* ((paths nil)
         (unresolved nil)
         (system (asdf:find-system root nil)))
    (unless system
      (error "ASDF system ~S is not registered." root))
    (let ((dependencies (system-dependency-names system)))
      (setf (gethash root graph) dependencies)
      (labels ((record-leaf (path)
               (push path paths))
             (record-unresolved (parent dependency)
               (pushnew (list parent dependency) unresolved
                        :test (function equal)))
             (walk (system-name prefix)
               (walk-main-dependencies
                system-name prefix graph #'record-leaf #'record-unresolved)))
        (if (member root *org-main-system-names* :test (function string=))
            (walk root nil)
            (if dependencies
                (dolist (dependency dependencies)
                  (walk dependency (list root)))
                (record-leaf (list root))))))
    (values
     (nreverse paths)
     (sort (nreverse unresolved)
           (lambda (left right)
             (string<
              (format nil "~A -> ~A" (first left) (second left))
              (format nil "~A -> ~A" (first right) (second right))))))))
(defun maximum-dependency-depth (paths)
  (loop for path in paths
        maximize
        (max 0
             (1- (count-if
                  (lambda (system-name)
                    (member system-name *org-main-system-names*
                            :test (function string=)))
                  path)))
          into maximum
        finally (return (or maximum 0))))
(defun report-dependency-paths (label root paths graph)
  (let* ((system-names
           (sort
            (remove-if-not
             (lambda (system-name)
               (member system-name *org-main-system-names*
                       :test (function string=)))
             (remove-duplicates
              (mapcan (lambda (path) (copy-list path)) paths)
              :test (function string=)))
            (function string<)))
         (maximum (maximum-dependency-depth paths)))
    (format t "~A~%" label)
    (format t "Direct org main-system dependencies for ~A: ~{~A~^, ~}~%"
            root (gethash root graph))
    (format t "Org main-system dependency graph:~%")
    (dolist (system-name system-names)
      (let* ((system (asdf:find-system system-name nil))
             (source (and system (asdf:system-source-file system)))
             (dependencies (gethash system-name graph)))
        (format t "  ~A [~A] -> ~A~%"
                system-name
                (if source (file-namestring source) "<unresolved>")
                (if dependencies
                    (format nil "~{~A~^, ~}" dependencies)
                    "<none>"))))
    (format t "Longest org-edge path depth ~D:~%" maximum)
    (dolist (path paths)
      (when (= (maximum-dependency-depth (list path))
               maximum)
        (format t "  ~{~A~^ -> ~}~%" path)))
    (format t "Over-limit org-edge paths (> 4):~%")
    (dolist (path paths)
      (let ((depth
              (maximum-dependency-depth (list path))))
        (when (> depth 4)
          (format t "  ~{~A~^ -> ~} [org-edge depth ~D]~%"
                  path depth))))
    maximum))
(defun report-unresolved-dependencies (label edges)
  (when edges
    (format t "~A unresolved dependency edges:~%" label)
    (dolist (edge edges)
      (format t "  ~A -> ~A~%" (first edge) (second edge)))))

(let* ((runtime-graph (make-hash-table :test (function equal)))
       (test-graph (make-hash-table :test (function equal)))
       (runtime-result
         (multiple-value-list
          (collect-dependency-paths "ntop" runtime-graph)))
       (test-result
         (multiple-value-list
          (collect-dependency-paths "ntop/test" test-graph)))
       (runtime-paths (first runtime-result))
       (runtime-unresolved (second runtime-result))
       (test-paths (first test-result))
       (test-unresolved (second test-result))
       (runtime-depth (maximum-dependency-depth runtime-paths))
       (test-depth (maximum-dependency-depth test-paths)))
  (unless runtime-paths
    (error "The runtime org main-system graph is empty."))
  (unless test-paths
    (error "The test dependency paths are empty."))
  (report-dependency-paths
   "Runtime org main-system paths:" "ntop" runtime-paths runtime-graph)
  (format t "MAX_RUNTIME_DEPTH=~D LIMIT=4~%" runtime-depth)
  (report-dependency-paths
   "Test-system org main-system paths (display only):"
   "ntop/test" test-paths test-graph)
  (format t "MAX_TEST_DEPTH=~D LIMIT=4 DISPLAY_ONLY=TRUE~%" test-depth)
  (report-unresolved-dependencies "Runtime org main systems" runtime-unresolved)
  (report-unresolved-dependencies "Test org main systems" test-unresolved)
  (when runtime-unresolved
    (error "The runtime org main-system graph contains unresolved systems."))
  (when (> runtime-depth 4)
    (error "A runtime org main-system path exceeds four repository edges.")))
