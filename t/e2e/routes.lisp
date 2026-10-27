;;; Each of the 173 parity records (68 keys, 12 mouse actions, 52 flags including
;;; --help and --version, 20 columns, 17 search fields, and 4 extensions) counts once,
;;; context and alternate-input cases are subcases, and the v, [, ], and G additions
;;; in spec §9.3 count only in their matching parity records.
(in-package #:ntop.e2e)

(defstruct (route-case
            (:constructor make-route-case
                (&key id kind action entry expected subcases classification
                      manual-reason comparable-p))
            (:conc-name route-))
  id
  kind
  action
  entry
  expected
  subcases
  classification
  manual-reason
  comparable-p)

(defun %route-subcases (entry kind)
  (case kind
    (:key
     (loop for context in (ntop.app.domain:parity-key-contexts entry)
           append
           (loop for input in (ntop.app.domain:parity-key-keys entry)
                 collect (list :context context :input input))))
    (:mouse
     (let ((contexts (ntop.app.domain:parity-mouse-contexts entry)))
       (if (string= (ntop.app.domain:parity-mouse-action entry)
                    "Ignore other buttons")
           (loop for context in contexts
                 append
                 (list (list :context context :gesture "right/middle/other button"
                             :button :right)
                       (list :context context :gesture "right/middle/other button"
                             :button :middle)
                       (list :context context :gesture "right/middle/other button"
                             :button :other)))
         (loop for context in contexts
               collect (list :context context
                             :gesture
                             (ntop.app.domain:parity-mouse-gesture entry))))))
    (:flag
     (append (list (list :context nil :variant :kebab)
                   (list :context nil :variant :snake))
             (when (and (stringp (ntop.app.domain:parity-flag-short entry))
                        (plusp (length
                                (ntop.app.domain:parity-flag-short entry))))
               (list (list :context nil :variant :short)))
             (loop for alias in (ntop.app.domain:parity-flag-aliases entry)
                   collect (list :context nil :variant :alias :alias alias))
             (list (list :context nil :variant :kebab :invalid-value t))))
    (:column (list (list :context "process table" :input nil)))
    (:search-field
     (let ((tokens (ntop.app.domain:parity-search-field-tokens entry)))
       (loop for token in (or tokens (list "sleep"))
             collect (list :context "process search" :input token))))
    (:extension
     (let* ((action (ntop.app.domain:parity-extension-action entry))
            (contexts
             (if (string= action "Cycle selected widget view")
                 (ntop.app.domain:parity-extension-contexts entry)
               (list "graphs"))))
       (loop for context in contexts
             collect (list :context context
                           :input (ntop.app.domain:parity-extension-key entry)))))
    (otherwise (error "Unknown route kind ~S." kind))))

(defparameter +route-subcase-plans+
              (append
               (list
                (list "key-5" (list 1) :graph :graph)
                (list "key-6" (list 1) :dashboard :selection nil
                      (list :selection-mode :expand-restore))
                (list "key-7" (list 1) :graph :graph)
                (list "key-8" (list 1 2 3 4) :dashboard :selection)
                (list "key-9" (list 2) :dashboard :selection)
                (list "key-10" (list 2 3 4) :dashboard :selection)
                (list "key-11" (list 1 2 4) :dashboard :selection)
                (list "key-12" (list 1 2) :processes :navigation nil
                      (list :fixture-navigation-start :last
                            :fixture-process-count 64))
                (list "key-13" (list 1 2) :processes :navigation nil
                      (list :fixture-navigation-start :first
                            :fixture-process-count 64))
                (list "key-14" (list 1 2) :processes :navigation nil
                       (list :fixture-navigation-start :last
                             :fixture-process-count 64))
                (list "key-15" (list 1) :processes :navigation nil
                       (list :fixture-navigation-start :first
                             :fixture-process-count 64))
                (list "key-16" (list 1) :processes :navigation nil
                       (list :fixture-navigation-start :last
                             :fixture-process-count 64))
                (list "key-17" (list 1) :processes :navigation nil
                       (list :fixture-navigation-start :first
                             :fixture-process-count 64))
                (list "key-18" (list 1) :processes :navigation nil
                       (list :fixture-navigation-start :last
                             :fixture-process-count 64))
                (list "key-19" (list 1) :processes :navigation nil
                       (list :fixture-navigation-start :first
                             :fixture-process-count 64))
                (list "key-20" (list 1 2) :process-tree :tree nil
                       (list :fixture-tree-query "ntop-t-"
                             :fixture-tree-focus :parent
                             :fixture-tree-expanded t
                             :observer-quiet-seconds 1.5d0))
                (list "key-20" (list 3 4) :search :search-cursor)
                (list "key-20" (list 5 6) :battery :selection :battery-tabs)
                (list "key-21" (list 1 2) :process-tree :tree nil
                       (list :fixture-tree-query "ntop-t-"
                             :fixture-tree-focus :parent
                             :fixture-tree-expanded nil
                             :observer-quiet-seconds 1.5d0))
                (list "key-21" (list 3 4) :search :search-cursor)
                (list "key-21" (list 5 6) :battery :selection :battery-tabs)
                (list "key-25" (list 1 2 3 4 5 6) :processes :search-results
                      nil (list :query-procedure :toggle-case))
                (list "key-26" (list 1 2 3 4 5) :processes :search-results
                      nil (list :query-procedure :toggle-whole-word))
                (list "key-27" (list 1 2 3 4) :processes :search-results
                      nil (list :query-procedure :toggle-regex))
                (list "key-28" (list 1 2) :search :query-edit
                      nil (list :query-procedure :cursor-start))
                (list "key-29" (list 1 2) :search :query-edit
                      nil (list :query-procedure :cursor-end))
                (list "key-30" (list 1 2) :search :query-edit
                      nil (list :query-procedure :clear))
                (list "key-31" (list 1 2) :search :query-edit
                      nil (list :query-procedure :delete-previous-word))
                (list "key-32" (list 1 2 3 4) :search :query-edit
                      nil (list :query-procedure :delete-previous-character))
                (list "key-33" (list 1 2) :search :query-edit
                      nil (list :query-procedure :delete-next-character))
                (list "key-35" (list 1 3) :processes :modal nil (list :modal-action :accept))
                (list "key-36" (list 6) :processes :modal nil (list :modal-action :close))
                (list "key-37" (list 1) :processes :modal)
                (list "key-38" (list 1) :processes :sort)
                (list "key-39" (list 1) :processes :sort)
                (list "key-40" (list 1) :processes :sort)
                (list "key-41" (list 1) :processes :sort)
                (list "key-42" (list 1) :processes :sort)
                (list "key-43" (list 1) :processes :sort)
                (list "key-44" (list 1) :processes :table)
                (list "key-46" (list 1) :processes :sort)
                (list "key-47" (list 1 2) :processes :tree)
                (list "key-49" (list 1) :process-tree :tree nil
                       (list :fixture-tree-query "ntop-t-"
                             :fixture-tree-focus :parent
                             :fixture-tree-expanded t))
                (list "key-50" (list 1 2) :process-tree :tree nil
                       (list :fixture-tree-query "ntop-t-"
                             :fixture-tree-focus :parent
                             :fixture-tree-expansion-by-input
                             (list (list "+" :collapsed)
                                   (list "-" :expanded))))
                (list "key-51" (list 1) :graph :graph)
                (list "key-52" (list 1) :graph :graph)
                (list "key-53" (list 1) :graph :graph)
                (list "key-67" (list 1 2) :processes :navigation)
                (list "key-66" (list 1 2 3 4 5 6 7 8 9) :help :navigation)
                (list "mouse-71" (list 1) :dashboard :mouse)
                (list "mouse-72" (list 1) :dashboard :mouse)
                (list "mouse-76" (list 1 2) :processes :mouse)
                (list "mouse-77" (list 1) :battery :selection :battery-tabs)
                (list "mouse-69" (list 1) :graph :graph)
                (list "mouse-69" (list 3) :help :navigation)
                (list "mouse-70" (list 1) :graph :graph)
                (list "mouse-70" (list 3) :help :navigation)
                (list "mouse-79" (list 1 2) :dashboard :transition)
                (list "mouse-79" (list 3) :capability :mouse :mouse-other)
                (list "flag-100" (list 1 2) :capability :transition :linux-only)
                (list "cpu-frequency" (list 1) :capability :cpu-frequency :powermetrics)
                (list "search-field-153" (list 1) :search :query-edit
                      nil (list :query-procedure :field-token))
                (list "search-field-154" (list 1) :search :query-edit
                      nil (list :query-procedure :field-token))
                (list "resize" (list 1) :dashboard :resize)
                (list "restore-exception" (list 1) :capability :restore
                      nil
                      (list :btm-self-na-reason
                            "The injected Lisp exception has no btm-self equivalent."))
                (list "extension-170" (list 1 2 3) :dashboard :extension
                      nil
                      (list :btm-self-na-reason
                            "The extension action has no btm-self equivalent."))
                (list "extension-171" (list 1) :dashboard :extension
                      nil
                      (list :btm-self-na-reason
                            "The extension action has no btm-self equivalent."))
                (list "extension-172" (list 1) :dashboard :extension
                      nil
                      (list :btm-self-na-reason
                            "The extension action has no btm-self equivalent."))
                (list "extension-173" (list 1) :dashboard :extension
                      nil
                      (list :btm-self-na-reason
                            "The extension action has no btm-self equivalent.")))
               (loop for index from 56 to 65
                     collect (list (format nil "key-~d" index) (list 1) :table :table))
               (loop for index from 133 to 152
                     collect (list (format nil "column-~d" index) (list 1)
                                   :processes :column))))

(defun %route-action (entry kind)
  (case kind
    (:key (ntop.app.domain:parity-key-action entry))
    (:mouse (ntop.app.domain:parity-mouse-action entry))
    (:flag (ntop.app.domain:parity-flag-snake-case entry))
    (:column (ntop.app.domain:parity-column-header entry))
    (:search-field (ntop.app.domain:parity-search-field-field entry))
    (:extension (ntop.app.domain:parity-extension-action entry))))

(defun %route-behavior (entry kind)
  (case kind
    (:key (ntop.app.domain:parity-key-behavior entry))
    (:mouse (ntop.app.domain:parity-mouse-behavior entry))
    (:flag (ntop.app.domain:parity-flag-condition entry))
    (:column (ntop.app.domain:parity-column-condition entry))
    (:search-field (ntop.app.domain:parity-search-field-condition entry))
    (:extension (ntop.app.domain:parity-extension-behavior entry))))

(defun %route-expectation (entry kind action)
  (case kind
    (:key
     (if (string= action "Documented horizontal movement")
         "The documented h/l keys move the selected widget left or right."
         (format nil "~a; compare the resulting condition-A projection with btm."
                 (ntop.app.domain:parity-key-behavior entry))))
    (:mouse
     (format nil "~a; compare the resulting condition-A projection with btm."
             (ntop.app.domain:parity-mouse-behavior entry)))
    (:flag
     (format nil "~a; require the documented CLI result or a condition-A frame."
             (ntop.app.domain:parity-flag-condition entry)))
    (:column
     (let ((condition (ntop.app.domain:parity-column-condition entry)))
       (if (search "optional" condition :test (function char-equal))
           (format nil "Header ~s stays absent under the default layout."
                   (ntop.app.domain:parity-column-header entry))
           (format nil "Header ~s follows condition ~a."
                   (ntop.app.domain:parity-column-header entry)
                   condition))))
    (:search-field
     (format nil "Search field ~a accepts a catalog token and remains visible."
             (ntop.app.domain:parity-search-field-field entry)))
    (:extension
     (format nil "~a; assert a visible state change in the target context."
             (ntop.app.domain:parity-extension-behavior entry)))))

(defun %route-classification (entry kind)
  (declare (ignore entry kind))
  (values :automated nil))

(defun %additional-route (id action input expected classification reason
                             comparable-p)
  (make-route-case :id id
                   :kind :additional
                   :action action
                   :entry nil
                   :expected expected
                   :subcases (%route-subcases-with-metadata id action (list (list :context nil :input input)))
                   :classification classification
                   :manual-reason reason
                   :comparable-p comparable-p))

(defun %additional-routes ()
  (list
   (%additional-route
    "cpu-frequency" "CPU frequency comparison"
    "sudo powermetrics --samplers cpu_power -i 1000 -n 5"
    "Run ntop beside powermetrics; compare each CPU cluster average within 10%."
    :automated nil
    nil)
   (%additional-route
    "basic-mode" "Basic mode" "--basic"
    "Basic mode shows RAM and RX/TX totals with Disks and Temperatures navigation."
    :automated nil t)
   (%additional-route
    "resize" "Terminal resize" "80x24 to 120x36"
    "The resized frame fills a 120-column by 36-row terminal."
    :automated nil t)
   (%additional-route
    "sleep-child" "Stop harness-owned sleep child"
    "search exact child PID; select its row; confirm stop"
    "Only the child started by this route exits after confirmation."
    :automated nil t)
   (%additional-route
    "restore-normal" "Terminal restore after normal exit" "q"
    "stty -g before and after are equal after normal exit."
    :automated nil t)
   (%additional-route
    "restore-sigint" "Terminal restore after SIGINT" "send SIGINT to app PID"
    "stty -g before and after are equal after SIGINT."
    :automated nil t)
   (%additional-route
    "restore-sigterm" "Terminal restore after SIGTERM" "send SIGTERM to app PID"
    "stty -g before and after are equal after SIGTERM."
    :automated nil t)
   (%additional-route
    "restore-sighup" "Terminal restore after SIGHUP" "send SIGHUP to app PID"
    "stty -g before and after are equal after SIGHUP."
    :automated nil t)
   (%additional-route
    "restore-exception" "Terminal restore after Lisp exception"
    "PTY-launched SBCL calls call-with-terminal-session; the thunk signals error."
    "Terminal state is restored at the infrastructure boundary as the error escapes."
    :automated nil nil)))

(defun %catalog-route (entry index)
  (let* ((kind (ntop.app.domain:parity-entry-kind entry))
         (action (%route-action entry kind))
         (id (format nil "~a-~d" (string-downcase (symbol-name kind)) index)))
    (multiple-value-bind (classification reason)
        (%route-classification entry kind)
      (make-route-case
       :id id
       :kind kind
       :action action
       :entry entry
       :expected (%route-expectation entry kind action)
       :subcases (%route-subcases-with-metadata id action (%route-subcases entry kind))
       :classification classification
       :manual-reason reason
       :comparable-p (not (eq kind :extension))))))

(defun route-parity-catalog ()
  (loop for entry in (ntop.app.domain:btm-parity-catalog)
        for index from 1
        collect (%catalog-route entry index)))

(defun route-additional-catalog ()
  (%additional-routes))

(defun route-catalog ()
  (append (route-parity-catalog) (route-additional-catalog)))

(defun route-count (&optional (routes (route-catalog)))
  (length routes))

(defun route-kind-count (kind &optional (routes (route-catalog)))
  (count kind routes :key #'route-kind))
