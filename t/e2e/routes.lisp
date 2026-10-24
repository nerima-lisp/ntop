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
           collect (list :context context
                         :inputs (ntop.app.domain:parity-key-keys entry))))
    (:mouse
     (loop for context in (ntop.app.domain:parity-mouse-contexts entry)
           collect (list :context context
                         :gesture (ntop.app.domain:parity-mouse-gesture entry))))
    (:flag
     (append (list (list :context nil :variant :kebab)
                   (list :context nil :variant :snake))
             (when (and (stringp (ntop.app.domain:parity-flag-short entry))
                        (plusp (length (ntop.app.domain:parity-flag-short entry))))
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
  (if (and (eq kind :mouse)
           (string= (ntop.app.domain:parity-mouse-action entry)
                    "Ignore other buttons"))
      (values :manual
              "Crossterm 0.29 cannot encode a distinct Other button-down event.")
    (values :automated nil)))

(defun %additional-route (id action input expected classification reason
                          comparable-p)
  (make-route-case :id id
                   :kind :additional
                   :action action
                   :entry nil
                   :expected expected
                   :subcases (list (list :context nil :input input))
                   :classification classification
                   :manual-reason reason
                   :comparable-p comparable-p))

(defun %additional-routes ()
  (list
   (%additional-route
    "cpu-frequency" "CPU frequency comparison"
    "sudo powermetrics --samplers cpu_power -i 1000 -n 5"
    "Run ntop beside powermetrics; compare each CPU cluster average within 10%."
    :manual
    "powermetrics needs sudo and returns device-specific samples."
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
    :manual
    "BTM 0.14.9 showed No data for the harness-owned PID on this macOS host."
    nil)
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

(progn
  (defun %catalog-route (entry index)
    (let* ((kind (ntop.app.domain:parity-entry-kind entry))
           (action (%route-action entry kind)))
      (multiple-value-bind (classification reason)
          (%route-classification entry kind)
        (make-route-case
         :id (format nil "~a-~d" (string-downcase (symbol-name kind)) index)
         :kind kind
         :action action
         :entry entry
         :expected (%route-expectation entry kind action)
         :subcases (%route-subcases entry kind)
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
    (length routes)))

(defun route-kind-count (kind &optional (routes (route-catalog)))
  (count kind routes :key #'route-kind))
