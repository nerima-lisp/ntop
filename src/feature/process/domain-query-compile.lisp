(in-package #:ntop.process.domain)
(defun %process-search-row-string-value (field row)
  (case field
    (:name (or (process-row-name row) ""))
    (:pid (princ-to-string (or (process-row-pid row) 0)))
    (:state (or (process-row-state row) ""))
    (:user (or (process-row-user-name row) "N/A"))
    (otherwise "")))
(defun %process-search-row-numeric-value (field row)
  (or (case field
        (:cpu-percent (process-row-cpu-percent row))
        (:memory-percent (process-row-memory-percent row))
        (:memory-bytes (process-row-memory-bytes row))
        (:read-per-second (process-row-read-bytes-per-second row))
        (:write-per-second (process-row-write-bytes-per-second row))
        (:total-read (process-row-total-read-bytes row))
        (:total-write (process-row-total-write-bytes row))
        (:elapsed-seconds (process-row-elapsed-seconds row))
        (:nice (process-row-nice row))
        (:priority (process-row-priority row))
        (:gpu-percent (process-row-gpu-usage-percent row))
        (:gpu-memory-bytes (process-row-gpu-memory-bytes row))
        (:gpu-memory-percent (process-row-gpu-memory-percent row))
        (otherwise 0))
      0))
(defun %process-search-compare-numbers (field operator actual expected)
  (let* ((actual (coerce actual 'double-float))
         (expected (coerce expected 'double-float))
         (exact-equality-p (eq field :elapsed-seconds)))
    (case operator
      (:equal
       (if exact-equality-p
           (= actual expected)
           (< (abs (- actual expected)) double-float-epsilon)))
      (:not-equal
       (if exact-equality-p
           (/= actual expected)
           (>= (abs (- actual expected)) double-float-epsilon)))
      (:less (< actual expected))
      (:less-or-equal (<= actual expected))
      (:greater (> actual expected))
      (:greater-or-equal (>= actual expected))
      (otherwise nil))))
(defun %process-search-compile-match
    (node regex-p case-sensitive-p whole-word-p spans &optional negate-p)
  (let* ((field (second node))
         (text (third node))
         (pattern (if regex-p text (cl-regex-kit:escape text)))
         (prefix-length (if whole-word-p 1 0))
         (pattern (if whole-word-p
                      (concatenate 'string "^" pattern "$")
                      pattern))
         (regex
           (handler-case
               (cl-regex-kit:compile-regex
                pattern :case-insensitive (not case-sensitive-p))
             (cl-regex-kit:regex-syntax-error (condition)
               (let* ((span (gethash node spans))
                      (body-start (third span))
                      (body-end (fourth span))
                      (pattern-position
                        (max 0 (- (or
                                   (cl-regex-kit:regex-syntax-error-position
                                    condition)
                                   prefix-length)
                                  prefix-length)))
                      (start (min body-end (+ body-start pattern-position)))
                      (end (min body-end (1+ start))))
                 (%signal-process-search-error
                  start end "Invalid regular expression."))))))
    (lambda (row)
      (let ((value (%process-search-row-string-value field row)))
        (if (stringp value)
            (let ((matched-p (cl-regex-kit:is-match-p regex value)))
              (values (if negate-p (not matched-p) matched-p) t))
            (values nil nil))))))
(defun %process-search-compile-ast
    (ast regex-p case-sensitive-p whole-word-p spans)
  (case (first ast)
    (:true
     (lambda (row)
       (declare (ignore row))
       (values t t)))
    (:match
     (%process-search-compile-match
      ast regex-p case-sensitive-p whole-word-p spans))
    (:compare
     (let ((field (second ast))
           (operator (third ast))
           (expected (fourth ast)))
       (lambda (row)
         (let ((actual (%process-search-row-numeric-value field row)))
           (if (realp actual)
               (values (%process-search-compare-numbers field operator actual expected)
                       t)
               (values nil nil))))))
    (:not
     (let ((child (second ast)))
       (if (eq (first child) :match)
           (%process-search-compile-match
            child regex-p case-sensitive-p whole-word-p spans t)
           (let ((evaluate-child
                   (%process-search-compile-ast
                    child regex-p case-sensitive-p whole-word-p spans)))
             (lambda (row)
               (multiple-value-bind (matched-p available-p)
                   (funcall evaluate-child row)
                 (values (and available-p (not matched-p)) available-p)))))))
    ((:and :or)
     (let* ((kind (first ast))
            (and-p (eq kind :and))
            (children
              (mapcar (lambda (child)
                        (%process-search-compile-ast
                         child regex-p case-sensitive-p whole-word-p spans))
                      (rest ast))))
       (lambda (row)
         (let ((matched-p and-p)
               (available-p t))
           (dolist (evaluate-child children)
             (multiple-value-bind (child-match-p child-available-p)
                 (funcall evaluate-child row)
               (setf matched-p
                     (if and-p
                         (and matched-p child-match-p)
                         (or matched-p child-match-p))
                     available-p (and available-p child-available-p))))
           (values matched-p available-p)))))
    (otherwise
     (error "Invalid process search AST node: ~S." ast))))
(defmethod compile-process-search
    ((query string) regex-p case-sensitive-p whole-word-p
     on-success on-syntax-error)
  (multiple-value-bind (predicate syntax-error)
      (handler-case
          (multiple-value-bind (ast spans)
              (parse-process-search query)
            (let ((evaluate
                    (%process-search-compile-ast
                     ast regex-p case-sensitive-p whole-word-p spans)))
              (values (lambda (row)
                        (nth-value 0 (funcall evaluate row)))
                      nil)))
        (process-search-query-error (condition)
          (values nil
                  (make-process-search-syntax-error
                   :start-offset
                   (process-search-query-error-start-offset condition)
                   :end-offset
                   (process-search-query-error-end-offset condition)
                   :message
                   (process-search-query-error-message condition)))))
    (if syntax-error
        (funcall on-syntax-error syntax-error)
        (funcall on-success predicate))))
