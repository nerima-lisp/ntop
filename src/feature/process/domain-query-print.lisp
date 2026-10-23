(in-package #:ntop.process.domain)
(defun %process-search-field-name (field)
  (or (car (find field
                  *process-search-field-aliases*
                  :key #'cdr
                  :test #'eq))
      (error "No process search prefix represents ~S." field)))
(defun %process-search-operator-text (operator)
  (case operator
    (:equal "=")
    (:not-equal "!=")
    (:less "<")
    (:greater ">")
    (:less-or-equal "<=")
    (:greater-or-equal ">=")
    (otherwise (error "Unknown process search operator: ~S." operator))))
(defun %process-search-number-text (value)
  (if (integerp value)
      (format nil "~D" value)
      (substitute #\e #\d
                  (string-downcase
                   (format nil "~,16E" (coerce value 'double-float))))))
(defun %process-search-duration-text (value)
  (let* ((text (format nil "~,9F" (coerce value 'double-float)))
         (dot (position #\. text))
         (end (length text)))
    (when dot
      (loop while (and (> end (1+ dot))
                       (char= #\0 (char text (1- end))))
            do (decf end))
      (when (= end (1+ dot))
        (decf end)))
    (subseq text 0 end)))
(defun %process-search-quoted-text (text)
  (when (position #\" text)
    (error "Process search text containing a quotation mark cannot be printed."))
  (format nil "\"~A\"" text))
(defun %print-process-search-match (field text negative-p)
  (let ((quoted (%process-search-quoted-text text)))
    (if (eq field :name)
        (format nil "~A~A" (if negative-p "!" "") quoted)
        (format nil "~A~A~A"
                (%process-search-field-name field)
                (if negative-p "!=" "=")
                quoted))))
(defun %print-process-search-ast-node (ast parent-precedence)
  (case (first ast)
    (:true
     "\"\"")
    (:match
     (%print-process-search-match (second ast) (third ast) nil))
    (:compare
     (let ((field (second ast))
           (operator (%process-search-operator-text (third ast)))
           (value (fourth ast)))
       (format nil "~A~A~A"
               (%process-search-field-name field)
               operator
               (if (eq field :elapsed-seconds)
                   (concatenate 'string
                                (%process-search-duration-text value)
                                "s")
                   (%process-search-number-text value)))))
    (:not
     (let ((child (second ast)))
       (if (eq (first child) :match)
           (%print-process-search-match
            (second child) (third child) t)
           (format nil "!(~A)"
                   (%print-process-search-ast-node child 0)))))
    ((:and :or)
     (let* ((kind (first ast))
            (precedence (if (eq kind :and) 2 1))
            (children
              (loop for child in (rest ast)
                    collect (%print-process-search-ast-node
                             child precedence)))
            (body
              (format nil
                      (if (eq kind :and) "~{~A~^ and ~}" "~{~A~^ or ~}")
                      children)))
       (if (< precedence parent-precedence)
           (format nil "(~A)" body)
           body)))
    (otherwise
     (error "Invalid process search AST node: ~S." ast))))
(defun print-process-search-ast (ast)
  (%print-process-search-ast-node ast 0))
