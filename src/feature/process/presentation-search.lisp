(in-package #:ntop.process.presentation)

(defstruct
    (process-search-field-state
     (:constructor %make-process-search-field-state))
  (cursor-index 0 :type (integer 0 *))
  (regex-p nil)
  (case-sensitive-p nil)
  (whole-word-p nil)
  (command-p nil)
  (focused-p t)
  (application-state (ntop.process.application:make-process-search-state)))

(defun %process-search-query (state)
  (or
   (ntop.process.application:process-search-state-query
    (process-search-field-state-application-state state))
   ""))

(defun %compile-process-search-field-state (state)
  (let* ((query (%process-search-query state))
         (application-state
          (process-search-field-state-application-state state)))
    (if (string= query "")
        (setf
          (ntop.process.application:process-search-state-query
           application-state)
          query
          (ntop.process.application:process-search-state-predicate
           application-state)
          nil
          (ntop.process.application:process-search-state-syntax-error
           application-state)
          nil)
      (ntop.process.domain:compile-process-search
        query
        (process-search-field-state-regex-p state)
        (process-search-field-state-case-sensitive-p state)
        (process-search-field-state-whole-word-p state)
        (process-search-field-state-command-p state)
        (lambda
          (predicate)
          (setf
            (ntop.process.application:process-search-state-query
             application-state)
            query
            (ntop.process.application:process-search-state-predicate
             application-state)
            predicate
            (ntop.process.application:process-search-state-syntax-error
             application-state)
            nil))
        (lambda
          (syntax-error)
          (setf
            (ntop.process.application:process-search-state-query
             application-state)
            query
            (ntop.process.application:process-search-state-syntax-error
             application-state)
            syntax-error))))
    state))

(defun make-process-search-field-state (&key config application-state)
  (let* ((config
          (or config (ntop.shared.application:make-application-config)))
         (application-state
          (or application-state
              (ntop.process.application:make-process-search-state)))
         (state
          (%make-process-search-field-state
            :cursor-index
            (length
             (or
              (ntop.process.application:process-search-state-query
               application-state)
              ""))
            :regex-p
            (ntop.shared.application:application-config-process-regex-p
             config)
            :case-sensitive-p
            (ntop.shared.application:application-config-process-case-sensitive-p
             config)
            :whole-word-p
            (ntop.shared.application:application-config-process-whole-word-p
             config)
            :command-p
            (ntop.shared.application:application-config-process-show-command-p
             config)
            :focused-p
            t
            :application-state
            application-state)))
    (%compile-process-search-field-state state)))

(defun %process-search-insert (query cursor text)
  (check-type text string)
  (values
   (concatenate 'string (subseq query 0 cursor) text (subseq query cursor))
   (+ cursor (length text))))

(defun %process-search-previous-word-start (query cursor)
  (loop
    while
    (and (plusp cursor)
         (find (char query (1- cursor))
               (quote (#\Space #\Tab #\Newline #\Return #\Page))))
    do
    (decf cursor))
  (loop
    while
    (and (plusp cursor)
         (not
          (find (char query (1- cursor))
                (quote (#\Space #\Tab #\Newline #\Return #\Page)))))
    do
    (decf cursor))
  cursor)

(defun %process-search-next-word-end (query cursor)
  (loop
    while
    (and (< cursor (length query))
         (not
          (find (char query cursor)
                (quote (#\Space #\Tab #\Newline #\Return #\Page)))))
    do
    (incf cursor))
  (loop
    while
    (and (< cursor (length query))
         (find (char query cursor)
               (quote (#\Space #\Tab #\Newline #\Return #\Page))))
    do
    (incf cursor))
  cursor)

(defun %process-search-replace-query (state query cursor)
  (setf
    (ntop.process.application:process-search-state-query
     (process-search-field-state-application-state state))
    query
    (process-search-field-state-cursor-index state)
    (min (length query) (max 0 cursor)))
  (%compile-process-search-field-state state))

(defun transition-process-search-field (state action &optional value)
  (check-type state process-search-field-state)
  (let* ((query (%process-search-query state))
         (cursor (process-search-field-state-cursor-index state))
         (next-query query)
         (next-cursor cursor)
         (recompile-p nil))
    (case action
      ((:insert-text :insert-paste)
        (multiple-value-setq (next-query next-cursor) (%process-search-insert
                                                       query
                                                       cursor
                                                       value))
        (setf
          recompile-p
          t))
      (:backspace
       (when
         (plusp cursor)
         (setf
           next-query
           (concatenate (quote string)
                        (subseq query 0 (1- cursor))
                        (subseq query cursor))
           next-cursor
           (1- cursor)
           recompile-p
           t)))
      (:delete-forward
       (when
         (< cursor (length query))
         (setf
           next-query
           (concatenate (quote string)
                        (subseq query 0 cursor)
                        (subseq query (1+ cursor)))
           recompile-p
           t)))
      (:delete-previous-word
       (let ((start (%process-search-previous-word-start query cursor)))
         (unless (= start cursor)
           (setf
             next-query
             (concatenate (quote string)
                          (subseq query 0 start)
                          (subseq query cursor))
             next-cursor
             start
             recompile-p
             t))))
      (:move-left
       (setf
         next-cursor
         (max 0 (1- cursor))))
      (:move-right
       (setf
         next-cursor
         (min (length query) (1+ cursor))))
      (:move-word-left
       (setf
         next-cursor
         (%process-search-previous-word-start query cursor)))
      (:move-word-right
       (setf
         next-cursor
         (%process-search-next-word-end query cursor)))
      (:move-home
       (setf
         next-cursor
         0))
      (:move-end
       (setf
         next-cursor
         (length query)))
      (:set-cursor
        (check-type value (integer 0 *))
        (setf
          next-cursor
          (min (length query) value)))
      (:clear
       (setf
         next-query
         ""
         next-cursor
         0
         recompile-p
         t))
      (:toggle-case-sensitive
       (setf
         (process-search-field-state-case-sensitive-p state)
         (not (process-search-field-state-case-sensitive-p state))
         recompile-p
         t))
      (:toggle-whole-word
       (setf
         (process-search-field-state-whole-word-p state)
         (not (process-search-field-state-whole-word-p state))
         recompile-p
         t))
      (:toggle-regex
       (setf
         (process-search-field-state-regex-p state)
         (not (process-search-field-state-regex-p state))
         recompile-p
         t))
      (:focus
       (setf
         (process-search-field-state-focused-p state)
         t))
      (:blur
       (setf
         (process-search-field-state-focused-p state)
         nil))
      (otherwise (error "Unknown process search field action ~S." action)))
    (if recompile-p
        (%process-search-replace-query state next-query next-cursor)
      (setf
        (process-search-field-state-cursor-index state)
        next-cursor))
    state))
