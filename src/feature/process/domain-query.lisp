(in-package #:ntop.process.domain)
(define-condition process-search-query-error (error)
  ((start-offset :initarg :start-offset
                 :reader process-search-query-error-start-offset
                 :type (integer 0 *))
   (end-offset :initarg :end-offset
               :reader process-search-query-error-end-offset
               :type (integer 0 *))
   (message :initarg :message
            :reader process-search-query-error-message
            :type string))
  (:report
   (lambda (condition stream)
     (write-string (process-search-query-error-message condition) stream))))
(defparameter *process-search-field-aliases*
  '(("cpu" . :cpu-percent)
    ("cpu%" . :cpu-percent)
    ("mem" . :memory-percent)
    ("mem%" . :memory-percent)
    ("memb" . :memory-bytes)
    ("read" . :read-per-second)
    ("r/s" . :read-per-second)
    ("rps" . :read-per-second)
    ("write" . :write-per-second)
    ("w/s" . :write-per-second)
    ("wps" . :write-per-second)
    ("tread" . :total-read)
    ("t.read" . :total-read)
    ("twrite" . :total-write)
    ("t.write" . :total-write)
    ("pid" . :pid)
    ("state" . :state)
    ("user" . :user)
    ("time" . :elapsed-seconds)
    ("nice" . :nice)
    ("priority" . :priority)
    ("gpu%" . :gpu-percent)
    ("gmem" . :gpu-memory-bytes)
    ("gmem%" . :gpu-memory-percent)))
(defun %process-search-field (name)
  (or (cdr (assoc name *process-search-field-aliases* :test #'string-equal))
      :name))
(defun %process-search-numeric-field-p (field)
  (member field
          '(:cpu-percent :memory-percent :memory-bytes
            :read-per-second :write-per-second :total-read :total-write
            :elapsed-seconds :nice :priority :gpu-percent
            :gpu-memory-bytes :gpu-memory-percent)
          :test #'eq))
(defun %process-search-comparison (tokens index)
  (let ((token (and (< index (length tokens)) (aref tokens index))))
    (when token
      (case (cl-parser-kit:token-type token)
        (:equal (values :equal 1))
        (:not-equal (values :not-equal 1))
        (:less
         (if (and (< (1+ index) (length tokens))
                  (eq :equal
                      (cl-parser-kit:token-type (aref tokens (1+ index)))))
             (values :less-or-equal 2)
             (values :less 1)))
        (:greater
         (if (and (< (1+ index) (length tokens))
                  (eq :equal
                      (cl-parser-kit:token-type (aref tokens (1+ index)))))
             (values :greater-or-equal 2)
             (values :greater 1)))
        (otherwise (values nil 0))))))
(defun %make-process-search-token (source type start end &optional value)
  (cl-parser-kit:make-token
   :type type
   :text (subseq source start end)
   :value (or value (subseq source start end))
   :start start
   :end end))
(defun %process-search-whitespace-p (character)
  (find character '(#\Space #\Tab #\Newline #\Return #\Page)))
(defun %collapse-process-search-whitespace (string)
  (let ((words nil)
        (start nil))
    (loop for index from 0 below (length string)
          for character = (char string index)
          do (if (%process-search-whitespace-p character)
                 (when start
                   (push (subseq string start index) words)
                   (setf start nil))
                 (unless start
                   (setf start index))))
    (when start
      (push (subseq string start) words))
    (format nil "~{~A~^ ~}" (nreverse words))))
(defun %scan-process-search (source)
  (when (> (length source) cl-parser-kit:*maximum-tokenizer-source-length*)
    (error 'process-search-query-error
           :start-offset 0
           :end-offset (length source)
           :message "Process search query exceeds the parser source limit."))
  (let ((tokens nil)
        (index 0)
        (length (length source)))
    (labels ((emit (type start end &optional value)
               (push (%make-process-search-token source type start end value)
                     tokens))
             (delimiter-p (character)
               (find character '(#\= #\> #\< #\! #\( #\) #\"))))
      (loop while (< index length)
            for character = (char source index)
            do (cond
                 ((%process-search-whitespace-p character)
                  (incf index))
                 ((char= character #\")
                  (let ((start index)
                        (content-start (1+ index)))
                    (setf index content-start)
                    (loop while (and (< index length)
                                     (not (char= (char source index) #\")))
                          do (incf index))
                    (unless (< index length)
                      (error 'process-search-query-error
                             :start-offset start
                             :end-offset length
                             :message "Missing closing quotation."))
                    (emit :quoted start (1+ index)
                          (%collapse-process-search-whitespace
                           (subseq source content-start index)))
                    (incf index)))
                 ((char= character #\()
                  (emit :left-paren index (1+ index))
                  (incf index))
                 ((char= character #\))
                  (emit :right-paren index (1+ index))
                  (incf index))
                 ((and (char= character #\!)
                       (< (1+ index) length)
                       (char= (char source (1+ index)) #\=))
                  (emit :not-equal index (+ index 2))
                  (incf index 2))
                 ((delimiter-p character)
                  (emit (case character
                          (#\= :equal)
                          (#\> :greater)
                          (#\< :less)
                          (otherwise :bang))
                        index (1+ index))
                  (incf index))
                 (t
                  (let ((start index))
                    (loop while (and (< index length)
                                     (not (%process-search-whitespace-p
                                           (char source index)))
                                     (not (delimiter-p (char source index))))
                          do (incf index))
                    (let* ((word (subseq source start index))
                           (type (cond
                                   ((member word '("and" "&&") :test #'string-equal)
                                    :and)
                                   ((member word '("or" "||") :test #'string-equal)
                                    :or)
                                   (t :word))))
                      (emit type start index))))))
      (coerce (nreverse tokens) 'vector))))
(defstruct process-search-parser-state
  source
  spans
  problem)
(defun %signal-process-search-error (start end message)
  (error 'process-search-query-error
         :start-offset (max 0 start)
         :end-offset (max start end)
         :message message))
(defun %process-search-token (tokens position)
  (and (< position (length tokens))
       (aref tokens position)))
(defun %process-search-token-type (token)
  (and token (cl-parser-kit:token-type token)))
(defun %process-search-token-value (token)
  (and token (cl-parser-kit:token-value token)))
(defun %record-process-search-span (state node token)
  (let* ((start (cl-parser-kit:token-start token))
         (end (cl-parser-kit:token-end token))
         (quoted-p (eq :quoted (cl-parser-kit:token-type token))))
    (setf (gethash node (process-search-parser-state-spans state))
          (list start
                end
                (if quoted-p (1+ start) start)
                (if quoted-p (1- end) end)))
    node))
(defun %process-search-token-index-at (tokens offset)
  (or (loop for index from 0 below (length tokens)
            for token = (aref tokens index)
            for start = (cl-parser-kit:token-start token)
            for end = (cl-parser-kit:token-end token)
            when (and (<= start offset) (< offset end))
              return index)
      (length tokens)))
(defun %combine-process-search-nodes (kind nodes)
  (let ((children
          (loop for node in nodes
                append (if (eq kind (first node))
                           (rest node)
                           (list node)))))
    (if (null (rest children))
        (first children)
        (cons kind children))))
(defun %signal-process-search-token-error (state token message)
  (let ((length (length (process-search-parser-state-source state))))
    (if token
        (%signal-process-search-error
         (cl-parser-kit:token-start token)
         (cl-parser-kit:token-end token)
         message)
        (%signal-process-search-error length length message))))
(defun %process-search-parser-state (source)
  (make-process-search-parser-state
   :source source
   :spans (make-hash-table :test #'eq)))
(defparameter *process-search-maximum-depth* 512)
(defun %parse-process-search-sequence
    (state tokens position depth stop-at-close opening-token)
  (when (> depth *process-search-maximum-depth*)
    (%signal-process-search-token-error
     state (%process-search-token tokens position)
     "Process search query is nested too deeply."))
  (let ((nodes nil)
        (cursor position))
    (loop
      for token = (%process-search-token tokens cursor)
      do (cond
           ((null token)
            (when stop-at-close
              (%signal-process-search-token-error
               state nil "Missing closing parentheses."))
            (return))
           ((eq :right-paren (%process-search-token-type token))
            (if stop-at-close
                (return)
                (%signal-process-search-token-error
                 state token "Missing opening parentheses.")))
           (t
            (multiple-value-bind (node next)
                (%parse-process-search-or state tokens cursor depth)
              (push node nodes)
              (setf cursor next)))))
    (unless nodes
      (%signal-process-search-token-error
       state (or opening-token (%process-search-token tokens cursor))
       "No values within parentheses group."))
    (values (%combine-process-search-nodes :and (nreverse nodes))
            cursor)))
(defun %parse-process-search-or (state tokens position depth)
  (multiple-value-bind (first next)
      (%parse-process-search-and state tokens position depth)
    (let ((nodes (list first))
          (cursor next))
      (loop while (eq :or
                      (%process-search-token-type
                       (%process-search-token tokens cursor)))
            do (incf cursor)
               (when (or (null (%process-search-token tokens cursor))
                         (eq :right-paren
                             (%process-search-token-type
                              (%process-search-token tokens cursor))))
                 (%signal-process-search-token-error
                  state (%process-search-token tokens cursor)
                  "Missing expression after OR."))
               (multiple-value-bind (node after)
                   (%parse-process-search-and state tokens cursor depth)
                 (push node nodes)
                 (setf cursor after)))
      (values (%combine-process-search-nodes :or (nreverse nodes))
              cursor))))
(defun %parse-process-search-and (state tokens position depth)
  (multiple-value-bind (first next)
      (%parse-process-search-prefix state tokens position depth)
    (let ((nodes (list first))
          (cursor next))
      (loop while (eq :and
                      (%process-search-token-type
                       (%process-search-token tokens cursor)))
            do (incf cursor)
               (when (or (null (%process-search-token tokens cursor))
                         (eq :right-paren
                             (%process-search-token-type
                              (%process-search-token tokens cursor))))
                 (%signal-process-search-token-error
                  state (%process-search-token tokens cursor)
                  "Missing expression after AND."))
               (multiple-value-bind (node after)
                   (%parse-process-search-prefix state tokens cursor depth)
                 (push node nodes)
                 (setf cursor after)))
      (values (%combine-process-search-nodes :and (nreverse nodes))
              cursor))))
(defun %parse-process-search-prefix (state tokens position depth)
  (let ((token (%process-search-token tokens position)))
    (when (> depth *process-search-maximum-depth*)
      (%signal-process-search-token-error
       state token "Process search query is nested too deeply."))
    (when (null token)
      (%signal-process-search-token-error state nil "Missing expression."))
    (case (%process-search-token-type token)
      (:bang
       (let* ((next-position (1+ position))
              (next-token (%process-search-token tokens next-position))
              (next-type (%process-search-token-type next-token))
              (next-value (%process-search-token-value next-token)))
         (when (or (null next-token)
                   (member next-type
                           '(:equal :not-equal :less :greater :right-paren)
                           :test #'eq))
           (%signal-process-search-token-error
            state token "Negation must be followed by an expression."))
         (when (and (member next-type '(:word :and :or) :test #'eq)
                    (not (eq :name (%process-search-field next-value))))
           (%signal-process-search-token-error
            state token "Negation cannot be applied to a prefix keyword."))
         (multiple-value-bind (node next)
             (%parse-process-search-prefix
              state tokens next-position (1+ depth))
           (values (list :not node) next))))
      (:left-paren
       (multiple-value-bind (node next)
           (%parse-process-search-sequence
            state tokens (1+ position) (1+ depth) t token)
         (let ((closing (%process-search-token tokens next)))
           (unless (and closing
                        (eq :right-paren
                            (%process-search-token-type closing)))
             (%signal-process-search-token-error
              state nil "Missing closing parentheses."))
           (values node (1+ next)))))
      (otherwise
       (%parse-process-search-atom state tokens position)))))
(defun %make-process-search-match (state field text token)
  (let ((node (list :match field text)))
    (%record-process-search-span state node token)))
(defun %parse-process-search-atom (state tokens position)
  (let* ((token (%process-search-token tokens position))
         (type (%process-search-token-type token))
         (text (%process-search-token-value token)))
    (case type
      (:quoted
       (values (if (zerop (length text))
                   '(:true)
                   (%make-process-search-match state :name text token))
               (1+ position)))
      ((:word :and :or)
       (let ((field (%process-search-field text)))
         (cond
           ((eq :name field)
            (values (%make-process-search-match state :name text token)
                    (1+ position)))
           ((member field '(:pid :state :user) :test #'eq)
            (%parse-process-search-string-field
             state field token tokens (1+ position)))
           ((eq :elapsed-seconds field)
            (%parse-process-search-time-field state field token tokens (1+ position)))
           ((%process-search-numeric-field-p field)
            (%parse-process-search-numeric-field
             state field token tokens (1+ position)))
           (t
            (%signal-process-search-token-error
             state token "Invalid process search field.")))))
      (otherwise
       (%signal-process-search-token-error
        state token "Expected a process search expression.")))))
(defun %parse-process-search-string-field
    (state field field-token tokens position)
  (declare (ignore field-token))
  (multiple-value-bind (operator operator-length)
      (%process-search-comparison tokens position)
    (let* ((string-comparison-p
             (member operator '(:equal :not-equal) :test #'eq))
           (value-position (if string-comparison-p
                               (+ position operator-length)
                               position))
           (value-token (%process-search-token tokens value-position)))
      (when (or (null value-token)
                (eq :right-paren (%process-search-token-type value-token)))
        (%signal-process-search-token-error
         state nil "Missing value for process search field."))
      (when (eq :bang (%process-search-token-type value-token))
        (%signal-process-search-token-error
         state value-token "The exclamation mark is reserved in field values."))
      (let* ((value (%process-search-token-value value-token))
             (match (%make-process-search-match state field value value-token))
             (node (if (and string-comparison-p
                            (eq :not-equal operator))
                       (list :not match)
                       match)))
        (values node (1+ value-position))))))