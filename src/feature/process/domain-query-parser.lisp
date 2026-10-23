(in-package #:ntop.process.domain)
(defun %process-search-ascii-digit-p (character)
  (and (char<= #\0 character) (char<= character #\9)))
(defun %parse-process-search-decimal (state token)
  (let* ((text (%process-search-token-value token))
         (length (length text))
         (cursor 0))
    (when (> length 128)
      (%signal-process-search-token-error
       state token "Numeric value exceeds the supported length."))
    (when (and (> length 0) (find (char text 0) "+-"))
      (incf cursor))
    (let* ((exponent-marker
             (position-if (lambda (character) (find character "eE"))
                          text :start cursor))
           (mantissa-end (or exponent-marker length))
           (decimal-point (position #\. text :start cursor :end mantissa-end))
           (integer-end (or decimal-point mantissa-end))
           (fraction-start (if decimal-point (1+ decimal-point) mantissa-end))
           (integer-count (- integer-end cursor))
           (fraction-count (- mantissa-end fraction-start))
           (exponent-text (and exponent-marker (subseq text (1+ exponent-marker))))
           (exponent-digits
             (and exponent-text
                  (if (and (> (length exponent-text) 0)
                           (find (char exponent-text 0) "+-"))
                      (subseq exponent-text 1)
                      exponent-text)))
           (digits (concatenate 'string
                                (subseq text cursor integer-end)
                                (subseq text fraction-start mantissa-end))))
      (unless (and (or (> integer-count 0) (> fraction-count 0))
                   (or (= integer-count 0)
                       (every #'%process-search-ascii-digit-p
                              (subseq text cursor integer-end)))
                   (or (= fraction-count 0)
                       (every #'%process-search-ascii-digit-p
                              (subseq text fraction-start mantissa-end)))
                   (or (null decimal-point)
                       (null (position #\. text
                                       :start (1+ decimal-point)
                                       :end mantissa-end)))
                   (or (null exponent-marker)
                       (and (> (length exponent-digits) 0)
                            (every #'%process-search-ascii-digit-p
                                   exponent-digits)
                            (null (position-if
                                   (lambda (character) (find character "eE"))
                                   text :start (1+ exponent-marker))))))
        (%signal-process-search-token-error state token "Invalid numeric value."))
      (let* ((significand (parse-integer digits))
             (exponent (if exponent-text (parse-integer exponent-text) 0)))
        (when (or (< exponent -324) (> exponent 308))
          (%signal-process-search-token-error
           state token "Numeric exponent is outside the supported range."))
        (let* ((base (/ significand (expt 10 fraction-count)))
               (scaled (if (minusp exponent)
                           (/ base (expt 10 (- exponent)))
                           (* base (expt 10 exponent))))
               (signed (if (and (> (length text) 0)
                                (char= (char text 0) #\-))
                           (- scaled)
                           scaled)))
          (handler-case
              (let ((value (coerce signed 'double-float)))
                (if (and (= value value)
                         (<= (abs value) most-positive-double-float))
                    value
                    (%signal-process-search-token-error
                     state token "Numeric value is not finite.")))
            (floating-point-overflow ()
              (%signal-process-search-token-error
               state token "Numeric value is not finite."))
            (floating-point-underflow () 0d0)))))))

(defparameter *process-search-byte-units*
  '(("b" . 1d0)
    ("kb" . 1d3)
    ("kib" . 1024d0)
    ("mb" . 1d6)
    ("mib" . 1048576d0)
    ("gb" . 1d9)
    ("gib" . 1073741824d0)
    ("tb" . 1d12)
    ("tib" . 1099511627776d0)))
(defun %process-search-byte-unit (token)
  (and token
       (member (%process-search-token-type token) '(:word) :test #'eq)
       (cdr (assoc (%process-search-token-value token)
                   *process-search-byte-units*
                   :test #'string-equal))))
(defun %parse-process-search-numeric-field
    (state field field-token tokens position)
  (multiple-value-bind (operator operator-length)
      (%process-search-comparison tokens position)
    (unless operator
      (%signal-process-search-token-error
       state field-token "A comparison operator is required."))
    (let* ((value-position (+ position operator-length))
           (value-token (%process-search-token tokens value-position)))
      (when (or (null value-token)
                (member (%process-search-token-type value-token)
                        '(:and :or :right-paren :left-paren)
                        :test #'eq))
        (%signal-process-search-token-error
         state value-token "Missing numeric value for process search field."))
      (unless (eq :word (%process-search-token-type value-token))
        (%signal-process-search-token-error
         state value-token "Invalid numeric value."))
      (let ((value (%parse-process-search-decimal state value-token))
            (next-position (1+ value-position)))
        (when (member field
                      '(:memory-bytes :read-per-second :write-per-second
                        :total-read :total-write :gpu-memory-bytes)
                      :test #'eq)
          (let* ((unit-token (%process-search-token tokens next-position))
                 (multiplier (%process-search-byte-unit unit-token)))
            (when multiplier
              (setf value (* value multiplier))
              (incf next-position))))
        (values (list :compare field operator value) next-position)))))
(defparameter *process-search-duration-units*
  '(("ns" . 1d-9)
    ("nsec" . 1d-9)
    ("nsecs" . 1d-9)
    ("nanosecond" . 1d-9)
    ("nanoseconds" . 1d-9)
    ("us" . 1d-6)
    ("usec" . 1d-6)
    ("usecs" . 1d-6)
    ("microsecond" . 1d-6)
    ("microseconds" . 1d-6)
    ("ms" . 1d-3)
    ("msec" . 1d-3)
    ("msecs" . 1d-3)
    ("millisecond" . 1d-3)
    ("milliseconds" . 1d-3)
    ("s" . 1d0)
    ("sec" . 1d0)
    ("secs" . 1d0)
    ("second" . 1d0)
    ("seconds" . 1d0)
    ("m" . 60d0)
    ("min" . 60d0)
    ("mins" . 60d0)
    ("minute" . 60d0)
    ("minutes" . 60d0)
    ("h" . 3600d0)
    ("hr" . 3600d0)
    ("hrs" . 3600d0)
    ("hour" . 3600d0)
    ("hours" . 3600d0)
    ("d" . 86400d0)
    ("day" . 86400d0)
    ("days" . 86400d0)
    ("w" . 604800d0)
    ("week" . 604800d0)
    ("weeks" . 604800d0)))
(defun %parse-process-search-duration (state token)
  (let* ((text (%process-search-token-value token))
         (length (length text))
         (cursor 0)
         (total 0d0)
         (segments 0))
    (when (> length 128)
      (%signal-process-search-token-error
       state token "Duration value exceeds the supported length."))
    (loop while (< cursor length)
          do (let ((number-start cursor))
               (loop while (and (< cursor length)
                                (%process-search-ascii-digit-p
                                 (char text cursor)))
                     do (incf cursor))
               (when (and (< cursor length)
                          (char= (char text cursor) #\.))
                 (incf cursor)
                 (loop while (and (< cursor length)
                                  (%process-search-ascii-digit-p
                                   (char text cursor)))
                       do (incf cursor)))
               (when (= cursor number-start)
                 (%signal-process-search-token-error
                  state token "Invalid duration value."))
               (let* ((number-end cursor)
                      (unit-start cursor))
                 (loop while (and (< cursor length)
                                  (or (char<= #\a (char text cursor) #\z)
                                      (char<= #\A (char text cursor) #\Z)))
                       do (incf cursor))
                 (when (= cursor unit-start)
                   (%signal-process-search-token-error
                    state token "Duration unit is required."))
                 (let* ((number-text (subseq text number-start number-end))
                        (number-token
                          (cl-parser-kit:make-token
                           :type :word
                           :text number-text
                           :value number-text
                           :start (+ (cl-parser-kit:token-start token)
                                     number-start)
                           :end (+ (cl-parser-kit:token-start token)
                                   number-end)))
                        (number (%parse-process-search-decimal state number-token))
                        (unit (subseq text unit-start cursor))
                        (multiplier
                          (cdr (assoc unit *process-search-duration-units*
                                      :test #'string-equal))))
                   (unless multiplier
                     (%signal-process-search-token-error
                      state token "Unknown duration unit."))
                   (incf total (* number multiplier))
                   (incf segments)
                   (when (> total 18446744073.709551615d0)
                     (%signal-process-search-token-error
                      state token "Duration exceeds the supported range."))))))
    (when (zerop segments)
      (%signal-process-search-token-error state token "Invalid duration value."))
    total))
(defun %make-process-search-parser (state)
  (cl-parser-kit:make-parser
   :name :process-search
   :fn (lambda (tokens position)
         (handler-case
             (multiple-value-bind (ast next)
                 (%parse-process-search-sequence state tokens position 0 nil nil)
               (values t ast next nil))
           (process-search-query-error (condition)
             (setf (process-search-parser-state-problem state) condition)
             (let* ((offset
                      (process-search-query-error-start-offset condition))
                    (token-index
                      (%process-search-token-index-at tokens offset))
                    (token (%process-search-token tokens token-index)))
               (values
                nil
                nil
                token-index
                (cl-parser-kit:make-parse-failure
                 :position token-index
                 :expected :process-search-expression
                 :actual (and token
                              (cl-parser-kit:token-type token))))))))))
(defun parse-process-search (query)
  (let* ((tokens (%scan-process-search query))
         (state (%process-search-parser-state query)))
    (multiple-value-bind (success-p ast position failure)
        (cl-parser-kit:parse-all
         (%make-process-search-parser state)
         tokens)
      (if success-p
          (values ast (process-search-parser-state-spans state))
          (let ((problem (process-search-parser-state-problem state)))
            (if problem
                (error problem)
                (let* ((failure-position
                         (if failure
                             (cl-parser-kit:parse-failure-position failure)
                             position))
                       (token (%process-search-token tokens failure-position))
                       (start (if token
                                  (cl-parser-kit:token-start token)
                                  (length query)))
                       (end (if token
                                (cl-parser-kit:token-end token)
                                (length query))))
                  (%signal-process-search-error
                   start end "Invalid process search query."))))))))
(defun %parse-process-search-time-field
    (state field field-token tokens position)
  (multiple-value-bind (operator operator-length)
      (%process-search-comparison tokens position)
    (unless operator
      (%signal-process-search-token-error
       state field-token "A comparison operator is required."))
    (let* ((value-position (+ position operator-length))
           (value-token (%process-search-token tokens value-position)))
      (when (or (null value-token)
                (member (%process-search-token-type value-token)
                        '(:and :or :right-paren :left-paren)
                        :test #'eq))
        (%signal-process-search-token-error
         state value-token "Missing duration value for process search field."))
      (unless (eq :word (%process-search-token-type value-token))
        (%signal-process-search-token-error
         state value-token "Invalid duration value."))
      (values (list :compare field operator
                    (%parse-process-search-duration state value-token))
              (1+ value-position)))))
