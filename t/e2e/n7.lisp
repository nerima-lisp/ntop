(in-package #:ntop.e2e)

(progn
  (defmacro define-n7-settings (&body settings)
    `(progn
       ,@(mapcar
          (lambda (setting)
            `(defconstant ,(first setting)
               ,(second setting)))
          settings)))
  (define-n7-settings (+n7-command-timeout-seconds+ 12d0)
                      (+n7-pattern-batch-size+ 16)
                      (+n7-total-timeout-seconds+ 180d0)))

(defconstant +n7-probe-output-limit+
  (* 1024 1024))

;; Exclude the scanner and its launch tools so N7 cannot match itself.
(defparameter +n7-probe-process-names+
  (list "grep"
        "ps"
        "ioreg"
        "sbcl"
        "bash"
        "sh"
        "zsh"
        "fish"
        "python3"
        "nix"
        "git"))

(defmacro define-n7-pattern-sources (name &body sources)
  (list (quote defparameter) name (list (quote quote) sources)))

(define-n7-pattern-sources *n7-pattern-sources*
                           :hostname
                           :username
                           :home-path
                           :working-directory
                           :root-path
                           :serial-number
                           :process-names)

(defun %value-after-key (text key)
  (let ((key-position (search key text)))
    (when key-position
      (let* ((assignment-position
              (position #\= text :start (+ key-position (length key))))
             (quote-position
              (and assignment-position
                   (position #\" text :start (1+ assignment-position))))
             (value-end
              (and quote-position
                   (position #\" text :start (1+ quote-position)))))
        (when (and quote-position value-end)
          (subseq text (1+ quote-position) value-end))))))

(defun %runtime-serial-number ()
  (when (search "Darwin" (software-type) :test (function char-equal))
    (let* ((result
            (host-kit:ensure-program-success
             (host-kit:run-program "ioreg"
                                   (quote
                                    ("-rd1" "-c" "IOPlatformExpertDevice"))
                                   :timeout
                                   +n7-command-timeout-seconds+
                                   :max-output-characters
                                   +n7-probe-output-limit+)))
           (output (host-kit:process-result-stdout result)))
      (when
          (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
        (error "N7 serial probe output exceeded its capture limit."))
      (or (%value-after-key output "IOPlatformSerialNumber")
          (error "N7 serial probe returned no serial number.")))))

(defun %process-line-patterns (line)
  (let* ((trimmed (string-trim (list #\Space #\Tab #\Return) line))
         (slash-position (position #\/ trimmed :from-end t))
         (name
          (subseq trimmed
                  (if slash-position
                      (1+ slash-position)
                      0))))
    (when
        (and (plusp (length name))
             (not
              (member name
                      +n7-probe-process-names+
                      :test
                      (function string-equal))))
      (if (string= trimmed name)
          (list name)
          (list trimmed name)))))

(defun %runtime-process-name-patterns ()
  (let* ((result
          (host-kit:ensure-program-success
           (host-kit:run-program "ps"
                                 (quote ("-axo" "comm="))
                                 :timeout
                                 +n7-command-timeout-seconds+
                                 :max-output-characters
                                 +n7-probe-output-limit+)))
         (output (host-kit:process-result-stdout result)))
    (when
        (or (host-kit:process-result-stdout-truncated-p result)
            (host-kit:process-result-stderr-truncated-p result))
      (error "N7 process probe output exceeded its capture limit."))
    (with-input-from-string (stream output)
      (loop for line = (read-line stream nil nil)
            while line
            append (%process-line-patterns line)))))

(defun %runtime-source-values (source root)
  (ecase source
    (:hostname (host-kit:hostname))
    (:username (host-kit:user-name))
    (:home-path (namestring (host-kit:user-home-directory)))
    (:working-directory (namestring (pathname (host-kit:getcwd))))
    (:root-path (namestring root))
    (:serial-number (%runtime-serial-number))
    (:process-names (%runtime-process-name-patterns))))

(defun %normalize-patterns (patterns)
  (dolist (pattern patterns)
    (check-type pattern string)
    (when (find #\Newline pattern)
      (error "N7 cannot scan a value containing a line break.")))
  (remove-duplicates
   (remove-if
    (lambda (pattern)
      (zerop (length pattern)))
    patterns)
   :test
   (function string=)
   :from-end
   t))

(defun %runtime-patterns (root)
  (let* ((values
          (loop for source in *n7-pattern-sources*
                append (let ((value (%runtime-source-values source root)))
                         (typecase value
                           (string (list value))
                           (list value)
                           (t
                            (error "N7 source returned an unsupported value."))))))
         (patterns (%normalize-patterns values)))
    (unless patterns
      (error "N7 has no runtime patterns to scan."))
    patterns))

(progn
  (defun %n7-pattern-batches (patterns)
    (loop for start from 0 below (length patterns) by +n7-pattern-batch-size+
          collect (subseq patterns
                          start
                          (min (length patterns)
                               (+ start +n7-pattern-batch-size+)))))
  (defun %n7-command-timeout (elapsed-seconds)
    (let ((remaining (- +n7-total-timeout-seconds+ elapsed-seconds)))
      (unless (plusp remaining)
        (error "N7 scan exceeded its ~,1f second limit."
               +n7-total-timeout-seconds+))
      (min +n7-command-timeout-seconds+ remaining)))
  (defun %search-patterns (root patterns
                                match-continuation
                                no-match-continuation)
    (let ((patterns (%normalize-patterns patterns)))
      (unless patterns
        (error "N7 requires at least one non-empty pattern."))
      (host-kit:call-with-temporary-directory
       (lambda (temporary-directory)
         (when
             (host-kit:pathname-within-p temporary-directory
                                         root
                                         :resolve-symlinks
                                         t)
           (error "N7 temporary storage overlaps the scan root."))
         (let ((pattern-file
                (merge-pathnames "patterns.txt" temporary-directory))
               (started-at (get-internal-real-time)))
           (labels ((search-next (batches)
                      (if (null batches)
                          (funcall no-match-continuation)
                          (progn
                            (host-kit:write-file-lines (first batches)
                                                       pattern-file)
                            (let* ((elapsed-seconds
                                    (/ (- (get-internal-real-time) started-at)
                                       internal-time-units-per-second))
                                   (result
                                    (host-kit:run-program "grep"
                                                          (list "-R"
                                                                "-F"
                                                                "-q"
                                                                "-f"
                                                                (namestring
                                                                 pattern-file)
                                                                "--"
                                                                (namestring
                                                                 root))
                                                          :timeout
                                                          (%n7-command-timeout
                                                           elapsed-seconds)
                                                          :max-output-characters
                                                          0))
                                   (exit-code
                                    (host-kit:process-result-exit-code result)))
                              (when
                                  (or
                                   (host-kit:process-result-stdout-truncated-p
                                    result)
                                   (host-kit:process-result-stderr-truncated-p
                                    result))
                                (error "N7 quiet grep produced output."))
                              (case exit-code
                                (0 (funcall match-continuation))
                                (1 (search-next (rest batches)))
                                (otherwise
                                 (host-kit:ensure-program-success result))))))))
             (search-next (%n7-pattern-batches patterns)))))
       :prefix
       "ntop-n7-"))))

progn

(defun %n7-ascii-alphanumeric-p (character)
  (and character
       (or (char<= #\0 character #\9)
           (char<= #\a (char-downcase character) #\z))))

(defun %n7-domain-character-p (character)
  (or (%n7-ascii-alphanumeric-p character)
      (member character '(#\- #\.) :test (function char=))))

(defun %n7-domain-boundary-character-p (character)
  (not
   (or (%n7-ascii-alphanumeric-p character)
       (member character '(#\_ #\- #\.) :test (function char=)))))

(defun %n7-valid-domain-label-p (line start end)
  (let ((length (- end start)))
    (and (<= 1 length 63)
         (%n7-ascii-alphanumeric-p (char line start))
         (%n7-ascii-alphanumeric-p (char line (1- end)))
         (loop for index from start below
               end
               for character = (char line index)
               always (or (%n7-ascii-alphanumeric-p character)
                          (char= character #\-))))))

(defun %n7-valid-local-hostname-p (line start end)
  (and (<= 1 (- end start) 253)
       (loop with label-start = start
             for separator = (position #\. line :start label-start :end end)
             for label-end = (or separator end)
             do (unless (%n7-valid-domain-label-p line label-start label-end)
                  (return nil)) (if separator
                                    (setf label-start (1+ separator))
                                    (return
                                     (string-equal
                                      (subseq line label-start label-end)
                                      "local"))))))

(defun %n7-nix-identifier-start-p (character)
  (or (and character (char<= #\a (char-downcase character) #\z))
      (eql character #\_)))

(defun %n7-nix-identifier-character-p (character)
  (or (%n7-ascii-alphanumeric-p character)
      (member character '(#\_ #\' #\-) :test (function char=))))

(defun %n7-nix-attribute-path-p (line start end)
  (and (< start end)
       (%n7-nix-identifier-start-p (char line start))
       (loop for index from (1+ start) below
             end
             for character = (char line index)
             always (if (char= character #\.)
                        (and (< (1+ index) end)
                             (%n7-nix-identifier-start-p (char line (1+ index))))
                        (%n7-nix-identifier-character-p character)))))

(defun %n7-inside-double-quoted-string-p (line position)
  (loop with quoted-p = nil
        with escaped-p = nil
        for index below position
        for character = (char line index)
        do (cond
             (quoted-p
              (cond
                (escaped-p
                 (setf escaped-p nil))
                ((char= character #\\)
                 (setf escaped-p t))
                ((char= character #\")
                 (setf quoted-p nil))))
             ((char= character #\")
              (setf quoted-p t)))
        finally (return quoted-p)))

(defun %n7-nix-attribute-reference-p (line start end)
  (let ((before
         (when (plusp start)
           (char line (1- start)))))
    (and (not (%n7-inside-double-quoted-string-p line start))
         (%n7-nix-attribute-path-p line start end)
         (or (null before)
             (member before '(#\Space #\Tab #\{ #\; #\,) :test (function char=)))
         (loop with cursor = end
               for next = (position-if-not
                           (lambda (character)
                             (member character
                                     '(#\Space #\Tab #\Return)
                                     :test
                                     (function char=)))
                           line
                           :start
                           cursor)
               while next
               for character = (char line next)
               do (cond
                    ((char= character #\=)
                     (return t))
                    ((char= character #\.)
                     (let* ((identifier-start (1+ next))
                            (identifier-end
                              (position-if-not
                               (function %n7-nix-identifier-character-p)
                               line
                               :start
                               identifier-start)))
                       (unless (and (< identifier-start (length line))
                                    (%n7-nix-identifier-start-p
                                     (char line identifier-start)))
                         (return nil))
                       (setf cursor (or identifier-end (length line)))))
                    (t
                     (return nil)))
               finally (return nil)))))

(defun %n7-local-hostname-p (line)
  (loop with search-start = 0
        for suffix-start = (search ".local"
                                   line
                                   :start2
                                   search-start
                                   :test
                                   (function char-equal))
        while suffix-start
        for hostname-end = (+ suffix-start (length ".local"))
        for hostname-start = (loop with start = suffix-start
                                   while (and (plusp start)
                                              (%n7-domain-character-p
                                               (char line (1- start))))
                                   do (decf start)
                                   finally (return start))
        do (when
               (and
                (%n7-valid-local-hostname-p line hostname-start hostname-end)
                (or (zerop hostname-start)
                    (%n7-domain-boundary-character-p
                     (char line (1- hostname-start))))
                (or (= hostname-end (length line))
                    (%n7-domain-boundary-character-p (char line hostname-end)))
                (not
                 (%n7-nix-attribute-reference-p line
                                                hostname-start
                                                hostname-end)))
             (return t)) (setf search-start (1+ suffix-start))
        finally (return nil)))

(defun %n7-user-path-p (line)
  (loop with search-start = 0
        for marker = (search "/Users/"
                             line
                             :start2
                             search-start
                             :test
                             (function char-equal))
        while marker
        for name-start = (+ marker (length "/Users/"))
        for name-end = (position #\/ line :start name-start)
        do (when (and name-end (> name-end name-start))
             (return t)) (setf search-start (1+ marker))
        finally (return nil)))

(defun %n7-generic-line-p (line)
  (or (%n7-user-path-p line) (%n7-local-hostname-p line)))

(defun %n7-source-files (directory)
  (append (uiop:directory-files directory)
          (loop for subdirectory in (uiop:subdirectories directory)
                append (%n7-source-files subdirectory))))

(defun %n7-file-matches-generic-pattern-p (pathname)
  (with-open-file
      (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (let ((line
           (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (labels ((line-matches-p ()
                 (%n7-generic-line-p (coerce line 'string))))
        (loop for byte = (read-byte stream nil nil)
              while byte
              do (if (= byte 10)
                     (progn
                       (when (line-matches-p)
                         (return-from %n7-file-matches-generic-pattern-p
                           t))
                       (setf (fill-pointer line) 0))
                     (vector-push-extend (or (code-char byte) #\?) line)))
        (line-matches-p)))))

(defun %n7-generic-content-match-p (root)
  (let ((files (%n7-source-files root)))
    (unless files
      (error "N7 scan root contains no source files."))
    (some (function %n7-file-matches-generic-pattern-p) files)))

(progn
  (defun %n7-scan-root (root patterns)
    (%search-patterns root
                      patterns
                      (lambda ()
                        (error "N7 detected machine-derived content."))
                      (lambda ()
                        0))
    (when (%n7-generic-content-match-p root)
      (error "N7 detected machine-derived content."))
    0)
  (defun run-n7 (&key root)
    (unless root
      (error "N7 requires a scan root."))
    (let ((root-path
           (host-kit:ensure-directory-pathname
            (host-kit:ensure-absolute-pathname root (host-kit:getcwd)))))
      (unless (host-kit:directory-exists-p root-path)
        (error "N7 scan root is not a directory."))
      (%n7-scan-root root-path (%runtime-patterns root-path)))))
