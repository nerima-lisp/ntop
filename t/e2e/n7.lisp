(in-package #:ntop.e2e)

(progn
  (defmacro define-n7-settings (&body settings)
    `(progn
       ,@(mapcar (lambda (setting)
                   `(defconstant ,(first setting) ,(second setting)))
                 settings)))
  (define-n7-settings
    (+n7-command-timeout-seconds+ 12d0)
    (+n7-pattern-batch-size+ 16)
    (+n7-total-timeout-seconds+ 180d0)))

(defconstant +n7-probe-output-limit+
  (* 1024 1024))

(defparameter +n7-probe-process-names+
  (list "grep" "ps" "ioreg"))

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
  (unless (search "Darwin" (software-type) :test (function char-equal))
    (error "N7 serial discovery requires a Darwin host."))
  (let* ((result
          (host-kit:ensure-program-success
           (host-kit:run-program "ioreg"
                                 (quote ("-rd1" "-c" "IOPlatformExpertDevice"))
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
        (error "N7 serial probe returned no serial number."))))

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
  (defun %search-patterns (root patterns match-continuation no-match-continuation)
    (let ((patterns (%normalize-patterns patterns)))
      (unless patterns
        (error "N7 requires at least one non-empty pattern."))
      (host-kit:call-with-temporary-directory
       (lambda (temporary-directory)
         (when (host-kit:pathname-within-p temporary-directory
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
                                     (host-kit:run-program
                                      "grep"
                                      (list "-R"
                                            "-F"
                                            "-q"
                                            "-f"
                                            (namestring pattern-file)
                                            "--"
                                            (namestring root))
                                      :timeout
                                      (%n7-command-timeout elapsed-seconds)
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

(defun run-n7 (&key root)
  (unless root
    (error "N7 requires a scan root."))
  (let ((root-path
          (host-kit:ensure-directory-pathname
           (host-kit:ensure-absolute-pathname root (host-kit:getcwd)))))
    (unless (host-kit:directory-exists-p root-path)
      (error "N7 scan root is not a directory."))
    (let ((patterns (%runtime-patterns root-path)))
      (%search-patterns root-path
                        patterns
                        (lambda ()
                          (error "N7 detected machine-derived content."))
                        (lambda () 0)))))
