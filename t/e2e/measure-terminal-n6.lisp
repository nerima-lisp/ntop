(in-package #:ntop.e2e)

(defun %between-markers (text start-marker end-marker)
  (let* ((start (search start-marker text))
         (content-start (and start (+ start (length start-marker))))
         (end
          (and content-start (search end-marker text :start2 content-start))))
    (when (and content-start end)
      (string-trim '(#\Space #\Tab #\Return #\Newline)
                   (subseq text content-start end)))))

(defun %mode-parameters (text)
  (loop with
    start
    =
    0
    for
    index
    from
    0
    to
    (length text)
    when
    (or (= index (length text)) (char= (char text index) #\;))
    collect
    (ignore-errors (parse-integer (subseq text start index)))
    and
    do
    (setf start (1+ index))))

(defun %private-mode-events (text)
  (loop with
    position
    =
    0
    with
    events
    =
    nil
    while
    (< position (length text))
    for
    escape
    =
    (position #\Escape text :start position)
    while
    escape
    do
    (if (and (< (+ escape 2) (length text)) (char= (char text (1+ escape)) #\[))
        (multiple-value-bind (raw final next)
            (parse-csi-sequence text (+ escape 2))
          (when
              (and (plusp (length raw))
                   (char= (char raw 0) #\?)
                   (member final '(#\h #\l)))
            (dolist (mode (%mode-parameters (subseq raw 1)))
              (when mode
                (push
                 (cons mode
                       (if (char= final #\h)
                           :set
                           :reset))
                 events))))
          (setf position next))
        (setf position (1+ escape)))
    finally
    (return (nreverse events))))

(defun %final-private-mode-states (events)
  (let ((states nil))
    (dolist (event events)
      (setf states (acons (car event)
                          (cdr event)
                          (remove (car event) states :key #'car))))
    states))

(defun %private-mode-reset-seen-p (events)
  (some
   (lambda (event)
     (eq (cdr event) :reset))
   events))

(defun %n6-reply-status (text)
  (labels ((whitespace-p (character)
             (find character '(#\Space #\Tab #\Return #\Newline #\Page)))
           (octet-value (start end)
             (when (= (- end start) 2)
               (let ((high (digit-char-p (char text start) 16))
                     (low (digit-char-p (char text (1+ start)) 16)))
                 (when (and high low)
                   (+ (* high 16) low)))))
           (scan (position octets on-success on-failure)
             (let ((start (position-if-not #'whitespace-p text :start position)))
               (if (null start)
                   (if octets
                       (funcall on-success
                                (coerce (nreverse octets)
                                        '(vector (unsigned-byte 8))))
                       (funcall on-failure))
                   (let* ((token-end (position-if #'whitespace-p text :start start))
                          (end (or token-end (length text)))
                          (octet (octet-value start end)))
                     (if octet
                         (scan end (cons octet octets) on-success on-failure)
                         (funcall on-failure)))))))
    (scan
     0
     nil
     (lambda (octets)
       (when (and (= (length octets) 9)
                  (equalp (subseq octets 0 6)
                          #(#x1b #x5b #x3f #x32 #x35 #x3b))
                  (equalp (subseq octets 7)
                          #(#x24 #x79)))
         (cdr (assoc (aref octets 6) *n6-reply-status-table* :test #'=))))
     (lambda ()
       nil))))

(defun %n6-script ()
  (format nil
          (concatenate 'string
                       "printf '%s\\n' '__NTOP_N6_BEFORE_BEGIN__'~%"
                       "stty -a || exit 91~%"
                       "printf '%s\\n' '__NTOP_N6_BEFORE_END__'~%"
                       "\"$1\" -C \"$2\"~%"
                       "btm_status=$?~%"
                       "printf '%s%s\\n' '__NTOP_N6_BTM_EXIT__' \"$btm_status\"~%"
                       "printf '%s\\n' '__NTOP_N6_AFTER_BEGIN__'~%"
                       "stty -a || exit 92~%"
                       "printf '%s\\n' '__NTOP_N6_AFTER_END__'~%"
                       "stty raw -echo || exit 93~%"
                       "printf '\\033[?25$p'~%"
                       "reply=$(dd bs=1 count=9 2>/dev/null | od -An -tx1)~%"
                       "printf '%s%s\\n' '__NTOP_N6_QUERY_REPLY__' \"$reply\"~%"
                       "exec /bin/sleep 3600~%")))

(defun %collect-n6-query-exchange (session combined timeout)
  (pty-send-input session
                  *measurement-n6-exit-key-octets*
                  :timeout-seconds
                  timeout)
  (multiple-value-bind (output last-byte quiet-complete)
      (pty-read-until-idle session
                           :quiet-seconds
                           *measurement-frame-quiet-seconds*
                           :timeout-seconds
                           timeout)
    (declare (ignore last-byte quiet-complete))
    (%append-octets combined output))
  (let ((query-seen (search *measurement-n6-query* (%octets-to-text combined))))
    (when query-seen
      (pty-send-input session
                      *measurement-n6-reply-octets*
                      :timeout-seconds
                      timeout)
      (multiple-value-bind (output last-byte quiet-complete)
          (pty-read-until-idle session
                               :quiet-seconds
                               *measurement-frame-quiet-seconds*
                               :timeout-seconds
                               timeout)
        (declare (ignore last-byte quiet-complete))
        (%append-octets combined output)))
    query-seen))

(defun %run-n6-once (directory btm rows columns rate timeout run)
  (let* ((config (%write-btm-config directory run rate))
         (session
          (launch-e2e-process "/bin/sh"
                              (list "-c"
                                    (%n6-script)
                                    "ntop-e2e-n6"
                                    btm
                                    (namestring config))
                              rows
                              columns
                              :environment
                              (%btm-environment)
                              :timeout-seconds
                              timeout))
         (combined
          (make-array 0
                      :element-type
                      (list (find-symbol "UNSIGNED-BYTE" "COMMON-LISP") 8)
                      :adjustable
                      t
                      :fill-pointer
                      0)))
    (unwind-protect
        (progn
          (multiple-value-bind (initial last-byte quiet)
              (%read-btm-frame session rate timeout)
            (declare (ignore last-byte quiet))
            (%append-octets combined initial))
          (%collect-n6-query-exchange session combined timeout)
          (let* ((raw-path
                  (merge-pathnames (format nil "n6-run-~d.ansi" run) directory))
                 (text (%octets-to-text combined))
                 (before
                  (%between-markers text
                                    "__NTOP_N6_BEFORE_BEGIN__"
                                    "__NTOP_N6_BEFORE_END__"))
                 (after
                  (%between-markers text
                                    "__NTOP_N6_AFTER_BEGIN__"
                                    "__NTOP_N6_AFTER_END__"))
                 (exit-position (search "__NTOP_N6_BTM_EXIT__" text))
                 (exit-code
                  (when exit-position
                    (let* ((start
                            (+ exit-position (length "__NTOP_N6_BTM_EXIT__")))
                           (end (position #\Newline text :start start)))
                      (ignore-errors
                       (parse-integer
                        (string-trim (list #\Space #\Return)
                                     (subseq text start end)))))))
                 (events (%private-mode-events text))
                 (reply
                  (let ((reply-text
                         (%between-markers text
                                           "__NTOP_N6_QUERY_REPLY__"
                                           (string #\Newline))))
                    (and reply-text
                         (%n6-reply-status reply-text))))
                 (query-seen (search *measurement-n6-query* text))
                 (resets-p (%private-mode-reset-seen-p events))
                 (record
                  (list :stty-before
                        before
                        :stty-after
                        after
                        :stty-equal
                        (and before after (string= before after))
                        :btm-exit-code
                        exit-code
                        :private-mode-events
                        events
                        :private-mode-final-states
                        (%final-private-mode-states events)
                        :private-mode-reset-sequence-seen
                        resets-p
                        :terminal-mode-query
                        *measurement-n6-query*
                        :terminal-mode-query-seen
                        query-seen
                        :terminal-mode-reply-status
                        reply
                        :terminal-query-provider
                        :harness)))
            (%write-octets raw-path combined)
            (%write-samples directory "n6" run (list record))
            (unless (and before after)
              (%measurement-fail
               "N6 could not capture both stty snapshots; artifact: ~a"
               (namestring raw-path)))
            (unless (and exit-code (zerop exit-code))
              (%measurement-fail
               "N6 btm exited unsuccessfully (~s); artifact: ~a"
               exit-code
               (namestring raw-path)))
            (unless (string= before after)
              (%measurement-fail
               "N6 stty state changed across btm; artifact: ~a"
               (namestring raw-path)))
            (unless query-seen
              (%measurement-unsupported
               "N6 terminal mode query was not observed; artifact: ~a"
               (namestring raw-path)))
            (cond
             ((null reply)
              (%measurement-unsupported
               (concatenate 'string
                            "N6 PTY supplied no DECRQM reply "
                            "before the ~,3f second quiet interval; "
                            "query was observed; artifact: ~a")
               *measurement-frame-quiet-seconds*
               (namestring raw-path)))
             ((zerop reply)
              (%measurement-unsupported
               "N6 PTY replied with DECRQM status 0 (mode unrecognized); artifact: ~a"
               (namestring raw-path))))
            (unless resets-p
              (%measurement-unsupported
               "N6 btm emitted no DEC private-mode reset sequence; events ~s; artifact: ~a"
               events
               (namestring raw-path)))
            (%write-samples directory
                            "n6-query"
                            run
                            (list
                             (list :query
                                   *measurement-n6-query*
                                   :reply-status
                                   reply
                                   :provider
                                   :harness
                                   :artifact
                                   raw-path)))))
      (close-e2e-session session))))

(defun %run-n6 (directory btm rows columns rate runs duration timeout)
  (declare (ignore duration))
  (loop for
    run
    from
    1
    to
    runs
    do
    (%run-n6-once directory btm rows columns rate timeout run)))
