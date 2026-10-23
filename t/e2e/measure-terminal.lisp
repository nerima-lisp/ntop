(in-package #:ntop.e2e)

(defparameter *measurement-n6-reply-octets* (make-array 9 :element-type '(unsigned-byte 8) :initial-contents '(#x1b #x5b #x3f #x32 #x35 #x3b #x31 #x24 #x79)))

(defparameter *measurement-n6-exit-key-octets* (make-array 1 :element-type '(unsigned-byte 8) :initial-element #x71))

(defparameter *n6-reply-status-table*
  '((0 . "1b 5b 3f 32 35 3b 30 24 79")
    (1 . "1b 5b 3f 32 35 3b 31 24 79")
    (2 . "1b 5b 3f 32 35 3b 32 24 79")
    (3 . "1b 5b 3f 32 35 3b 33 24 79")
    (4 . "1b 5b 3f 32 35 3b 34 24 79")))

(defun %btm-environment ()
  (cons "TERM=xterm-256color"
        (remove-if
         (lambda (entry)
           (and (>= (length entry) 5) (string= entry "TERM=" :end1 5)))
         (sb-ext:posix-environ))))

(defun %write-btm-config (directory run rate)
  (let ((path (merge-pathnames (format nil "btm-run-~d.toml" run) directory)))
    (%write-text path (btm-config-string rate))
    path))

(defun %launch-btm (btm config rows columns timeout)
  (launch-e2e-process btm
                      (list "-C" (namestring config))
                      rows
                      columns
                      :environment
                      (%btm-environment)
                      :timeout-seconds
                      timeout))

(defun %read-btm-frame (session rate timeout)
  (pty-read-until-idle session
                       :quiet-seconds
                       *measurement-frame-quiet-seconds*
                       :first-frame-not-before
                       (* *measurement-first-frame-factor* (/ rate 1000d0))
                       :timeout-seconds
                       timeout))

(defun %parse-grid (octets rows columns)
  (let ((grid (make-terminal-grid :rows rows :cols columns)))
    (parse-ansi (%octets-to-text octets)
                grid
                (lambda (value)
                  value)
                (lambda (condition)
                  (error condition)))))

(defun %grid-signature (grid)
  (list (grid-height grid)
        (grid-width grid)
        (loop for row below (grid-height grid)
              append (loop for column below (grid-width grid)
                           for cell = (grid-cell-at grid row column)
                           collect (list (cell-character cell)
                                         (cell-foreground cell)
                                         (cell-background cell)
                                         (cell-bold-p cell))))))

(defun %first-grid-and-output (session rows columns rate timeout)
  (multiple-value-bind (octets last-byte quiet-complete)
      (%read-btm-frame session rate timeout)
    (declare (ignore last-byte quiet-complete))
    (values (%parse-grid octets rows columns)
            (make-array (length octets)
                        :element-type
                        '(unsigned-byte 8)
                        :adjustable
                        t
                        :fill-pointer
                        0
                        :initial-contents
                        octets))))

(defun %run-n4-once (directory btm rows columns rate timeout run)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
        (multiple-value-bind (grid cumulative)
            (%first-grid-and-output session rows columns rate timeout)
          (let ((previous (%grid-signature grid)))
            (dotimes (index *measurement-interaction-samples*)
              (let* ((number (1+ index))
                     (sent-at
                      (pty-send-input session
                                      *measurement-key-octets*
                                      :timeout-seconds
                                      timeout)))
                (multiple-value-bind (octets last-byte quiet)
                    (pty-read-until-idle session
                                         :quiet-seconds
                                         *measurement-frame-quiet-seconds*
                                         :timeout-seconds
                                         timeout)
                  (%write-octets
                   (merge-pathnames
                    (format nil "n4-run-~d-sample-~2,'0d.ansi" run number)
                    directory)
                   octets)
                  (%append-octets cumulative octets)
                  (let ((current
                         (%grid-signature (%parse-grid cumulative rows columns))))
                    (unless
                        (and last-byte
                             (> last-byte sent-at)
                             (not (equalp previous current)))
                      (%measurement-unsupported
                       "N4 key sample ~d had no visible change; artifacts: ~a"
                       number
                       (namestring directory)))
                    (push
                     (list :sample
                           number
                           :key
                           #\?
                           :sent-at-seconds
                           sent-at
                           :last-byte-seconds
                           last-byte
                           :key-to-last-byte-seconds
                           (- last-byte sent-at)
                           :quiet-complete-seconds
                           quiet
                           :key-to-quiet-complete-seconds
                           (- quiet sent-at))
                     samples)
                    (setf previous current))))))
          (%write-samples directory "n4" run (nreverse samples)))
      (close-e2e-session session))))

(defun %run-n4 (directory btm rows columns rate runs duration timeout)
  (declare (ignore duration))
  (loop for run from 1 to runs
        do (%run-n4-once directory btm rows columns rate timeout run)))

(defun %run-n5-once (directory btm rows columns rate timeout run)
  (let* ((config (%write-btm-config directory run rate))
         (session (%launch-btm btm config rows columns timeout))
         (samples nil))
    (unwind-protect
        (multiple-value-bind (grid cumulative)
            (%first-grid-and-output session rows columns rate timeout)
          (declare (ignore grid))
          (dotimes (index *measurement-interaction-samples*)
            (let* ((number (1+ index))
                   (target-rows
                    (if (oddp number)
                        (1+ rows)
                        rows))
                   (target-columns
                    (if (oddp number)
                        (1+ columns)
                        columns))
                   (resized-at
                    (pty-resize-session session
                                        target-rows
                                        target-columns
                                        :timeout-seconds
                                        timeout)))
              (multiple-value-bind (octets last-byte quiet)
                  (pty-read-until-idle session
                                       :quiet-seconds
                                       *measurement-frame-quiet-seconds*
                                       :timeout-seconds
                                       timeout)
                (unless
                    (and last-byte
                         (> last-byte resized-at)
                         (plusp (length octets)))
                  (%measurement-unsupported
                   "N5 resize sample ~d had no post-resize bytes; artifacts: ~a"
                   number
                   (namestring directory)))
                (%write-octets
                 (merge-pathnames
                  (format nil "n5-run-~d-sample-~2,'0d.ansi" run number)
                  directory)
                 octets)
                (%append-octets cumulative octets)
                (%parse-grid cumulative target-rows target-columns)
                (push
                 (list :sample
                       number
                       :rows
                       target-rows
                       :columns
                       target-columns
                       :resize-at-seconds
                       resized-at
                       :last-byte-seconds
                       last-byte
                       :resize-to-last-byte-seconds
                       (- last-byte resized-at)
                       :quiet-complete-seconds
                       quiet
                       :resize-to-quiet-complete-seconds
                       (- quiet resized-at))
                 samples))))
          (%write-samples directory "n5" run (nreverse samples)))
      (close-e2e-session session))))

(defun %run-n5 (directory btm rows columns rate runs duration timeout)
  (declare (ignore duration))
  (loop for run from 1 to runs
        do (%run-n5-once directory btm rows columns rate timeout run)))

(defun %between-markers (text start-marker end-marker)
  (let* ((start (search start-marker text))
         (content-start (and start (+ start (length start-marker))))
         (end
          (and content-start (search end-marker text :start2 content-start))))
    (when (and content-start end)
      (string-trim '(#\Space #\Tab #\Return #\Newline)
                   (subseq text content-start end)))))

(defun %mode-parameters (text)
  (loop with start = 0
        for index from 0 to (length text)
        when (or (= index (length text)) (char= (char text index) #\;))
          collect (ignore-errors (parse-integer (subseq text start index)))
        and
        do (setf start (1+ index))))

(defun %private-mode-events (text)
  (loop with position = 0
        with events = nil
        while (< position (length text))
        for escape = (position #\Escape text :start position)
        while escape
        do (if (and (< (+ escape 2) (length text))
                    (char= (char text (1+ escape)) #\[))
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
        finally (return (nreverse events))))

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
  (loop for (status . hex-sequence) in *n6-reply-status-table*
        when (search hex-sequence text)
          return status))

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
  (pty-send-input session *measurement-n6-exit-key-octets*
                  :timeout-seconds timeout)
  (multiple-value-bind (output last-byte quiet-complete)
      (pty-read-until-idle session
                           :quiet-seconds *measurement-frame-quiet-seconds*
                           :timeout-seconds timeout)
    (declare (ignore last-byte quiet-complete))
    (%append-octets combined output))
  (let ((query-seen (search *measurement-n6-query*
                            (%octets-to-text combined))))
    (when query-seen
      (pty-send-input session *measurement-n6-reply-octets*
                      :timeout-seconds timeout)
      (multiple-value-bind (output last-byte quiet-complete)
          (pty-read-until-idle session
                               :quiet-seconds *measurement-frame-quiet-seconds*
                               :timeout-seconds timeout)
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
          (pty-send-input session *measurement-n6-exit-key-octets* :timeout-seconds timeout)
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
                 (reply (%n6-reply-status text))
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
                "N6 PTY supplied no DECRQM reply before the ~,3f second quiet interval; query was observed; artifact: ~a"
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
  (loop for run from 1 to runs
        do (%run-n6-once directory btm rows columns rate timeout run)))
