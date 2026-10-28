(in-package #:ntop.e2e)

(defvar *route-frame-stability-seconds* 3.3d0)

(defvar *route-frame-capture-side* :unknown)

(define-condition route-frame-capture-timeout
  (e2e-timeout)
  ((side :initarg :side :reader route-frame-timeout-side)
   (stage :initarg :stage :reader route-frame-timeout-stage)
   (elapsed-seconds :initarg :elapsed-seconds
                    :reader route-frame-timeout-elapsed-seconds)
   (last-output-seconds :initarg :last-output-seconds
                        :reader route-frame-timeout-last-output-seconds)
   (stable-signature :initarg :stable-signature
                     :reader route-frame-timeout-stable-signature)
   (awaiting-post-query-output-p
    :initarg :awaiting-post-query-output-p
    :initform nil
    :reader route-frame-timeout-awaiting-post-query-output-p)
   (candidate-grid-p :initarg :candidate-grid-p
                     :initform nil
                     :reader route-frame-timeout-candidate-grid-p)
   (stable-check-at-seconds :initarg :stable-check-at-seconds
                            :initform nil
                            :reader route-frame-timeout-stable-check-at-seconds)
   (grid-unavailable-reason :initarg :grid-unavailable-reason
                            :initform nil
                            :reader route-frame-timeout-grid-unavailable-reason))
  (:report
   (lambda (condition stream)
     (format stream
             (concatenate 'string
                         "E2E route frame capture timed out. Side: ~s; stage: ~s; "
                         "elapsed: ~,3f seconds; last output: ~s; "
                         "last stable signature: ~s; awaiting post-query output: ~s; "
                         "candidate grid: ~s; stable check at: ~s; "
                         "grid unavailable: ~s.")
             (route-frame-timeout-side condition)
             (route-frame-timeout-stage condition)
             (route-frame-timeout-elapsed-seconds condition)
             (route-frame-timeout-last-output-seconds condition)
             (route-frame-timeout-stable-signature condition)
             (route-frame-timeout-awaiting-post-query-output-p condition)
             (route-frame-timeout-candidate-grid-p condition)
             (route-frame-timeout-stable-check-at-seconds condition)
             (route-frame-timeout-grid-unavailable-reason condition)))))

(declaim (ftype function %route-grid
                             %route-grid-stable-signature
                             %route-incomplete-escape-suffix-p
                             %route-incomplete-utf8-suffix-p
                             %route-capture-frame))

(defun %route-frame-capture-copy-octets (octets)
  (unless (typep octets '(vector (unsigned-byte 8)))
    (error "PREFIX must be a vector of unsigned bytes."))
  (make-array (length octets)
              :element-type '(unsigned-byte 8)
              :initial-contents octets))

(defun %route-frame-capture-current-grid (octets rows columns)
  (cond
    ((%route-incomplete-escape-suffix-p octets)
     (values nil :incomplete-escape-suffix))
    ((%route-incomplete-utf8-suffix-p octets)
     (values nil :incomplete-utf8-suffix))
    (t
     (let ((grid (%route-grid octets rows columns)))
       (if (and (= (grid-height grid) rows)
                (= (grid-width grid) columns))
           (values grid nil)
         (values nil
                 (list :dimension-mismatch
                       :expected (list rows columns)
                       :actual (list (grid-height grid)
                                     (grid-width grid)))))))))

(defun %route-frame-capture-signal-timeout
    (side stage started-at timeout-seconds session stable-signature
     &key awaiting-post-query-output-p candidate-grid-p stable-check-at-seconds
       grid-unavailable-reason)
  (let* ((now (%now))
         (last-output-at (e2e-session-last-output-at session)))
    (error 'route-frame-capture-timeout
           :operation "route frame capture"
           :timeout-seconds timeout-seconds
           :side side
           :stage stage
           :elapsed-seconds (%ticks-to-seconds (- now started-at))
           :last-output-seconds
           (and last-output-at (%elapsed-seconds session last-output-at))
           :stable-signature stable-signature
           :awaiting-post-query-output-p awaiting-post-query-output-p
           :candidate-grid-p candidate-grid-p
           :stable-check-at-seconds stable-check-at-seconds
           :grid-unavailable-reason grid-unavailable-reason)))

(defun %route-capture-frame
    (session prefix rows columns
             &key side stage timeout-seconds stability-seconds
             completion-predicate allow-unchanged-p)
  (let* ((started-at (%now))
         (side (or side *route-frame-capture-side* :unknown))
         (stage (or stage :capture))
         (stability-seconds
          (or stability-seconds *route-frame-stability-seconds*))
         (allow-unchanged-p (not (null allow-unchanged-p))))
    (%ensure-session-open session)
    (unless (and (integerp rows) (plusp rows))
      (error "ROWS must be a positive integer."))
    (unless (and (integerp columns) (plusp columns))
      (error "COLUMNS must be a positive integer."))
    (unless stability-seconds
      (error "*ROUTE-FRAME-STABILITY-SECONDS* must be set or passed explicitly."))
    (%validate-seconds stability-seconds "STABILITY-SECONDS")
    (when (and completion-predicate (not (functionp completion-predicate)))
      (error "COMPLETION-PREDICATE must be a function or NIL."))
    (let* ((deadline-values
            (handler-case
                (multiple-value-list
                 (%deadline-for-call session "route frame capture"
                                     timeout-seconds started-at))
              (e2e-timeout (condition)
                           (%route-frame-capture-signal-timeout
                            side stage started-at (e2e-timeout-seconds condition)
                            session nil))))
           (prefix (%route-frame-capture-copy-octets prefix))
           (prefix-length (length prefix))
           (output-gap-count
            (length (e2e-session-output-gaps-seconds session)))
           (cumulative
            (make-array prefix-length
                        :element-type '(unsigned-byte 8)
                        :initial-contents prefix
                        :adjustable t
                        :fill-pointer prefix-length))
           (buffer (make-array 4096 :element-type '(unsigned-byte 8)))
           (stability-ticks (%seconds-to-ticks stability-seconds))
           (candidate-grid nil)
           (candidate-signature nil)
           (candidate-canonical nil)
           (candidate-stable-since nil)
           (candidate-stability-tested-p nil)
           (last-stable-signature nil)
           (last-grid-unavailable-reason :empty-prefix))
      (destructuring-bind (deadline timeout-budget)
          deadline-values
        (labels
            ((predicate-p (grid)
                          (if completion-predicate
                              (let ((*route-frame-capture-current-octets* cumulative))
                                (not (null (funcall completion-predicate grid))))
                            t))
             (install-grid (grid now)
                           (multiple-value-bind (signature canonical)
                               (%route-grid-stable-signature grid)
                             (if (and candidate-canonical
                                      (equal canonical candidate-canonical))
                                 (setf candidate-grid grid
                                       candidate-signature signature)
                               (setf candidate-grid grid
                                     candidate-signature signature
                                     candidate-canonical canonical
                                     candidate-stable-since now))
                             (setf candidate-stability-tested-p nil)
                             (when completion-predicate
                               (predicate-p candidate-grid))))
             (clear-candidate ()
                              (setf candidate-grid nil
                                    candidate-signature nil
                                    candidate-canonical nil
                                    candidate-stable-since nil
                                    candidate-stability-tested-p nil))
             (compute-stable-check-at ()
                                      (and candidate-grid
                                           candidate-stable-since
                                           (not (e2e-session-awaiting-post-query-output-p session))
                                           (not candidate-stability-tested-p)
                                           (max (+ candidate-stable-since stability-ticks)
                                                (or *route-frame-capture-not-before-at*
                                                    (+ candidate-stable-since
                                                       stability-ticks)))))
             (output-gaps-since ()
                                (let* ((gaps (e2e-session-output-gaps-seconds session))
                                       (new-count (max 0 (- (length gaps) output-gap-count))))
                                  (reverse (subseq gaps 0 new-count))))
             (return-capture (now stable-check-at)
                             (values
                              (%route-frame-capture-copy-octets cumulative)
                              candidate-grid
                              (list :side side
                                    :stage stage
                                    :elapsed-seconds (%ticks-to-seconds (- now started-at))
                                    :last-output-seconds
                                    (let ((last-output-at
                                           (e2e-session-last-output-at session)))
                                      (and last-output-at
                                           (%elapsed-seconds session last-output-at)))
                                    :output-gaps-seconds (output-gaps-since)
                                    :stable-signature candidate-signature
                                    :awaiting-post-query-output-p
                                    (e2e-session-awaiting-post-query-output-p session)
                                    :candidate-grid-p (not (null candidate-grid))
                                    :stable-check-at-seconds
                                    (and stable-check-at
                                         (%elapsed-seconds session stable-check-at)))))
             (signal-timeout ()
                             (let ((stable-check-at (compute-stable-check-at)))
                               (%route-frame-capture-signal-timeout
                                side stage started-at timeout-budget session
                                last-stable-signature
                                :awaiting-post-query-output-p
                                (e2e-session-awaiting-post-query-output-p session)
                                :candidate-grid-p (not (null candidate-grid))
                                :stable-check-at-seconds
                                (and stable-check-at
                                     (%elapsed-seconds session stable-check-at))
                                :grid-unavailable-reason
                                last-grid-unavailable-reason))))
          (multiple-value-bind (prefix-grid unavailable-reason)
              (if (plusp prefix-length)
                  (%route-frame-capture-current-grid
                   cumulative rows columns)
                (values nil :empty-prefix))
            (setf last-grid-unavailable-reason unavailable-reason)
            (when prefix-grid
              (multiple-value-bind (signature canonical)
                  (%route-grid-stable-signature prefix-grid)
                (setf last-stable-signature signature)
                (when allow-unchanged-p
                  (setf candidate-grid prefix-grid
                        candidate-signature signature
                        candidate-canonical canonical
                        candidate-stable-since started-at)
                  (when completion-predicate
                    (predicate-p candidate-grid))))))
          (unless (e2e-session-cursor-query-scan-index session)
            (setf (e2e-session-cursor-query-scan-index session)
                  (max 0 (- prefix-length 3))))
          (handler-case
              (loop
               for now = (%now)
               do
               (when (>= now deadline)
                 (signal-timeout))
               (let* ((stable-check-at
                       (compute-stable-check-at)))
                 (when (and stable-check-at (>= now stable-check-at))
                   (setf last-stable-signature candidate-signature)
                   (if (predicate-p candidate-grid)
                       (return-from %route-capture-frame
                                    (return-capture now stable-check-at))
                     (setf candidate-stability-tested-p t)))
                 (let* ((wake-at
                         (if (and stable-check-at
                                  (not candidate-stability-tested-p)
                                  (< now stable-check-at))
                             (min deadline stable-check-at)
                           deadline))
                        (chunk
                         (%pty-read-chunk-before
                          session buffer wake-at deadline timeout-budget)))
                   (when chunk
                     (loop for byte across chunk
                           do (vector-push-extend byte cumulative))
                     (setf (e2e-session-awaiting-post-query-output-p session)
                           nil)
                     (let ((query-response-at nil))
                       (multiple-value-bind (next-index answered-p)
                           (%pty-answer-cursor-query
                            session cumulative
                            (e2e-session-cursor-query-scan-index session)
                            deadline)
                         ;; Start the stability window when the CPR response is written.
                         (setf (e2e-session-cursor-query-scan-index session)
                               next-index
                               (e2e-session-awaiting-post-query-output-p
                                session)
                               nil)
                         (when answered-p
                           (setf query-response-at (%now))))
                       (multiple-value-bind
                           (grid unavailable-reason)
                           (%route-frame-capture-current-grid
                            cumulative rows columns)
                         (setf last-grid-unavailable-reason
                               unavailable-reason)
                         ;; A partial PTY chunk has not changed the last complete grid.
                         (if grid
                             (progn
                               (install-grid grid (or query-response-at (%now)))
                               (when query-response-at
                                 (setf candidate-stable-since
                                       query-response-at)))
                           (if (and candidate-grid
                                    (or (eq unavailable-reason
                                           :incomplete-escape-suffix)
                                        (eq unavailable-reason
                                            :incomplete-utf8-suffix)))
                               (when query-response-at
                                 (setf candidate-stable-since
                                       query-response-at
                                       candidate-stability-tested-p nil))
                             (clear-candidate)))))))))
            (route-frame-capture-timeout (condition)
                                         (error condition))
            (e2e-timeout ()
                         (signal-timeout))))))))

(defun %route-grid (octets rows columns)
  (let ((grid (make-terminal-grid :rows rows :cols columns)))
    (parse-ansi (%octets-to-text octets)
                grid
                (lambda (value) value)
                (lambda (condition) (error condition)))))

(defun %route-grid-stable-signature (grid)
  (let* ((height (grid-height grid))
         (width (grid-width grid))
         (mask (make-array (list height width) :initial-element nil))
         (frames (criterion-projection-frames grid)))
    (labels
        ((canonical-color (color)
   (case (car color)
     (:default '(:default))
     (:indexed (list :indexed (cdr color)))
     (:rgb (list :rgb (coerce (cdr color) 'list)))))
 ;; Stability tracks frame structure; route predicates inspect the unmasked grid.
 ;; Process body rows therefore stay out of the signature.
 (mask-process-rows (frame header-row)
   (loop for row from (1+ header-row) below (second frame)
         unless (criterion-frame-divider-row-p
                 grid row (third frame) (fourth frame))
           do (loop for column from (1+ (third frame)) below (fourth frame)
                    do (setf (aref mask row column) t)))))
      (dotimes (row height)
        (let* ((text (criterion-row-text grid row))
               (span (criterion-cpu-title-border-span text row frames)))
          (when span
            (loop for column from (car span) below (cdr span)
                  for character =
                  (cell-character (grid-cell-at grid row column))
                  unless (criterion-border-cell-p
                          grid row column character frames)
                  do (setf (aref mask row column) t)))))
      (dolist (frame frames)
        (let ((header-row
               (loop for row from (1+ (first frame)) below (second frame)
                     when (criterion-process-header-row-p grid row (list frame))
                       return row)))
          (when header-row
            (mask-process-rows frame header-row))))
      (let ((canonical
             (loop for row below height
                   collect
                   (loop for column below width
                         for cell = (grid-cell-at grid row column)
                         collect
                         (if (aref mask row column)
                             :dynamic
                           (list (cell-character cell)
                                 (canonical-color (cell-foreground cell))
                                 (canonical-color (cell-background cell))
                                 (cell-bold-p cell)))))))
        (values (list :rows height
                      :columns width
                      :hash (sxhash canonical))
                canonical)))))

(defun %route-incomplete-escape-suffix-p (octets)
  (let* ((end (length octets))
         (escape-position (position #x1b octets :from-end t))
         (csi-position (position #x9b octets :from-end t)))
    (labels ((csi-state (start)
               (loop for index from start below end
                     for byte = (aref octets index)
                     do (cond
                          ((<= #x40 byte #x7e) (return :complete))
                          ((<= #x20 byte #x3f))
                          (t (return :invalid)))
                     finally (return :incomplete))))
      (or (and escape-position (= (1+ escape-position) end))
          (and escape-position
               (< (1+ escape-position) end)
               (= (aref octets (1+ escape-position)) #x5b)
               (eq (csi-state (+ escape-position 2)) :incomplete))
          (and csi-position
               (not (%route-utf8-continuation-byte-p octets csi-position))
               (or (null escape-position)
                   (> csi-position escape-position))
               (eq (csi-state (1+ csi-position)) :incomplete))))))

(defun %route-utf8-continuation-byte-p (octets position)
  (let ((end (length octets)))
    (loop for start from (max 0 (- position 3)) below position
          for lead = (aref octets start)
          for expected = (cond ((<= #xc2 lead #xdf) 2)
                               ((<= #xe0 lead #xef) 3)
                               ((<= #xf0 lead #xf4) 4))
          when (and expected
                    (< (- position start) expected)
                    (<= (+ start expected) end)
                    (loop for index from (1+ start) below (+ start expected)
                          for byte = (aref octets index)
                          always
                          (and (<= #x80 byte #xbf)
                               (or (/= index (1+ start))
                                   (cond ((= lead #xe0) (<= #xa0 byte #xbf))
                                         ((= lead #xed) (<= #x80 byte #x9f))
                                         ((= lead #xf0) (<= #x90 byte #xbf))
                                         ((= lead #xf4) (<= #x80 byte #x8f))
                                         (t t))))))
            do (return t)
          finally (return nil))))

(defun %route-incomplete-utf8-suffix-p (octets)
  (let ((end (length octets)))
    (loop for start from (max 0 (- end 3)) below end
          for lead = (aref octets start)
          for expected = (cond ((<= #xc2 lead #xdf) 2)
                               ((<= #xe0 lead #xef) 3)
                               ((<= #xf0 lead #xf4) 4))
          for available = (- end start)
          when (and expected
                    (< available expected)
                    (loop for index from (1+ start) below end
                          for byte = (aref octets index)
                          always
                          (and (<= #x80 byte #xbf)
                               (or (/= index (1+ start))
                                   (cond ((= lead #xe0) (<= #xa0 byte #xbf))
                                         ((= lead #xed) (<= #x80 byte #x9f))
                                         ((= lead #xf0) (<= #x90 byte #xbf))
                                         ((= lead #xf4) (<= #x80 byte #x8f))
                                         (t t))))))
            do (return t)
          finally (return nil))))
