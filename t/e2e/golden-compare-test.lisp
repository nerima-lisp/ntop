(in-package #:ntop.e2e.test)

(cl-weave:describe "FR-001 condition A golden comparison"
  (cl-weave:it
   "expands comparison cases declaratively"
   (cl-weave:expect
    (macroexpand-1
     '(ntop.e2e::define-golden-comparison-cases (:same :left :right t)))
    :to-equal
    '(cl-weave:it-each
      ((:same :left :right t))
      "compares criterion-A projections for ~A"
      (ntop.e2e::case-name ntop.e2e::left ntop.e2e::right ntop.e2e::expected)
      (declare (ignore ntop.e2e::case-name))
      (cl-weave:expect
       (ntop.e2e::criterion-projections-equal-p
        ntop.e2e::left ntop.e2e::right)
       :to-be ntop.e2e::expected))))
  (ntop.e2e::define-golden-comparison-cases
    (:same
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     t)
    (:character
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 0 "┐" #("indexed" 7) #("default" nil) 0)))
     nil)
    (:foreground
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
     #(40 12 #(#("title" 0 1 "C" #("indexed" 4) #("default" nil) 1)))
     nil)
    (:background
     #(40 12 #(#("title" 0 1 "C" #("default" nil) #("indexed" 2) 1)))
     #(40 12 #(#("title" 0 1 "C" #("default" nil) #("indexed" 3) 1)))
     nil)
    (:bold
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 1)))
     #(40 12 #(#("title" 0 1 "C" #("indexed" 3) #("default" nil) 0)))
     nil)
    (:coordinates
     #(40 12 #(#("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)))
     #(40 12 #(#("border" 0 1 "┌" #("indexed" 7) #("default" nil) 0)))
     nil)
    (:dimension #(40 12 #()) #(50 12 #()) nil)
    (:malformed-cell
     #(40 12 #(#("data" 0 0 "x" #("default" nil) #("default" nil) 0)))
     #(40 12 #(#("data" 0 0 "x" #("default" nil) #("default" nil) 0)))
     nil))
  (cl-weave:it
   "ignores header text, positions, and missing columns while comparing shared fields"
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "Count Name(n) CPU%(c)▼ Mem%(m) State")))
          (btm
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "PID      Name(n)                      CPU%(c)▼ Mem%(m)"))))
     (cl-weave:expect
      (ntop.e2e::criterion-projections-equal-p candidate btm)
      :to-be
      t)))
  (cl-weave:it-each
   ((:foreground "1;38;5;2;49" "1;38;5;3;49")
    (:background "1;38;5;2;48;5;3" "1;38;5;2;48;5;4")
    (:bold "1;38;5;2;49" "38;5;2;49"))
   "compares process header ~A attributes on shared columns"
   (attribute left-sgr right-sgr)
   (declare (ignore attribute))
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid 12 60 "live process" left-sgr)))
          (btm
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid 12 60 "live process" right-sgr))))
     (cl-weave:expect
      (ntop.e2e::criterion-projections-equal-p candidate btm)
      :to-be
      nil)))
  (cl-weave:it
   "compares the sort marker attribute as part of its logical column"
   (let* ((candidate
            (ntop.e2e:project-criterion-grid
             (criterion-test-grid
              12 60 "live process" "1;38;5;2;49"
              "Count Name(n) CPU%(c)▼ Mem%(m)")))
          (btm (copy-seq candidate))
          (btm-cells (map 'vector #'copy-seq (aref candidate 2))))
     (setf (aref btm 2) btm-cells)
     (let ((marker
            (find-if (lambda (cell)
                       (and (string= (aref cell 0) "process-header")
                            (string= (aref cell 3) "▼")))
                     btm-cells)))
       (cl-weave:expect (not (null marker)) :to-be t)
       (setf (aref marker 4) #("indexed" 3))
       (cl-weave:expect
        (ntop.e2e::criterion-projections-equal-p candidate btm)
        :to-be
        nil))))
  (cl-weave:it
   "routes complete golden comparisons through condition A semantics"
   (let* ((golden-directory
            (asdf:system-relative-pathname "ntop/e2e" "t/e2e/goldens/"))
          (candidate-directory
            (ntop.e2e::%make-e2e-temporary-directory))
          (target-path
            (ntop.e2e::%golden-pathname candidate-directory 120 54)))
     (unwind-protect
          (progn
            (dolist (dimension (ntop.e2e::golden-dimensions))
              (destructuring-bind (width height) dimension
                (let ((source
                        (ntop.e2e::%golden-pathname
                         golden-directory width height))
                      (target
                        (ntop.e2e::%golden-pathname
                         candidate-directory width height)))
                  (ensure-directories-exist target)
                  (uiop:copy-file source target))))
            (let* ((projection
                     (ntop.e2e::%read-criterion-golden target-path 120 54))
                   (header-cells
                     (remove-if-not
                      (lambda (cell)
                        (string= (aref cell 0) "process-header"))
                      (coerce (aref projection 2) 'list))))
              (cl-weave:expect (not (null header-cells)) :to-be t)
              (let* ((minimum
                       (reduce #'min header-cells
                               :key (lambda (cell) (aref cell 2))))
                     (maximum
                       (reduce #'max header-cells
                               :key (lambda (cell) (aref cell 2))))
                     (shift (cond
                              ((plusp minimum) -1)
                              ((< maximum 119) 1)
                              (t (error "Cannot shift the process header fixture.")))))
                (dolist (cell header-cells)
                  (incf (aref cell 2) shift)))
              (with-open-file (stream target-path
                                      :direction :output
                                      :if-exists :supersede
                                      :if-does-not-exist :create
                                      :external-format :utf-8)
                (json-kit:write-json projection stream)
                (terpri stream)))
            (cl-weave:expect
             (ntop.e2e::compare-goldens
              :left golden-directory :right candidate-directory)
             :to-equal
             0))
       (uiop:delete-directory-tree candidate-directory :validate t)))))

(cl-weave:describe "FR-001 condition A golden reader"
                   (cl-weave:it
                    "reads the checked-in JSON golden with empty default colors"
                    (let* ((pathname
                            (asdf:system-relative-pathname "ntop/e2e"
                                                           "t/e2e/goldens/40-12.json"))
                           (projection
                            (ntop.e2e::%read-criterion-golden pathname 40 12))
                           (cells (aref projection 2)))
                      (cl-weave:expect
                       (list (aref projection 0) (aref projection 1))
                       :to-equal
                       (quote (40 12)))
                      (cl-weave:expect
                       (and (vectorp cells) (plusp (length cells)))
                       :to-be
                       t))))
(cl-weave:describe
 "FR-001 condition A live-grid comparison"
 (cl-weave:it
  "separates frame geometry and border, title, and process-header SGR at 40x12"
  (let* ((reference (criterion-test-grid 12 40 "live process"))
         (same (ntop.e2e::copy-terminal-grid reference))
         (border-style (ntop.e2e::copy-terminal-grid reference))
         (title-style (ntop.e2e::copy-terminal-grid reference))
         (header-style (ntop.e2e::copy-terminal-grid reference))
         (shifted-frame (ntop.e2e::copy-terminal-grid reference)))
    (cl-weave:expect
     (ntop.e2e::condition-a-difference-kinds reference same)
     :to-equal nil)
    (setf (ntop.e2e:cell-foreground
           (ntop.e2e:grid-cell-at border-style 0 0))
          '(:indexed . 1))
    (cl-weave:expect
     (ntop.e2e::condition-a-difference-kinds reference border-style)
     :to-equal '(:border-sgr))
    (setf (ntop.e2e:cell-foreground
           (ntop.e2e:grid-cell-at title-style 0 3))
          '(:indexed . 4))
    (cl-weave:expect
     (ntop.e2e::condition-a-difference-kinds reference title-style)
     :to-equal '(:title-sgr))
    (setf (ntop.e2e:cell-background
           (ntop.e2e:grid-cell-at header-style 9 2))
          '(:indexed . 5))
    (cl-weave:expect
     (ntop.e2e::condition-a-difference-kinds reference header-style)
     :to-equal '(:process-header-sgr))
    (setf (ntop.e2e:cell-character
           (ntop.e2e:grid-cell-at shifted-frame 0 39))
          #\Space)
    (setf (ntop.e2e:cell-character
           (ntop.e2e:grid-cell-at shifted-frame 0 38))
          #\┐)
    (cl-weave:expect (not (null (member :frame-geometry
                                        (ntop.e2e::condition-a-difference-kinds
                                         reference shifted-frame))))
                     :to-be t)))
 (cl-weave:it
  "parses accumulated raw bytes with incomplete UTF-8 and CSI suffixes"
  (let* ((complete-output
          (sb-ext:string-to-octets
           (format nil "~c[2J~c[HA" #\Escape #\Escape)
           :external-format :utf-8))
         (partial-output
          (concatenate '(vector (unsigned-byte 8))
                       complete-output
                       (vector #xe2 #x94)))
         (grid (ntop.e2e::%condition-a-live-grid partial-output 12 40))
         (partial-csi-output
          (concatenate '(vector (unsigned-byte 8))
                       complete-output
                       (vector #x1b #x5b #x33)))
         (complete-csi-output
          (concatenate '(vector (unsigned-byte 8))
                       partial-csi-output
                       (vector #x31 #x6d)))
         (incomplete-csi-grid
          (ntop.e2e::%condition-a-live-grid partial-csi-output 12 40))
         (complete-csi-grid
          (ntop.e2e::%condition-a-live-grid complete-csi-output 12 40)))
    (cl-weave:expect
     (ntop.e2e:cell-character (ntop.e2e:grid-cell-at grid 0 0))
     :to-equal #\A)
    (cl-weave:expect
     (ntop.e2e:cell-character (ntop.e2e:grid-cell-at grid 0 1))
     :to-equal #\Space)
    (cl-weave:expect incomplete-csi-grid :to-be nil)
    (cl-weave:expect
     (ntop.e2e:cell-character (ntop.e2e:grid-cell-at complete-csi-grid 0 0))
     :to-equal #\A)))
 (cl-weave:it
  "keeps the capture session open across samples and closes it on errors"
  (let* ((original-launch
          (symbol-function 'ntop.e2e::launch-e2e-process))
         (original-close
          (symbol-function 'ntop.e2e::close-e2e-session))
         (original-functions
          (list (cons 'ntop.e2e::launch-e2e-process original-launch)
                (cons 'ntop.e2e::%route-read-interval
                      (symbol-function 'ntop.e2e::%route-read-interval))
                (cons 'ntop.e2e::%condition-a-live-grid
                      (symbol-function 'ntop.e2e::%condition-a-live-grid))
                (cons 'ntop.e2e::%pty-visible-screen-output-p
                      (symbol-function 'ntop.e2e::%pty-visible-screen-output-p))
                (cons 'ntop.e2e::%elapsed-seconds
                      (symbol-function 'ntop.e2e::%elapsed-seconds))
                (cons 'ntop.e2e::close-e2e-session original-close)))
         (active-session nil)
         (grid (let ((grid (ntop.e2e:make-terminal-grid :rows 12 :cols 40)))
                 (criterion-test-write
                  grid 2 0 "38;5;7;49"
                  (concatenate 'string "┌─ Processes ─"
                               (make-string 25 :initial-element #\─)
                               "┐"))
                 (loop for row from 3 below 10
                       do (criterion-test-write grid row 0 "38;5;7;49" "│")
                       (criterion-test-write grid row 39 "38;5;7;49" "│"))
                 (criterion-test-write grid 4 2 "38;5;8;49" "No data")
                 (criterion-test-write
                  grid 10 0 "38;5;7;49"
                  (concatenate 'string "└"
                               (make-string 38 :initial-element #\─)
                               "┘"))
                 grid))
         (sample-count 0)
         (close-count 0)
         (fail-on-sample nil))
    (setf active-session
          (funcall original-launch "sleep" (list "30") 12 40
                   :timeout-seconds 5d0))
    (unwind-protect
        (progn
          (setf (symbol-function 'ntop.e2e::launch-e2e-process)
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  active-session)
                (symbol-function 'ntop.e2e::%route-read-interval)
                (lambda (session raw seconds)
                  (declare (ignore raw seconds))
                  (cl-weave:expect (eq session active-session) :to-be t)
                  (cl-weave:expect
                   (ntop.e2e::e2e-session-closed-p session)
                   :to-be nil)
                  (incf sample-count)
                  (when (eql sample-count fail-on-sample)
                    (error "sample failure")))
                (symbol-function 'ntop.e2e::%condition-a-live-grid)
                (lambda (octets rows columns)
                  (declare (ignore octets rows columns))
                  grid)
                (symbol-function 'ntop.e2e::%pty-visible-screen-output-p)
                (lambda (octets)
                  (declare (ignore octets))
                  t)
                (symbol-function 'ntop.e2e::%elapsed-seconds)
                (lambda (session now)
                  (declare (ignore session now))
                  10d0)
                (symbol-function 'ntop.e2e::close-e2e-session)
                (lambda (session)
                  (incf close-count)
                  (funcall original-close session)))
          (cl-weave:expect
           (eq (ntop.e2e::%condition-a-capture
                "ntop" nil 12 40 700 5d0 nil nil)
               grid)
           :to-be
           t)
          (cl-weave:expect sample-count :to-equal 3)
          (cl-weave:expect close-count :to-equal 1)
          (cl-weave:expect
           (ntop.e2e::e2e-session-closed-p active-session)
           :to-be
           t)
          (setf active-session
                (funcall original-launch "sleep" (list "30") 12 40
                         :timeout-seconds 5d0)
                sample-count 0
                close-count 0
                fail-on-sample 2)
          (let ((condition
                 (handler-case
                     (progn
                       (ntop.e2e::%condition-a-capture
                        "ntop" nil 12 40 700 5d0 nil nil)
                       nil)
                   (error (condition) condition))))
            (cl-weave:expect (typep condition 'simple-error) :to-be t))
          (cl-weave:expect sample-count :to-equal 2)
          (cl-weave:expect close-count :to-equal 1)
          (cl-weave:expect
           (ntop.e2e::e2e-session-closed-p active-session)
           :to-be
           t))
      (dolist (entry original-functions)
        (setf (symbol-function (car entry)) (cdr entry)))
      (when (and active-session
                 (not (ntop.e2e::e2e-session-closed-p active-session)))
        (funcall original-close active-session)))))
)
