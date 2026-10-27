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
        (header-text (ntop.e2e::copy-terminal-grid reference))
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
          (ntop.e2e:grid-cell-at header-text 9 2))
         #\X)
   (cl-weave:expect
    (ntop.e2e::condition-a-difference-kinds reference header-text)
    :to-equal nil)
   (setf (ntop.e2e:cell-character
          (ntop.e2e:grid-cell-at shifted-frame 0 39))
         #\Space)
   (setf (ntop.e2e:cell-character
          (ntop.e2e:grid-cell-at shifted-frame 0 38))
         #\┐)
   (cl-weave:expect
    (not (null (member :frame-geometry
                       (ntop.e2e::condition-a-difference-kinds
                        reference shifted-frame))))
    :to-be t)))
 (cl-weave:it
 "reports process-header SGR differences in one line when pretty printing is enabled"
 (let* ((reference (criterion-test-grid 12 40 "live process"))
        (candidate (ntop.e2e::copy-terminal-grid reference)))
   (setf (ntop.e2e:cell-background
          (ntop.e2e:grid-cell-at candidate 9 2))
         '(:indexed . 4))
   (let* ((*print-pretty* t)
          (*print-right-margin* 12)
          (difference-kinds
            (ntop.e2e::condition-a-difference-kinds reference candidate))
          (detail
            (ntop.e2e::%condition-a-first-difference
             reference candidate difference-kinds)))
     (cl-weave:expect difference-kinds :to-equal '(:process-header-sgr))
     (cl-weave:expect (find #\Newline detail) :to-be nil))))
 (cl-weave:it
  "uses one stable frame capture and closes the session on errors"
  (let* ((original-launch
          (symbol-function 'ntop.e2e::launch-e2e-process))
         (original-close
          (symbol-function 'ntop.e2e::close-e2e-session))
         (original-functions
          (list (cons 'ntop.e2e::launch-e2e-process original-launch)
                (cons 'ntop.e2e::%route-capture-frame
                      (symbol-function 'ntop.e2e::%route-capture-frame))
                (cons 'ntop.e2e::%write-octets
                      (symbol-function 'ntop.e2e::%write-octets))
                (cons 'ntop.e2e::close-e2e-session original-close)))
         (active-session nil)
         (grid (criterion-test-grid 12 40 "live process"))
         (capture-output (ntop.e2e::%route-octets "captured output"))
         (written-output nil)
         (capture-count 0)
         (close-count 0)
         (fail-capture nil)
         (ntop.e2e::*route-frame-stability-seconds* 0.05d0)
         (ntop.e2e::*route-frame-capture-side* :reference))
    (setf active-session
          (funcall original-launch "sleep" (list "30") 12 40
                   :timeout-seconds 5d0))
    (unwind-protect
        (progn
          (setf (symbol-function 'ntop.e2e::launch-e2e-process)
                (lambda (&rest arguments)
                  (declare (ignore arguments))
                  active-session)
                (symbol-function 'ntop.e2e::%route-capture-frame)
                (lambda (session prefix rows columns
                         &key side stage timeout-seconds stability-seconds
                           completion-predicate allow-unchanged-p)
                  (incf capture-count)
                  (cl-weave:expect (eq session active-session) :to-be t)
                  (cl-weave:expect
                   (ntop.e2e::e2e-session-closed-p session)
                   :to-be nil)
                  (cl-weave:expect (length prefix) :to-equal 0)
                  (cl-weave:expect rows :to-equal 12)
                  (cl-weave:expect columns :to-equal 40)
                  (cl-weave:expect side :to-equal :reference)
                  (cl-weave:expect stage :to-equal :condition-a)
                  (cl-weave:expect timeout-seconds :to-equal 5d0)
                  (cl-weave:expect stability-seconds :to-equal 0.05d0)
                  (cl-weave:expect allow-unchanged-p :to-be nil)
                  (if fail-capture
                      (error "capture failure")
                    (progn
                      (cl-weave:expect
                       (funcall completion-predicate grid)
                       :to-be-truthy)
                      (values capture-output grid
                              (list :side side :stage stage)))))
                (symbol-function 'ntop.e2e::%write-octets)
                (lambda (path octets)
                  (declare (ignore path))
                  (setf written-output octets))
                (symbol-function 'ntop.e2e::close-e2e-session)
                (lambda (session)
                  (incf close-count)
                  (funcall original-close session)))
          (setf (ntop.e2e::e2e-session-output active-session) capture-output)
          (cl-weave:expect
           (eq (ntop.e2e::%condition-a-capture
                "ntop" nil 12 40 5d0 nil "unused")
               grid)
           :to-be t)
          (cl-weave:expect capture-count :to-equal 1)
          (cl-weave:expect (equalp written-output capture-output) :to-be t)
          (cl-weave:expect close-count :to-equal 1)
          (cl-weave:expect
           (ntop.e2e::e2e-session-closed-p active-session)
           :to-be t)
          (setf active-session
                (funcall original-launch "sleep" (list "30") 12 40
                         :timeout-seconds 5d0)
                capture-count 0
                close-count 0
                written-output nil
                fail-capture t)
          (setf (ntop.e2e::e2e-session-output active-session) capture-output)
          (let ((condition
                 (handler-case
                     (progn
                       (ntop.e2e::%condition-a-capture
                        "ntop" nil 12 40 5d0 nil "unused")
                       nil)
                   (error (condition) condition))))
            (cl-weave:expect (typep condition 'simple-error) :to-be t))
          (cl-weave:expect capture-count :to-equal 1)
          (cl-weave:expect close-count :to-equal 1)
          (cl-weave:expect
           (ntop.e2e::e2e-session-closed-p active-session)
           :to-be t))
      (dolist (entry original-functions)
        (setf (symbol-function (car entry)) (cdr entry)))
      (when (and active-session
                 (not (ntop.e2e::e2e-session-closed-p active-session)))
        (funcall original-close active-session)))))
)
(cl-weave:describe
 "FR-001 condition A capture readiness"
 (cl-weave:it
  "waits for process-header cells on a full process frame"
  (let* ((empty-projection #(40 12 #()))
         (header-projection
           (ntop.e2e:project-criterion-grid
            (criterion-test-grid 12 40 "live process"))))
    (cl-weave:expect
     (ntop.e2e::%condition-a-process-header-ready-p
      (quote (8 11 2 39)) empty-projection)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.e2e::%condition-a-process-header-ready-p
      (quote (8 11 2 39)) header-projection)
     :to-be
     t)
    (cl-weave:expect
     (ntop.e2e::%condition-a-process-header-ready-p
      (quote (8 10 2 39)) empty-projection)
     :to-be
     t))))
