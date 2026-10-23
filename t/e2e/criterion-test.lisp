(in-package #:ntop.e2e.test)

(defun criterion-test-write (grid row column sgr text)
  (ntop.e2e:parse-ansi (format nil "~c[~d;~dH~c[~am~a" #\Escape (1+ row) (1+ column) #\Escape sgr text)
                       grid
                       (lambda (value)
                         value)
                       (lambda (condition)
                         (error condition))))

(defun criterion-test-grid (rows columns process-name)
  (let* ((grid (ntop.e2e:make-terminal-grid :rows rows :cols columns))
         (process-frame-top (- rows 4))
         (process-header-row (- rows 3))
         (process-data-row (- rows 2))
         (process-frame-bottom (1- rows)))
    (criterion-test-write grid 0 0 "38;5;7;49" "┌─")
    (criterion-test-write grid 0 3 "1;38;5;3;48;5;4" "CPU")
    (criterion-test-write grid 0 7 "38;5;8;49" "45.70 45.58 53.02")
    (criterion-test-write grid 0 (- columns 7) "38;2;1;2;3;48;2;4;5;6" "Memory")
    (criterion-test-write grid 0 (1- columns) "38;5;7;49" "┐")
    (loop for row from 1 below (1- rows)
          do (criterion-test-write grid row 0 "38;5;7;49" "│")
          (criterion-test-write grid row (1- columns) "38;5;7;49" "│"))
    (criterion-test-write grid (1- rows) 0 "38;5;7;49" "└")
    (criterion-test-write grid (1- rows) (1- columns) "38;5;7;49" "┘")
    (criterion-test-write grid process-frame-top 1 "38;5;7;49" "┌")
    (criterion-test-write grid process-frame-top 2 "38;5;8;49" "─ Processes ─")
    (criterion-test-write grid process-frame-top (1- columns) "38;5;7;49" "┐")
    (criterion-test-write grid process-header-row 1 "38;5;7;49" "│")
    (criterion-test-write grid process-header-row 2 "1;38;5;2;49" "Count Name(n) CPU%(c)")
    (criterion-test-write grid process-header-row (1- columns) "38;5;7;49" "│")
    (criterion-test-write grid process-data-row 1 "38;5;7;49" "│")
    (criterion-test-write grid process-data-row 2 "38;5;1;49" process-name)
    (criterion-test-write grid process-data-row (1- columns) "38;5;7;49" "│")
    (criterion-test-write grid process-frame-bottom 1 "38;5;7;49" "└")
    (criterion-test-write grid process-frame-bottom (1- columns) "38;5;7;49" "┘")
    grid))

(defun criterion-test-has-entry-p (projection entry)
  (not (null (find entry (aref projection 2) :test #'equalp))))

(cl-weave:describe "FR-001 condition A criterion projection"
                   (cl-weave:it "expands declarative vocabulary data"
                                (cl-weave:expect
                                 (macroexpand-1 '(ntop.e2e:define-criterion-vocabulary (:sample "value")))
                                 :to-equal
                                 '(defparameter ntop.e2e::*criterion-vocabulary*
                                    '((:sample "value")))))
                   (cl-weave:it "preserves frame, title, header, and style while excluding data"
                                (let* ((grid (criterion-test-grid 12 40 "SENTINEL_PROCESS_NAME"))
                                       (projection (ntop.e2e:project-criterion-grid grid))
                                       (entries (aref projection 2)))
                                  (dolist
                                      (expected
                                       (list #("border" 0 0 "┌" #("indexed" 7) #("default" nil) 0)
                                             #("border" 0 39 "┐" #("indexed" 7) #("default" nil) 0)
                                             #("border" 1 0 "│" #("indexed" 7) #("default" nil) 0)
                                             #("border" 1 39 "│" #("indexed" 7) #("default" nil) 0)
                                             #("border" 11 0 "└" #("indexed" 7) #("default" nil) 0)
                                             #("border" 11 39 "┘" #("indexed" 7) #("default" nil) 0)
                                             #("title" 0 3 "C" #("indexed" 3) #("indexed" 4) 1)
                                             #("title" 0 33 "M" #("rgb" #(1 2 3)) #("rgb" #(4 5 6)) 0)
                                             #("process-header" 9 2 "C" #("indexed" 2) #("default" nil) 1)))
                                    (cl-weave:expect (criterion-test-has-entry-p projection expected)
                                                     :to-be
                                                     t))
                                  (cl-weave:expect (search "SENTINEL_PROCESS_NAME" (prin1-to-string entries))
                                                   :to-be
                                                   nil)
                                  (cl-weave:expect (search "45.70" (prin1-to-string entries)) :to-be nil)))
                   (cl-weave:it "projects all 171 specified terminal dimensions"
                                (let ((dimensions
                                       (loop for columns from 40 to 220 by 10
                                             append (loop for rows from 12 to 60 by 6
                                                          collect (list columns rows)))))
                                  (cl-weave:expect (length dimensions) :to-be 171)
                                  (dolist (dimension dimensions)
                                    (destructuring-bind (columns rows) dimension
                                      (let* ((sentinel (format nil "SENTINEL_~d_~d" columns rows))
                                             (projection
                                              (ntop.e2e:project-criterion-grid
                                               (criterion-test-grid rows columns sentinel)))
                                             (entries (aref projection 2)))
                                        (cl-weave:expect (list (aref projection 0) (aref projection 1))
                                                         :to-equal
                                                         (list columns rows))
                                        (cl-weave:expect (search sentinel (prin1-to-string entries))
                                                         :to-be
                                                         nil)
                                        (cl-weave:expect (search "45.70" (prin1-to-string entries))
                                                         :to-be
                                                         nil)))))))
(cl-weave:describe
 "FR-001 condition A process header boundary"
 (cl-weave:it
  "does not project dynamic process labels as a table header"
  (let* ((projection
          (ntop.e2e:project-criterion-grid
           (criterion-test-grid 12 40 "Count Name(n) CPU%(c)")))
         (entries (aref projection 2)))
    (cl-weave:expect
     (loop for entry across entries
           thereis (and (string= (aref entry 0) "process-header")
                        (= (aref entry 1) 10)))
     :to-be
     nil))))
