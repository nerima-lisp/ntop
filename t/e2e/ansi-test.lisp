(in-package #:ntop.e2e.test)

(cl-weave:describe "ANSI terminal grid"
                   (cl-weave:it "expands declarative SGR rules"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(ntop.e2e:define-sgr-rule (1 :bold t)
                                                             (22 :bold nil)))
                                 :to-equal
                                 '(list (list 1 '(:bold t))
                                        (list 22 '(:bold nil)))))
                   (cl-weave:it
                    "parses indexed and RGB colors, bold states, reset, and Unicode"
                    (let ((cases
                           (list
                            (list "38;5;4;48;5;12m"
                                  #\A
                                  '(:indexed . 4)
                                  '(:indexed . 12)
                                  nil)
                            (list "1;38;2;1;2;3;48;2;4;5;6m"
                                  #\λ
                                  '(:rgb 1 2 3)
                                  '(:rgb 4 5 6)
                                  t)
                            (list "1;22;38;5;9m"
                                  #\B
                                  '(:indexed . 9)
                                  '(:default)
                                  nil)
                            (list "1;38;5;9;48;5;10;0m"
                                  #\C
                                  '(:default)
                                  '(:default)
                                  nil))))
                      (dolist
                          (case cases)
                        (destructuring-bind (sgr character
                                                 expected-foreground
                                                 expected-background
                                                 expected-bold-p) case
                          (let* ((text
                                  (format nil "~c[~a~c" #\Escape sgr character))
                                 (grid
                                  (ntop.e2e:make-terminal-grid :rows 1 :cols 2))
                                 (success-grid nil)
                                 (result
                                  (ntop.e2e:parse-ansi text
                                                       grid
                                                       (lambda (value)
                                                         (setf success-grid value)
                                                         :accepted)
                                                       (lambda (condition)
                                                         (declare (ignore
                                                                   condition))
                                                         :rejected)))
                                 (cell (ntop.e2e:grid-cell-at grid 0 0)))
                            (cl-weave:expect result :to-be :accepted)
                            (cl-weave:expect success-grid :to-be grid)
                            (cl-weave:expect
                             (list (ntop.e2e:cell-character cell)
                                   (ntop.e2e:cell-foreground cell)
                                   (ntop.e2e:cell-background cell)
                                   (ntop.e2e:cell-bold-p cell))
                             :to-equal
                             (list character
                                   expected-foreground
                                   expected-background
                                   expected-bold-p)))))))
                   (cl-weave:it
                    "moves the cursor, erases a line, and wraps at the screen edge"
                    (let* ((escape #\Escape)
                           (text
                            (format nil
                                    (concatenate 'string
                                                 "~c[?1049h~c]0;test title~c~c[2J"
                                                 "~c[2;2HA~c[1DB~c[3;3HZ~c[2K~c[1;4HXY")
                                    escape
                                    escape
                                    #\Bell
                                    escape
                                    escape
                                    escape
                                    escape
                                    escape
                                    escape))
                           (grid (ntop.e2e:make-terminal-grid :rows 4 :cols 4))
                           (result
                            (ntop.e2e:parse-ansi text
                                                 grid
                                                 (lambda (value)
                                                   (declare (ignore value))
                                                   :ok)
                                                 (lambda (condition)
                                                   (error condition)))))
                      (cl-weave:expect result :to-be :ok)
                      (cl-weave:expect
                       (list
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 0 1))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 1 0))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 1 1))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 2 0))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 2 1))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 2 2))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 2 3))
                        (ntop.e2e:cell-character
                         (ntop.e2e:grid-cell-at grid 0 3)))
                       :to-equal
                       (list #\Space
                             #\Y
                             #\B
                             #\Space
                             #\Space
                             #\Space
                             #\Space
                             #\X))))
                   (cl-weave:it
                    "passes malformed SGR data to the failure continuation"
                    (let ((grid (ntop.e2e:make-terminal-grid :rows 1 :cols 1))
                          (success-value nil)
                          (failure-value nil))
                      (let ((result
                             (ntop.e2e:parse-ansi
                              (format nil "~c[38;2;256;0;0mX" #\Escape)
                              grid
                              (lambda (value)
                                (setf success-value value)
                                :accepted)
                              (lambda (condition)
                                (setf failure-value condition)
                                :rejected))))
                        (cl-weave:expect result :to-be :rejected)
                        (cl-weave:expect success-value :to-be nil)
                        (cl-weave:expect
                         (typep failure-value 'ntop.e2e:ansi-parse-error)
                         :to-be
                         t)))))

(cl-weave:describe "ANSI indexed color properties"
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "round trips generated indexed foreground and background colors"
                    ((colors
                      (cl-weave:gen-tuple (cl-weave:gen-integer :min 0 :max 255)
                                          (cl-weave:gen-integer :min 0 :max 255))))
                    (destructuring-bind (foreground background) colors
                      (let* ((grid
                              (ntop.e2e:make-terminal-grid :rows 1 :cols 1))
                             (result
                              (ntop.e2e:parse-ansi
                               (format nil
                                       "~c[38;5;~d;48;5;~dmX"
                                       (code-char 27)
                                       foreground
                                       background)
                               grid
                               (lambda (value)
                                 value)
                               (lambda (condition)
                                 (error condition))))
                             (cell (ntop.e2e:grid-cell-at grid 0 0)))
                        (cl-weave:expect (eq result grid) :to-be t)
                        (cl-weave:expect (ntop.e2e:cell-foreground cell)
                                         :to-equal
                                         (cons :indexed foreground))
                        (cl-weave:expect (ntop.e2e:cell-background cell)
                                         :to-equal
                                         (cons :indexed background))))))
