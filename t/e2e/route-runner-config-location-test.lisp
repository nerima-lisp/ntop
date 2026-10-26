(in-package #:ntop.e2e.test)

(cl-weave:describe
 "config_location dispatch in ntop mode"
 (cl-weave:it
  "routes config_location through TUI and CLI while classifying candidate outcomes"
  (let* ((route
           (find-if
            (lambda (entry)
              (and (eq (ntop.e2e::route-kind entry) :flag)
                   (string= (ntop.app.domain:parity-flag-snake-case
                             (ntop.e2e::route-entry entry))
                            "config_location")))
            (ntop.e2e::route-catalog)))
         (subcases (ntop.e2e::%route-subcases-expanded route))
         (valid-subcases
           (remove-if (lambda (subcase) (getf subcase :invalid-value)) subcases))
         (missing-subcase
           (find-if (lambda (subcase) (getf subcase :invalid-value)) subcases))
         (symbols
           (list (find-symbol "%ROUTE-RUN-INTERACTIVE-SIDE" "NTOP.E2E")
                 (find-symbol "%ROUTE-RUN-CLI-SIDE" "NTOP.E2E")
                 (find-symbol "%ROUTE-RUN-INVALID-FLAG-PAIR" "NTOP.E2E")))
         (originals (mapcar #'symbol-function symbols))
         (reference-calls nil)
         (candidate-calls nil)
         (invalid-calls nil)
         (candidate-mode :valid)
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (config-path (merge-pathnames "btm-route-config.toml" directory))
         (valid-subcase (first valid-subcases)))
    (unwind-protect
         (progn
           (setf (symbol-function (first symbols))
                 (lambda (program route subcase prefix rows columns rate timeout
                          passed-config evidence-directory)
                   (declare (ignore route subcase prefix rows columns rate timeout
                                    evidence-directory))
                   (push (list program passed-config) reference-calls)
                   (list :actual "captured BTM TUI frame")))
           (setf (symbol-function (second symbols))
                 (lambda (program arguments prefix timeout)
                   (declare (ignore prefix timeout))
                   (push (list program arguments) candidate-calls)
                   (case candidate-mode
                     (:valid
                      (list :exit-code 2 :stdout "invalid command-line input"
                            :stderr "" :actual "exit 2"))
                     (:unexpected
                      (list :exit-code 2 :stdout "unexpected CLI error"
                            :stderr "" :actual "exit 2"))
                     (otherwise (error "candidate CLI capture failed")))))
           (setf (symbol-function (third symbols))
                 (lambda (route description reference-args candidate-args mode btm ntop
                          reference-prefix candidate-prefix timeout)
                   (declare (ignore route description reference-args mode btm ntop
                                    reference-prefix candidate-prefix timeout))
                   (push candidate-args invalid-calls)
                   (list :dispatch :invalid-value :status :pass :passed t)))
           (cl-weave:expect (length valid-subcases) :to-equal 4)
           (dolist (subcase valid-subcases)
             (let ((result
                     (ntop.e2e::%route-side-pair
                      route subcase 1 "ntop" "btm" "ntop" directory config-path
                      24 80 250 1)))
               (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                                :to-equal :pass)
               (cl-weave:expect
                (not
                 (null
                  (member
                   (namestring config-path)
                   (ntop.e2e::%route-base-arguments
                    "btm" config-path route subcase directory)
                   :test (function equal))))
                :to-be t)))
           (progn
             (cl-weave:expect (length reference-calls) :to-equal 4)
             (cl-weave:expect (length candidate-calls) :to-equal 4))
           (cl-weave:expect
            (every (lambda (call)
                     (and (string= (first call) "btm")
                          (equal (namestring (second call))
                                 (namestring config-path))))
                   reference-calls)
            :to-be t)
           (cl-weave:expect
            (every (lambda (call)
                     (and (string= (first call) "ntop")
                          (member (namestring config-path) (second call)
                                  :test #'equal)))
                   candidate-calls)
            :to-be t)
           (setf candidate-mode :unexpected)
           (let ((result
                   (ntop.e2e::%route-side-pair
                    route valid-subcase 5 "ntop" "btm" "ntop" directory config-path
                    24 80 250 1)))
             (cl-weave:expect (getf result :classification)
                              :to-equal :candidate-failure)
             (cl-weave:expect (ntop.e2e::%route-subresult-status result)
                              :to-equal :fail))
           (setf candidate-mode :error)
           (let ((result
                   (ntop.e2e::%route-capture-subcase
                    route valid-subcase 6 directory
                    (lambda ()
                      (ntop.e2e::%route-side-pair
                       route valid-subcase 6 "ntop" "btm" "ntop" directory
                       config-path 24 80 250 1)))))
             (cl-weave:expect (getf result :classification)
                              :to-equal :candidate-error)
             (cl-weave:expect (getf result :status) :to-equal :fail)
             (cl-weave:expect
              (ntop.e2e::%route-classification-group-count
               (list result) :harness-error)
              :to-equal 1)
             (cl-weave:expect
              (ntop.e2e::%route-results-exit-code (list result))
              :to-equal 1))
           (let ((reference-count (length reference-calls))
                 (candidate-count (length candidate-calls)))
             (cl-weave:expect
              (getf (ntop.e2e::%route-side-pair
                     route missing-subcase 7 "ntop" "btm" "ntop" directory
                     config-path 24 80 250 1)
                    :dispatch)
              :to-equal :invalid-value)
             (cl-weave:expect (length invalid-calls) :to-equal 1)
             (cl-weave:expect (length (first invalid-calls)) :to-equal 1)
             (cl-weave:expect (length reference-calls) :to-equal reference-count)
             (cl-weave:expect (length candidate-calls) :to-equal candidate-count)))
      (loop for symbol in symbols
            for function in originals
            do (setf (symbol-function symbol) function))
      (uiop:delete-directory-tree directory :validate t)))))
(cl-weave:describe
 "config_location btm-self dispatch"
 (cl-weave:it
  "uses paired BTM TUI capture for both sides"
  (let* ((route
           (find-if
            (lambda (entry)
              (and (eq (ntop.e2e::route-kind entry) :flag)
                   (string= (ntop.app.domain:parity-flag-snake-case
                             (ntop.e2e::route-entry entry))
                            "config_location")))
            (ntop.e2e::route-catalog)))
         (valid-subcase
           (find-if (lambda (subcase)
                      (not (getf subcase :invalid-value)))
                    (ntop.e2e::%route-subcases-expanded route)))
         (symbols
           (list (find-symbol "%ROUTE-RUN-INTERACTIVE-PAIR" "NTOP.E2E")
                 (find-symbol "%ROUTE-RUN-CONFIG-LOCATION-PAIR" "NTOP.E2E")))
         (originals (mapcar #'symbol-function symbols))
         (interactive-arguments nil)
         (cli-called nil)
         (directory (ntop.e2e::%make-e2e-temporary-directory))
         (config-path (merge-pathnames "btm-self-config.toml" directory)))
    (unwind-protect
         (progn
           (setf (symbol-function (first symbols))
                 (lambda (&rest arguments)
                   (setf interactive-arguments arguments)
                   (list :dispatch :interactive)))
           (setf (symbol-function (second symbols))
                 (lambda (&rest arguments)
                   (declare (ignore arguments))
                   (setf cli-called t)
                   (list :dispatch :config-cli)))
           (let ((result
                   (ntop.e2e::%route-side-pair
                    route valid-subcase 1 "btm-self" "btm" "btm" directory
                    config-path 24 80 250 1)))
             (cl-weave:expect (getf result :dispatch)
                              :to-equal :interactive)
             (cl-weave:expect (nth 3 interactive-arguments)
                              :to-equal "btm-self")
             (cl-weave:expect (nth 4 interactive-arguments)
                              :to-equal "btm")
             (cl-weave:expect (nth 5 interactive-arguments)
                              :to-equal "btm")
             (cl-weave:expect cli-called :to-be nil)))
      (loop for symbol in symbols
            for function in originals
            do (setf (symbol-function symbol) function))
      (uiop:delete-directory-tree directory :validate t)))))
