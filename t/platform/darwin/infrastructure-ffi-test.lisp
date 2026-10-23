(in-package #:ntop.test)

(cl-weave:describe "Darwin FFI declarations"
                   (cl-weave:it
                    "expands fixed routine and variable declarations"
                    (let* ((form
                            '(ntop.platform.darwin::define-darwin-ffi
                              (:routine ntop.test::probe
                                        "probe"
                                        sb-alien:int
                                        (value sb-alien:int))
                              (:variable ntop.test::task-self
                                         "mach_task_self_"
                                         sb-alien:unsigned-int)))
                           (expansion (macroexpand-1 form)))
                      (cl-weave:expect (first expansion) :to-be 'progn)
                      (cl-weave:expect (rest expansion)
                                       :to-equal
                                       '((sb-alien:define-alien-routine
                                          ("probe" ntop.test::probe)
                                          sb-alien:int
                                          (value sb-alien:int))
                                         (sb-alien:define-alien-variable
                                          ("mach_task_self_"
                                           ntop.test::task-self)
                                          sb-alien:unsigned-int)))))
                   (cl-weave:it
                    "expands optional C arguments into fixed-arity bindings"
                    (let* ((form
                            '(ntop.platform.darwin::define-darwin-ffi
                              (:routine ntop.test::probe-fcntl
                                        "fcntl"
                                        sb-alien:int
                                        (file-descriptor sb-alien:int)
                                        (command sb-alien:int)
                                        &optional
                                        (argument sb-alien:int))))
                           (expansion (macroexpand-1 form))
                           (forms (rest expansion))
                           (variadic-forms (rest (first forms)))
                           (two-argument-binding (first variadic-forms))
                           (three-argument-binding (second variadic-forms))
                           (wrapper (third variadic-forms))
                           (zero-argument-alien-name
                            (second (second two-argument-binding)))
                           (one-argument-alien-name
                            (second (second three-argument-binding)))
                           (dispatch (fourth wrapper)))
                      (cl-weave:expect (length forms) :to-be 1)
                      (cl-weave:expect (first (first forms)) :to-be 'progn)
                      (cl-weave:expect (length variadic-forms) :to-be 3)
                      (cl-weave:expect (first two-argument-binding)
                                       :to-be
                                       'sb-alien:define-alien-routine)
                      (cl-weave:expect (first three-argument-binding)
                                       :to-be
                                       'sb-alien:define-alien-routine)
                      (cl-weave:expect (length (nthcdr 3 two-argument-binding))
                                       :to-be
                                       2)
                      (cl-weave:expect
                       (length (nthcdr 3 three-argument-binding))
                       :to-be
                       3)
                      (cl-weave:expect (first dispatch) :to-be 'if)
                      (cl-weave:expect (first (third dispatch))
                                       :to-be
                                       one-argument-alien-name)
                      (cl-weave:expect (first (fourth dispatch))
                                       :to-be
                                       zero-argument-alien-name)))
                   (cl-weave:it-run-if (member :darwin *features*)
                                       "calls fcntl with each declared C arity"
                                       (with-open-file
                                           (stream "/dev/null"
                                                   :direction
                                                   :input)
                                         (let* ((file-descriptor
                                                 (sb-sys:fd-stream-fd stream))
                                                (original-flags
                                                 (ntop.platform.darwin::%fcntl
                                                  file-descriptor
                                                  1)))
                                           (cl-weave:expect
                                            (>= original-flags 0)
                                            :to-be
                                            t)
                                           (when (>= original-flags 0)
                                             (unwind-protect
                                                 (let ((updated-flags
                                                        (logior original-flags
                                                                1)))
                                                   (cl-weave:expect
                                                    (=
                                                     (ntop.platform.darwin::%fcntl
                                                      file-descriptor
                                                      2
                                                      updated-flags)
                                                     0)
                                                    :to-be
                                                    t)
                                                   (cl-weave:expect
                                                    (ntop.platform.darwin::%fcntl
                                                     file-descriptor
                                                     1)
                                                    :to-be
                                                    updated-flags))
                                               (ntop.platform.darwin::%fcntl
                                                file-descriptor
                                                2
                                                original-flags)))))))
