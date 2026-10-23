(in-package #:ntop.test)

(cl-weave:describe "ntop package loading"
                   (cl-weave:it "loads the shared domain package"
                                (cl-weave:expect
                                 (find-package '#:ntop.shared.domain)
                                 :to-be-truthy))
                   (cl-weave:it "expands declarative package definitions"
                                (let ((expansion
                                       (macroexpand-1
                                        (quote
                                         (cl-user::define-ntop-package
                                          "ntop.expansion-fixture"
                                          (:documentation "Fixture")
                                          (:export "VALUE"))))))
                                  (cl-weave:expect
                                   (and
                                    (eq (first expansion) (quote defpackage))
                                    (string= (second expansion)
                                             "ntop.expansion-fixture")
                                    (eq (first (third expansion)) :use)
                                    (string=
                                     (symbol-name (second (third expansion)))
                                     "CL")
                                    (equal (fourth expansion)
                                           (quote (:documentation "Fixture")))
                                    (equal (fifth expansion)
                                           (quote (:export "VALUE"))))
                                   :to-be-truthy))))
