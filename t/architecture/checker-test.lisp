(in-package #:ntop.test)

(cl-weave:describe "package by feature layer policy"
                   (cl-weave:it "expands layer policy declarations into data"
                                (cl-weave:expect
                                 (macroexpand-1
                                  '(define-layer-import-policy
                                    +fixture-layer-policy+
                                    (:feature-domain :shared-domain)))
                                 :to-equal
                                 '(defparameter +fixture-layer-policy+
                                    '((:feature-domain :shared-domain)))))
                   (cl-weave:it-each
                    (("src/shared/domain-rate.lisp" "NTOP.SHARED.DOMAIN")
                     ("src/platform/darwin/infrastructure-base.lisp"
                      "NTOP.PLATFORM.DARWIN")
                     ("src/feature/process/domain-query.lisp"
                      "NTOP.PROCESS.DOMAIN")
                     ("src/feature/cpu/domain.lisp" "NTOP.CPU.DOMAIN")
                     ("src/feature/cpu/infrastructure.lisp" "NTOP.CPU.INFRASTRUCTURE")
                     ("src/feature/cpu/presentation.lisp" "NTOP.CPU.PRESENTATION")
                     ("src/app/domain-parity.lisp" "NTOP.APP.DOMAIN")
                     ("src/cli/presentation-main.lisp" "NTOP.CLI"))
                    "maps ~A to its declared package"
                    (path expected)
                    (cl-weave:expect (path-layer-package (pathname path))
                                     :to-equal
                                     expected))
                   (cl-weave:it-each
                    (("NTOP.CPU.DOMAIN" "NTOP.SHARED.DOMAIN" t)
                     ("NTOP.CPU.APPLICATION" "NTOP.CPU.DOMAIN" t)
                     ("NTOP.CPU.APPLICATION" "NTOP.GPU.DOMAIN" nil)
                     ("NTOP.CPU.INFRASTRUCTURE" "NTOP.PLATFORM.DARWIN" t)
                     ("NTOP.CPU.PRESENTATION" "NTOP.UI" t)
                     ("NTOP.CPU.DOMAIN" "NTOP.MEMORY.DOMAIN" nil)
                     ("NTOP.APP.DOMAIN" "NTOP.GPU.DOMAIN" t)
                     ("NTOP.PROCESS.DOMAIN" "NTOP.CPU.DOMAIN" nil))
                    "checks whether ~A may reference ~A"
                    (source target expected)
                    (cl-weave:expect (layer-dependency-allowed-p source target)
                                     :to-be
                                     expected))
                   (cl-weave:it-property
                    "feature domains reject imports from other features"
                    ((source-owner
                      (cl-weave:gen-member
                       '("NTOP.CPU" "NTOP.MEMORY" "NTOP.GPU" "NTOP.NETWORK")))
                     (target-owner
                      (cl-weave:gen-member
                       '("NTOP.CPU" "NTOP.MEMORY" "NTOP.GPU" "NTOP.NETWORK"))))
                    (cl-weave:expect
                     (layer-dependency-allowed-p
                      (format nil "~A.DOMAIN" source-owner)
                      (format nil "~A.DOMAIN" target-owner))
                     :to-be
                     (string= source-owner target-owner)))
                   (cl-weave:it "inspects package definitions throughout src/"
                                (let* ((root (asdf:system-source-directory "ntop"))
                                       (source-root (merge-pathnames "src/" root))
                                       (package-files
                                        (sort
                                         (mapcar
                                          (lambda (pathname)
                                            (enough-namestring pathname root))
                                          (source-files source-root t))
                                         (function string<))))
                                  (cl-weave:expect package-files
                                                   :to-equal
                                                   (quote
                                                    ("src/app/package.lisp"
                                                     "src/cli/package.lisp"
                                                     "src/feature/battery/package.lisp"
                                                     "src/feature/cpu/package.lisp"
                                                     "src/feature/disk/package.lisp"
                                                     "src/feature/gpu/package.lisp"
                                                     "src/feature/memory/package.lisp"
                                                     "src/feature/network/package.lisp"
                                                     "src/feature/process/package.lisp"
                                                     "src/feature/temperature/package.lisp"
                                                     "src/package.lisp"
                                                     "src/platform/package.lisp"
                                                     "src/shared/package.lisp"
                                                     "src/ui/package.lisp")))
                                  (cl-weave:expect (check-source-tree) :to-equal nil)))
                   (cl-weave:it "rejects the cross-feature fixture"
                                (let* ((root
                                        (asdf:system-source-directory "ntop"))
                                       (fixture
                                        (merge-pathnames
                                         "t/architecture/fixtures/illegal-cross-feature.lisp"
                                         root))
                                       (violations
                                        (source-file-violations fixture
                                                                root
                                                                "NTOP.CPU.DOMAIN")))
                                  (cl-weave:expect (length violations) :to-be 1)
                                  (cl-weave:expect
                                   (search "NTOP.MEMORY.DOMAIN"
                                           (first violations))
                                   :to-be-truthy))))
