(in-package #:ntop.e2e.test)

(defun n7-test-with-root (content thunk)
  (host-kit:call-with-temporary-directory
   (lambda (directory)
     (let* ((root (merge-pathnames "scan/" directory))
            (nested (merge-pathnames "nested/" root)))
       (host-kit:create-directory root)
       (host-kit:create-directory nested)
       (host-kit:write-file-string content
                                   (merge-pathnames "sample.txt" nested))
       (funcall thunk root)))
   :prefix
   "ntop-n7-test-"))

(cl-weave:describe
 "N7 machine-specific content scan"
 (cl-weave:it
  "expands declarative N7 scan settings"
  (cl-weave:expect
   (macroexpand-1
    (quote
     (ntop.e2e::define-n7-settings
      (ntop.e2e::+sample-timeout+ 7d0)
      (ntop.e2e::+sample-batch-size+ 16))))
   :to-equal
   (quote
    (progn
     (defconstant ntop.e2e::+sample-timeout+ 7d0)
     (defconstant ntop.e2e::+sample-batch-size+ 16))))
  (cl-weave:expect
   (macroexpand-1
    (quote
     (ntop.e2e::define-n7-pattern-sources
      ntop.e2e::*example-pattern-sources*
      :hostname
      :username)))
   :to-equal
   (quote
    (defparameter
     ntop.e2e::*example-pattern-sources*
     (quote (:hostname :username)))))
  (cl-weave:expect
   ntop.e2e::*n7-pattern-sources*
   :to-equal
   (quote
    (:hostname
     :username
     :home-path
     :working-directory
     :root-path
     :serial-number
     :process-names))))
 (cl-weave:it
  "divides patterns into bounded batches"
  (let* ((patterns
           (loop for index below 130
                 collect (format nil "PATTERN_~d" index)))
         (batches (ntop.e2e::%n7-pattern-batches patterns)))
    (cl-weave:expect (every (lambda (batch)
                              (<= (length batch)
                                  ntop.e2e::+n7-pattern-batch-size+))
                            batches)
                     :to-be
                     t)
    (cl-weave:expect (length batches) :to-equal 9)
    (cl-weave:expect (apply (function append) batches)
                     :to-equal
                     patterns)))
 (cl-weave:it-each
  ((:initial 0d0 12d0) (:final-second 179d0 1d0))
  "bounds each command timeout for the ~A scan"
  (case-name elapsed-seconds expected-seconds)
  (declare (ignore case-name))
  (cl-weave:expect
   (ntop.e2e::%n7-command-timeout elapsed-seconds)
   :to-equal
   expected-seconds))
 (cl-weave:it
  "rejects an exhausted scan deadline"
  (cl-weave:expect
   (lambda () (ntop.e2e::%n7-command-timeout 180d0))
   :to-throw
   (quote error)))
 (cl-weave:it-each
  ((:found "N7_TEST_SENTINEL" :found 1)
   (:later-batch "N7_TEST_SENTINEL" :found 65)
   (:absent "N7_ABSENT_SENTINEL" :absent 1))
  "searches nested files for the ~A pattern"
  (case-name pattern expected-outcome pattern-count)
  (declare (ignore case-name))
  (n7-test-with-root
   "fixture contains N7_TEST_SENTINEL"
   (lambda (root)
     (let* ((patterns
              (append
               (loop for index below (1- pattern-count)
                     collect (format nil "N7_ABSENT_~d" index))
               (list pattern)))
            (outcome
              (ntop.e2e::%search-patterns
               root
               patterns
               (lambda () :found)
               (lambda () :absent))))
       (cl-weave:expect outcome :to-be expected-outcome)))))
 (cl-weave:it-each
  ((:present "entry \"SerialKey\" = \"SERIAL_FIXTURE\"" "SERIAL_FIXTURE")
   (:missing "entry without a serial value" nil))
  "extracts the value for the ~A serial field"
  (case-name output expected)
  (declare (ignore case-name))
  (cl-weave:expect
   (ntop.e2e::%value-after-key output "SerialKey")
   :to-equal
   expected))
 (cl-weave:it-each
  ((:ordinary "fixture-worker" ("fixture-worker"))
   (:path "runtime/fixture-worker" ("runtime/fixture-worker" "fixture-worker"))
   (:probe "grep" nil)
   (:lisp-runtime "sbcl" nil)
   (:shell-runtime "bash" nil))
  "builds process patterns for the ~A command"
  (case-name line expected-patterns)
  (declare (ignore case-name))
  (cl-weave:expect
   (ntop.e2e::%process-line-patterns line)
   :to-equal
   expected-patterns)))
(cl-weave:describe
 "N7 generic content patterns"
 (cl-weave:it
  "matches generic markers and skips Nix attribute references"
  (let* ((user-path (format nil "~CUsers~Cfixture~CLibrary" #\/ #\/ #\/))
         (local-hostname (concatenate 'string "workstation" "." "local"))
         (multi-label-hostname
           (concatenate 'string "api" "." "workstation" "." "local"))
         (invalid-hostname
           (concatenate 'string "bad.." "fixture" "." "local"))
         (nix-attribute (format nil "~A.~A = {}" "checks" "local"))
         (nested-nix-attribute
           (format nil "~A.~A.~A = {}" "checks" "feature" "local")))
    (cl-weave:expect (ntop.e2e::%n7-generic-line-p user-path) :to-be t)
    (cl-weave:expect
     (ntop.e2e::%n7-generic-line-p local-hostname)
     :to-be
     t)
    (cl-weave:expect
     (ntop.e2e::%n7-generic-line-p multi-label-hostname)
     :to-be
     t)
    (cl-weave:expect
     (ntop.e2e::%n7-generic-line-p invalid-hostname)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.e2e::%n7-generic-line-p nix-attribute)
     :to-be
     nil)
    (cl-weave:expect
     (ntop.e2e::%n7-generic-line-p nested-nix-attribute)
     :to-be
     nil)
    (cl-weave:expect
     (lambda ()
       (n7-test-with-root
        user-path
        (lambda (root)
          (ntop.e2e::%n7-scan-root root (list "N7_UNMATCHED_TEST_MARKER")))))
     :to-throw
     (quote error))
    (cl-weave:expect
     (n7-test-with-root
      nix-attribute
      (lambda (root)
        (ntop.e2e::%n7-scan-root root (list "N7_UNMATCHED_TEST_MARKER"))))
     :to-equal
     0)
    (cl-weave:expect
     (n7-test-with-root
      nested-nix-attribute
      (lambda (root)
        (ntop.e2e::%n7-scan-root root (list "N7_UNMATCHED_TEST_MARKER"))))
     :to-equal
     0)))
 (cl-weave:it
  "scans generic patterns without runtime-derived values"
  (let* ((ntop.e2e::*n7-pattern-sources* nil)
         (user-path (format nil "~CUsers~Cfixture~CLibrary" #\/ #\/ #\/))
         (nix-attribute (format nil "~A.~A = {}" "checks" "local")))
    (cl-weave:expect
     (n7-test-with-root
      nix-attribute
      (lambda (root)
        (ntop.e2e:run-n7-generic :root root)))
     :to-equal
     0)
    (cl-weave:expect
     (lambda ()
       (n7-test-with-root
        user-path
        (lambda (root)
          (ntop.e2e:run-n7-generic :root root))))
     :to-throw
     (quote error)))))
