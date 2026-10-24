from __future__ import annotations

import os
from pathlib import Path


TEST_TIMEOUT_MS = 40000


def _lisp_string(value: str) -> str:
    return '"' + value.replace("\\", "\\\\").replace('"', '\\"') + '"'


def _driver_source(
    project: Path,
    tests: list[Path],
    support: list[Path],
    empty_test_control: bool,
) -> str:
    root_path = _lisp_string(str(project) + os.sep)
    root_form = f"(uiop:ensure-directory-pathname (truename #P{root_path}))"
    test_forms = " ".join(f"(truename #P{_lisp_string(str(path))})" for path in tests)
    support_loads = "\n".join(
        f"(load (truename #P{_lisp_string(str(path))}))" for path in support
    )
    test_loads = "\n".join(
        f"(load (truename #P{_lisp_string(str(path))}))" for path in tests
    )
    empty_control = "t" if empty_test_control else "nil"
    return f'''(require "asdf")
(defparameter *mutation-project-root* {root_form})
(let ((stage "runner-setup"))
  (handler-case
      (progn
        (setf stage "production-load")
        (asdf:load-asd (merge-pathnames "ntop.asd" *mutation-project-root*))
        (asdf:load-system "ntop")
        (setf stage "cl-weave-load")
        (asdf:load-system "cl-weave"))
    (error (condition)
      (format *error-output* "MUTATION_RUNNER_ERROR_STAGE=~A~%" stage)
      (format *error-output* "MUTATION_RUNNER_ERROR=~A~%" condition)
      (uiop:quit 20))))
(let ((stage "test-package-load")
      (exit-code 0))
  (handler-case
      (let* ((root *mutation-project-root*)
             (targets (list {test_forms}))
             (empty-control {empty_control}))
        (setf stage "test-package-load")
        (load (merge-pathnames "t/package.lisp" root))
        (setf stage "support-load")
        {support_loads}
        (setf stage "selected-test-load")
        {test_loads}
        (setf stage "test-selection")
        (let* ((plan (cl-weave:list-tests
                      :reporter :sexp
                      :stream (make-string-output-stream)
                      :location-filter targets))
               (selected-count (length plan))
               (runnable-count
                 (count :run plan :key #'cl-weave:test-plan-entry-status)))
          (format t "MUTATION_SELECTED_COUNT=~D~%" selected-count)
          (format t "MUTATION_RUNNABLE_COUNT=~D~%" runnable-count)
          (force-output)
          (cond
            (empty-control
             (setf stage "empty-test-control")
             (if (and (zerop selected-count) (zerop runnable-count))
                 (if (cl-weave:run-all
                      :reporter :spec
                      :stream *standard-output*
                      :location-filter targets
                      :timeout-ms {TEST_TIMEOUT_MS}
                      :max-workers 1
                      :pass-with-no-tests t)
                     (format t "MUTATION_EMPTY_CONTROL=confirmed~%")
                     (progn
                       (format t "MUTATION_EMPTY_CONTROL=failed~%")
                       (setf exit-code 20)))
                 (progn
                   (format t "MUTATION_EMPTY_CONTROL=not-empty~%")
                   (setf exit-code 20))))
            ((zerop runnable-count)
             (format t "MUTATION_SELECTION_NO_RUNNABLE=~%"))
            ((zerop selected-count)
             (format t "MUTATION_SELECTION_EMPTY=~%"))
            (t
             (setf stage "test-run")
             (if (cl-weave:run-all
                  :reporter :spec
                  :stream *standard-output*
                  :location-filter targets
                  :timeout-ms {TEST_TIMEOUT_MS}
                  :max-workers 1
                  :pass-with-no-tests nil)
                 (format t "MUTATION_TEST_RESULT=passed~%")
                 (progn
                   (format t "MUTATION_TEST_RESULT=failed~%")
                   (setf exit-code 10)))))))
    (error (condition)
      (format *error-output* "MUTATION_RUNNER_ERROR_STAGE=~A~%" stage)
      (format *error-output* "MUTATION_RUNNER_ERROR=~A~%" condition)
      (setf exit-code 20)))
  (uiop:quit exit-code))
'''


