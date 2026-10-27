(in-package #:ntop.e2e)
(defun %route-manual-procedure (route subcase input context) (declare (ignore subcase context)) (format nil "Run ~a and verify ~a." input (route-expected route)))
