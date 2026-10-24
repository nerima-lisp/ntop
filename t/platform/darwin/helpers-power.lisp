(in-package #:ntop.test)

(defconstant +battery-boundary-command-timeout-seconds+
  5)

(defconstant +battery-boundary-output-limit+
  8192)

(defun %percent-values-from-output (output)
  "Extract integer percentages from captured pmset output."
  (let ((cursor 0)
        (values nil))
    (loop for end = (position #\% output :start cursor)
          while end
          do (let ((start end))
               (loop while (and (plusp start)
                                (digit-char-p (char output (1- start))))
                     do (decf start))
               (when (and (< start end)
                          (or (zerop start)
                              (not (char= (char output (1- start)) #\.))))
                 (let ((value (parse-integer output :start start :end end)))
                   (when (<= 0 value 100)
                     (push value values))))
               (setf cursor (1+ end))))
    (nreverse values)))

(defun %pmset-battery-reading ()
  "Read a bounded charge and status sample from pmset."
  (let* ((result
          (host-kit:run-program
           "pmset" '("-g" "batt")
           :timeout +battery-boundary-command-timeout-seconds+
           :max-output-characters +battery-boundary-output-limit+))
         (output (host-kit:process-result-stdout result)))
    (unless (zerop (host-kit:process-result-exit-code result))
      (error "pmset failed while reading battery status."))
    (when (or (host-kit:process-result-stdout-truncated-p result)
              (host-kit:process-result-stderr-truncated-p result))
      (error "pmset output exceeded its capture limit."))
    (let ((charge
           (or (first (%percent-values-from-output output))
               (error "pmset reported no battery charge percentage.")))
          (status
           (cond
            ((search "not charging" output :test #'char-equal) :unknown)
            ((search "discharging" output :test #'char-equal) :discharging)
            ((search "charging" output :test #'char-equal) :charging)
            ((search "finishing charge" output :test #'char-equal) :charging)
            ((search "charged" output :test #'char-equal) :full)
            (t nil))))
      (unless status
        (error "pmset reported no supported battery status."))
      (values charge status))))
