(in-package #:ntop.e2e)

(defun %route-side-pair
    (route subcase evidence-index mode btm ntop evidence-directory config-path
           rows columns rate timeout)
  (let* ((description (%route-input-description route subcase))
         (route-id (route-id route))
         (effective-rate
          (if (member route-id '("key-20" "key-21") :test #'string=)
              (max rate 1000)
            rate))
         (context (or (getf subcase :context) ""))
         (reference-prefix
          (namestring
           (merge-pathnames
            (format nil "~a-~3,'0d-reference" route-id evidence-index)
            evidence-directory)))
         (candidate-prefix
          (namestring
           (merge-pathnames
            (format nil "~a-~3,'0d-candidate" route-id evidence-index)
            evidence-directory)))
         (base-args (progn
                      (%write-text config-path
                                   (let ((*btm-layout* (%route-config-layout subcase)))
                                     (btm-config-string
                                      (if (member route-id
                                                  '("resize" "mouse-75" "mouse-77") :test #'string=)
                                          (max rate 1000)
                                        effective-rate))))
                      (%route-base-arguments btm config-path route subcase evidence-directory)))
         (candidate-args (%route-base-arguments ntop config-path route subcase
                                                evidence-directory)))
    (cond
     ((and (eq (route-kind route) :flag)
           (getf subcase :invalid-value))
      (%route-run-invalid-flag-pair
       route description base-args candidate-args mode btm ntop reference-prefix
       candidate-prefix timeout))
     ((and (eq (route-kind route) :flag)
           (string= (ntop.app.domain:parity-flag-snake-case (route-entry route))
                    "hide_k_threads")
           (not (member :linux *features*)))
      (list :status :manual
            :passed nil
            :input description
            :expected (route-expected route)
            :actual (format nil "skipped on ~a; valid flag requires Linux"
                            (software-type))
            :cause "this host cannot verify the Linux-only flag effect"
            :first-difference nil
            :evidence nil))
     ((and (eq (route-kind route) :flag)
           (string= (ntop.app.domain:parity-flag-value-type (route-entry route))
                    "control"))
      (%route-run-control-pair route description base-args candidate-args mode
                               btm ntop reference-prefix candidate-prefix timeout))
     ((member route-id
              '("restore-normal" "restore-sigint" "restore-sigterm"
                                 "restore-sighup")
              :test #'string=)
      (%route-run-terminal-pair
       route subcase description btm ntop reference-prefix candidate-prefix rows
       columns timeout config-path evidence-directory))
     ((string= route-id "restore-exception")
      (%route-run-exception route subcase description ntop candidate-prefix rows
                            columns timeout))
     ((string= route-id "resize")
      ;; A captured resized frame took 20.583 seconds; keep the allowance local.
      (%route-run-resize-pair
       route subcase description btm ntop reference-prefix candidate-prefix rate
       (max timeout 45) config-path evidence-directory))
     ((or (string= route-id "sleep-child")
          (eq (route-kind route) :column)
          (string-equal context "kill dialog")
          (and (eq (route-kind route) :key)
               (string-equal context "process table")
               (member (route-action route)
                       (quote ("Delete selected process" "Open process kill dialog"))
                       :test (function string=)))
          (and (eq (route-kind route) :flag)
               (member (route-action route)
                       (quote ("read_only" "disable_advanced_kill"))
                       :test (function string=)))
          (and (eq (route-kind route) :mouse)
               (string= (route-action route) "Select process row")))
      (%route-run-sleep-child-pair
       route subcase description mode btm ntop reference-prefix candidate-prefix
       rows columns rate timeout config-path evidence-directory))
     ((and (eq (route-kind route) :key)
           (string= (route-action route) "Quit"))
      (%route-run-exit-pair
       route subcase description mode btm ntop reference-prefix candidate-prefix
       rows columns rate timeout config-path evidence-directory))
     (t
      (%route-run-interactive-pair
       route subcase description mode btm ntop reference-prefix candidate-prefix
       rows columns effective-rate timeout config-path evidence-directory)))))
