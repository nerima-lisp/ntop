(in-package #:ntop.e2e.test)
(progn
  (defun %route-subcase-variants (route)
    (mapcar (function (lambda (subcase) (getf subcase :variant)))
            (ntop.e2e::route-subcases route)))
  (defun %route-has-subcases-p (route)
    (let ((subcases (ntop.e2e::route-subcases route)))
      (and (consp subcases)
           (every
            (function
             (lambda (subcase)
               (and (listp subcase) (consp subcase))))
            subcases))))
  (defun %route-flag-spellings-p (route)
  (let* ((entry (ntop.e2e::route-entry route))
         (short (ntop.app.domain:parity-flag-short entry))
         (kebab (ntop.app.domain:parity-flag-kebab-case entry))
         (snake (ntop.app.domain:parity-flag-snake-case entry))
         (subcases (ntop.e2e::route-subcases route))
         (variants (%route-subcase-variants route))
         (aliases (ntop.app.domain:parity-flag-aliases entry))
         (actual-aliases
           (loop for subcase in subcases
                 when (eq :alias (getf subcase :variant))
                   collect (getf subcase :alias)))
         (invalid
           (find-if (lambda (subcase)
                      (and (getf subcase :invalid-value)
                           (eq :kebab (getf subcase :variant))))
                    subcases))
         (invalid-count
           (count t subcases
                  :key (lambda (subcase) (getf subcase :invalid-value)))))
    (and (stringp short)
         (stringp kebab)
         (stringp snake)
         (string= kebab (substitute #\- #\_ snake))
         (string= (ntop.e2e::route-action route) snake)
         (member :kebab variants)
         (member :snake variants)
         (eql (not (null (member :short variants)))
              (plusp (length short)))
         (equal aliases actual-aliases)
         (= invalid-count 1)
         invalid)))
  (defun %route-control-flags-p (routes)
    (every
     (function
      (lambda (control)
        (let* ((route
                 (find (first control) routes
                       :key (function ntop.e2e::route-action)
                       :test (function string=)))
               (short
                 (and route
                      (ntop.app.domain:parity-flag-short
                       (ntop.e2e::route-entry route))))
               (variants
                 (and route (%route-subcase-variants route))))
          (and route
               (string= short (second control))
               (member :kebab variants)
               (member :snake variants)
               (member :short variants)))))
     (list (list "help" "-h") (list "version" "-V"))))
  (cl-weave:describe
   "typed route catalog"
   (cl-weave:it
    "counts each typed record and the nine additions once"
    (let* ((parity (ntop.e2e::route-parity-catalog))
           (additional (ntop.e2e::route-additional-catalog))
           (all (ntop.e2e::route-catalog))
           (kinds (list :key :mouse :flag :column :search-field :extension)))
      (cl-weave:expect (ntop.e2e::route-count parity) :to-equal 173)
      (cl-weave:expect
       (mapcar
        (function
         (lambda (kind)
           (cons kind (ntop.e2e::route-kind-count kind parity))))
        kinds)
       :to-equal
       (list (cons :key 68)
             (cons :mouse 12)
             (cons :flag 52)
             (cons :column 20)
             (cons :search-field 17)
             (cons :extension 4)))
      (cl-weave:expect (ntop.e2e::route-count additional) :to-equal 9)
      (cl-weave:expect (ntop.e2e::route-count all) :to-equal 182)))
   (cl-weave:it
    "has nonempty subcases for all 182 routes"
    (let ((routes (ntop.e2e::route-catalog)))
      (cl-weave:expect (ntop.e2e::route-count routes) :to-equal 182)
      (cl-weave:expect (every (function %route-has-subcases-p) routes)
                       :to-be t)))
   (cl-weave:it
    "records every flag spelling and the help and version controls"
    (let* ((parity (ntop.e2e::route-parity-catalog))
           (flags
             (remove-if-not
              (function (lambda (route)
                          (eq :flag (ntop.e2e::route-kind route))))
              parity)))
      (cl-weave:expect (length flags) :to-equal 52)
      (cl-weave:expect (every (function %route-flag-spellings-p) flags)
                       :to-be t)
      (cl-weave:expect (%route-control-flags-p flags) :to-be t)))))
