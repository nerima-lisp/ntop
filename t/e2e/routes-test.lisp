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
      (cl-weave:expect (%route-control-flags-p flags) :to-be t))))
  (cl-weave:describe
 "BTM self subcase manual classifications"
 (cl-weave:it
  "classifies exactly six expanded BTM self subcases"
  (let* ((routes (ntop.e2e::route-catalog))
         (target-ids
           (list "key-15-002" "flag-113-001" "search-field-153-001"
                 "search-field-154-001" "search-field-155-001" "resize-001")))
    (labels ((manual-targets (mode)
               (loop for route in routes
                     append
                     (loop for subcase in
                             (ntop.e2e::%route-subcases-expanded route)
                           for index from 1
                           for spec =
                             (ntop.e2e::%route-btm-self-manual-spec
                              mode route subcase)
                           when spec
                             collect
                             (format nil "~a-~3,'0d"
                                     (ntop.e2e::route-id route) index))))
             (target-route-subcase-counts ()
               (loop for id in '("key-15" "flag-113" "search-field-153"
                                 "search-field-154" "search-field-155" "resize")
                     for route =
                       (find id routes :key #'ntop.e2e::route-id
                             :test #'string=)
                     collect
                     (cons id
                           (length
                            (ntop.e2e::%route-subcases-expanded route)))))
             (manual-specs-have-procedure-p ()
               (every
                (lambda (route)
                  (loop for subcase in
                          (ntop.e2e::%route-subcases-expanded route)
                        for spec =
                          (ntop.e2e::%route-btm-self-manual-spec
                           "btm-self" route subcase)
                        always
                        (or (null spec)
                            (and (stringp (getf spec :reason))
                                 (plusp (length (getf spec :reason)))
                                 (stringp (getf spec :procedure))
                                 (plusp (length (getf spec :procedure)))))))
                routes)))
      (cl-weave:expect (manual-targets "btm-self") :to-equal target-ids)
      (cl-weave:expect (manual-targets "ntop") :to-equal nil)
      (cl-weave:expect
       (target-route-subcase-counts)
       :to-equal
       (list (cons "key-15" 4)
             (cons "flag-113" 4)
             (cons "search-field-153" 1)
             (cons "search-field-154" 2)
             (cons "search-field-155" 1)
             (cons "resize" 1)))
      (cl-weave:expect (manual-specs-have-procedure-p) :to-be t))))
 (cl-weave:it
  "keeps a failed sibling visible when another subcase is manual"
  (let ((route (ntop.e2e::make-route-case :id "test")))
    (cl-weave:expect
     (ntop.e2e::%route-result-status
      route
      (list (list :passed t)
            (list :status :manual :passed nil)
            (list :passed t)))
     :to-be :manual)
    (cl-weave:expect
     (ntop.e2e::%route-result-status
      route
      (list (list :passed t)
            (list :status :manual :passed nil)
            (list :passed nil)))
     :to-be :fail)))
 (cl-weave:it
  "uses one status classification for subcase rows and totals"
  (cl-weave:expect
   (mapcar #'ntop.e2e::%route-subresult-status
           (list (list :passed t)
                 (list :status :manual :passed nil)
                 (list :passed nil)))
   :to-equal '(:pass :manual :fail)))
 (cl-weave:it
  "recognizes an empty process table as an unavailable mouse target"
  (let ((subcase (list :context "tables"))
        (empty-grid (criterion-test-grid 12 50 ""))
        (visible-grid (criterion-test-grid 12 50 "sshd")))
    (cl-weave:expect
     (ntop.e2e::%route-mouse-target-unavailable-p subcase empty-grid)
     :to-be t)
    (cl-weave:expect
     (ntop.e2e::%route-mouse-target-unavailable-p subcase visible-grid)
     :to-be nil)))))
