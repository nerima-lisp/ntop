(in-package #:ntop.test)

(defun process-query-test-row (profile)
  (let* ((no-process-p (eq profile :no-process))
         (special-name (eq profile :special-name)))
    (ntop.process.domain::make-process-row
     :count (unless no-process-p 1)
     :pid (unless no-process-p 4242)
     :name (unless no-process-p
             (cond
               (special-name "a or b Thread<15> a=b a!=b a(b) a.b !")
               ((eq profile :operator-words) "a z b")
               ((eq profile :cjk-name) "施氏食獅史")
               ((eq profile :hindi-name) "परीक्षा")
               ((eq profile :emoji-name) "❤️🇨🇦❤️")
               ((eq profile :han-alternates) "施氏食獅史 and 施氏食狮史")
               ((eq profile :mixed-unicode) "施氏食獅史test")
               (t "Alpha Worker")))
     :command (unless no-process-p "/usr/bin/alpha --serve")
     :cpu-percent (case profile
                    ((:no-cpu :no-process) nil)
                    (:cpu-zero 0.0)
                    (otherwise 42.5))
     :memory-percent (unless (or (eq profile :no-memory) no-process-p) 25.0)
     :memory-bytes (unless no-process-p (* 1024 1024 1024))
     :virtual-memory-bytes (unless no-process-p (* 2 1024 1024 1024))
     :read-bytes-per-second
     (unless (or (eq profile :no-read) no-process-p) 2048)
     :write-bytes-per-second
     (unless (or (eq profile :no-write) no-process-p) 4096)
     :total-read-bytes (unless no-process-p (* 1024 1024 1024))
     :total-write-bytes (unless no-process-p (* 2 1024 1024 1024))
     :user-name (cond
                  ((or (eq profile :nil-user) no-process-p) nil)
                  ((eq profile :bang-user) "!")
                  (t "root"))
     :state (unless no-process-p
              (if (eq profile :multiword-state) "two words" "sleeping"))
     :elapsed-seconds (cond
                        (no-process-p nil)
                        ((eq profile :no-time) nil)
                        ((eq profile :fractional-time) 5400)
                        (t 3661))
     :priority (unless no-process-p 20)
     :nice (unless no-process-p 0)
     :gpu-usage-percent
     (unless (or (eq profile :no-gpu-percent) no-process-p) 65.0)
     :gpu-memory-bytes
     (unless (or (eq profile :no-gpu-memory-bytes) no-process-p)
       (* 512 1024 1024))
     :gpu-memory-percent
     (unless (or (eq profile :no-gpu-memory) no-process-p) 12.5))))

(defun process-query-test-result (query &optional regex-p case-sensitive-p whole-word-p)
  (let ((result nil))
    (ntop.process.domain::compile-process-search
     query regex-p case-sensitive-p whole-word-p
     (lambda (predicate)
       (setf result (list :predicate predicate)))
     (lambda (syntax-error)
       (setf result (list :syntax-error syntax-error))))
    result))

(defun process-query-test-syntax-error (query &optional regex-p case-sensitive-p whole-word-p)
  (let ((result (process-query-test-result
                 query regex-p case-sensitive-p whole-word-p)))
    (and (eq (first result) :syntax-error)
         (second result))))

(cl-weave:describe "process search evaluation"
  (cl-weave:it-each
   (("Alpha" :base t)
    ("alpha" :base t)
    ("ALPHA" :base t)
    ("Alpha Worker" :base t)
    ("Worker" :base t)
    ("Gamma" :base nil)
    ("^Alpha" :base nil)
    ("^alpha" :base nil)
    ("." :base nil)
    ("\"\"" :base t)
    ("\"Alpha Worker\"" :base t)
    ("Alpha and Worker" :base t)
    ("alpha beta" :base nil)
    ("!Gamma" :base t)
    ("!Alpha" :base nil)
    ("!!Alpha" :base t)
    ("!(Alpha or Gamma)" :base nil)
    ("Alpha or Gamma" :base t)
    ("Alpha || Gamma" :base t)
    ("Alpha && Worker" :base t)
    ("Alpha and Gamma" :base nil)
    ("Alpha and Gamma or Worker" :base t)
    ("Alpha or Gamma and state = running" :base t)
    ("(Alpha and Gamma) or Worker" :base t)
    ("Alpha and (Gamma or Worker)" :base t)
    ("(Alpha or Gamma) and Worker" :base t)
    ("(Alpha or Gamma) and Gamma" :base nil)
    ("Alpha Worker" :base t)
    ("alpha worker" :base t)
    ("a or b" :operator-words t)
    ("\"a or b\"" :operator-words nil)
    ("\"a z b\"" :operator-words t)
    ("\"Thread<15>\"" :special-name t)
    ("\"a=b\"" :special-name t)
    ("\"a!=b\"" :special-name t)
    ("\"a(b)\"" :special-name t)
    ("a.b" :special-name t)
    ("\"!\"" :special-name t)
    ("\"\" or Alpha" :base t)
    ("pid = 4242" :base t)
    ("pid = 42" :base t)
    ("pid = 9" :base nil)
    ("pid != 4242" :base nil)
    ("state = sleeping" :base t)
    ("state = SLEEPING" :base t)
    ("state != running" :base t)
    ("user = root" :base t)
    ("user != alice" :base t)
    ("user = N/A" :nil-user t)
    ("user = root" :nil-user nil)
    ("pid = 4242" :no-process nil)
    ("state = sleeping" :no-process nil)
    ("cpu > 40" :base t)
    ("cpu >= 42.5" :base t)
    ("cpu = 42.5" :base t)
    ("cpu < 42.5" :base nil)
    ("cpu <= 42.5" :base t)
    ("cpu != 42.5" :base nil)
    ("cpu != 0" :base t)
    ("cpu% = 42.5" :base t)
    ("cpu > 30" :base t)
    ("cpu < 20" :base nil)
    ("cpu > 40" :cpu-fallback t)
    ("cpu > 0" :no-cpu nil)
    ("cpu = 0" :no-cpu t)
    ("mem > 20" :base t)
    ("mem = 25" :base t)
    ("mem% < 26" :base t)
    ("memb = 1 GiB" :base t)
    ("memb >= 1024 MiB" :base t)
    ("memb > 1 GB" :base t)
    ("memb > 1000 MB" :base t)
    ("memb = 1073741824 B" :base t)
    ("memb > 1 TiB" :base nil)
    ("mem > 20" :no-memory nil)
    ("read > 2000" :base t)
    ("r/s = 2 KiB" :base t)
    ("rps = 2048" :base t)
    ("read > 1" :no-read nil)
    ("write = 4 KiB" :base t)
    ("write > 0" :no-write nil)

    ("w/s = 4096" :base t)

    ("wps < 4096" :base nil)

    ("wps <= 4096" :base t)
    ("tread = 1 GiB" :base t)
    ("t.read >= 1024 MiB" :base t)
    ("twrite = 2 GiB" :base t)
    ("t.write = 2 GiB" :base t)
    ("time = 1h1m1s" :base t)
    ("time > 1h" :base t)
    ("time < 2h" :base t)
    ("time > 2h" :base nil)
    ("time = 1h" :base nil)
    ("time > 0s" :no-time nil)
    ("nice = 0" :base t)
    ("priority = 20" :base t)
    ("priority < 20" :base nil)
    ("gpu% = 65" :base t)
    ("gmem = 512 MiB" :base t)
    ("gmem% = 12.5" :base t)
    ("gmem > 1 GiB" :base nil)
    ("gmem% = 50" :no-gpu-memory nil)
    ("state = \"sleeping\"" :base t)
    ("state = \"two words\"" :multiword-state t)
    ("user = \"!\"" :bang-user t)
    ("user != \"!\"" :bang-user nil)
    ("a \"or\" b" :special-name t)
    ("cpu!=42.5" :base nil)
    ("time != 1h" :base t)
    ("state != sleeping" :base nil)
    ("memb != 1 GiB" :base nil)
    ("食" :cjk-name t)
    ("\"食\"" :cjk-name t)
    ("परीक्षा" :hindi-name t)
    ("🇨🇦" :emoji-name t)
    ("獅 or 狮" :han-alternates t)
    ("食 or test" :mixed-unicode t)
    ("pid=4242" :base t)
    ("state=sleeping" :base t)
    ("user=root" :base t)
    ("cpu>=42.5" :base t)
    ("(memb = 1 MiB || state = \"sleeping\")" :base t)
    ("Alpha" :no-process nil)
    ("gpu% > 0" :no-gpu-percent nil)
    ("gmem > 0" :no-gpu-memory-bytes nil)
    ("cpu = 0.0000000000000001" :cpu-zero t)
    ("cpu != 0.0000000000000001" :cpu-zero nil)
    ("cpu = 0.000000000000001" :cpu-zero nil)
    ("cpu != 0.000000000000001" :cpu-zero t)
    ("!!(cpu > 5)" :base t)
    ("!(cpu > 0)" :no-cpu t)
    ("mem = 0" :no-memory t)
    ("gmem% = 0" :no-gpu-memory t)
    ("gpu% = 0" :no-gpu-percent t)
    ("time = 1.5h" :fractional-time t)
    ("!(cpu = 0)" :no-cpu nil))
   "query ~S matches profile ~S as ~S"
   (query profile expected)
   (let ((result (process-query-test-result query)))
     (cl-weave:expect (first result) :to-be :predicate)
     (cl-weave:expect
      (funcall (second result) (process-query-test-row profile))
      :to-be expected)))

  (cl-weave:it-each
   (("Alpha" :base nil nil nil t)
    ("alpha" :base nil t nil nil)
    ("Alpha" :base nil t nil t)
    ("alpha" :base nil t nil nil)
    ("\"Alpha Worker\"" :base nil nil t t)
    ("Alpha" :base nil nil t nil)
    ("^Alpha" :base t nil nil t)
    ("\"^[A-Z][a-z]+ Worker$\"" :base t t t t)
    ("." :base t nil nil t)
    ("\"Alpha|Gamma\"" :base t nil nil t)
    ("Alpha Worker" :base nil nil t nil))
   "query ~S honors regex ~S, case-sensitive ~S, and whole-field ~S"
   (query profile regex-p case-sensitive-p whole-word-p expected)
   (let ((result (process-query-test-result
                  query regex-p case-sensitive-p whole-word-p)))
     (cl-weave:expect (first result) :to-be :predicate)
     (cl-weave:expect
      (funcall (second result) (process-query-test-row profile))
      :to-be expected))))

(cl-weave:describe "process search syntax"
  (cl-weave:it-each
   (("(" 1 1)
    (")" 0 1)
    ("cpu > nope" 6 10)
    ("cpu >" 5 5)
    ("alpha and" 9 9)
    ("alpha or )" 9 10)
    ("é and cpu > nope" 12 16)
    ("\"unterminated" 0 13)
    ("()" 0 1)
    ("!" 0 1)
    ("state =" 7 7)
    ("a or" 4 4)
    ("a >" 2 3)
    ("cpu != abc" 7 10)
    ("cpu = ! 5" 6 7)
    ("cpu =!" 5 6)
    ("pid = !" 6 7)
    ("user = !" 7 8)
    ("state !" 6 7)
    ("!cpu > 5" 0 1)
    ("! cpu > 5" 0 1)
    ("user != !" 8 9)
    ("gmem = \"what\"" 7 13)
    ("cpu = 食" 6 7)
    ("gmem% = \"the\"" 8 13)
    ("gpu% = \"heck\"" 7 13)
    ("user !=" 7 7)
    ("state !=" 8 8)
    ("time !=" 7 7)
    ("cpu = \"42.5\"" 6 12)
    ("time = \"1h\"" 7 11))
   "invalid query ~S has character span ~S to ~S"
   (query start-offset end-offset)
   (let ((syntax-error (process-query-test-syntax-error query)))
     (cl-weave:expect
      (typep syntax-error
             'ntop.process.domain::process-search-syntax-error)
      :to-be t)
     (cl-weave:expect
      (list
       (ntop.process.domain::process-search-syntax-error-start-offset
        syntax-error)
       (ntop.process.domain::process-search-syntax-error-end-offset
        syntax-error))
      :to-equal (list start-offset end-offset))))
  (cl-weave:it-each
   (("cpu = 1.2.3" 6 11)
    ("cpu = 1e2e3" 6 11)
    ("cpu = 1e309" 6 11)
    ("cpu = 1e-325" 6 12)
    ("cpu 5" 0 3)
    ("time 5" 0 4)
    ("time = 1" 7 8)
    ("time = xyz" 7 10)
    ("time = 1fortnight" 7 17)
    ("time = 18446744074s" 7 19))
   "invalid numeric or duration query ~S has character span ~S to ~S"
   (query start-offset end-offset)
   (let ((syntax-error (process-query-test-syntax-error query)))
     (cl-weave:expect
      (typep syntax-error
             'ntop.process.domain::process-search-syntax-error)
      :to-be t)
     (cl-weave:expect
      (list
       (ntop.process.domain::process-search-syntax-error-start-offset
        syntax-error)
       (ntop.process.domain::process-search-syntax-error-end-offset
        syntax-error))
      :to-equal (list start-offset end-offset))))

  (cl-weave:it "reports the regex kit position for an invalid expression"
  (let ((syntax-error (process-query-test-syntax-error "[Alpha" t)))
    (cl-weave:expect
     (typep syntax-error
            'ntop.process.domain::process-search-syntax-error)
     :to-be t)
    (cl-weave:expect
     (list
      (ntop.process.domain::process-search-syntax-error-start-offset
       syntax-error)
      (ntop.process.domain::process-search-syntax-error-end-offset
       syntax-error))
     :to-equal '(6 6))))
  (cl-weave:it
   "rejects oversized and overnested queries at their exact spans"
   (let* ((maximum-depth
            ntop.process.domain::*process-search-maximum-depth*)
          (too-long-number
            (concatenate 'string "cpu = "
                         (make-string 129 :initial-element #\1)))
          (too-long-duration
            (concatenate 'string "time = "
                         (make-string 129 :initial-element #\1)))
          (too-long-source
            (make-string (1+ cl-parser-kit:*maximum-tokenizer-source-length*)
                         :initial-element #\a))
          (too-deep-groups
            (concatenate 'string
                         (make-string (1+ maximum-depth) :initial-element #\()
                         "Alpha"
                         (make-string (1+ maximum-depth) :initial-element #\))))
          (too-deep-negations
            (concatenate 'string
                         (make-string (1+ maximum-depth) :initial-element #\!)
                         "Alpha"))
          (cases
            (list (list too-long-number 6 (length too-long-number))
                  (list too-long-duration 7 (length too-long-duration))
                  (list too-long-source 0 (length too-long-source))
                  (list too-deep-groups (1+ maximum-depth)
                        (+ (1+ maximum-depth) (length "Alpha")))
                  (list too-deep-negations (1+ maximum-depth)
                        (+ (1+ maximum-depth) (length "Alpha"))))))
     (dolist (case cases)
       (destructuring-bind (query start-offset end-offset) case
         (let ((syntax-error (process-query-test-syntax-error query)))
           (cl-weave:expect
            (typep syntax-error
                   'ntop.process.domain::process-search-syntax-error)
            :to-be t)
           (cl-weave:expect
            (list
             (ntop.process.domain::process-search-syntax-error-start-offset
              syntax-error)
             (ntop.process.domain::process-search-syntax-error-end-offset
              syntax-error))
            :to-equal (list start-offset end-offset))))))))
(defun gen-process-search-query ()
  (cl-weave:gen-recursive
   (cl-weave:gen-member
    (quote ("Alpha" "pid = 4242" "cpu >= 10" "state = sleeping" "memb > 1 MiB")))
   (lambda (self)
     (cl-weave:gen-one-of
      (cl-weave:gen-map
       (lambda (query) (format nil "!(~A)" query))
       self)
      (cl-weave:gen-map
       (lambda (pair) (format nil "(~A and ~A)" (first pair) (second pair)))
       (cl-weave:gen-tuple self self))
      (cl-weave:gen-map
       (lambda (pair) (format nil "(~A or ~A)" (first pair) (second pair)))
       (cl-weave:gen-tuple self self))))
   :max-depth 3))

(cl-weave:describe "process search AST serialization"
  (cl-weave:around-each
   (next)
   (let ((cl-weave:*property-seed* 20261024))
     (funcall next)))
  (cl-weave:it-property
   "nested queries round-trip through the canonical AST printer"
   ((query (gen-process-search-query)))
   (let* ((ast (ntop.process.domain::parse-process-search query))
          (canonical
            (ntop.process.domain::print-process-search-ast ast))
          (round-tripped
            (ntop.process.domain::parse-process-search canonical)))
     (cl-weave:expect round-tripped :to-equal ast)
     (cl-weave:expect
      (ntop.process.domain::print-process-search-ast round-tripped)
      :to-equal canonical)))
  (cl-weave:it
   "parses double negation around a numeric group"
   (cl-weave:expect
    (ntop.process.domain::parse-process-search "!!(cpu > 5)")
    :to-equal
    (quote
     (:not
      (:not
       (:compare :cpu-percent :greater 5.0d0))))))
  (cl-weave:it-each
   (((:true) "\"\"" (:true))
    ((:compare :cpu-percent :equal 5) "cpu=5" nil)
    ((:compare :cpu-percent :equal 5.0d0) "cpu=5.0000000000000000e+0" (:compare :cpu-percent :equal 5.0d0))
    ((:compare :cpu-percent :not-equal 5.0d0) "cpu!=5.0000000000000000e+0" (:compare :cpu-percent :not-equal 5.0d0))
    ((:compare :cpu-percent :less 5.0d0) "cpu<5.0000000000000000e+0" (:compare :cpu-percent :less 5.0d0))
    ((:compare :cpu-percent :greater 5.0d0) "cpu>5.0000000000000000e+0" (:compare :cpu-percent :greater 5.0d0))
    ((:compare :cpu-percent :less-or-equal 5.0d0) "cpu<=5.0000000000000000e+0" (:compare :cpu-percent :less-or-equal 5.0d0))
    ((:compare :cpu-percent :greater-or-equal 5.0d0) "cpu>=5.0000000000000000e+0" (:compare :cpu-percent :greater-or-equal 5.0d0))
    ((:compare :elapsed-seconds :equal 1.5d0) "time=1.5s" (:compare :elapsed-seconds :equal 1.5d0))
    ((:compare :elapsed-seconds :greater-or-equal 2.0d0) "time>=2s" (:compare :elapsed-seconds :greater-or-equal 2.0d0))
    ((:not (:match :name "Alpha")) "!\"Alpha\"" (:not (:match :name "Alpha")))
    ((:not (:compare :cpu-percent :greater 5.0d0)) "!(cpu>5.0000000000000000e+0)" (:not (:compare :cpu-percent :greater 5.0d0)))
    ((:and (:match :name "Alpha") (:or (:match :name "Beta") (:match :name "Gamma"))) "\"Alpha\" and (\"Beta\" or \"Gamma\")" (:and (:match :name "Alpha") (:or (:match :name "Beta") (:match :name "Gamma")))))
   "AST ~S has canonical spelling ~S"
   (ast canonical round-tripped)
   (let ((printed (ntop.process.domain::print-process-search-ast ast)))
     (cl-weave:expect printed :to-equal canonical)
     (when round-tripped
       (cl-weave:expect
        (ntop.process.domain::parse-process-search printed)
        :to-equal round-tripped))))
  (cl-weave:it-each
   (("cpu = +5" (:compare :cpu-percent :equal 5.0d0))
    ("cpu = -5" (:compare :cpu-percent :equal -5.0d0))
    ("cpu = 1e-3" (:compare :cpu-percent :equal 0.001d0))
    ("cpu = 1e+3" (:compare :cpu-percent :equal 1000.0d0))
    ("cpu = 1e-324" (:compare :cpu-percent :equal 0.0d0)))
   "numeric query ~S parses to ~S"
   (query expected)
   (cl-weave:expect
    (ntop.process.domain::parse-process-search query)
    :to-equal expected))
  (cl-weave:it
   "preserves a quoted regex alternation term"
   (cl-weave:expect
    (ntop.process.domain::parse-process-search "\"Alpha|Gamma\"")
    :to-equal
    (quote (:match :name "Alpha|Gamma")))))
