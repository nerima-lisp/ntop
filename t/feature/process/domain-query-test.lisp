(in-package #:ntop.test)

(defun process-query-test-row (profile)
  (let* ((no-process-p (eq profile :no-process))
         (special-name (eq profile :special-name)))
    (ntop.process.domain::make-process-row :count
                                           (unless no-process-p
                                             1)
                                           :pid
                                           (unless no-process-p
                                             4242)
                                           :name
                                           (unless no-process-p
                                             (cond
                                               (special-name
                                                "a or b Thread<15> a=b a!=b a(b) a.b !")
                                               ((eq profile :operator-words)
                                                "a z b")
                                               ((eq profile :cjk-name)
                                                "施氏食獅史")
                                               ((eq profile :hindi-name)
                                                "परीक्षा")
                                               ((eq profile :emoji-name)
                                                "❤️🇨🇦❤️")
                                               ((eq profile :han-alternates)
                                                "施氏食獅史 and 施氏食狮史")
                                               ((eq profile :mixed-unicode)
                                                "施氏食獅史test")
                                               (t "Alpha Worker")))
                                           :command
                                           (unless no-process-p
                                             "/usr/bin/alpha --serve")
                                           :cpu-percent
                                           (case profile
                                             ((:no-cpu :no-process) nil)
                                             (:cpu-zero 0.0)
                                             (otherwise 42.5))
                                           :memory-percent
                                           (unless
                                               (or (eq profile :no-memory)
                                                   no-process-p)
                                             25.0)
                                           :memory-bytes
                                           (unless no-process-p
                                             (* 1024 1024 1024))
                                           :virtual-memory-bytes
                                           (unless no-process-p
                                             (* 2 1024 1024 1024))
                                           :read-bytes-per-second
                                           (unless
                                               (or (eq profile :no-read)
                                                   no-process-p)
                                             2048)
                                           :write-bytes-per-second
                                           (unless
                                               (or (eq profile :no-write)
                                                   no-process-p)
                                             4096)
                                           :total-read-bytes
                                           (unless no-process-p
                                             (* 1024 1024 1024))
                                           :total-write-bytes
                                           (unless no-process-p
                                             (* 2 1024 1024 1024))
                                           :user-name
                                           (cond
                                             ((or (eq profile :nil-user)
                                                  no-process-p) nil)
                                             ((eq profile :bang-user) "!")
                                             (t "root"))
                                           :state
                                           (unless no-process-p
                                             (if (eq profile :multiword-state)
                                                 "two words"
                                                 "sleeping"))
                                           :elapsed-seconds
                                           (cond
                                             (no-process-p nil)
                                             ((eq profile :no-time) nil)
                                             ((eq profile :fractional-time)
                                              5400)
                                             (t 3661))
                                           :priority
                                           (unless no-process-p
                                             20)
                                           :nice
                                           (unless no-process-p
                                             0)
                                           :gpu-usage-percent
                                           (unless
                                               (or (eq profile :no-gpu-percent)
                                                   no-process-p)
                                             65.0)
                                           :gpu-memory-bytes
                                           (unless
                                               (or
                                                (eq profile
                                                    :no-gpu-memory-bytes)
                                                no-process-p)
                                             (* 512 1024 1024))
                                           :gpu-memory-percent
                                           (unless
                                               (or (eq profile :no-gpu-memory)
                                                   no-process-p)
                                             12.5))))

(defun process-query-test-result (query &optional
                                        regex-p
                                        case-sensitive-p
                                        whole-word-p
                                        command-p)
  (let ((result nil))
    (ntop.process.domain::compile-process-search query
                                                 regex-p
                                                 case-sensitive-p
                                                 whole-word-p
                                                 command-p
                                                 (lambda (predicate)
                                                   (setf result (list :predicate
                                                                      predicate)))
                                                 (lambda (syntax-error)
                                                   (setf result (list
                                                                 :syntax-error
                                                                 syntax-error))))
    result))

(defun process-query-test-syntax-error (query &optional
                                              regex-p
                                              case-sensitive-p
                                              whole-word-p
                                              command-p)
  (let ((result
         (process-query-test-result query
                                    regex-p
                                    case-sensitive-p
                                    whole-word-p
                                    command-p)))
    (and (eq (first result) :syntax-error) (second result))))

(cl-weave:describe "process search evaluation"
                   (cl-weave:it-each
                    (("Alpha" :base t) ("alpha" :base t)
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
                     ("cpu = 0" :no-cpu nil)
                     ("cpu != 0" :no-cpu nil)
                     ("cpu < 42.5" :no-cpu nil)
                     ("cpu > 42.5" :no-cpu nil)
                     ("cpu <= 42.5" :no-cpu nil)
                     ("cpu >= 42.5" :no-cpu nil)
                     ("mem > 20" :base t)
                     ("mem = 25" :base t)
                     ("mem% < 26" :base t)
                     ("memb = 1 GiB" :base t)
                     ("memb >= 1024 MiB" :base t)
                     ("memb > 1 GB" :base t)
                     ("memb > 1000 MB" :base t)
                     ("memb = 1073741824 B" :base t)
                     ("memb > 1 TiB" :base nil)
                     ("mem < 5" :no-memory nil)
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
                     ("!(cpu > 0)" :no-cpu nil)
                     ("mem = 0" :no-memory nil)
                     ("gmem% = 0" :no-gpu-memory nil)
                     ("gpu% = 0" :no-gpu-percent nil)
                     ("time = 1.5h" :fractional-time t)
                     ("!(cpu = 0)" :no-cpu nil))
                    "query ~S matches profile ~S as ~S"
                    (query profile expected)
                    (let ((result (process-query-test-result query)))
                      (cl-weave:expect (first result) :to-be :predicate)
                      (cl-weave:expect
                       (funcall (second result)
                                (process-query-test-row profile))
                       :to-be
                       expected)))
                   (cl-weave:it-each
                    (("Alpha" :base nil nil nil nil t)
                     ("alpha" :base nil t nil nil nil)
                     ("Alpha" :base nil t nil nil t)
                     ("alpha" :base nil t nil nil nil)
                     ("\"Alpha Worker\"" :base nil nil t nil t)
                     ("Alpha" :base nil nil t nil nil)
                     ("^Alpha" :base t nil nil nil t)
                     ("\"^[A-Z][a-z]+ Worker$\"" :base t t t nil t)
                     ("." :base t nil nil nil t)
                     ("\"Alpha|Gamma\"" :base t nil nil nil t)
                     ("Alpha Worker" :base nil nil t nil nil)
                     ("serve" :base nil nil nil t t)
                     ("serve" :base nil nil nil nil nil))
                    "query ~S/profile ~S: regex ~S case ~S whole ~S command ~S"
                    (query profile
                           regex-p
                           case-sensitive-p
                           whole-word-p
                           command-p
                           expected)
                    (let ((result
                           (process-query-test-result query
                                                      regex-p
                                                      case-sensitive-p
                                                      whole-word-p
                                                      command-p)))
                      (cl-weave:expect (first result) :to-be :predicate)
                      (cl-weave:expect
                       (funcall (second result)
                                (process-query-test-row profile))
                       :to-be
                       expected))))

(defun gen-process-search-query ()
  (cl-weave:gen-recursive
   (cl-weave:gen-member
    (quote ("Alpha" "pid = 4242" "cpu >= 10" "state = sleeping" "memb > 1 MiB")))
   (lambda (self)
     (cl-weave:gen-one-of
      (cl-weave:gen-map
       (lambda (query)
         (format nil "!(~A)" query))
       self)
      (cl-weave:gen-map
       (lambda (pair)
         (format nil "(~A and ~A)" (first pair) (second pair)))
       (cl-weave:gen-tuple self self))
      (cl-weave:gen-map
       (lambda (pair)
         (format nil "(~A or ~A)" (first pair) (second pair)))
       (cl-weave:gen-tuple self self))))
   :max-depth
   3))

(cl-weave:describe "process search AST serialization"
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "nested queries round-trip through the canonical AST printer"
                    ((query (gen-process-search-query)))
                    (let* ((ast
                            (ntop.process.domain::parse-process-search query))
                           (canonical
                            (ntop.process.domain::print-process-search-ast ast))
                           (round-tripped
                            (ntop.process.domain::parse-process-search
                             canonical)))
                      (cl-weave:expect round-tripped :to-equal ast)
                      (cl-weave:expect
                       (ntop.process.domain::print-process-search-ast
                        round-tripped)
                       :to-equal
                       canonical)))
                   (cl-weave:it "parses double negation around a numeric group"
                                (cl-weave:expect
                                 (ntop.process.domain::parse-process-search
                                  "!!(cpu > 5)")
                                 :to-equal
                                 (quote
                                  (:not
                                   (:not (:compare :cpu-percent :greater 5.0d0))))))
                   (cl-weave:it-each
                    (((:true) "\"\"" (:true))
                     ((:compare :cpu-percent :equal 5) "cpu=5" nil)
                     ((:compare :cpu-percent :equal 5.0d0)
                      "cpu=5.0000000000000000e+0"
                      (:compare :cpu-percent :equal 5.0d0))
                     ((:compare :cpu-percent :not-equal 5.0d0)
                      "cpu!=5.0000000000000000e+0"
                      (:compare :cpu-percent :not-equal 5.0d0))
                     ((:compare :cpu-percent :less 5.0d0)
                      "cpu<5.0000000000000000e+0"
                      (:compare :cpu-percent :less 5.0d0))
                     ((:compare :cpu-percent :greater 5.0d0)
                      "cpu>5.0000000000000000e+0"
                      (:compare :cpu-percent :greater 5.0d0))
                     ((:compare :cpu-percent :less-or-equal 5.0d0)
                      "cpu<=5.0000000000000000e+0"
                      (:compare :cpu-percent :less-or-equal 5.0d0))
                     ((:compare :cpu-percent :greater-or-equal 5.0d0)
                      "cpu>=5.0000000000000000e+0"
                      (:compare :cpu-percent :greater-or-equal 5.0d0))
                     ((:compare :elapsed-seconds :equal 1.5d0) "time=1.5s"
                      (:compare :elapsed-seconds :equal 1.5d0))
                     ((:compare :elapsed-seconds :greater-or-equal 2.0d0)
                      "time>=2s"
                      (:compare :elapsed-seconds :greater-or-equal 2.0d0))
                     ((:not (:match :name "Alpha")) "!\"Alpha\""
                      (:not (:match :name "Alpha")))
                     ((:not (:compare :cpu-percent :greater 5.0d0))
                      "!(cpu>5.0000000000000000e+0)"
                      (:not (:compare :cpu-percent :greater 5.0d0)))
                     ((:and (:match :name "Alpha")
                            (:or (:match :name "Beta") (:match :name "Gamma")))
                      "\"Alpha\" and (\"Beta\" or \"Gamma\")"
                      (:and (:match :name "Alpha")
                            (:or (:match :name "Beta") (:match :name "Gamma")))))
                    "AST ~S has canonical spelling ~S"
                    (ast canonical round-tripped)
                    (let ((printed
                           (ntop.process.domain::print-process-search-ast ast)))
                      (cl-weave:expect printed :to-equal canonical)
                      (when round-tripped
                        (cl-weave:expect
                         (ntop.process.domain::parse-process-search printed)
                         :to-equal
                         round-tripped))))
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
                     :to-equal
                     expected))
                   (cl-weave:it "preserves a quoted regex alternation term"
                                (cl-weave:expect
                                 (ntop.process.domain::parse-process-search
                                  "\"Alpha|Gamma\"")
                                 :to-equal
                                 (quote (:match :name "Alpha|Gamma")))))
