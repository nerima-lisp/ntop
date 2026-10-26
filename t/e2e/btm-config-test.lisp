(in-package #:ntop.e2e.test)

(defun btm-config-test-contains-p (text fragment)
  (not (null (search fragment text))))

(cl-weave:describe
 "btm configuration projection"
 (cl-weave:it
  "expands declarative layout rows"
  (cl-weave:expect
   (macroexpand-1
    (quote
     (ntop.e2e:define-btm-layout
       ((cpu 2) (mem 1))
       ((net 2) (disk 2) (temp 1))
       ((proc 2 :default)))))
   :to-equal
   (quote
     '(((cpu 2) (mem 1))
      ((net 2) (disk 2) (temp 1))
      ((proc 2 :default))))))
 (cl-weave:it
  "writes the exact configured TOML"
  ;; The expected 1:1:2 row sizing comes from the spec §3.4 reference layout.
  (cl-weave:expect
   (string=
    (ntop.e2e:btm-config-string 700)
    "[flags]
basic = false
dot_marker = false
rate = 700

[processes]
case_sensitive = false
default_grouped = true

[[row]]
ratio = 1
[[row.child]]
ratio = 2
type = \"cpu\"
[[row.child]]
ratio = 1
type = \"mem\"

[[row]]
ratio = 1
[[row.child]]
ratio = 2
type = \"net\"
[[row.child]]
ratio = 2
type = \"disk\"
[[row.child]]
ratio = 1
type = \"temp\"

[[row]]
ratio = 2
[[row.child]]
ratio = 2
type = \"proc\"
default = true

")
   :to-be t))
 (cl-weave:it-each
  ((:minimum-rate 1) (:configured-rate 700) (:higher-rate 1200))
  "uses the runtime sampling rate ~A"
  (case-name rate)
  (declare (ignore case-name))
  (cl-weave:expect
   (btm-config-test-contains-p
    (ntop.e2e:btm-config-string rate)
    (format nil "rate = ~d" rate))
   :to-be t))
 (cl-weave:it
  "preserves custom layouts with nondefault row counts"
  (let* ((ntop.e2e::*btm-layout*
           '(((cpu)) ((mem)) ((net)) ((proc))))
         (config (ntop.e2e:btm-config-string 700)))
    (cl-weave:expect
     (not (null
           (search
            "[[row]]
ratio = 1
[[row.child]]
type = \"proc\""
            config)))
     :to-be t))))
