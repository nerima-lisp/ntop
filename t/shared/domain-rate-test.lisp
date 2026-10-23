(in-package #:ntop.test)

(cl-weave:describe "counter rates use elapsed monotonic time"
  (cl-weave:it-each ((0 1 1 1)
                     (0 1000 2 500)
                     (10 20 2 5)
                     (20 20 3 0)
                     (20 10 2 0)
                     (100 0 1 0)
                     (nil 5 1 nil)
                     (5 nil 1 nil)
                     (nil nil 1 nil))
      "computes rate from ~A and ~A over ~A seconds"
      (previous current elapsed expected)
    (let ((actual
            (ntop.shared.domain:counter-rate-per-second
             previous current elapsed)))
      (cl-weave:expect
       (if (null expected)
           (null actual)
           (and actual (= actual expected)))
       :to-be t)))

  (cl-weave:it "rejects a zero elapsed interval"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.shared.domain:counter-rate-per-second 0 1 0)
           :returned)
       (error () :signaled))
     :to-be :signaled))

  (cl-weave:it "rejects a negative elapsed interval"
    (cl-weave:expect
     (handler-case
         (progn
           (ntop.shared.domain:counter-rate-per-second 0 1 -1)
           :returned)
       (error () :signaled))
     :to-be :signaled))

  (cl-weave:around-each (next)
    (let ((cl-weave:*property-seed* 20261024))
      (funcall next)))

  (cl-weave:it-property "advancing counters yield the exact elapsed rate"
      ((values
        (cl-weave:gen-tuple
         (cl-weave:gen-integer :min 0 :max 1000000)
         (cl-weave:gen-integer :min 0 :max 1000000)
         (cl-weave:gen-integer :min 1 :max 3600))))
    (destructuring-bind (previous increment elapsed) values
      (cl-weave:expect
       (ntop.shared.domain:counter-rate-per-second
        previous (+ previous increment) elapsed)
       :to-satisfy
       (lambda (rate) (= rate (/ increment elapsed))))))

  (cl-weave:it-property "counter wraparound never yields a negative rate"
      ((values
        (cl-weave:gen-tuple
         (cl-weave:gen-integer :min 0 :max 1000000)
         (cl-weave:gen-integer :min 0 :max 1000000)
         (cl-weave:gen-integer :min 1 :max 3600))))
    (destructuring-bind (previous current elapsed) values
      (let ((rate
              (ntop.shared.domain:counter-rate-per-second
               previous current elapsed)))
        (cl-weave:expect rate :to-satisfy (lambda (value) (>= value 0)))))))
