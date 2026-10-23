(in-package #:ntop.test)

(cl-weave:describe "memory domain calculations"
                   (cl-weave:it-each
                    ((:mixed-pages 100 20 30 10 4096 491520)
                     (:large-page-size 5 1 1 2 16384 114688)
                     (:zero-pages 0 0 0 0 4096 0)
                     (:negative-result 10 20 0 0 4096 -40960))
                    "~A computes macOS memory usage from page counts"
                    (case-name internal-pages
                               purgeable-pages
                               wire-pages
                               compressor-pages
                               page-size
                               expected)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.memory.domain::memory-used-bytes-from-pages
                      internal-pages
                      purgeable-pages
                      wire-pages
                      compressor-pages
                      page-size)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((:missing-internal nil 1 1 1 4096)
                     (:missing-purgeable 1 nil 1 1 4096)
                     (:missing-wire 1 1 nil 1 4096)
                     (:missing-compressor 1 1 1 nil 4096)
                     (:malformed-internal 1.0 1 1 1 4096)
                     (:malformed-purgeable 1 1/2 1 1 4096)
                     (:malformed-wire 1 1 "bad" 1 4096)
                     (:malformed-compressor 1 1 1 :bad 4096)
                     (:negative-internal -1 1 1 1 4096)
                     (:negative-purgeable 1 -1 1 1 4096)
                     (:negative-wire 1 1 -1 1 4096)
                     (:negative-compressor 1 1 1 -1 4096)
                     (:zero-page-size 1 1 1 1 0)
                     (:negative-page-size 1 1 1 1 -4096)
                     (:malformed-page-size 1 1 1 1 4096.0))
                    "~A returns NIL for unavailable or invalid page inputs"
                    (case-name internal-pages
                               purgeable-pages
                               wire-pages
                               compressor-pages
                               page-size)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.memory.domain::memory-used-bytes-from-pages
                      internal-pages
                      purgeable-pages
                      wire-pages
                      compressor-pages
                      page-size)
                     :to-be
                     nil))
                   (cl-weave:it-each
                    ((:ordinary 1000 200 800) (:zero 0 0 0)
                     (:unclamped-negative 100 200 -100))
                    "~A computes cache usage without clamping the difference"
                    (case-name available-bytes free-bytes expected)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.memory.domain::memory-cache-used-bytes
                      available-bytes
                      free-bytes)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((:missing-available nil 20) (:missing-free 100 nil)
                     (:malformed-available "100" 20)
                     (:malformed-free 100 20.0))
                    "~A preserves unavailable cache inputs as NIL"
                    (case-name available-bytes free-bytes)
                    (declare (ignore case-name))
                    (cl-weave:expect
                     (ntop.memory.domain::memory-cache-used-bytes
                      available-bytes
                      free-bytes)
                     :to-be
                     nil))
                   (cl-weave:around-each (next)
                                         (let ((cl-weave:*property-seed*
                                                20261024))
                                           (funcall next)))
                   (cl-weave:it-property
                    "memory page usage follows the documented formula"
                    ((values
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 0 :max 1000000)
                       (cl-weave:gen-integer :min 1 :max 65536))))
                    (destructuring-bind (internal-pages purgeable-pages
                                                        wire-pages
                                                        compressor-pages
                                                        page-size) values
                      (cl-weave:expect
                       (ntop.memory.domain::memory-used-bytes-from-pages
                        internal-pages
                        purgeable-pages
                        wire-pages
                        compressor-pages
                        page-size)
                       :to-equal
                       (*
                        (+ (- internal-pages purgeable-pages)
                           wire-pages
                           compressor-pages)
                        page-size))))
                   (cl-weave:it-property
                    "cache usage always preserves the byte difference"
                    ((values
                      (cl-weave:gen-tuple
                       (cl-weave:gen-integer :min 0 :max 1000000000000)
                       (cl-weave:gen-integer :min 0 :max 1000000000000))))
                    (destructuring-bind (available-bytes free-bytes) values
                      (cl-weave:expect
                       (ntop.memory.domain::memory-cache-used-bytes
                        available-bytes
                        free-bytes)
                       :to-equal
                       (- available-bytes free-bytes)))))
