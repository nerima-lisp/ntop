(in-package #:ntop.test)

(cl-weave:describe "GPU domain metrics"
                   (cl-weave:it-each
                    (("Radeon Fixture" 0 1 t "GPU")
                     ("NVIDIA Fixture" 0 2 t "GPU0")
                     ("Radeon Fixture" 1 2 t "GPU1")
                     ("NVIDIA Fixture" 0 2 nil "NVIDIA Fixture"))
                    "formats device name ~S at index ~D of ~D"
                    (name index count short-names-p expected)
                    (cl-weave:expect
                     (ntop.gpu.domain:gpu-display-name name
                                                       index
                                                       count
                                                       short-names-p)
                     :to-equal
                     expected))
                   (cl-weave:it-each
                    ((4096 8192 50) (0 8192 0) (8192 8192 100) (1 3 100/3))
                    "computes GPU memory usage for ~D of ~D bytes"
                    (used total expected)
                    (cl-weave:expect
                     (ntop.gpu.domain:gpu-memory-percent used total)
                     :to-be
                     expected))
                   (cl-weave:it-each
                    ((nil 8192) (4096 nil)
                     (-1 8192)
                     (8193 8192)
                     (1 0)
                     (1.0 8192))
                    "returns NIL for invalid GPU memory values ~S and ~S"
                    (used total)
                    (cl-weave:expect
                     (ntop.gpu.domain:gpu-memory-percent used total)
                     :to-be
                     nil)))
