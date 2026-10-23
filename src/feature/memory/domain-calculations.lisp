(in-package #:ntop.memory.domain)

(defun memory-used-bytes-from-pages (internal-pages purgeable-pages
                                                    wire-pages
                                                    compressor-pages
                                                    page-size)
  (when
      (and (integerp internal-pages)
           (not (minusp internal-pages))
           (integerp purgeable-pages)
           (not (minusp purgeable-pages))
           (integerp wire-pages)
           (not (minusp wire-pages))
           (integerp compressor-pages)
           (not (minusp compressor-pages))
           (integerp page-size)
           (plusp page-size))
    (* (+ (- internal-pages purgeable-pages) wire-pages compressor-pages)
       page-size)))

(defun memory-cache-used-bytes (available-bytes free-bytes)
  (when (and (integerp available-bytes) (integerp free-bytes))
    (- available-bytes free-bytes)))
