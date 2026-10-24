(in-package #:ntop.test)

(defun %f4-sample (sampled-at rx-bytes tx-bytes rx-packets tx-packets)
  "Builds a single en0 sample for deterministic rate and total rendering cases."
  (let ((interface
         (ntop.network.domain:make-network-interface :name
                                                     "en0"
                                                     :rx-bytes rx-bytes
                                                     :tx-bytes tx-bytes
                                                     :rx-packets rx-packets
                                                     :tx-packets tx-packets)))
    (ntop.network.domain:make-network-sample :sampled-at sampled-at :interfaces (list interface))))

(defun %f4-network-presentation-samples ()
  "Returns two fixed samples whose counter deltas exercise rate formatting."
  (vector (%f4-sample 1d0 1000 2000 10 20)
          (%f4-sample 3d0 2100 6000 30 40)))

(defun %f4-state (&rest arguments)
  "Constructs presentation state with the baseline options selected by a test."
  (apply (function ntop.network.presentation:make-network-presentation-state)
         arguments))

(defun %f4-snapshot (screen)
  "Returns the rendered contents of SCREEN for text assertions."
  (cl-tty-kit:screen-to-string screen))

(defun %f4-basic-config (&key packets-p start-zeroed-p)
  "Creates the basic network view config with packet and baseline options."
  (ntop.shared.application:make-application-config
   :basic-p t
   :network-show-packets-p packets-p
   :network-start-zeroed-p start-zeroed-p))

(defun %f4-baseline (state)
  "Reads the display baseline selected while rendering STATE."
  (ntop.network.presentation:network-presentation-state-baseline-sample state))

(defun %f4-packet-visible-p (show-packets-p width)
  "Reports whether packet details fit in WIDTH columns under SHOW-PACKETS-P."
  (ntop.network.presentation::%network-packet-legend-visible-p show-packets-p width))

(defun %f4-network-screen-lines (screen)
  "Returns SCREEN rows without right-side cell padding."
  (with-input-from-string (stream (cl-tty-kit:screen-to-string screen))
    (loop for line = (read-line stream nil)
          while line
          collect (string-right-trim (quote (#\Space #\Return)) line))))

(defun %f4-network-golden-lines ()
  "Reads the committed expected rows and signals an error if the fixture is missing."
  (with-open-file
      (stream
       (asdf:system-relative-pathname "ntop/test"
                                      "t/feature/network/goldens/presentation-50x16.txt"))
    (loop for line = (read-line stream nil)
          while line
          collect line)))
