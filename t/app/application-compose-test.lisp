(in-package #:ntop.test)

(defvar *wp12b-composition-help-calls* nil)
(defvar *wp12b-composition-help-capture-p* nil)

(defmethod ntop.ui:draw-widget :around
    ((widget-key (eql :help)) target rectangle state config series)
  (when *wp12b-composition-help-capture-p*
    (push (list target rectangle state config series)
          *wp12b-composition-help-calls*))
  (call-next-method))

(defun %composition-test-gpu-device (name used total)
  (ntop.gpu.domain:make-gpu-device
   :name name :usage-percent 35d0
   :memory-used-bytes used :memory-total-bytes total))

(defun %composition-test-process-sample (sampled-at cpu-time read-bytes)
  (ntop.process.domain:make-process-sample
   :sampled-at sampled-at
   :processes
   (list (ntop.process.domain:make-process-record
          :pid 77 :name "worker" :cpu-time-ns cpu-time
          :read-bytes read-bytes :write-bytes read-bytes))))

(defun %composition-test-snapshot
    (sampled-at &key cpu-usage memory-used gpu-devices network-p process-sample)
  (ntop.app.domain:make-collection-snapshot
   :sampled-at sampled-at
   :cpu-sample
   (when cpu-usage
     (ntop.cpu.domain:make-cpu-sample
      :sampled-at sampled-at :average-usage-percent cpu-usage))
   :memory-sample
   (when memory-used
     (ntop.memory.domain:make-memory-sample
      :sampled-at sampled-at :ram-used-bytes memory-used :ram-total-bytes 100))
   :gpu-sample
   (when gpu-devices
     (ntop.gpu.domain:make-gpu-sample
      :sampled-at sampled-at :devices gpu-devices))
   :network-sample
   (when network-p
     (ntop.network.domain:make-network-sample
      :sampled-at sampled-at :interfaces nil))
   :process-sample process-sample))

(defun %make-composition-test-instance (&key on-action on-render)
  (ntop.app.application:make-application-composition
   (ntop.shared.application:make-application-config)
   :on-action on-action
   :draw-screen
   (lambda (target state config snapshot columns rows reasons series-by-widget)
     (declare (ignore target state config snapshot columns rows reasons))
     (when on-render (funcall on-render series-by-widget)))))

(defun %render-composition-test-instance (composition state snapshot reasons
                                            &optional (rows 24))
  (with-output-to-string (stream)
    (let ((*standard-output* stream))
      (funcall (ntop.app.application:application-composition-render composition)
               state (ntop.shared.application:make-application-config)
               snapshot 80 rows reasons))))

(defun %composition-test-history-store (composition key)
  (gethash key
           (ntop.app.application::application-composition-history-index
            composition)))
(cl-weave:describe "application composition coverage"
  (cl-weave:it "maps history groups for selected views"
    (loop for (widget view groups)
            in '((:cpu nil (:cpu)) (:memory nil (:memory))
                 (:memory :gpu (:gpu :gpu-memory)) (:gpu nil (:gpu))
                 (:network nil (:network)) (:disks nil nil)
                 (:disks :disk-io-graph (:disk-io))
                 (:disk-io-graph nil (:disk-io))
                 (:temperature nil nil)
                 (:temperature :temperature-graph (:temperature))
                 (:temperature-graph nil (:temperature))
                 (:unknown nil nil))
          for state = (ntop.app.domain:make-app-state :selected-widget widget)
          do (setf (gethash widget (ntop.app.domain:app-state-widget-v-states state))
                   view)
             (cl-weave:expect
              (ntop.app.application::%history-groups-for-state state)
              :to-equal groups)))
  (cl-weave:it "applies history operations to selected stores"
    (let* ((composition (%make-composition-test-instance))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition)))
      (cl-weave:expect (ntop.app.application::%network-visible-samples composition) :to-equal nil)
      (ntop.app.application::%ensure-composition-history composition '(:network :window) "Network")
      (vector-push-extend (ntop.shared.domain:make-sample-record :sampled-at 5d0)
                    (ntop.app.application::application-composition-network-samples composition))
      (cl-weave:expect
       (length (ntop.app.application::%network-visible-samples composition))
       :to-be 1)
      (funcall on-snapshot (%composition-test-snapshot 10d0 :cpu-usage 50d0 :network-p t))
      (flet ((act (kind value)
               (funcall on-action state
                        (ntop.app.domain:make-app-action :kind kind :value value))))
        (loop for (kind value width)
                in '((:zoom :in 45d0) (:zoom :out 60d0) (:zoom 30d0 30d0)
                     (:zoom "invalid" 30d0) (:zoom :reset 60d0)
                     (:history-scroll :invalid 60d0))
              do (act kind value)
                 (cl-weave:expect
                  (ntop.shared.domain:history-store-window-width-seconds
                   (%composition-test-history-store
                    composition '(:cpu :usage :average)))
                  :to-be width))
        (setf (ntop.app.domain:app-state-selected-widget state) :network)
        (ntop.shared.domain:history-scroll-older
         (%composition-test-history-store composition '(:network :window)))
        (act :history-scroll :forward)
        (cl-weave:expect
         (null (ntop.shared.domain::history-store-view-endpoint
                (%composition-test-history-store composition '(:network :window))))
         :to-be t)
        (setf (ntop.app.domain:app-state-selected-widget state) :disks)
        (act :zoom :in)
        (cl-weave:expect
         (ntop.app.domain:app-state-zoom-seconds state)
         :to-be 60d0))))
  (cl-weave:it "routes help search submission and classifies actions"
    (let* ((composition (%make-composition-test-instance))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-action
             (ntop.app.application:application-composition-on-action composition)))
      (flet ((act (kind value)
               (funcall on-action state
                        (ntop.app.domain:make-app-action :kind kind :value value))))
        (act :search :help)
        (act :search :submit)
        (cl-weave:expect (ntop.app.domain:app-state-active-dialog state) :to-be :help)
        (loop for (kind value expected)
                in '((:search "term" t) (:search :submit t)
                     (:search :invalid nil) (:navigation :page-down t)
                     (:navigation :left nil) (:select :cpu t)
                     (:select 7 nil) (:unknown nil nil))
              do (cl-weave:expect
                  (not (null
                        (ntop.app.application::%help-action-p
                         (ntop.app.domain:make-app-action :kind kind :value value))))
                  :to-be expected))))))

