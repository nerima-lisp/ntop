(in-package #:ntop.test)

(cl-weave:describe "application composition pause"
  (cl-weave:it "keeps the displayed collection snapshot fixed while paused"
    (let* ((drawn-snapshots nil)
           (config (ntop.shared.application:make-application-config))
           (composition
             (ntop.app.application:make-application-composition
              config
              :draw-screen
              (lambda (target draw-state draw-config snapshot columns rows reasons series)
                (declare
                 (ignore target draw-state draw-config columns rows reasons series))
                (push snapshot drawn-snapshots)
                nil)))
           (state (ntop.app.domain:make-app-state :selected-widget :cpu))
           (on-snapshot
             (ntop.app.application:application-composition-on-snapshot composition))
           (on-action
             (ntop.app.application:application-composition-on-action composition))
           (first-snapshot (%composition-test-snapshot 10d0 :cpu-usage 40d0))
           (second-snapshot (%composition-test-snapshot 20d0 :cpu-usage 60d0)))
      (funcall on-snapshot first-snapshot)
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :pause))
      (funcall on-snapshot second-snapshot)
      (%render-composition-test-instance composition state second-snapshot nil)
      (cl-weave:expect (eq (first drawn-snapshots) first-snapshot) :to-be t)
      (funcall on-action state
               (ntop.app.domain:make-app-action :kind :pause))
      (%render-composition-test-instance composition state second-snapshot nil)
      (cl-weave:expect (eq (first drawn-snapshots) second-snapshot) :to-be t)
      (cl-weave:expect
       (ntop.app.application::application-composition-frozen-snapshot composition)
       :to-be nil)))
)