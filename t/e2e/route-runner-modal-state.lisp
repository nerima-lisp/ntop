(in-package #:ntop.e2e)

(defun %route-modal-action-state-p (route subcase before-grid after-grid)
  (let* ((id (route-id route))
         (action (route-action route))
         (context (or (getf subcase :context) ""))
         (before (%route-grid-text before-grid))
         (after (%route-grid-text after-grid)))
    (cond
      ((and (string= id "key-35") (string= action "Accept current choice"))
       (cond
         ((string-equal context "help search")
          (and (%route-help-search-query-visible-p before-grid "process")
               (search "Help" after :test #'char-equal)
               (not (search "Search: process" after :test #'char-equal))))
         ((member context
                  '("process search" "process sort" "kill dialog")
                  :test
                  #'string-equal) :manual)
         (t nil)))
      ((and (string= id "key-36") (string= action "Close current modal"))
       (cond
         ((string-equal context "process search")
          (and (%route-process-search-open-state-p before nil)
               (not (%route-process-search-open-state-p after nil))))
         ((string-equal context "help search")
          (and (%route-help-search-query-visible-p before-grid "process")
               (search "Help" after :test #'char-equal)
               (not (search "Search: process" after :test #'char-equal))))
         ((string-equal context "process sort")
          (and (search "Sort By" before :test #'char-equal)
               (not (search "Sort By" after :test #'char-equal))))
         ((string-equal context "expanded widget")
          (and (%route-expanded-widget-p before-grid)
               (not (%route-expanded-widget-p after-grid))))
         ((member context '("kill dialog" "help") :test #'string-equal) :manual)
         (t nil)))
      (t nil))))

(defun %route-help-navigation-state-p (route subcase before-grid after-grid)
  (when
      (and (string-equal (or (getf subcase :context) "") "help")
           before-grid
           after-grid
           (member (route-action route)
                   (list "Go to first row"
                         "Go to last row"
                         "Move to previous row"
                         "Move to next row"
                         "Page up"
                         "Page down"
                         "Half-page up"
                         "Half-page down")
                   :test
                   (function string=)))
    (%route-grid-first-difference before-grid after-grid)))
