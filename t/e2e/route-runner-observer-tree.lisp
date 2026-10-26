(in-package #:ntop.e2e)

(defun %route-observer-tree-shape (grid)
  (let ((rows (%route-observer-process-rows grid)))
    (when rows
      (list (length rows)
            (loop for row in rows
                  sum (if (or (search "├─" row) (search "└─" row)) 1 0))
            (loop for row in rows sum (if (search "+ " row) 1 0))))))

(defun %route-observer-tree-transition-p (action before after)
  (and before after
       (cond
         ((string= action "Toggle process tree")
          (/= (second before) (second after)))
         ((string= action "Toggle process tree branch")
          (or (/= (first before) (first after))
              (/= (third before) (third after))))
         (t nil))))

(defun %route-observer-tree-name (subcase role)
  (getf subcase (ecase role
                  (:parent :fixture-tree-parent-name)
                  (:child :fixture-tree-child-name))))

(defun %route-observer-tree-row-index (rows name)
  (when (and rows (stringp name))
    (position-if (lambda (row) (search name row :test #'char-equal)) rows)))

(defun %route-observer-tree-row-indentation-p
    (parent-row child-row parent-name child-name)
  (let* ((parent-position (search parent-name parent-row :test #'char-equal))
         (child-position (search child-name child-row :test #'char-equal))
         (branch-position
           (or (search "├─" child-row)
               (search "└─" child-row))))
    (and parent-position child-position branch-position
         (< branch-position child-position)
         (> child-position parent-position))))

(defun %route-observer-tree-selected-row (grid rows)
  (multiple-value-bind (selected-index last-index)
      (%route-observer-process-selection-index grid)
    (declare (ignore last-index))
    (and (integerp selected-index) (nth selected-index rows))))

(defun %route-observer-tree-expansion-transition-p
    (action subcase initial grid)
  (let* ((parent-name (%route-observer-tree-name subcase :parent))
         (child-name (%route-observer-tree-name subcase :child))
         (before-rows (%route-observer-process-rows initial))
         (after-rows (%route-observer-process-rows grid))
         (parent-before-index
           (%route-observer-tree-row-index before-rows parent-name))
         (child-before-index
           (%route-observer-tree-row-index before-rows child-name))
         (parent-after-index
           (%route-observer-tree-row-index after-rows parent-name))
         (child-after-index
           (%route-observer-tree-row-index after-rows child-name))
         (before-selected
           (%route-observer-tree-selected-row initial before-rows))
         (after-selected
           (%route-observer-tree-selected-row grid after-rows))
         (before-expanded-p (getf subcase :fixture-tree-expanded))
         (after-expanded-p
           (cond
             ((string= action "Move left within widget") nil)
             ((string= action "Move right within widget") t))))
    (and (stringp parent-name) (stringp child-name)
         (member action
                 '("Move left within widget" "Move right within widget")
                 :test #'string=)
         (member before-expanded-p (list t nil))
         (eq (not (null child-before-index)) before-expanded-p)
         parent-before-index parent-after-index
         (stringp before-selected) (stringp after-selected)
         (search parent-name before-selected :test #'char-equal)
         (search parent-name after-selected :test #'char-equal)
         (if before-expanded-p
             (and child-before-index
                  (< parent-before-index child-before-index)
                  (%route-observer-tree-row-indentation-p
                   (nth parent-before-index before-rows)
                   (nth child-before-index before-rows)
                   parent-name child-name))
             (null child-before-index))
         (eq (not (null child-after-index)) after-expanded-p)
         (if after-expanded-p
             (and child-after-index
                  (< parent-after-index child-after-index)
                  (%route-observer-tree-row-indentation-p
                   (nth parent-after-index after-rows)
                   (nth child-after-index after-rows)
                   parent-name child-name))
             (null child-after-index)))))
(defun %route-observer-tree-p (route subcase initial grid)
  (let* ((action (route-action route))
         (before (%route-observer-tree-shape initial))
         (after (%route-observer-tree-shape grid)))
    (cond
      ((member action
               '("Go to first row" "Go to last row"
                 "Move to previous row" "Move to next row"
                 "Page up" "Page down" "Half-page up" "Half-page down")
               :test #'string=)
       (and before after
            (plusp (second before))
            (plusp (second after))
            (multiple-value-bind (old old-last)
                (%route-observer-process-selection-index initial)
              (multiple-value-bind (new new-last)
                  (%route-observer-process-selection-index grid)
                (%route-observer-navigation-index-p
                 action old new old-last new-last)))))
      ((member action '("Move left within widget"
                        "Move right within widget")
               :test #'string=)
       (and before after
            (%route-observer-tree-expansion-transition-p
             action subcase initial grid)))
      (t (%route-observer-tree-transition-p action before after)))))
