(in-package #:ntop.test)

(defun %wp4-tree-row (pid ppid &key (name (format nil "p~A" pid)) cpu memory)
  (ntop.process.domain:make-process-row :count
                                        1
                                        :pid
                                        pid
                                        :ppid
                                        ppid
                                        :name
                                        name
                                        :command
                                        name
                                        :cpu-percent
                                        cpu
                                        :memory-bytes
                                        memory))

(defun %wp4-tree-shape (nodes)
  (mapcar
   (lambda (node)
     (list
      (ntop.process.domain:process-row-pid
       (ntop.process.domain::process-tree-node-row node))
      (%wp4-tree-shape (ntop.process.domain::process-tree-node-children node))))
   nodes))

(cl-weave:describe "WP4 process trees"
                   (cl-weave:it-each
                    ((:parent-and-child ((1 nil) (2 1) (3 1))
                                        ((1 ((2 nil) (3 nil)))))
                     (:missing-parent ((2 99)) ((2 nil)))
                     (:orphan-with-child ((2 99) (3 2)) ((2 ((3 nil)))))
                     (:self-parent ((4 4)) ((4 nil)))
                     (:multiple-roots ((8 nil) (2 nil)) ((2 nil) (8 nil)))
                     (:nil-pid-root ((nil nil) (5 nil)) ((nil nil) (5 nil)))
                     (:cycle-breaks-at-minimum-pid ((4 3) (3 4))
                                                   ((3 ((4 nil))))))
                    "WP4 tree case ~A preserves parentage and roots"
                    (case-name records expected-shape)
                    (declare (ignorable case-name))
                    (let* ((rows
                            (mapcar
                             (lambda (pair)
                               (%wp4-tree-row (first pair) (second pair)))
                             records))
                           (tree
                            (ntop.process.domain::build-process-tree rows
                                                                     (ntop.process.domain::make-process-sort-state
                                                                      :key
                                                                      :pid))))
                      (cl-weave:expect (%wp4-tree-shape tree)
                                       :to-equal
                                       expected-shape)))
                   (cl-weave:it
                    "sorts each sibling list by the active process key"
                    (let* ((rows
                            (list (%wp4-tree-row 1 nil :cpu 0)
                                  (%wp4-tree-row 2 1 :cpu 5)
                                  (%wp4-tree-row 3 1 :cpu 10)))
                           (tree
                            (ntop.process.domain::build-process-tree rows
                                                                     (ntop.process.domain::make-process-sort-state))))
                      (cl-weave:expect (%wp4-tree-shape tree)
                                       :to-equal
                                       '((1 ((3 nil) (2 nil)))))))
                   (cl-weave:it "keeps equal-key siblings in their input order"
                                (let* ((rows
                                        (list (%wp4-tree-row 1 nil :cpu 0)
                                              (%wp4-tree-row 7 1 :cpu 4)
                                              (%wp4-tree-row 5 1 :cpu 4)))
                                       (tree
                                        (ntop.process.domain::build-process-tree
                                         rows
                                         (ntop.process.domain::make-process-sort-state))))
                                  (cl-weave:expect (%wp4-tree-shape tree)
                                                   :to-equal
                                                   '((1 ((7 nil) (5 nil)))))))
                   (cl-weave:it "rejects duplicate process identifiers"
                                (cl-weave:expect
                                 (handler-case (progn
                                                 (ntop.process.domain::build-process-tree
                                                  (list (%wp4-tree-row 1 nil)
                                                        (%wp4-tree-row 1 nil))
                                                  (ntop.process.domain::make-process-sort-state))
                                                 :returned)
                                   (error ()
                                     :signaled))
                                 :to-be
                                 :signaled))
                   (cl-weave:it
                    "rejects grouped rows because tree and group modes are exclusive"
                    (let* ((grouped
                            (first
                             (ntop.process.domain::group-process-domain-rows
                              (list (%wp4-tree-row 1 nil :name "worker")
                                    (%wp4-tree-row 2 nil :name "worker"))
                              :name))))
                      (cl-weave:expect
                       (handler-case (progn
                                       (ntop.process.domain::build-process-tree
                                        (list grouped)
                                        (ntop.process.domain::make-process-sort-state))
                                       :returned)
                         (error ()
                           :signaled))
                       :to-be
                       :signaled)))
                   (cl-weave:it
                    "flattens expanded trees with depth and sibling position"
                    (let* ((tree
                            (ntop.process.domain::build-process-tree
                             (list (%wp4-tree-row 1 nil)
                                   (%wp4-tree-row 2 1)
                                   (%wp4-tree-row 3 1)
                                   (%wp4-tree-row 4 2))
                             (ntop.process.domain::make-process-sort-state :key
                                                                           :pid)))
                           (entries
                            (ntop.process.domain::process-tree-visible-rows tree
                                                                            (ntop.process.domain::make-process-tree-state))))
                      (cl-weave:expect
                       (mapcar
                        (lambda (entry)
                          (list
                           (ntop.process.domain:process-row-pid
                            (ntop.process.domain::process-tree-entry-row entry))
                           (ntop.process.domain::process-tree-entry-depth entry)
                           (ntop.process.domain::process-tree-entry-last-child-p
                            entry)))
                        entries)
                       :to-equal
                       '((1 0 t) (2 1 nil) (4 2 t) (3 1 t))))
                    (cl-weave:it
                     "collapses a branch into its aggregate row and hides descendants"
                     (let* ((tree
                             (ntop.process.domain::build-process-tree
                              (list (%wp4-tree-row 1 nil :cpu 10 :memory 100)
                                    (%wp4-tree-row 2 1 :cpu 20 :memory 200)
                                    (%wp4-tree-row 3 1 :cpu 30 :memory 300))
                              (ntop.process.domain::make-process-sort-state :key
                                                                            :pid)))
                            (state
                             (ntop.process.domain::process-tree-state-set-collapsed
                              (ntop.process.domain::make-process-tree-state)
                              1
                              t))
                            (entries
                             (ntop.process.domain::process-tree-visible-rows
                              tree
                              state))
                            (entry (first entries))
                            (row
                             (ntop.process.domain::process-tree-entry-row entry)))
                       (cl-weave:expect (length entries) :to-be 1)
                       (cl-weave:expect
                        (list (ntop.process.domain:process-row-cpu-percent row)
                              (ntop.process.domain:process-row-memory-bytes row)
                              (ntop.process.domain:process-row-count row)
                              (ntop.process.domain:process-row-pid row)
                              (ntop.process.domain:process-tree-entry-has-children-p
                               entry)
                              (ntop.process.domain:process-tree-entry-collapsed-p
                               entry))
                        :to-equal
                        '(60 600 3 1 t t))))
                    (cl-weave:it-each
                     ((:expand-default nil nil) (:collapse-default t t))
                     "WP4 tree default collapse state is ~A"
                     (case-name default-collapsed-p expected)
                     (declare (ignorable case-name))
                     (let ((state
                            (ntop.process.domain::make-process-tree-state
                             :default-collapsed-p
                             default-collapsed-p)))
                       (cl-weave:expect
                        (ntop.process.domain::process-tree-state-collapsed-p
                         state
                         12)
                        :to-be
                        expected)))
                    (cl-weave:it
                     "toggles a branch without mutating its previous state"
                     (let* ((original
                             (ntop.process.domain::make-process-tree-state))
                            (collapsed
                             (ntop.process.domain::process-tree-state-toggle
                              original
                              12))
                            (expanded
                             (ntop.process.domain::process-tree-state-toggle
                              collapsed
                              12)))
                       (cl-weave:expect
                        (list
                         (ntop.process.domain::process-tree-state-collapsed-p
                          original
                          12)
                         (ntop.process.domain::process-tree-state-collapsed-p
                          collapsed
                          12)
                         (ntop.process.domain::process-tree-state-collapsed-p
                          expanded
                          12))
                        :to-equal
                        '(nil t nil))))
                    (cl-weave:it "resets an override to the default state"
                                 (let* ((state
                                         (ntop.process.domain::make-process-tree-state
                                          :default-collapsed-p
                                          t))
                                        (expanded
                                         (ntop.process.domain::process-tree-state-set-collapsed
                                          state
                                          14
                                          nil))
                                        (reset
                                         (ntop.process.domain::process-tree-state-set-collapsed
                                          expanded
                                          14
                                          t)))
                                   (cl-weave:expect
                                    (list
                                     (ntop.process.domain::process-tree-state-collapsed-p
                                      expanded
                                      14)
                                     (ntop.process.domain::process-tree-state-collapsed-p
                                      reset
                                      14))
                                    :to-equal
                                    '(nil t))))
                    (cl-weave:it "does not draw a collapsed marker for a leaf"
                                 (let* ((tree
                                         (ntop.process.domain::build-process-tree
                                          (list (%wp4-tree-row 20 nil))
                                          (ntop.process.domain::make-process-sort-state
                                           :key
                                           :pid)))
                                        (state
                                         (ntop.process.domain::process-tree-state-set-collapsed
                                          (ntop.process.domain::make-process-tree-state)
                                          20
                                          t))
                                        (entry
                                         (first
                                          (ntop.process.domain::process-tree-visible-rows
                                           tree
                                           state))))
                                   (cl-weave:expect
                                    (list
                                     (ntop.process.domain::process-tree-entry-has-children-p
                                      entry)
                                     (ntop.process.domain::process-tree-entry-collapsed-p
                                      entry))
                                    :to-equal
                                    '(nil nil))))
                    (cl-weave:it "returns an empty forest for no processes"
                                 (cl-weave:expect
                                  (ntop.process.domain::build-process-tree nil
                                                                           (ntop.process.domain::make-process-sort-state))
                                  :to-equal
                                  nil)))
                   (cl-weave:it
                    "ignores collapse updates without a PID and rejects invalid states"
                    (let ((state (ntop.process.domain::make-process-tree-state)))
                      (cl-weave:expect
                       (eq state
                           (ntop.process.domain::process-tree-state-set-collapsed
                            state
                            nil
                            t))
                       :to-be
                       t)
                      (cl-weave:expect
                       (handler-case (progn
                                       (ntop.process.domain::process-tree-state-set-collapsed
                                        state
                                        1
                                        :sometimes)
                                       :returned)
                         (error ()
                           :signaled))
                       :to-be
                       :signaled)))
                   (cl-weave:it "cuts a three-process cycle at the lowest PID"
                                (let ((tree
                                       (ntop.process.domain::build-process-tree
                                        (list (%wp4-tree-row 5 4)
                                              (%wp4-tree-row 4 3)
                                              (%wp4-tree-row 3 5))
                                        (ntop.process.domain::make-process-sort-state
                                         :key
                                         :pid))))
                                  (cl-weave:expect (%wp4-tree-shape tree)
                                                   :to-equal
                                                   (list
                                                    (list 3
                                                          (list
                                                           (list 4
                                                                 (list
                                                                  (list 5 nil)))))))))
                   (cl-weave:it
                    "keeps a collapsed child hidden under an expanded parent"
                    (let* ((tree
                            (ntop.process.domain::build-process-tree
                             (list (%wp4-tree-row 1 nil)
                                   (%wp4-tree-row 2 1)
                                   (%wp4-tree-row 3 2))
                             (ntop.process.domain::make-process-sort-state :key
                                                                           :pid)))
                           (state
                            (ntop.process.domain::process-tree-state-set-collapsed
                             (ntop.process.domain::make-process-tree-state)
                             2
                             t))
                           (entries
                            (ntop.process.domain::process-tree-visible-rows tree
                                                                            state)))
                      (cl-weave:expect
                       (mapcar
                        (lambda (entry)
                          (list
                           (ntop.process.domain:process-row-pid
                            (ntop.process.domain::process-tree-entry-row entry))
                           (ntop.process.domain::process-tree-entry-depth entry)
                           (ntop.process.domain::process-tree-entry-collapsed-p
                            entry)))
                        entries)
                       :to-equal
                       (list (list 1 0 nil) (list 2 1 t)))))
                   (cl-weave:it "treats PID zero as a parent identifier"
                                (let ((tree
                                       (ntop.process.domain::build-process-tree
                                        (list (%wp4-tree-row 0 nil)
                                              (%wp4-tree-row 1 0))
                                        (ntop.process.domain::make-process-sort-state
                                         :key
                                         :pid))))
                                  (cl-weave:expect (%wp4-tree-shape tree)
                                                   :to-equal
                                                   (list
                                                    (list 0 (list (list 1 nil)))))))
                   (cl-weave:it "returns no visible entries for an empty tree"
                                (cl-weave:expect
                                 (ntop.process.domain::process-tree-visible-rows
                                  nil
                                  (ntop.process.domain::make-process-tree-state))
                                 :to-be
                                 nil))
                   (cl-weave:it
                    "keeps a nil PID toggle from changing the default state"
                    (let* ((collapsed
                            (ntop.process.domain::make-process-tree-state
                             :default-collapsed-p
                             t))
                           (after-toggle
                            (ntop.process.domain::process-tree-state-toggle
                             collapsed
                             nil)))
                      (cl-weave:expect (eq after-toggle collapsed) :to-be t)
                      (cl-weave:expect
                       (ntop.process.domain::process-tree-state-collapsed-p
                        after-toggle
                        10)
                       :to-be
                       t))))
