(in-package #:ntop.process.domain)

(defstruct (process-tree-node
            (:constructor %make-process-tree-node (row)))
  row
  (children nil)
  parent)

(defstruct (process-tree-state
            (:constructor %make-process-tree-state
                (default-collapsed-p overrides)))
  default-collapsed-p
  overrides)

(defstruct (process-tree-entry
            (:constructor %make-process-tree-entry
                (row depth last-child-p has-children-p collapsed-p)))
  row
  depth
  last-child-p
  has-children-p
  collapsed-p)

(defun make-process-tree-state (&key default-collapsed-p)
  (%make-process-tree-state default-collapsed-p nil))

(defun process-tree-state-collapsed-p (state pid)
  (let ((override (assoc pid (process-tree-state-overrides state) :test #'eql)))
    (if override
        (cdr override)
        (process-tree-state-default-collapsed-p state))))

(defun process-tree-state-set-collapsed (state pid collapsed-p)
  (unless (member collapsed-p '(t nil))
    (error "Collapsed state must be true or false: ~S" collapsed-p))
  (if (null pid)
      state
      (let* ((default (process-tree-state-default-collapsed-p state))
             (overrides
               (remove pid (process-tree-state-overrides state)
                       :key #'car :test #'eql)))
        (%make-process-tree-state
         default
         (if (eql collapsed-p default)
             overrides
             (cons (cons pid collapsed-p) overrides))))))

(defun process-tree-state-toggle (state pid)
  (process-tree-state-set-collapsed
   state pid (not (process-tree-state-collapsed-p state pid))))

(defun %detach-tree-node (node)
  (let ((parent (process-tree-node-parent node)))
    (when parent
      (setf (process-tree-node-children parent)
            (delete node (process-tree-node-children parent) :test #'eq))
      (setf (process-tree-node-parent node) nil))))

(defun %break-process-tree-cycles (nodes roots)
  (let ((marks (make-hash-table :test #'eq)))
    (labels ((visit (node chain)
               (case (gethash node marks)
                 (:done nil)
                 (:visiting
                  (let* ((cycle-position (position node chain :test #'eq))
                         (cycle (cons node (subseq chain 0 cycle-position)))
                         (root
                           (reduce
                            (lambda (left right)
                              (if (< (process-row-pid
                                      (process-tree-node-row right))
                                     (process-row-pid
                                      (process-tree-node-row left)))
                                  right
                                  left))
                            cycle)))
                    (%detach-tree-node root)
                    (push root roots)))
                 (otherwise
                  (setf (gethash node marks) :visiting)
                  (when (process-tree-node-parent node)
                    (visit (process-tree-node-parent node) (cons node chain)))
                  (setf (gethash node marks) :done)))))
      (dolist (node nodes)
        (visit node nil)))
    roots))

(defun %sort-process-tree-nodes (nodes sort-state nodes-by-row)
  (mapcar
   (lambda (row) (gethash row nodes-by-row))
   (sort-process-domain-rows (mapcar #'process-tree-node-row nodes) sort-state)))

(defun build-process-tree (rows &optional (sort-state (make-process-sort-state)))
  (let* ((nodes (mapcar #'%make-process-tree-node rows))
         (nodes-by-pid (make-hash-table :test #'eql))
         (nodes-by-row (make-hash-table :test #'eq))
         (roots nil))
    (dolist (node nodes)
      (let* ((row (process-tree-node-row node))
             (pid (process-row-pid row)))
        (when (> (or (process-row-count row) 1) 1)
          (error "Grouped process rows cannot form a process tree."))
        (setf (gethash row nodes-by-row) node)
        (when pid
          (multiple-value-bind (existing present-p)
              (gethash pid nodes-by-pid)
            (declare (ignore existing))
            (when present-p
              (error "Duplicate process PID in tree: ~S" pid))
            (setf (gethash pid nodes-by-pid) node)))))
    (dolist (node nodes)
      (let* ((row (process-tree-node-row node))
             (pid (process-row-pid row))
             (parent-pid (process-row-ppid row))
             (parent (and parent-pid (gethash parent-pid nodes-by-pid))))
        (if (and parent (not (eql parent-pid pid)))
            (progn
              (setf (process-tree-node-parent node) parent)
              (push node (process-tree-node-children parent)))
            (push node roots))))
    (dolist (node nodes)
      (setf (process-tree-node-children node)
            (nreverse (process-tree-node-children node))))
    (setf roots (nreverse roots))
    (setf roots (%break-process-tree-cycles nodes roots))
    (dolist (node nodes)
      (setf (process-tree-node-children node)
            (%sort-process-tree-nodes
             (process-tree-node-children node) sort-state nodes-by-row)))
    (%sort-process-tree-nodes roots sort-state nodes-by-row)))

(defun %process-tree-subtree-rows (node)
  (cons (process-tree-node-row node)
        (mapcan #'%process-tree-subtree-rows
                (copy-list (process-tree-node-children node)))))

(defun %collapsed-process-tree-row (node)
  (let* ((row (process-tree-node-row node))
         (rows (%process-tree-subtree-rows node)))
    (%aggregate-process-rows rows row)))

(defun process-tree-visible-rows (roots state)
  (let ((entries nil))
    (labels ((visit-siblings (siblings depth)
               (loop for node in siblings
                     for index from 0
                     for children = (process-tree-node-children node)
                     for has-children-p = (not (null children))
                     for collapsed-p =
                       (and has-children-p
                            (process-tree-state-collapsed-p
                             state
                             (process-row-pid
                              (process-tree-node-row node))))
                     do (push
                         (%make-process-tree-entry
                          (if collapsed-p
                              (%collapsed-process-tree-row node)
                              (process-tree-node-row node))
                          depth
                          (= index (1- (length siblings)))
                          has-children-p
                          collapsed-p)
                         entries)
                        (unless collapsed-p
                          (visit-siblings children (1+ depth))))))
      (visit-siblings roots 0))
    (nreverse entries)))
