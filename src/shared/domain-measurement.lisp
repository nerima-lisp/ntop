;;; Samples, snapshots, and value records are immutable after construction. Collectors build
;;; a fresh sample each cycle; UI code must not change any struct slots.
;;; Read-only slots do not prevent changes to lists or vectors stored in those slots.
;;; Handoff passes samples directly without copying.
(in-package #:ntop.shared.domain)

(deftype measurement-value () '(or null real))
(deftype measurement-counter () '(integer 0 *))
(deftype optional-counter () '(or null measurement-counter))
(deftype monotonic-seconds () 'real)

(defun measurement-available-p (value)
  (not (null value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-domain-type (name slots sample-p)
    (let* ((slot-specs
            (mapcar (lambda (slot)
                      (if (symbolp slot)
                          (list slot nil nil nil)
                        (let* ((slot-options (cddr slot))
                               (type-position (position :type slot-options)))
                          (list (first slot)
                                (second slot)
                                (when type-position
                                  (nth (1+ type-position) slot-options))
                                (not (null type-position))))))
                    slots))
           (slot-declarations
            (mapcar (lambda (slot-spec)
                      (destructuring-bind (slot-name default type typed-p)
                          slot-spec
                        (declare (ignore type typed-p))
                        (list slot-name default :read-only t)))
                    slot-specs))
           (constructor-parameters
            (append (when sample-p
                      '((sampled-at
                         (error "Sample timestamp is required."))))
                    (mapcar (lambda (slot-spec)
                              (list (first slot-spec) (second slot-spec)))
                            slot-specs)))
           (validation-forms
            (append (when sample-p
                      '((list sampled-at
                              'ntop.shared.domain:monotonic-seconds)))
                    (mapcan (lambda (slot-spec)
                              (when (fourth slot-spec)
                                (list
                                 `(list ,(first slot-spec)
                                        ',(third slot-spec)))))
                            slot-specs)))
           (constructor-arguments
            (append (when sample-p '(:sampled-at sampled-at))
                    (mapcan (lambda (slot-spec)
                              (list
                               (intern (symbol-name (first slot-spec))
                                       "KEYWORD")
                               (first slot-spec)))
                            slot-specs)))
           (constructor-name
            (intern (format nil "MAKE-~A" (symbol-name name))
                    (symbol-package name)))
           (raw-constructor-name
            (intern (format nil "MAKE-~A-RAW" (symbol-name name))
                    (symbol-package name))))
      `(progn
         (defstruct (,name
                     ,@(when sample-p '((:include sample-record)))
                     (:constructor ,raw-constructor-name)
                     (:copier nil))
                    ,@slot-declarations)
         (defun ,constructor-name (&key ,@constructor-parameters)
           (validate-typed-slots (list ,@validation-forms))
           (,raw-constructor-name ,@constructor-arguments)))))
  (defmacro define-value-type (name &body slots)
    (expand-domain-type name slots nil)))

(progn
  (defun validate-typed-slots (typed-slots)
    (dolist (typed-slot typed-slots)
      (destructuring-bind (value expected-type) typed-slot
        (unless (typep value expected-type)
          (error 'type-error :datum value :expected-type expected-type)))))
  (defmacro define-sample-type (name &body slots)
    (expand-domain-type name slots t)))


(ntop.shared.domain:define-value-type sample-record
  (sampled-at
    (error "Sample timestamp is required.")
    :type monotonic-seconds))
