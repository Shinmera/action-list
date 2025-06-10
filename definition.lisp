(in-package #:org.shirakumo.fraf.action-list)

(defvar *action-lists* (make-hash-table :test 'eql))
(defvar *action-definition-parsers* (make-hash-table :test 'equal))

(defmethod action-list ((name symbol))
  (or (gethash name *action-lists*)
      (error "No action list by the name ~s." name)))

(defmethod (setf action-list) ((list action-list) (name symbol))
  (setf (gethash name *action-lists*) list))

(defmethod (setf action-list) ((none null) (name symbol))
  (remhash name *action-lists*))

(defmacro define-action-list (name &body actions)
  `(setf (action-list ',name)
         (make-instance 'action-list :name ',name :actions (list ,@(mapcar #'compile-action-definition actions)))))

(defun compile-action-definition (definition)
  (destructuring-bind (type &rest args) definition
    (apply (or (gethash type *action-definition-parsers*)
               (gethash (string type) *action-definition-parsers*)
               (error "Don't know how to parse an action definition of type ~s." type))
           args)))

(defmacro define-action-definition-parser (type args &body body)
  `(progn (setf (gethash ',(string type) *action-definition-parsers*)
                (lambda ,args ,@body))
          (setf (gethash ',type *action-definition-parsers*)
                (gethash ',(string type) *action-definition-parsers*))
          ',type))

(define-action-definition-parser eval (&rest body)
  `(make-instance 'basic :update (lambda (action dt)
                                   (declare (ignore action dt))
                                   ,@body)
                         :duration 0.0))

(define-action-definition-parser setf (&rest body)
  `(make-instance 'basic :update (lambda (action dt)
                                   (declare (ignore action dt))
                                   (setf ,@body))
                         :duration 0.0))

(define-action-definition-parser ease (duration place &rest initargs)
  `(make-instance 'ease :update (lambda (action x)
                                  (declare (ignore action))
                                  (setf ,place x))
                        :duration ,duration ,@initargs :blocking T))

(define-action-definition-parser delay (duration &rest initargs)
  `(make-instance 'delay :duration ,duration ,@initargs))

(define-action-definition-parser synchronize (&rest initargs)
  `(make-instance 'synchronize ,@initargs))

(define-action-definition-parser repeat (duration interval &rest body)
  `(make-instance 'repeat :update (lambda (action dt)
                                    (declare (ignore action dt))
                                    ,@body)
                          :duration ,duration
                          :interval ,interval
                          :blocking T))
