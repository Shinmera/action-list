(in-package #:org.shirakumo.fraf.action-list)

(defmethod make-instance ((list action-list) &key)
  (clone-into (allocate-instance (class-of list)) list))

(defmethod shared-initialize :after ((list action-list) slots &key (actions () actions-p))
  (when actions-p
    (sequences:adjust-sequence list (length actions) :initial-contents actions)))

(defmethod print-object ((list action-list) stream)
  (print-unreadable-object (list stream :type T :identity T)
    (format stream "~@[~a ~]~,1f / ~,1f (~d)" (name list) (elapsed-time list) (duration list) (length (actions list)))))

(defmethod sequences:length ((list action-list))
  (length (actions list)))

(defmethod sequences:elt ((list action-list) index)
  (nth index (actions list)))

(defmethod (setf sequences:elt) ((action action) (list action-list) index)
  (let ((cons (nthcdr index (actions list))))
    (slot-makunbound (car cons) 'action-list)
    (setf (car cons) action)))

;; Copied from sbcl's list iterator.
(let ((exhausted (cons NIL NIL)))
  (defmethod sequences:make-sequence-iterator ((sequence action-list) &key (start 0) end from-end)
    (multiple-value-bind (iterator limit from-end)
        (let ((list (actions sequence)))
          (if from-end
              (let* ((termination (if (= start 0) exhausted (nthcdr (1- start) list)))
                     (init (if (<= (or end (length list)) start)
                               termination
                               (if end (last list (- (length list) (1- end))) (last list)))))
                (values init termination t))
              (cond
                ((not end) (values (nthcdr start list) nil nil))
                (t (let ((st (nthcdr start list)))
                     (values st (nthcdr (- end start) st) nil))))))
      (values iterator limit from-end
              (if from-end
                  (lambda (sequence iterator from-end)
                    (declare (ignore from-end))
                    (if (eq iterator (actions sequence))
                        exhausted
                        (do* ((cdr (actions sequence) (cdr cdr)))
                             ((eq (cdr cdr) iterator) cdr))))
                  (lambda (sequence iterator from-end)
                    (declare (ignore sequence from-end))
                    (cdr iterator)))
              (lambda (sequence iterator limit from-end)
                (declare (ignore sequence from-end))
                (eq iterator limit))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                (car iterator))
              (lambda (new-value sequence iterator)
                (declare (ignore sequence))
                (setf (car iterator) new-value))
              (lambda (sequence iterator)
                (loop for cdr on (actions sequence)
                      for i from 0
                      when (eq cdr iterator)
                      return i))
              (lambda (sequence iterator)
                (declare (ignore sequence))
                iterator)))))

(defmethod sequences:make-sequence-like ((list action-list) length &rest args)
  (apply #'sequences:adjust-sequence (clone-into (allocate-instance (class-of list)) list) length args))

(defmethod sequences:adjust-sequence ((list action-list) length &key initial-element initial-contents)
  (let ((len (length (actions list))))
    (when initial-contents
      (unless (equal length (length initial-contents))
        (error "initial-contents do not match requested size.")))
    (cond ((= 0 length)
           (loop for action = (pop (actions list))
                 while action
                 do (slot-makunbound action 'action-list)))
          ((= 0 len)
           (setf (actions list)
                 (loop repeat length
                       for action = (if initial-element
                                        (clone-into T initial-element)
                                        (make-instance 'dummy))
                       do (setf (action-list action) list)
                       collect action)))
          ((< len length)
           (setf (cdr (last (actions list)))
                 (loop repeat (- length len)
                       for action = (if initial-element
                                        (clone-into T initial-element)
                                        (make-instance 'dummy))
                       do (setf (action-list action) list)
                       collect action)))
          ((< length len)
           (let ((cons (nthcdr (1- length) (actions list))))
             (loop for action = (pop (cdr cons))
                   while (cdr cons)
                   do (slot-makunbound action 'action-list)))))
    ;; Now replace contents
    (when initial-contents
      (loop for cons on (actions list)
            for item in initial-contents
            do (slot-makunbound (car cons) 'action-list)
               (setf (action-list item) list)
               (setf (car cons) item)))
    list))

(defmethod clone-into progn ((new action-list) (list action-list))
  (setf (name new) (name list))
  (setf (elapsed-time new) (elapsed-time list))
  (setf (actions new) (loop for action in (actions list)
                            for clone = (clone-into T action)
                            do (setf (action-list clone) list)
                            collect clone))
  new)

(defmethod push-front ((action action) (list action-list))
  (setf (action-list action) list)
  (push action (actions list))
  (start action)
  action)

(defmethod push-back ((action action) (list action-list))
  (setf (action-list action) list)
  (if (actions list)
      (setf (cdr (last (actions list))) (cons action NIL))
      (setf (actions list) (list action)))
  (start action)
  action)

(defmethod push-before ((new action) (old action))
  (let* ((list (action-list old))
         (actions (actions list)))
    (loop for cons on actions
          do (when (eq (car cons) old)
               (setf (action-list new) list)
               (setf (car cons) new)
               (setf (cdr cons) (list* old (cdr cons)))
               (return))
          finally (error "Cannot insert~%  ~a~%before~%  ~a~%as it's no longer contained on the list~%  ~a"
                         new old (action-list old)))
    (start new)
    new))

(defmethod push-after ((new action) (old action))
  (let* ((list (action-list old))
         (actions (actions list)))
    (loop for cons on actions
          do (when (eq (car cons) old)
               (setf (action-list new) list)
               (setf (cdr cons) (list* new (cdr cons)))
               (return))
          finally (error "Cannot insert~%  ~a~%after~%  ~a~%as it's no longer contained on the list~%  ~a"
                         new old (action-list old)))
    (start new)
    new))

(defmethod pop-action ((action action))
  (let ((list (action-list action)))
    (setf (actions list) (delete action (actions list)))
    (slot-makunbound action 'action-list)
    action))

(defmethod update ((list action-list) dt)
  (let ((lanes (1- (ash 1 32)))
        (actions (actions list)))
    (loop while actions
          for action = (pop actions)
          do (when (< 0 (logand lanes (lanes action)))
               ;; We have to check FINISHED-P twice. The first time to avoid the situation of
               ;; the action being finished outside of its UPDATE, in which case we do not want
               ;; to update it again or block later actions.
               (unless (finished-p action)
                 (update action dt)
                 (when (blocking-p action)
                   (setf lanes (logandc2 lanes (lanes action)))))
               (when (finished-p action)
                 (stop action))))
    (setf (actions list) (delete-if (lambda (action)
                                      (when (finished-p action)
                                        (slot-makunbound action 'action-list)
                                        T))
                                    (actions list)))))

(defmethod update :after ((list action-list) dt)
  (incf (elapsed-time list) dt))

(defmethod duration ((list action-list))
  (+ (elapsed-time list) (remaining-time list)))

(defmethod remaining-time ((list action-list))
  (let ((remaining 0.0)
        (choices (list 0.0)))
    ;; FIXME: pretty sure this is not correct in the presence of lanes.
    (loop for action in (actions list)
          do (cond ((blocking-p action)
                    (incf remaining (reduce #'max choices))
                    (incf remaining (remaining-time action))
                    (setf choices (list 0.0)))
                   (T
                    (push (remaining-time action) choices))))
    (incf remaining (reduce #'max choices))))

(defmethod blocking-p ((list action-list))
  (loop for action in (actions list)
        thereis (blocking-p action)))

(defmethod finished-p ((list action-list))
  (null (actions list)))

(defmethod print-object ((action action) stream)
  (print-unreadable-object (action stream :type T :identity T)
    (format stream "~,1f / ~,1f ~:[~;BLOCKING~]" (elapsed-time action) (duration action) (blocking-p action))))

(defmethod update :after ((action action) dt)
  (incf (elapsed-time action) dt))

(defmethod duration ((action action))
  most-positive-single-float)

(defmethod remaining-time ((action action))
  (- (duration action) (elapsed-time action)))

(defmethod blocking-p ((action action))
  NIL)

(defmethod lanes ((action action))
  1)

(defmethod start ((action action)))

(defmethod start :before ((action action))
  (setf (elapsed-time action) 0.0))

(defmethod stop ((action action)))

(defmethod clone-into :around ((new action) (action action))
  (call-next-method)
  new)

(defmethod clone-into progn ((new (eql T)) (action action))
  (clone-into (allocate-instance (class-of action)) action))

(defmethod clone-into progn ((new action) (action action))
  (setf (elapsed-time new) 0.0)
  (setf (finished-p new) (finished-p action)))

(defmethod update :after ((action time-limited-action) dt)
  (when (< (duration action) (elapsed-time action))
    (setf (finished-p action) T)))

(defmethod clone-into progn ((new time-limited-action) (action time-limited-action))
  (setf (duration new) (duration action)))

(defmethod clone-into progn ((new lane-limited-action) (action lane-limited-action))
  (setf (lanes new) (lanes action)))

(defmethod update ((action dummy) dt)
  (setf (finished-p action) T))

(defmethod duration ((action dummy))
  0.0)

(defmethod update ((action delay) dt))

(defmethod blocking-p ((action delay))
  T)

(defmethod update ((action synchronize) dt)
  (when (eq action (sequences:elt (action-list action) 0))
    (setf (finished-p action) T)))

(defmethod blocking-p ((action synchronize))
  T)

(defmethod start ((action basic))
  (funcall (start-fun action) action))

(defmethod stop ((action basic))
  (funcall (stop-fun action) action))

(defmethod update ((action basic) dt)
  (funcall (update-fun action) action dt))

(defmethod clone-into progn ((new basic) (action basic))
  (setf (update-fun new) (update-fun action))
  (setf (start-fun new) (start-fun action))
  (setf (stop-fun new) (stop-fun action))
  (setf (blocking-p new) (blocking-p action)))

(defmethod update ((action ease) dt)
  (let* ((x (/ (+ (elapsed-time action) dt) (duration action)))
         (x (funcall (ease-fun action) (min 1.0 (max 0.0 x))))
         (x (+ (from action) (* x (- (to action) (from action))))))
    (funcall (update-fun action) action x)))

(defmethod clone-into progn ((new ease) (action ease))
  (setf (ease-fun new) (ease-fun action))
  (setf (from new) (from action))
  (setf (to new) (to action)))

(defmethod update ((action repeat) dt)
  (when (<= (interval action) (incf (last-time action) dt))
    (setf (last-time action) (mod (last-time action) (interval action)))
    (funcall (update-fun action) action dt)))

(defmethod clone-into progn ((new repeat) (action repeat))
  (setf (interval new) (interval action))
  (setf (last-time new) (last-time action)))
