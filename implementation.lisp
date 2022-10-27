#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.action-list)

(defmethod sequences:length ((list action-list))
  (length (actions list)))

(defmethod sequences:elt ((list action-list) index)
  (nth index (actions list)))

(defmethod (setf sequences:elt) ((action action) (list action-list) index)
  (let ((cons (nthcdr index (actions list))))
    (slot-makunbound (car cons) 'action-list)
    (setf (car cons) action)))

(defmethod sequences:make-sequence-iterator ((list action-list) &rest args)
  (apply #'sequences:make-sequence-iterator (actions list) args))

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

(defmethod pop-action ((action action) (list action-list))
  (setf (actions list) (delete action (actions list)))
  (slot-makunbound (action-list action) 'action-list)
  action)

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
                 (stop action)
                 ;; FIXME: This could be optimised to not have to search through the list again.
                 (pop-action action list))))))

;; TODO: implement these. It's a bit tricky, as we need to regard
;;       blocking and non-blocking actions and multiple lanes
(defmethod duration ((list action-list))
  (remaining-time list))

(defmethod elapsed-time ((list action-list))
  (let ((lanes (1- (ash 1 32)))
        (elapsed 0.0)
        (choices (list 0.0)))
    (loop for action in (actions list)
          do (when (< 0 (logand lanes (lanes action)))
               (cond ((blocking-p action)
                      (incf elapsed (reduce #'max choices))
                      (incf elapsed (elapsed-time action))
                      (setf choices (list 0.0))
                      (setf lanes (logandc2 lanes (lanes action))))
                     (T
                      (push (elapsed-time action) choices)))))
    (incf elapsed (reduce #'max choices))))

(defmethod remaining-time ((list action-list))
  (let ((lanes (1- (ash 1 32)))
        (remaining 0.0)
        (choices (list 0.0)))
    (loop for action in (actions list)
          do (when (< 0 (logand lanes (lanes action)))
               (cond ((blocking-p action)
                      (incf remaining (reduce #'max choices))
                      (incf remaining (remaining-time action))
                      (setf choices (list 0.0))
                      (setf lanes (logandc2 lanes (lanes action))))
                     (T
                      (push (remaining-time action) choices)))))
    (incf remaining (reduce #'max choices))))

(defmethod blocking-p ((list action-list))
  (loop for action in (actions list)
        thereis (blocking-p action)))

(defmethod finished-p ((list action-list))
  (null (actions list)))

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

(defmethod stop ((action action)))

(defmethod update :after ((action time-limited-action) dt)
  (when (< (duration action) (elapsed-time action))
    (setf (finished-p action) T)))

(defmethod update ((action delay-action) dt))

(defmethod blocking-p ((action delay-action))
  T)

(defmethod update ((action synchronize-action) dt)
  (when (eq action (sequences:elt (action-list action) 0))
    (setf (finished-p action) T)))

(defmethod blocking-p ((action synchronize-action))
  T)

(defmethod start ((action basic-action))
  (funcall (start-fun action) action))

(defmethod stop ((action basic-action))
  (funcall (stop-fun action) action))

(defmethod update ((action basic-action) dt)
  (funcall (update-fun action) action dt))
