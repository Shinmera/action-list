#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.action-list)

(defclass action-list (sequences:sequence standard-object)
  ((actions :initform () :accessor actions)
   (elapsed-time :initform 0.0 :accessor elapsed-time)))

(defclass action ()
  ((action-list :initarg :action-list :accessor action-list)
   (elapsed-time :initform 0.0 :accessor elapsed-time)
   (finished-p :initform NIL :accessor finished-p)))

(defgeneric push-front (new-action action-list))
(defgeneric push-back (new-action action-list))
(defgeneric push-before (new-action action))
(defgeneric push-after (new-action action))
(defgeneric pop-action (action))

(defgeneric update (object dt))
(defgeneric duration (object))
(defgeneric elapsed-time (object))
(defgeneric remaining-time (object))
(defgeneric blocking-p (object))
(defgeneric finished-p (object))

(defgeneric lanes (action))
(defgeneric start (action))
(defgeneric stop (action))

(defgeneric clone-into (target source)
  (:method-combination progn :most-specific-last))

(defclass time-limited-action (action)
  ((duration :initarg :duration :initform (error "DURATION required.") :accessor duration)))

(defclass lane-limited-action (action)
  ((lanes :initarg :lanes :initform 1 :accessor lanes)))

(defclass dummy (action)
  ())

(defclass delay (time-limited-action lane-limited-action)
  ((lanes :initform (1- (ash 1 32)))))

(defclass synchronize (lane-limited-action)
  ((lanes :initform (1- (ash 1 32)))))

(defclass basic (time-limited-action lane-limited-action)
  ((update-fun :initarg :update :initform (lambda (action dt) action) :accessor update-fun)
   (start-fun :initarg :start :initform #'identity :accessor start-fun)
   (stop-fun :initarg :stop-fun :initform #'identity :accessor stop-fun)
   (blocking-p :initarg :blocking :initform NIL :accessor blocking-p)))

(defclass action-list-action (action action-list)
  ())

(defclass ease (basic)
  ((ease-fun :initarg :ease :initform #'identity :accessor ease-fun)
   (from :initarg :from :initform 0.0 :accessor from)
   (to :initarg :to :initform 1.0 :accessor to)))
