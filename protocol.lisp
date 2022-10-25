#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.action-list)

(defclass action-list (sequences:sequence standard-object)
  ((actions :initform () :accessor actions)))

(defclass action ()
  ((action-list :initarg :action-list :accessor action-list)
   (elapsed-time :initform 0.0 :accessor elapsed-time)
   (finished-p :initform NIL :accessor finished-p)))

(defgeneric push-front (new-action action-list))
(defgeneric push-back (new-action action-list))
(defgeneric push-before (new-action action))
(defgeneric push-after (new-action action))
(defgeneric remove (action action-list))

(defgeneric update (object dt))
(defgeneric duration (object))
(defgeneric elapsed-time (object))
(defgeneric remaining-time (object))
(defgeneric blocking-p (object))
(defgeneric finished-p (object))

(defgeneric lanes (action))
(defgeneric start (action))
(defgeneric stop (action))

(defclass time-limited-action (action)
  ((duration :initarg :duration :initform (error "DURATION required.") :accessor duration)))

(defclass lane-limited-action (action)
  ((lanes :initarg :lanes :initform (1- (ash 1 32)) :accessor lanes)))

(defclass delay-action (time-limited-action) ())
(defclass synchronize-action (action) ())
(defclass action-list-action (action action-list) ())
(defclass basic-action (time-limited-action lane-limited-action) ())
