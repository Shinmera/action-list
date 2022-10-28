#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.fraf.action-list
  (:use #:cl)
  (:local-nicknames
   (#:sequences #:org.shirakumo.trivial-extensible-sequences))
  (:export
   #:action-list
   #:action
   #:push-front
   #:push-back
   #:push-before
   #:push-after
   #:pop-action
   #:update
   #:duration
   #:elapsed-time
   #:remaining-time
   #:blocking-p
   #:finished-p
   #:lanes
   #:start
   #:stop
   #:clone-into
   #:time-limited-action
   #:lane-limited-action
   #:dummy
   #:delay
   #:synchronize
   #:action-list-action
   #:basic
   #:ease)
