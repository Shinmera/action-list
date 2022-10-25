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
   #:remove
   #:update
   #:duration
   #:elapsed-time
   #:remaining-time
   #:blocking-p
   #:finished-p
   #:lanes
   #:start
   #:stop
   #:time-limited-action
   #:lane-limited-action
   #:delay-action
   #:synchronize-action
   #:action-list-action
   #:basic-action))
