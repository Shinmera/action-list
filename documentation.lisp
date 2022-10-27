#|
 This file is a part of Action List
 (c) 2022 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.fraf.action-list)

(docs:define-docs
  (type action-list
        "Representation of an action list.

An action list is a sequence of actions that are updated in turn until
a blocking action is encountered.

See ACTION
See PUSH-FRONT
See PUSH-BACK
See PUSH-BEFORE
See PUSH-AFTER
See POP-ACTION
See UPDATE
See DURATION
See ELAPSED-TIME
See REMAINING-TIME
See BLOCKING-P
See FINISHED-P")
  
  (type action
        "Base class representing an action.

An action is an object that is updated and performs... actions until a
specified time at which point it terminates and is removed from the
action list.

An action can be present on one or more lanes. If the action is
blocking, it prevents all later actions that are present on the same
lane/s as the blocking action from being updated.

You should create a subclass of this action that in the very least
implements UPDATE, and possibly START, STOP, DURATION, and LANES.

See ACTION-LIST
See PUSH-FRONT
See PUSH-BACK
See PUSH-BEFORE
See PUSH-AFTER
See POP-ACTION
See UPDATE
See DURATION
See ELAPSED-TIME
See REMAINING-TIME
See BLOCKING-P
See FINISHED-P
See LANES
See START
See STOP")

  (function action-list
            "Returns the action list in which it is contained.

If the action is not currently contained in an action list, an error
is signalled instead.

See ACTION
See ACTION-LIST (type)")
  
  (function push-front
            "Push the action to the front of the action list.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function push-back
            "Push the action to the back of the action list.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function push-before
            "Push the NEW action in front of the OLD action.

Signals an error if the OLD action is not contained in any
action list.

See ACTION (type)")
  
  (function push-after
            "Push the NEW action immediately after the OLD action.

Signals an error if the OLD action is not contained in any action
list.

See ACTION (type)")
  
  (function pop-action
            "Removes the action from the action list.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function update
            "Performs the update step of the object, advancing it by DT seconds.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function duration
            "Returns the duration of the object's runtime, in seconds.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function elapsed-time
            "Returns the length of time for which the object has been updated, in seconds.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function remaining-time
            "Returns the length of time for which the object will remain running, in seconds.

Note that this only considers time for which the object is actually
updated. If the object is blocked from receiving updates, the
remaining time will not decrease.

Objects may also not finish even after the remaining time has passed.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function blocking-p
            "Returns whether the object blocks later objects from being updated.

For an ACTION-LIST this returns T when the list contains a blocking
action.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function finished-p
            "Accesses whether the object is finished.

An action should set this to T when it should no longer be UPADTEd and
should be removed from the ACTION-LIST at the next opportunity.

For an ACTION-LIST this returns T when the list is empty.

See ACTION (type)
See ACTION-LIST (type)")
  
  (function lanes
            "Returns an integer bitfield that represents the lanes the action is active on.

Every bit of the integer that is active (1) represents a lane the
action is active on. If the action is blocking, the lanes it is active
on will be blocked from being updated until the action is finished.

Either way, an action is only updated if at least one of the lanes it
is active on is not blocked by a prior action.

See ACTION (type)")
  
  (function start
            "Function called when an action is added to an action-list.

See ACTION (type)")
  
  (function stop
            "Function called when an action is finished and removed from an action-list.

Note that this is NOT called if the action is manually removed from
the action list via POP-ACTION.
It IS called once when the action is FINISHED-P and the action list is
next updated.

See ACTION (type)")
  
  (type time-limited-action
        "Superclass for actions that have a specific duration before they automatically finish.

Accepts the DURATION as an initarg.

See ACTION (type)
See DUARTION")
  
  (type lane-limited-action
        "Superclass for actions that are restricted to a set of lanes.

Accepts the LANES as an  initarg.

See ACTION (type)
See LANES")
  
  (type delay-action
        "Action that delays execution by the passed DURATION.

By default blocks all lanes.

See TIME-LIMITED-ACTION
See LANE-LIMITED-ACTION")
  
  (type synchronize-action
        "Action that finishes once it reaches the front of the list.

By default blocks all lanes.

See LANE-LIMITED-ACTION")
  
  (type basic-action
        "Action that lets you dynamically specify its parts.

To customise UPDATE, pass a closure of two arguments (the action and
the delta time) as an initarg.
To customise START, pass a closure of one argument (the action) as an
initarg.
To customise STOP, pass a closure of one argument (the action) as an
initarg.
To specify whether the action should block or not, pass the BLOCKING
initarg.

See LANE-LIMITED-ACTION
See TIME-LIMITED-ACTION")
  
  (type action-list-action
        "Action that is simultaneously an action-list. Lets you hierarchically nest actions.

See ACTION-LIST (type)
See ACTION (type)"))
