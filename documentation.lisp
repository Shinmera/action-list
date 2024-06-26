(in-package #:org.shirakumo.fraf.action-list)

(docs:define-docs
  (type action-list
    "Representation of an action list.

An action list is a sequence of actions that are updated in turn until
a blocking action is encountered.

When an action list is copied, each of the actions within is copied
via CLONE-INTO, as an action can only ever be contained within a
single action-list. If the action list's length is adjusted downwards,
excessive actions are removed from the list as if by POP-ACTION. If
the action list's length is adjusted upwards, new elements are filled
by copying INITIAL-ELEMENT if passed, and constructing DUMMY-ACTIONs
otherwise. If INITIAL-CONTENTS is passed to a copying or adjusting
operation, each action in the list is removed as if by POP-ACTION, and
the new actions as supplied are inserted instead.

In short, copied action-lists will not contain actions that are EQ to
the ones of the source list.

An action list can be copied via COPY-SEQ or MAKE-INSTANCE.

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
See FINISHED-P
See CLONE-INTO")
  
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
    "Returns the action list named by the symbol or in which the action is contained.

If the action is not currently contained in an action list, an error
is signalled instead.
If the symbol does not name a globally defined action list, an error
is signalled instead.

When SETF, sets the global action list by that name. If the value is
NIL, the binding is removed. If the value is not an action-list, an
error is signalled.

See ACTION (type)
See ACTION-LIST (type)
See DEFINE-ACTION-LIST")
  
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
    "Removes the action from its action list.

Signals an error if the action is not contained in any action list.

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

  (function clone-into
    "Copy the properties of SOURCE into NEW.

When NEW is T, a shallow copy of SOURCE is created and CLONE-INTO is
called with this new copy as NEW.

This function is used when copying ACTION-LISTs.

See ACTION-LIST (type)")
  
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

  (type dummy
    "Action that immediately finishes.

SEE ACTION (type)")
  
  (type delay
    "Action that delays execution by the passed DURATION.

By default blocks all lanes.

See TIME-LIMITED-ACTION
See LANE-LIMITED-ACTION")
  
  (type synchronize
    "Action that finishes once it reaches the front of the list.

By default blocks all lanes.

See LANE-LIMITED-ACTION")
  
  (type basic
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
See ACTION (type)")

  (type ease
    "Action that uses an easing function to tween between values.

The UPDATE-FUN receives two arguments: the action, and the current
value, rather than the delta. The current value is computed as
follows:

  from + ease-fun(elapsed / duration) * (to-from)

The EASE-FUN should be a function of one argument, a single-float in
the range [0,1] and should return another value to linearly
interpolate between FROM and TO.

See BASIC")

  (type repeat
    "Action that repeats the update only in specific intervals.

The UPDATE-FUN receives two arguments: the action, and the delta.

See BASIC"))

(docs:define-docs
  (function define-action-list
    "Define a globally named action list.

This is useful to define more static action lists such as animation
sequences and other fixed sequences of events that don't require the
more specific capabilities of action lists with dynamic action
insertion and removal.

It is intended that you instantiate a global action list for use like
so:

  (make-instance (action-list 'foo))

Each of the body forms is an action definition, parsed into an action
constructor. By default the following are recognised:

  (EVAL form...)
     An action that runs exactly once and executes the given FORMs.
  (SETF place value...)
     Same as EVAL except wrapped in setf.
  (EASE duration place initarg...)
     Sets PLACE to the value obtained by the EASE action. The action
     is blocking by default.
  (DELAY duration initarg...)
     Delays later actions by the given duration.
  (SYNCHRONIZE initarg...)
     Delays later actions until all prior actions finish.
  (REPEAT duration interval form...)
     An action that runs every INTERVAL seconds for up to DURATION.
     The action is blocking.

Note that the symbol at the front of each definition names how to
parse the rest, and must be a symbol. However, it must not necessarily
be a symbol from the ORG.SHIRAKUMO.FRAF.ACTION-LIST package. If the
symbol does not match exactly, a string search is performed
instead. This allows convenient definition of action lists without
importing the relevant symbols.

Further syntax can be added via DEFINE-ACTION-DEFINITION-PARSER

See ACTION-LIST (function)
See DEFINE-ACTION-DEFINITION-PARSER")

  (function define-action-definition-parser
    "Define a parsing function for action definitions.

The ARGS destructure the action definition. Should emit a form that
evaluates to a fresh ACTION instance.

See DEFINE-ACTION-LIST"))
