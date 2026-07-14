;;; Utilities for exporting visible gay://chat events from an in-memory room actor.
;;; This file is loaded by worlds/tests that have a live ^chat-room actor; it does
;;; not create rooms itself because Brassica rooms require Goblins vats.

(use-modules (brassica gay event)
             (ice-9 match))

(define (visible-message->gay-event message)
  (match message
    ((_ _ _ _ _ _ contents _)
     (and (gay-event? contents) contents))
    (_ #f)))

(define (room->gay-events room send)
  "Return visible gay-event contents from ROOM. SEND is a procedure of
shape (send room 'all-messages), allowing callers to use local `:' or
backend send wrappers."
  (filter-map visible-message->gay-event (send room 'all-messages)))
