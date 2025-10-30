;;; Operation-based CRDT
;;; Copyright (C) 2025 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; A simple operation-based CRDT with an append-only event log.
;;; Operations are processed in causal order.
;;;
;;; Code:

(define-module (crdt)
  #:use-module ((goblins) #:hide ($))
  #:use-module ((goblins) #:select (($ . :)))
  #:use-module (goblins abstract-types)
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils hashmap)
  #:use-module (hlc)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (make-event
            event?
            event-id
            event-parents
            event-timestamp
            event-data

            ^crdt
            ^crypto-crdt))

(define (hashmap-keys h)
  (hashmap-fold (lambda (k v memo) (cons k memo)) '() h))

(define empty-vclock (make-hashmap))

(define (list->vclock clocks)
  (fold (lambda (clock vclock)
          (hashmap-set vclock (clock-id clock) clock))
        (make-hashmap) clocks))

(define (vclock->list vclock)
  (hashmap-fold (lambda (id clock memo)
                  (cons clock memo))
                '() vclock))

(define (clock->vclock clock)
  (hashmap ((clock-id clock) clock)))

(define (vclock-set vclock clock)
  (hashmap-set vclock (clock-id clock) clock))

(define-record-type <event>
  (make-event id parents timestamp data)
  event?
  (id event-id)               ; any
  (parents event-parents)     ; list of id
  (timestamp event-timestamp) ; HLC
  (data event-data))          ; any

(define (marshall-clock clock)
  (match clock
    (($ <clock> real logical id)
     (list real logical id))))

(define (marshall-vclock vclock)
  (hashmap-fold (lambda (id clock memo)
                  (cons (marshall-clock clock) memo))
                '() vclock))

(define (unmarshall-clock clock)
  (match clock
    ((real logical id)
     (%make-clock real logical id))))

(define (unmarshall-vclock vclock)
  (fold (lambda (clock memo)
          (let ((clock (unmarshall-clock clock)))
            (hashmap-set memo (clock-id clock) clock)))
        (make-hashmap) vclock))

(define marshallers
  (list (cons event?
              (match-lambda
                (($ <event> id parents timestamp data)
                 (make-tagged 'event (list id parents timestamp data)))))
        (cons clock?
              (match-lambda
                (($ <clock> real logical id)
                 (make-tagged 'clock (list real logical id)))))
        (cons char?
              (lambda (ch)
                (make-tagged 'char (list (char->integer ch)))))))

(define unmarshallers
  (list (cons (lambda (label) (eq? label 'event)) make-event)
        (cons (lambda (label) (eq? label 'clock)) %make-clock)
        (cons (lambda (label) (eq? label 'char)) integer->char)))

(define (encode x)
  (syrup-encode x #:marshallers marshallers))

(define (decode bv)
  (syrup-decode bv #:unmarshallers unmarshallers))

;; a < b when all node clocks in a are <= the associated clocks in b
;; and at least one clock in a is < than the associated clock in b.
(define (vclock<? a b)
  (let ((ids (delete-duplicates (append (hashmap-keys a) (hashmap-keys b)))))
    (let lp ((ids ids)
             (less? #f))
      (match ids
        (() less?)
        ((id . ids)
         (match (hashmap-ref a id)
           (#f (lp ids #t))
           (clock-a
            (match (hashmap-ref b id)
              (#f #f)
              (clock-b
               (match (clock-compare-partial clock-a clock-b)
                 (0 (lp ids less?))
                 ((? negative?) (lp ids #t))
                 (_ #f)))))))))))

;; Default to using the timestamp as the unique event ID.
(define (prepare/default timestamp parents data)
  (make-event timestamp parents timestamp data))

;; Default to doing absolutely nothing at all.
(define (effect/default timestamp data prev) prev)

;; TODO: Use a hybrid op/state-based approach where replicas coming
;; online can sync the entire state at once and then receive
;; incremental updates while they remain online.
;;
;; TODO: Deliver incremental updates to the user so they don't have to
;; call 'ref' and diff the result with the previous version.
;;
;; TODO: Byzantine fault tolerance.
;;
;; Replica IDs must be unique amongst processes editing the CRDT.
;; Replica IDs are ephemeral and should be generated fresh for each
;; editing session.  Note that a replica ID is *not* the same as a
;; user ID.  Any given user could have multiple devices in use, or
;; multiple browser tabs open, and each should have a different
;; replica ID.
(define-actor (^crdt become replica-id #:key
                     init
                     (prepare prepare/default)
                     (effect effect/default)
                     (query identity))
  (define replicas (spawn ^cell (make-hashmap))) ; data sync peers
  (define vclock (spawn ^cell empty-vclock))     ; vector clock
  (define log (spawn ^cell (make-hashmap))) ; append-only event log
  (define pending (spawn ^cell (make-hashmap))) ; pending events
  (define heads (spawn ^cell '()))     ; immediate causal predecessors
  (define state (spawn ^cell init))    ; accumulated internal state
  (define (clock-ref id)
    (or (hashmap-ref (: vclock) id) (make-clock replica-id)))
  (define (tick!)
    (let ((new (clock-tick (clock-ref replica-id))))
      (: vclock (hashmap-set (: vclock) replica-id new))
      new))
  (define (join! timestamp)
    (let ((ours (clock-ref replica-id)))
      (: vclock
         (hashmap-set (hashmap-set (: vclock) replica-id (clock-join ours timestamp))
                      (clock-id timestamp) timestamp))))
  (define (append! event)
    (: log (hashmap-set (: log) (event-id event) event)))
  (define (update! event)
    (match event
      (($ <event> id parents timestamp data)
       (: state (effect timestamp data (: state))))))
  (define (causally-consistent? event-ids)
    (every (lambda (id) (hashmap-ref (: log) id)) event-ids))
  (define (lookup-events event-ids)
    (let ((log (: log)))
      (map (lambda (event-id) (hashmap-ref log event-id)) event-ids)))
  ;; Exactly-once delivery in causal order.  Events remain in the
  ;; pending set until their direct predecessor events have arrived.
  (define (deliver! pending)
    (hashmap-fold
     (lambda (event-id event pending)
       (match event
         (($ <event> _ parents timestamp)
          (cond
           ;; Predecessors are all here; apply the event!
           ((causally-consistent? parents)
            ;; Advance clock with every message delivered.
            (join! event-id)
            (append! event)
            (update! event)
            ;; Check if this event is concurrent with the
            ;; predecessors.  If so, we have another branch to merge.
            ;;
            ;; TODO: I don't think this is quite right and the code is
            ;; inefficient.
            (if (vclock<? (list->vclock
                           (map event-timestamp (lookup-events (: heads))))
                          (list->vclock
                           (map event-timestamp (lookup-events parents))))
                (: heads (list event-id))
                (: heads (cons event-id (: heads))))
            (hashmap-remove pending event-id))
           ;; Predecessors aren't all here; do nothing.
           (else pending)))))
     pending pending))
  (define (sync! replica)
    ;; TODO: Do we need to send the complete vector clock (which grows
    ;; without bound) or can we just send the direct predecessor
    ;; clocks?
    (let-on ((events (<- replica 'events-since (marshall-vclock (: vclock)))))
      (let ((pending*
             ;; Append the new events, filtering out events we
             ;; already know about.
             (fold (lambda (event memo)
                     (match (decode event)
                       ((and event ($ <event> event-id))
                        (if (and (not (hashmap-ref (: log) event-id))
                                 (not (hashmap-ref memo event-id)))
                            (hashmap-set memo event-id event)
                            memo))))
                   (: pending) events)))
        (unless (eq? (: pending) pending*)
          ;; Process pending events until we reach a fixed point.
          (let lp ((memo pending*))
            (let ((new (deliver! memo)))
              (if (eq? memo new)
                  (: pending new)
                  (lp new))))
          ;; Notify other replicas.
          (hashmap-for-each
           (lambda (_ r)
             (unless (eq? r replica)
               (<-np r 'refresh replica-id)))
           (: replicas))))))
  (methods
   ((replica-id) replica-id)
   ;; Add a new replica.
   ;;
   ;; TODO: Remove replica on severance.
   ((add-replica replica)
    (let-on ((id* (<- replica 'replica-id)))
      (: replicas (hashmap-set (: replicas) id* replica))
      (sync! replica)))
   ;; Request to refresh using a specific replica.
   ((refresh replica-id)
    (and=> (hashmap-ref (: replicas) replica-id) sync!))
   ;; Collect and return a subset of events with timestamps newer than
   ;; the given vector clock.  Used by replicas to find new events and
   ;; reach eventual consistency.
   ((events-since vclock*)
    (let ((vclock* (unmarshall-vclock vclock*)))
      (define (visit-parents parents memo)
        (fold (lambda (event-id memo)
                (visit (hashmap-ref (: log) event-id) memo))
              memo parents))
      (define (visit event memo)
        (match event
          (($ <event> event-id parents timestamp exp)
           (match (hashmap-ref memo event-id)
             ;; Event isn't already in result set; continue.
             (#f
              (let ((clock (hashmap-ref vclock* (clock-id timestamp))))
                ;; Continue as long as the event did not happen before the
                ;; last recorded event seen by the caller.
                (if (or (not clock)
                        (negative? (clock-compare-partial clock timestamp)))
                    (visit-parents parents (hashmap-set memo event-id event))
                    memo)))
             ;; Event is already in result set; terminate.
             (_ memo)))))
      (hashmap-fold (lambda (id event result)
                      (cons (encode event) result))
                    '()
                    (visit-parents (: heads) (make-hashmap)))))
   ;; Commit a local event to the log.
   ((commit data)
    ;; Advance our clock and create a new event.
    (let* ((event-id (tick!))
           (event (prepare event-id (: heads) data)))
      (append! event)
      (update! event)
      ;; The log branches are now merged into one.
      (: heads (list event-id))
      ;; Notify replicas that we have fresh data.
      (hashmap-for-each
       (lambda (_ r) (<-np r 'refresh replica-id))
       (: replicas))))
   ;; Return the user-visible representation of the current state.
   ((ref) (query (: state)))))

;; Wrapper CRDT that signs and verifies all operations.
(define-actor (^crypto-crdt become replica-id private-key #:key
                            init
                            (prepare prepare/default)
                            (effect effect/default)
                            (query identity))
  (define public-key
    (captp-public-key->bytevector
     (key-pair->public-key private-key)))
  (define (prepare* timestamp parents exp)
    (let* ((data (encode exp))
           (sig (sign data private-key)))
      (make-event timestamp parents timestamp (list public-key sig data))))
  (define (effect* timestamp exp prev)
    (match exp
      ((public-key sig data)
       (let ((public-key* (bytevector->crypto-public-key public-key))
             (sig (captp-signature->crypto-signature sig)))
         (if (verify sig data public-key*)
             (effect timestamp public-key (decode data) prev)
             ;; TODO: Probably should do something other than silently
             ;; ignoring the message so Mallet can be held accountable
             ;; for their attempt at deception.
             prev)))
      (_ prev)))
  (spawn ^crdt replica-id
         #:init init
         #:prepare prepare*
         #:effect effect*
         #:query query))
