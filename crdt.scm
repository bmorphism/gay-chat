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
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins utils hashmap)
  #:use-module (hlc)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (^crdt))

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
  (make-event id parents exp)
  event?
  (id event-id)           ; HLC
  (parents event-parents) ; vclock
  (exp event-exp))        ; any

(define (event->list event)
  (match event
    (($ <event> id parents exp)
     (list id (vclock->list parents) exp))))

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

;; TODO: Use a hybrid op/state-based approach where replicas coming
;; online can sync the entire state at once and then receive
;; incremental updates while they remain online.
;;
;; TODO: Deliver incremental updates to the user so they don't have to
;; call 'ref' and diff the result with the previous version.
;;
;; TODO: Byzantine fault tolerance.
(define-actor (^crdt become id #:key init (prepare identity) effect (query identity))
  (define replicas (spawn ^cell (make-hashmap))) ; data sync peers
  (define vclock (spawn ^cell empty-vclock))     ; vector clock
  (define log (spawn ^cell (make-hashmap))) ; append-only event log
  (define pending (spawn ^cell (make-hashmap))) ; pending events
  (define prev (spawn ^cell empty-vclock)) ; immediate causal predecessors
  (define state (spawn ^cell init))     ; accumulated internal state
  (define (clock-ref id)
    (or (hashmap-ref (: vclock) id) (make-clock id)))
  (define (tick!)
    (let ((new (clock-tick (clock-ref id))))
      (: vclock (hashmap-set (: vclock) id new))
      new))
  (define (join! timestamp)
    (let ((ours (clock-ref id)))
      (: vclock
         (hashmap-set (hashmap-set (: vclock) id (clock-join ours timestamp))
                      (clock-id timestamp) timestamp))))
  (define (append! event)
    (: log (hashmap-set (: log) (event-id event) event)))
  (define (update! timestamp exp)
    (: state (effect timestamp exp (: state))))
  (define (causally-consistent? vclock)
    (every (lambda (t) (hashmap-ref (: log) t))
           (vclock->list vclock)))
  ;; Exactly-once delivery in causal order.  Events remain in the
  ;; pending set until their direct predecessor events have arrived.
  (define (deliver! pending)
    (hashmap-fold
     (lambda (event-id event pending)
       (match event
         (($ <event> _ parents exp)
          (cond
           ;; Predecessors are all here; apply the event!
           ((causally-consistent? parents)
            ;; Advance clock with every message delivered.
            (join! event-id)
            (append! event)
            (update! event-id exp)
            ;; Check if this event is concurrent with the
            ;; predecessors.  If so, we have another branch to merge.
            (if (vclock<? (: prev) parents)
                (: prev (clock->vclock event-id))
                (: prev (vclock-set (: prev) event-id)))
            (hashmap-remove pending event-id))
           ;; Predecessors aren't all here; do nothing.
           (else pending)))))
     pending pending))
  (define (sync! replica)
    (let-on ((events (<- replica 'events-since (: vclock))))
      (let ((pending*
             ;; Append the new events, filtering out events we
             ;; already know about.
             (fold (lambda (event memo)
                     (match event
                       ((id parents exp)
                        (if (and (not (hashmap-ref (: log) id))
                                 (not (hashmap-ref memo id)))
                            (let ((event (make-event id (list->vclock parents) exp)))
                              (hashmap-set memo id event))
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
          (hashmap-for-each (lambda (_ r) (<-np r 'refresh id)) (: replicas))))))
  (methods
   ;; Add a new replica.
   ((add-replica id* replica)
    (: replicas (hashmap-set (: replicas) id* replica))
    (sync! replica))
   ;; Request to refresh using a specific replica.
   ((refresh who)
    (and=> (hashmap-ref (: replicas) who) sync!))
   ;; Collect and return a subset of events with timestamps newer than
   ;; the given vector clock.  Used by replicas to find new events and
   ;; reach eventual consistency.
   ((events-since vclock*)
    (define (visit-parents parents memo)
      (hashmap-fold (lambda (id event-id memo)
                      (visit (hashmap-ref (: log) event-id) memo))
                    memo parents))
    (define (visit event memo)
      (match event
        (($ <event> event-id parents exp)
         (match (hashmap-ref memo event-id)
           ;; Event isn't already in result set; continue.
           (#f
            (let ((clock (hashmap-ref vclock* (clock-id event-id))))
              ;; Continue as long as the event did not happen before the
              ;; last recorded event seen by the caller.
              (if (or (not clock)
                      (negative? (clock-compare-partial clock event-id)))
                  (let ((e (event->list event)))
                    (visit-parents parents (hashmap-set memo event-id e)))
                  memo)))
           ;; Event is already in result set; terminate.
           (_ memo)))))
    (hashmap-fold (lambda (id event result)
                    (cons event result))
                  '()
                  (visit-parents (: prev) (make-hashmap))))
   ;; Commit a local event to the log.
   ((commit exp)
    ;; Advance our clock and create a new event.
    (let* ((event-id (tick!))
           (event (make-event event-id (: prev) (prepare exp))))
      (append! event)
      (update! event-id exp)
      ;; The log branches are now merged into one.
      (: prev (clock->vclock event-id))
      ;; Notify replicas that we have fresh data.
      (hashmap-for-each (lambda (_ r) (<-np r 'refresh id)) (: replicas))))
   ;; Return the user-visible representation of the current state.
   ((ref) (query (: state)))))
