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
;;; A simple, but Byzantine fault tolerant, operation-based CRDT with
;;; causal delivery order.
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
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (^crdt))

(define-record-type <event>
  (make-event id parents timestamp public-key signature blob)
  event?
  (id event-id)                 ; SHA-256 hash
  (parents event-parents)       ; list of ID
  (timestamp event-timestamp)   ; HLC
  (public-key event-public-key) ; ed25519 key
  (signature event-signature)   ; ed25519 signature
  (blob event-blob))            ; bytevector

(define marshallers
  (list (cons event?
              (match-lambda
                (($ <event> id parents timestamp public-key signature blob)
                 (make-tagged 'event (list id parents timestamp public-key
                                           signature blob)))))
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

;; Default to leaving the event data as-is.
(define (prepare/default timestamp parents exp) exp)

;; Everything converges when nothing ever happens.
(define (effect/default id timestamp public-key exp prev) prev)

;; TODO: Use a hybrid op/state-based approach where replicas coming
;; online can sync the entire state at once and then receive
;; incremental updates while they remain online.
;;
;; TODO: Deliver incremental updates to the user so they don't have to
;; call 'ref' and diff the result with the previous version.
;;
;; Replica IDs must be unique amongst processes editing the CRDT.
;; Replica IDs are ephemeral and should be generated fresh for each
;; editing session.  Note that a replica ID is *not* the same as a
;; user ID.  Any given user could have multiple devices in use, or
;; multiple browser tabs open, and each should have a different
;; replica ID.
(define-actor (^crdt become replica-id private-key #:key
                     init
                     (prepare prepare/default)
                     (effect effect/default)
                     (query identity))
  (define public-key
    (captp-public-key->bytevector
     (key-pair->public-key private-key)))
  (define replicas (spawn ^cell (make-hashmap))) ; data sync peers
  (define clock (spawn ^cell (make-clock replica-id))) ; hybrid logical clock
  (define log (spawn ^cell (make-hashmap))) ; append-only event log
  (define pending (spawn ^cell (make-hashmap))) ; event queue
  (define heads (spawn ^cell '())) ; immediate causal predecessors
  (define state (spawn ^cell init)) ; accumulated internal state
  (define (append! event)
    (: log (hashmap-set (: log) (event-id event) event)))
  (define (update! id timestamp public-key exp)
    (: state (effect id timestamp public-key exp (: state))))
  (define (causally-consistent? event-ids)
    (every (lambda (id) (hashmap-ref (: log) id)) event-ids))
  ;; Exactly-once delivery in causal order.  Events remain in the
  ;; pending set until their direct predecessor events have arrived.
  ;;
  ;; Security note: Mallet could DoS Alice by writing a lot of events
  ;; with fake parent IDs and grow the pending set without bound.
  ;; Mallet could also do this with legitimate events and grow the
  ;; event log without bound.  Once Mallet has access to propagate
  ;; events to Alice's replica there's really nothing that can be done
  ;; (simply, anyway) from within the CRDT.  Mallet can instead be
  ;; held accountable by revoking the object capability that grants
  ;; access to Alice's replica.
  (define (deliver! pending)
    (hashmap-fold
     (lambda (event-id event pending)
       (match event
         (($ <event> event-id parents timestamp public-key _ blob)
          (cond
           ;; Predecessors are all here; apply the event!
           ((causally-consistent? parents)
            ;; Advance clock with every message delivered.
            (: clock (clock-join (: clock) timestamp))
            (append! event)
            (update! event-id timestamp public-key (decode blob))
            ;; Update heads (events with no successors).
            (: heads
               (cons event-id
                     (remove (lambda (event-id)
                               (member event-id parents))
                             (: heads))))
            (hashmap-remove pending event-id))
           ;; Missing one or more predecessors; do nothing.
           (else pending)))))
     pending pending))
  ;; Check event hash and signature.
  (define (valid? event)
    (match event
      (($ <event> event-id parents timestamp public-key signature blob)
       (and (bytevector=? event-id (sha256 (encode (list timestamp parents blob))))
            (verify (captp-signature->crypto-signature signature)
                    (encode (list parents blob))
                    ;; TODO: Canonicalize keys so we don't construct
                    ;; the same ones over and over?
                    (bytevector->crypto-public-key public-key))))))
  (define (sync! replica event-ids)
    (on-match (<- replica 'missing event-ids)
      ;; Remote replica has what we have; nothing to do!
      (() #t)
      ;; We have some things the remote replica doesn't.  Push the
      ;; specified missing commits and then recursively sync the
      ;; predecessors.
      (missing
       (let ((events (map (lambda (event-id)
                            (hashmap-ref (: log) event-id))
                          missing)))
         (let-on ((_ (<- replica 'push (map encode events))))
           (match (delete-duplicates
                   (append-map event-parents events))
             ;; We've reached the root of the DAG.
             (() #t)
             (event-ids
              (sync! replica event-ids))))))))
  (define (sync-all!)
    (hashmap-for-each (lambda (_ r) (sync! r (: heads))) (: replicas)))
  (methods
   ;; Return the user-visible representation of the current state.
   ((ref) (query (: state)))
   ;; Add a remote replica.
   ;;
   ;; TODO: Remove replica on severance.
   ((add-replica new)
    (let-on ((id* (<- new 'replica-id)))
      (: replicas (hashmap-set (: replicas) id* new))
      (sync! new (: heads))))
   ;; Query to see if any of the given events *or* their
   ;; predecessors are missing.
   ((missing event-ids)
    (hashmap-fold
     (lambda (event-id event memo) (cons event-id memo))
     '()
     (let lp ((event-ids event-ids) (missing (make-hashmap)))
       (fold (lambda (event-id missing)
               (cond
                ((hashmap-ref (: log) event-id)
                 missing)
                ((hashmap-ref (: pending) event-id) =>
                 (match-lambda
                   (($ <event> _ parents)
                    (lp parents missing))))
                (else
                 (hashmap-set missing event-id #t))))
             missing event-ids))))
   ;; Push (possibly) new events to the event log.
   ((push blobs)
    (let ((pending*
           ;; Add the new events to the pending set, filtering out
           ;; events we already have or that are garbage.
           (fold (lambda (event memo)
                   (match event
                     ((and event ($ <event> event-id))
                      (if (and (not (hashmap-ref (: log) event-id))
                               (not (hashmap-ref memo event-id))
                               (valid? event))
                          (hashmap-set memo event-id event)
                          memo))))
                 (: pending)
                 (map decode blobs))))
      (unless (eq? (: pending) pending*)
        ;; Deliver pending events until we reach a fixed point.
        ;;
        ;; TODO: Use a topological sort.
        (let lp ((events pending*))
          (let ((new (deliver! events)))
            (cond
             ((eq? events new)
              ;; Sync replicas when new events have been delivered.
              (unless (eq? events pending*)
                (sync-all!))
              (: pending events))
             (else (lp new))))))))
   ;; Commit a new operation to the event log.
   ((commit exp)
    ;; Advance our clock and create a new event.
    (let* ((timestamp (clock-tick (: clock)))
           (parents (: heads))
           (prepared (prepare timestamp parents exp))
           ;; Store user data as a blob in the log.
           (blob (encode prepared))
           ;; Event IDs are content-addressed.  The event ID is a
           ;; SHA-256 hash of the timestamp, parents, and blob.
           ;; Byzantine Mallet cannot send Alice and Bob different
           ;; events with the same ID.  One or both will not hash
           ;; properly and the invalid events will be rejected.
           (event-id (sha256 (encode (list timestamp parents blob))))
           ;; The signature incorporates the blob and the parent event
           ;; IDs.  Mallet cannot replay Alice's message in a new
           ;; event.  The parent events will have to be different due
           ;; to content addressing.  Since the parents are part of
           ;; the signature, Mallet cannot reuse one of Alice's
           ;; previous signatures.
           (signature (sign (encode (list parents blob)) private-key))
           (event (make-event event-id parents timestamp public-key signature blob)))
      (: clock timestamp)
      (append! event)
      (update! event-id timestamp public-key exp)
      ;; The log branches are now merged into one.
      (: heads (list event-id))
      (sync-all!)
      event-id))))
