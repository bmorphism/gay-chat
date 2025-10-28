;;; Local-first chat room
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
;;; An eventually consistent chat room CRDT.  Supports message
;;; editing, deletion, and emoji reacts.
;;;
;;; Code:

(define-module (chat)
  #:use-module (crdt)
  #:use-module (hlc)
  #:use-module ((goblins) #:hide ($))
  #:use-module ((goblins) #:select (($ . :)))
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins contrib base64)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (^identity
            ^chat-room))


;;;
;;; CRDT components
;;;

(define-record-type <lww-register>
  (make-lww-register timestamp value)
  lww-register?
  (timestamp lww-register-timestamp) ; HLC
  (value lww-register-value))        ; any

(define (lww-register-set register timestamp value)
  (match register
    (($ <lww-register> timestamp*)
     (if (negative? (clock-compare timestamp* timestamp))
         (make-lww-register timestamp value)
         register))))

(define-record-type <message>
  (%make-message id author created modified deleted contents reacts)
  message?
  (id message-id)             ; HLC
  (author message-author)     ; pubkey
  (created message-created)   ; epoch time
  (modified message-modified) ; epoch time | #f
  (deleted message-deleted)   ; epoch time | #f
  (contents message-contents) ; LWW register
  (reacts message-reacts))    ; char -> user -> LWW register

(define (make-message id author created contents)
  (%make-message id author created #f #f
                 (make-lww-register id contents)
                 (make-hashvmap)))

(define* (message-edit msg timestamp modified new)
  (match msg
    (($ <message> id author created _ deleted contents reacts)
     (let ((contents* (lww-register-set contents timestamp new)))
       (if (eq? contents contents*)
           msg
           (%make-message id author created modified deleted contents* reacts))))))

(define* (message-delete msg deleted)
  (match msg
    (($ <message> id author created modified #f contents reacts)
     (%make-message id author created modified deleted contents reacts))
    (_ msg)))

(define (%message-react msg timestamp reactor char value)
  (match msg
    (($ <message> id author created modified deleted contents reacts)
     (let ((char-reacts (or (hashmap-ref reacts char) (make-hashmap))))
       (match (hashmap-ref char-reacts reactor)
         (#f
          (let* ((register (make-lww-register timestamp value))
                 (char-reacts (hashmap-set char-reacts reactor register)))
            (%make-message id author created modified deleted contents
                           (hashmap-set reacts char char-reacts))))
         (register
          (let ((new (lww-register-set register timestamp value)))
            (if (eq? register new)
                msg
                (let ((char-reacts (hashmap-set char-reacts reactor new)))
                  (%make-message id author created modified deleted contents
                                 (hashmap-set reacts char char-reacts)))))))))))

(define (message-react msg timestamp reactor char)
  (%message-react msg timestamp reactor char #t))

(define (message-unreact msg timestamp reactor char)
  (%message-react msg timestamp reactor char #f))

(define (message<? a b)
  (clock<? (message-id a) (message-id b)))


;;;
;;; Actors
;;;

(define-actor (^identity become spn #:optional (private-key (generate-key-pair)))
  (define public-key (key-pair->public-key private-key))
  (methods
   ((spn) spn)
   ((private-key) private-key)
   ((public-key) public-key)
   ((sign data) (sign data private-key))))

;; A group is a CRDT for associating cryptographic identity with
;; self-proposed names.
(define-actor (^group become replica-id private-key)
  (define (query members)
    (hashmap-fold
     (lambda (public-key spn memo)
       (hashmap-set memo public-key (lww-register-value spn)))
     (make-hashmap) members))
  (define (effect timestamp public-key exp members)
    (match exp
      (('set-spn name)
       ;; The name might not be valid UTF-8, in which case we should
       ;; ignore the message entirely.
       (match (hashmap-ref members public-key)
         (#f
          (hashmap-set members public-key
                       (make-lww-register timestamp name)))
         (r
          (let ((r* (lww-register-set r timestamp name)))
            (if (eq? r r*)
                members
                (hashmap-set members public-key r*))))))
      (_ members)))
  (define crdt
    (spawn ^crypto-crdt replica-id private-key
           #:init (make-hashmap)
           #:query query
           #:effect effect))
  (methods
   ((add-replica replica)
    (: crdt 'add-replica replica))
   ((refresh replica-id)
    (: crdt 'refresh replica-id))
   ((events-since vclock)
    (: crdt 'events-since vclock))
   ((ref)
    (: crdt 'ref))
   ((set-spn name)
    (: crdt 'commit `(set-spn ,name)))))

(define-actor (^chat-log become replica-id private-key)
  (define (query messages)
    (map (match-lambda
           (($ <message> id author created modified deleted contents reacts)
            (list id author created modified deleted
                  (and (not deleted)
                       (lww-register-value contents))
                  (if deleted
                      '()
                      (hashmap-fold
                       (lambda (char registers memo)
                         (match (hashmap-fold
                                 (lambda (id register memo)
                                   (if (lww-register-value register)
                                       (cons id memo)
                                       memo))
                                 '() registers)
                           (() memo)
                           (reacts (cons (cons char reacts) memo))))
                       '() reacts)))))
         (sort (hashmap-fold (lambda (id msg memo) (cons msg memo)) '() messages)
               message<?)))
  (define (effect timestamp public-key exp messages)
    (match exp
      (('post author created contents)
       (hashmap-set messages timestamp
                    (make-message timestamp author created contents)))
      (('edit msgid when contents)
       (let ((msg (hashmap-ref messages msgid)))
         ;; Only the original author can edit.
         (if (equal? (message-author msg) public-key)
             (hashmap-set messages msgid
                          (message-edit msg timestamp when contents))
             messages)))
      (('delete msgid when)
       (let ((msg (hashmap-ref messages msgid)))
         ;; Only the original author can delete.
         (if (equal? (message-author msg) public-key)
             (hashmap-set messages msgid (message-delete msg when))
             messages)))
      (('react msgid char)
       (hashmap-set messages msgid
                    (message-react (hashmap-ref messages msgid)
                                   timestamp public-key char)))
      (('unreact msgid char)
       (hashmap-set messages msgid
                    (message-unreact (hashmap-ref messages msgid)
                                     timestamp public-key char)))
      (_ messages)))
  (define crdt
    (spawn ^crypto-crdt replica-id private-key
           #:init (make-hashmap)
           #:query query
           #:effect effect))
  (methods
   ((add-replica replica)
    (: crdt 'add-replica replica))
   ((refresh replica-id)
    (: crdt 'refresh replica-id))
   ((events-since vclock)
    (: crdt 'events-since vclock))
   ((ref)
    (: crdt 'ref))
   ((post author when contents)
    (: crdt 'commit `(post ,author ,when ,contents)))
   ((edit msgid when contents)
    (: crdt 'commit `(edit ,msgid ,when ,contents)))
   ((delete msgid when)
    (: crdt 'commit `(delete ,msgid ,when)))
   ((react msgid char)
    (: crdt 'commit `(react ,msgid ,char)))
   ((unreact msgid char)
    (: crdt 'commit `(unreact ,msgid ,char)))))

(define-actor (^chat-room become id #:optional (period (* 30 60)))
  (define spn (: id 'spn))
  (define private-key (: id 'private-key))
  (define public-key (: id 'public-key))
  ;; Generate a random replica ID.
  (define replica-id (base64-encode (strong-random-bytes 32) #:padding? #f))
  (define group (spawn ^group replica-id private-key))
  (: group 'set-spn spn)
  (define (^partition-replica become replica key)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((refresh id) (<-np replica 'refresh id key))
     ((events-since vclock) (<- replica 'events-since vclock key))))
  (define (^group-replica become replica)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((refresh id) (<-np replica 'group-refresh id))
     ((events-since vclock) (<- replica 'group-events-since vclock))))
  (define replicas (spawn ^cell '()))
  ;; The chat log is partitioned by time to keep the size of each
  ;; individual CRDT small and allow for dropping entire chunks of
  ;; history.  The message creation timestamp is used as the partition
  ;; key.  This means that editing/deleting/reacting require knowing
  ;; not only the message id, but also the creation timestamp so we
  ;; can find the right partition in which to apply the operation.
  (define partitions (spawn ^cell (make-hashvmap)))
  (define (partition-ref key)
    (or (hashmap-ref (: partitions) key)
        (let ((log (spawn ^chat-log replica-id private-key)))
          (: partitions (hashmap-set (: partitions) key log))
          (for-each
           (lambda (replica)
             (let ((replica* (spawn ^partition-replica replica key)))
               (: log 'add-replica replica*)))
           (: replicas))
          log)))
  (define (partition-for-time time)
    (partition-ref (floor (/ time period))))
  (methods
   ((replica-id) replica-id)
   ((add-replica replica)
    (: replicas (cons replica (: replicas)))
    (: group 'add-replica (spawn ^group-replica replica))
    (hashmap-for-each
     (lambda (key log)
       (let ((replica* (spawn ^partition-replica replica key)))
         (: log 'add-replica replica*)))
     (: partitions)))
   ((refresh id key)
    (: (partition-ref key) 'refresh id))
   ((events-since vclock key)
    (: (partition-ref key) 'events-since vclock))
   ((group-refresh id)
    (: group 'refresh id))
   ((group-events-since vclock)
    (: group 'events-since vclock))
   ((ref time)
    (: (partition-for-time time) 'ref))
   ;; Mainly for testing.
   ((ref-all)
    (append-map (match-lambda ((_ . log) (: log 'ref)))
                (sort (hashmap-fold (lambda (k v memo) (cons (cons k v) memo))
                                    '() (: partitions))
                      (lambda (a b) (< (car a) (car b))))))
   ((names) (: group 'ref))
   ((set-spn name) (: group 'set-spn name))
   ((post contents #:optional (now (current-time)))
    (: (partition-for-time now) 'post public-key now contents))
   ((edit msgid created contents #:optional (now (current-time)))
    (: (partition-for-time created) 'edit msgid now contents))
   ((delete msgid created #:optional (now (current-time)))
    (: (partition-for-time created) 'delete msgid now))
   ;; TODO: We're not tracking when someone reacted, but maybe we
   ;; should?
   ((react msgid created char)
    (: (partition-for-time created) 'react msgid char))
   ((unreact msgid created char)
    (: (partition-for-time created) 'unreact msgid char))))
