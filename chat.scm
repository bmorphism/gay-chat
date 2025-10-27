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
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (^chat-room))

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
  (%make-message id created modified deleted from contents reacts)
  message?
  (id message-id)             ; HLC
  (created message-created)   ; epoch time
  (modified message-modified) ; epoch time | #f
  (deleted message-deleted)   ; epoch time | #f
  (from message-from)         ; string (for now)
  (contents message-contents) ; LWW register
  (reacts message-reacts))    ; char -> user -> LWW register

(define (make-message id created from contents)
  (%make-message id created #f #f from
                 (make-lww-register id contents)
                 (make-hashmap)))

(define* (message-edit msg timestamp modified new)
  (match msg
    (($ <message> id created _ deleted from contents reacts)
     (let ((contents* (lww-register-set contents timestamp new)))
       (if (eq? contents contents*)
           msg
           (%make-message id created modified deleted from contents* reacts))))))

(define* (message-delete msg deleted)
  (match msg
    (($ <message> id created modified #f from contents reacts)
     (%make-message id created modified deleted from contents reacts))
    (_ msg)))

(define (%message-react msg timestamp reactor char value)
  (match msg
    (($ <message> id created modified deleted from contents reacts)
     (let ((char-reacts (or (hashmap-ref reacts char) (make-hashvmap))))
       (match (hashmap-ref char-reacts reactor)
         (#f
          (let* ((register (make-lww-register timestamp value))
                 (char-reacts (hashmap-set char-reacts reactor register)))
            (%make-message id created modified deleted from contents
                           (hashmap-set reacts char char-reacts))))
         (register
          (let ((new (lww-register-set register timestamp value)))
            (if (eq? register new)
                msg
                (let ((char-reacts (hashmap-set char-reacts reactor new)))
                  (%make-message id created modified deleted from contents
                                 (hashmap-set reacts char char-reacts)))))))))))

(define (message-react msg timestamp reactor char)
  (%message-react msg timestamp reactor char #t))

(define (message-unreact msg timestamp reactor char)
  (%message-react msg timestamp reactor char #f))

(define (message<? a b)
  (clock<? (message-id a) (message-id b)))

;; TODO: Sign messages.
;;
;; TODO: Only allow editing by the user that posted the message.
;;
;; TODO: Certificate capabilities generally.
(define (^chat-log become id)
  (define (query messages)
    (map (match-lambda
           (($ <message> id created modified deleted from contents reacts)
            (list id created modified deleted from
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
  (define (effect timestamp exp messages)
    (match exp
      (('post created from contents)
       (hashmap-set messages timestamp
                    (make-message timestamp created from contents)))
      (('edit id at contents)
       (hashmap-set messages id
                    (message-edit (hashmap-ref messages id)
                                  timestamp at contents)))
      (('delete id at)
       (hashmap-set messages id
                    (message-delete (hashmap-ref messages id) at)))
      (('react id char)
       (hashmap-set messages id
                    (message-react (hashmap-ref messages id)
                                   timestamp
                                   (clock-id timestamp)
                                   char)))
      (('unreact id char)
       (hashmap-set messages id
                    (message-unreact (hashmap-ref messages id)
                                     timestamp
                                     (clock-id timestamp)
                                     char)))))
  (define crdt
    (spawn ^crdt id
           #:init (make-hashmap)
           #:query query
           #:effect effect))
  (methods
   ((add-replica id replica)
    (: crdt 'add-replica id replica))
   ((refresh id)
    (: crdt 'refresh id))
   ((events-since vclock)
    (: crdt 'events-since vclock))
   ((ref)
    (: crdt 'ref))
   ((post from when contents)
    (: crdt 'commit `(post ,when ,from ,contents)))
   ((edit msgid when contents)
    (: crdt 'commit `(edit ,msgid ,when ,contents)))
   ((delete msgid when)
    (: crdt 'commit `(delete ,msgid ,when)))
   ((react msgid char)
    (: crdt 'commit `(react ,msgid ,char)))
   ((unreact msgid char)
    (: crdt 'commit `(unreact ,msgid ,char)))))

(define-actor (^chat-room become id #:optional (period (* 30 60)))
  (define (^partition-replica become replica key)
    (methods
     ((refresh id)
      (: replica 'refresh id key))
     ((events-since vclock)
      (: replica 'events-since vclock key))))
  (define replicas (spawn ^cell (make-hashmap)))
  ;; The chat log is partitioned by time to keep the size of each
  ;; individual CRDT small and allow for dropping entire chunks of
  ;; history.  The message creation timestamp is used as the partition
  ;; key.  This means that editing/deleting/reacting require knowing
  ;; not only the message id, but also the creation timestamp so we
  ;; can find the right partition in which to apply the operation.
  (define partitions (spawn ^cell (make-hashvmap)))
  (define (partition-ref key)
    (or (hashmap-ref (: partitions) key)
        (let ((log (spawn ^chat-log id)))
          (: partitions (hashmap-set (: partitions) key log))
          (hashmap-for-each
           (lambda (id replica)
             (let ((replica* (spawn ^partition-replica replica key)))
               (: log 'add-replica id replica*)))
           (: replicas))
          log)))
  (define (partition-for-time time)
    (partition-ref (floor (/ time period))))
  (methods
   ((add-replica id replica)
    (: replicas (hashmap-set (: replicas) id replica))
    (hashmap-for-each (lambda (key log)
                        (let ((replica* (spawn ^partition-replica replica key)))
                          (: log 'add-replica id replica*)))
                      (: partitions)))
   ((refresh id key)
    (: (partition-ref key) 'refresh id))
   ((events-since vclock key)
    (: (partition-ref key) 'events-since vclock))
   ((ref time)
    (: (partition-ref time) 'ref))
   ((ref-all)
    (append-map (match-lambda ((_ . log) (: log 'ref)))
                (sort (hashmap-fold (lambda (k v memo) (cons (cons k v) memo))
                                    '() (: partitions))
                      (lambda (a b) (< (car a) (car b))))))
   ((post from contents)
    (let ((now (current-time)))
      (: (partition-for-time now) 'post from now contents)))
   ((edit msgid created contents)
    (: (partition-for-time created) 'edit msgid (current-time) contents))
   ((delete msgid created)
    (: (partition-for-time created) 'delete msgid (current-time)))
   ((react msgid created char)
    (: (partition-for-time created) 'react msgid char))
   ((unreact msgid created char)
    (: (partition-for-time created) 'unreact msgid char))))
