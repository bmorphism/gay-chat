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

(define-module (brassica chat)
  #:use-module (brassica crdt)
  #:use-module (brassica hlc)
  #:use-module ((goblins) #:hide ($))
  #:use-module ((goblins) #:select (($ . :)))
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib pubsub)
  #:use-module (goblins contrib base64)
  #:use-module (goblins contrib syrup)
  #:use-module (goblins utils crypto)
  #:use-module (goblins utils hashmap)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:export (certificate-allows?
            ^identity
            ^chat-room))


;;;
;;; CRDT utilities
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


;;;
;;; Certificates
;;;

(define-record-type <certificate>
  (%make-certificate id parent signer controllers predicate revoked?)
  certificate?
  (id certificate-id)                   ; SHA-256 hash
  (parent certificate-parent)           ; SHA-256 hash
  (signer certificate-signer)           ; ed25519 public key
  (controllers certificate-controllers) ; list of ed25519 public key
  (predicate certificate-predicate)     ; procedure
  (revoked? certificate-revoked?))      ; boolean

(define (make-certificate root-signer id parent signer controllers exp)
  ;; Simple combinator language for capability attenuation.
  (define (compile-predicate exp)
    (match exp
      (#t (lambda (op author who) #t))
      (#f (lambda (op author who) #f))
      (('when-op (ops ...) exp)
       (let ((pred (compile-predicate exp)))
         (lambda (op author who)
           (if (memq op ops)
               (pred op author who)
               #t))))
      (('allow-self)
       (lambda (op author who)
         (and (member who controllers)
              (equal? author who))))))
  (and (or (and (not parent) (equal? signer root-signer))
           (member signer (certificate-controllers parent)))
       (let ((pred (compile-predicate exp)))
         (%make-certificate id parent signer controllers pred #f))))

(define (revoke-certificate certificate who)
  (match certificate
    (($ <certificate> id parent signer controllers predicate revoked?)
     (cond
      ;; Already revoked; no-op.
      (revoked? certificate)
      ;; Revoker is the signer; revoke!
      ((equal? signer who)
       (%make-certificate id parent signer controllers predicate #t))
      ;; Revoker is not the signer; rejected.
      ;;
      ;; TODO: Caller should record this incident for auditing
      ;; purposes.
      (else #f)))))


;;;
;;; Profiles
;;;

(define-record-type <profile>
  (%make-profile who self-proposed-name)
  profile?
  (who profile-who)
  (self-proposed-name profile-self-proposed-name))

(define (make-profile timestamp who name)
  (%make-profile who (make-lww-register timestamp name)))

(define (set-profile-self-proposed-name profile timestamp name)
  (match profile
    (($ <profile> who spn)
     (let ((new (lww-register-set spn timestamp name)))
       (if (eq? spn new)
           profile
           (%make-profile who new))))))


;;;
;;; Chat messages
;;;

(define-record-type <message>
  (%make-message id timestamp author certificate created-at contents
                 reacts edits deletes)
  message?
  (id message-id)                   ; SHA-256 hash
  (timestamp message-timestamp)     ; HLC
  (author message-author)           ; ed25519 public key
  (certificate message-certificate) ; SHA-256 hash
  (created-at message-created-at)   ; epoch time (ms)
  (contents message-contents)       ; any
  (reacts message-reacts)           ; list of <react>
  (edits message-edits)             ; list of <edit>
  (deletes message-deletes))        ; list of <delete>

(define-record-type <react>
  (make-react id timestamp reactor certificate reacted-at emoji reacted?)
  react?
  (id react-id)                   ; SHA-256 hash
  (timestamp react-timestamp)     ; HLC
  (reactor react-reactor)         ; ed25519 public key
  (certificate react-certificate) ; SHA-256 hash
  (reacted-at react-reacted-at)   ; epoch time (ms)
  (emoji react-emoji)             ; string
  (reacted? react-reacted?))      ; boolean

(define-record-type <edit>
  (make-edit id timestamp editor certificate modified-at contents)
  edit?
  (id edit-id)                   ; SHA-256 hash
  (timestamp edit-timestamp)     ; HLC
  (editor edit-editor)           ; ed25519 public key
  (certificate edit-certificate) ; SHA-256 hash
  (modified-at edit-modified-at) ; epoch time (ms)
  (contents edit-contents))      ; any

(define-record-type <delete>
  (make-delete id timestamp deleter certificate deleted-at)
  delete?
  (id delete-id)                   ; SHA-256 hash
  (timestamp delete-timestamp)     ; HLC
  (deleter delete-deleter)         ; ed25519 public key
  (certificate delete-certificate) ; SHA-256 hash
  (deleted-at delete-deleted-at))  ; epoch time (ms)

(define (make-message id timestamp author cert-id when contents)
  (%make-message id timestamp author cert-id when contents '() '() '()))

(define* (message-edit msg id timestamp editor cert-id when new)
  (match msg
    (($ <message> id* timestamp* author cert-id* created-at contents
                  reacts edits deletes)
     (let ((edit (make-edit id timestamp editor cert-id when new)))
       (%make-message id* timestamp* author cert-id* created-at contents
                      reacts (cons edit edits) deletes)))))

(define* (message-delete msg id timestamp deleter cert-id when)
  (match msg
    (($ <message> id* author timestamp* cert-id* created-at contents
                  reacts edits deletes)
     (let ((delete (make-delete id timestamp deleter cert-id when)))
       (%make-message id* author timestamp* cert-id* created-at contents
                      reacts edits (cons delete deletes))))))

(define (%message-react msg id timestamp reactor cert-id when emoji value)
  (match msg
    (($ <message> id* author timestamp* cert-id* created-at contents
                  reacts edits deletes)
     (let ((react (make-react id timestamp reactor cert-id when emoji value)))
       (%make-message id* author timestamp* cert-id* created-at contents
                      (cons react reacts) edits deletes)))))

(define (message-react msg id timestamp reactor cert-id when emoji)
  (%message-react msg id timestamp reactor cert-id when emoji #t))

(define (message-unreact msg id timestamp reactor cert-id when emoji)
  (%message-react msg id timestamp reactor cert-id when emoji #f))

(define (message<? a b)
  (if (= (message-created-at a) (message-created-at b))
      (clock<? (message-timestamp a) (message-timestamp b))
      (< (message-created-at a) (message-created-at b))))

(define (react<? a b)
  (clock<? (react-timestamp a) (react-timestamp b)))

(define (edit>? a b)
  (clock>? (edit-timestamp a) (edit-timestamp b)))

(define (delete>? a b)
  (clock>? (delete-timestamp a) (delete-timestamp b)))

(define (certificate-allows? cert op author who)
  (and (member who (certificate-controllers cert))
       (let check ((cert cert))
         (or (not cert)
             (and (not (certificate-revoked? cert))
                  (check (certificate-parent cert))
                  ((certificate-predicate cert) op author who))))))


;;;
;;; Actors
;;;

(define-actor (^identity become spn #:optional (private-key (generate-key-pair)))
  (define public-key
    (captp-public-key->bytevector
     (key-pair->public-key private-key)))
  (methods
   ((spn) spn)
   ((private-key) private-key)
   ((public-key) public-key)
   ((sign data) (sign data private-key))))

(define-syntax-rule (effect+publish effect pubsub arg ...)
  (lambda (id timestamp who exp prev)
    (let ((result (effect id timestamp who exp prev)))
      (<-np pubsub 'publish arg ...)
      result)))

;; The certificates CRDT accumulates a set of certificate
;; capabilities.
(define-actor (^certificates become replica-id root-signer private-key pubsub)
  (define (effect id timestamp who exp certificates)
    (match exp
      (('add-certificate parent-id controllers pred)
       (let ((parent (hashmap-ref certificates parent-id)))
         (match (make-certificate root-signer id parent who controllers pred)
           (#f certificates)
           (cert (hashmap-set certificates id cert)))))
      (('revoke-certificate cert-id)
       (match (hashmap-ref certificates cert-id)
         (#f certificates)
         (cert
          (match (revoke-certificate cert who)
            (#f certificates)
            (new
             (if (eq? cert new)
                 certificates
                 (hashmap-set certificates cert-id new)))))))
      (_ certificates)))
  (define crdt
    (spawn ^crdt replica-id private-key
           #:init (make-hashmap)
           #:effect (effect+publish effect pubsub 'update-certificates)))
  (methods
   ((add-replica replica) (: crdt 'add-replica replica))
   ((remove-replica replica) (: crdt 'remove-replica replica))
   ((ref) (: crdt 'ref))
   ((missing event-ids) (: crdt 'missing event-ids))
   ((push events) (: crdt 'push events))
   ((add-certificate parent-id controllers pred)
    (: crdt 'commit `(add-certificate ,parent-id ,controllers ,pred)))
   ((revoke-certificate cert-id)
    (: crdt 'commit `(revoke-certificate ,cert-id)))))

;; The profiles CRDT accumulates user metadata.
(define-actor (^profiles become replica-id private-key pubsub)
  (define (query profiles)
    (hashmap-fold
     (lambda (who profile memo)
       (match profile
         (($ <profile> _ spn)
          (alist-cons who (lww-register-value spn) memo))))
     '() profiles))
  (define (effect id timestamp who exp profiles)
    (match exp
      (('set-spn name)
       (match (hashmap-ref profiles who)
         (#f (hashmap-set profiles who (make-profile timestamp who name)))
         (profile
           (let ((new (set-profile-self-proposed-name profile timestamp name)))
             (if (eq? profile new)
                 profiles
                 (hashmap-set profiles who new))))))
      (_ profiles)))
  (define crdt
    (spawn ^crdt replica-id private-key
           #:init (make-hashmap)
           #:query query
           #:effect (effect+publish effect pubsub 'update-profiles)))
  (methods
   ((add-replica replica) (: crdt 'add-replica replica))
   ((remove-replica replica) (: crdt 'remove-replica replica))
   ((ref) (: crdt 'ref))
   ((missing event-ids) (: crdt 'missing event-ids))
   ((push events) (: crdt 'push events))
   ((set-spn name) (: crdt 'commit `(set-spn ,name)))))

;; A chat log holds one chunk of a chat room's history.
(define-actor (^chat-log become replica-id private-key pubsub)
  (define (query messages)
    (map (match-lambda
           (($ <message> id timestamp author cert-id created-at contents
                         reacts edits deletes)
            (list id author cert-id created-at contents
                  ;; Reacts.
                  (map (match-lambda
                         (($ <react> _ _ reactor cert-id when emoji reacted?)
                          (list reactor cert-id when emoji reacted?)))
                       (sort reacts react<?))
                  ;; Edits, in reverse chronological order.
                  (map (match-lambda
                         (($ <edit> _ _ editor cert-id when contents)
                          (list editor cert-id when contents)))
                       (sort edits edit>?))
                  ;; Deletes, in reverse chronological order.
                  (map (match-lambda
                         (($ <delete> _ _ deleter cert-id when)
                          (list deleter cert-id when)))
                       (sort deletes delete>?)))))
         ;; Chronological order.
         (sort (hashmap-fold (lambda (id msg memo) (cons msg memo)) '() messages)
               message<?)))
  ;; TODO: Ignore events referring to messages that are not from
  ;; causal predecessors.
  (define (effect id timestamp who exp messages)
    (define-syntax-rule (publish arg ...)
      (<-np pubsub 'publish arg ...))
    (match exp
      (('post cert-id created contents)
       (let ((msg (make-message id timestamp who cert-id created contents)))
         (publish 'new-message id created)
         (hashmap-set messages id msg)))
      (('edit cert-id msg-id when contents)
       (let* ((old (hashmap-ref messages msg-id))
              (new (message-edit old id timestamp who cert-id when contents)))
         (publish 'modify-message msg-id (message-created-at new))
         (hashmap-set messages msg-id new)))
      (('delete cert-id msg-id when)
       (let* ((old (hashmap-ref messages msg-id))
              (new (message-delete old id timestamp who cert-id when)))
         (publish 'modify-message msg-id (message-created-at new))
         (hashmap-set messages msg-id new)))
      (('react cert-id msg-id when emoji)
       (let* ((old (hashmap-ref messages msg-id))
              (new (message-react old id timestamp who cert-id when emoji)))
         (publish 'modify-message msg-id (message-created-at new))
         (hashmap-set messages msg-id new)))
      (('unreact cert-id msg-id when emoji)
       (let* ((old (hashmap-ref messages msg-id))
              (new (message-unreact old id timestamp who cert-id when emoji)))
         (publish 'modify-message msg-id (message-created-at new))
         (hashmap-set messages msg-id new)))
      (_ messages)))
  (define crdt
    (spawn ^crdt replica-id private-key
           #:init (make-hashmap)
           #:query query
           #:effect effect))
  (methods
   ((add-replica replica) (: crdt 'add-replica replica))
   ((remove-replica replica) (: crdt 'remove-replica replica))
   ((ref) (: crdt 'ref))
   ((missing event-ids) (: crdt 'missing event-ids))
   ((push events) (: crdt 'push events))
   ((post cert-id when contents)
    (: crdt 'commit `(post ,cert-id ,when ,contents)))
   ((edit cert-id msgid when contents)
    (: crdt 'commit `(edit ,cert-id ,msgid ,when ,contents)))
   ((delete cert-id msgid when)
    (: crdt 'commit `(delete ,cert-id ,msgid ,when)))
   ((react cert-id msgid when emoji)
    (: crdt 'commit `(react ,cert-id ,msgid ,when ,emoji)))
   ((unreact cert-id msgid when emoji)
    (: crdt 'commit `(unreact ,cert-id ,msgid ,when ,emoji)))))

(define (spawn-revokable-and-revoker obj)
  (define token (list 'revoke))
  (define (revoked . args) (error "revoked"))
  (define (^revokable become)
    (case-lambda
      ((x)
       (if (eq? x token)
           (become revoked)
           (: obj x)))
      (args (apply : obj args))))
  (define (^revoker become)
    (lambda () (: proxy token)))
  (define proxy (spawn ^revokable))
  (list proxy (spawn ^revoker)))

(define-actor (^chat-room become id root-signer #:optional (period (* 30 60 1000)))
  (define (^partition-replica become replica key)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((missing event-ids) (<- replica 'missing/messages event-ids key))
     ((push events) (<- replica 'push/messages events key))))
  (define (^certificates-replica become replica)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((missing event-ids) (<- replica 'missing/certificates event-ids))
     ((push events) (<- replica 'push/certificates events))))
  (define (^profiles-replica become replica)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((missing event-ids) (<- replica 'missing/profiles event-ids))
     ((push events) (<- replica 'push/profiles events))))
  (define (^chat-room-replica become)
    (methods
     ((replica-id) replica-id)
     ((missing/messages heads key)
      (: (partition-ref key) 'missing heads))
     ((push/messages events key)
      (: (partition-ref key) 'push events))
     ((missing/certificates heads)
      (: certificates 'missing heads))
     ((push/certificates events)
      (: certificates 'push events))
     ((missing/profiles heads)
      (: profiles 'missing heads))
     ((push/profiles events)
      (: profiles 'push events))))
  (define spn (: id 'spn))
  (define private-key (: id 'private-key))
  (define public-key (: id 'public-key))
  ;; Generate a random replica ID.
  (define replica-id (base64-encode (strong-random-bytes 32) #:padding? #f))
  ;; Our replica interface.
  (define replica (spawn ^chat-room-replica))
  (define pubsub (spawn ^pubsub))
  (define certificates
    (spawn ^certificates replica-id root-signer private-key pubsub))
  (define profiles (spawn ^profiles replica-id private-key pubsub))
  ;; Tell the group our self-proposed name.
  (: profiles 'set-spn spn)
  (define replicas (spawn ^cell '()))
  (define partition-replicas (spawn ^cell (make-hashqmap)))
  ;; The chat log is partitioned by time to keep the size of each
  ;; individual CRDT small and allow for dropping entire chunks of
  ;; history.  The message creation timestamp is used as the partition
  ;; key.  This means that editing/deleting/reacting require knowing
  ;; not only the message id, but also the creation timestamp so we
  ;; can find the right partition in which to apply the operation.
  (define partitions (spawn ^cell (make-hashvmap)))
  (define (partition-ref key)
    (or (hashmap-ref (: partitions) key)
        (let ((log (spawn ^chat-log replica-id private-key pubsub)))
          (: partitions (hashmap-set (: partitions) key log))
          (for-each
           (lambda (replica)
             (let ((replica* (spawn ^partition-replica replica key)))
               (: log 'add-replica replica*)
               ;; Add to set of partition replicas for this remote
               ;; replica.  We use this for removing the local proxy
               ;; replicas later when the remote replica disconnects.
               (: partition-replicas
                  (hashmap-set (: partition-replicas) replica
                               (cons (cons log replica*)
                                     (hashmap-ref (: partition-replicas)
                                                  replica '()))))))
           (: replicas))
          log)))
  (define (partition-for-time time)
    (partition-ref (floor (/ time period))))
  (define (render-message message certs)
    (define (allowed? cert-id op author who)
      (match (hashmap-ref certs cert-id)
        (#f #f)
        (cert
         (certificate-allows? cert op author who))))
    (define (latest-edit author edits)
      (find (match-lambda
              ((who cert-id when contents)
               (allowed? cert-id 'edit author who)))
            edits))
    (define (latest-delete author deletes)
      (find (match-lambda
              ((who cert-id when)
               (allowed? cert-id 'delete author who)))
            deletes))
    (define (reactions author reacts)
      (fold (lambda (react memo)
              (match react
                ((who cert-id when emoji reacted?)
                 (if (allowed? cert-id 'react author who)
                     (let ((whos (hashmap-ref memo emoji '())))
                       (if reacted?
                           (hashmap-set memo emoji
                                        (lset-adjoin equal? whos who))
                           (match (delete who whos)
                             (() (hashmap-remove memo emoji))
                             (whos (hashmap-set memo emoji whos)))))
                     memo))))
            (make-hashmap) reacts))
    (match message
      ((msg-id author cert-id created-at contents reacts edits deletes)
       (if (allowed? cert-id 'post author author)
           (let ((edited (latest-edit author edits))
                 (deleted (latest-delete author deletes)))
             (list msg-id author cert-id
                   ;; Created, modified, deleted timestamps.
                   created-at
                   (match edited
                     (#f #f)
                     ((_ _ when _) when))
                   (match deleted
                     (#f #f)
                     ((_ _ when) when))
                   ;; Contents, either the original, the latest
                   ;; edit, or nothing if the message was
                   ;; "deleted".
                   (and (not deleted)
                        (match edited
                          (#f contents)
                          ((_ _ _ contents) contents)))
                   ;; Emoji reactions.
                   (hashmap-fold alist-cons '()
                                 (reactions author reacts))))))))
  ;; TODO: This is O(n), ugh.
  (define (message-view messages msg-id certs)
    (let lp ((messages messages))
      (match messages
        (() #f)
        ((message . messages)
         (match message
           ((msg-id* . _)
            (if (equal? msg-id msg-id*)
                (render-message message certs)
                (lp messages))))))))
  (define (messages-view messages certs)
    (fold-right
     (lambda (message memo)
       (cons (render-message message certs) memo))
     '() messages))
  (methods
   ((replica-id) replica-id)
   ((root-signer) root-signer)
   ((subscribe subscriber) (: pubsub 'subscribe subscriber))
   ((unsubscribe subscriber) (: pubsub 'unsubscribe subscriber))
   ;; Spawn a revokable proxy to our replica that we can share with
   ;; someone else.
   ((fresh-replica)
    (spawn-revokable-and-revoker replica))
   ((add-replica replica)
    ;; Create local proxies to the remote replica for each component
    ;; CRDT inside this chat room.
    (define certs-replica (spawn ^certificates-replica replica))
    (define profiles-replica (spawn ^profiles-replica replica))
    (: replicas (cons replica (: replicas)))
    (: certificates 'add-replica certs-replica)
    (: profiles 'add-replica profiles-replica)
    ;; Create and register new partition replicas so they can be
    ;; cleaned up later when the remote replica disconnects.
    (: partition-replicas
       (hashmap-set (: partition-replicas) replica
                    (hashmap-fold
                     (lambda (key log replicas)
                       (let ((replica* (spawn ^partition-replica replica key)))
                         (: log 'add-replica replica*)
                         (cons (cons log replica*) replicas)))
                     '()  (: partitions))))
    ;; Remove all local proxy replicas when the remote replica
    ;; disconnects.
    (when (remote-refr? replica)
      (on-sever
       replica
       (lambda (type reason)
         (: replicas (delq replica (: replicas)))
         (: certificates 'remove-replica certs-replica)
         (: profiles 'remove-replica profiles-replica)
         (for-each (match-lambda
                     ((log . partition-replica)
                      (: log 'remove-replica partition-replica)))
                   (hashmap-ref (: partition-replicas) replica)))))
    #t)
   ;; Return data for a single message.
   ((ref-message msg-id created-at)
    (message-view (: (partition-for-time created-at) 'ref)
                  msg-id
                  (: certificates 'ref)))
   ;; Return a list of *all* messages across all partitions, in
   ;; chronological order.
   ((all-messages)
    (messages-view
     (append-map (match-lambda ((_ . log) (: log 'ref)))
                 ;; Partitions in chronological order.
                 (sort (hashmap-fold (lambda (k v memo) (cons (cons k v) memo))
                                     '() (: partitions))
                       (lambda (a b) (< (car a) (car b)))))
     (: certificates 'ref)))
   ((certificates) (: certificates 'ref))
   ((profiles) (: profiles 'ref))
   ((set-spn name) (: profiles 'set-spn name))
   ((add-certificate #:key parent (controllers '()) (predicate #t))
    (: certificates 'add-certificate parent controllers predicate))
   ((revoke-certificate cert-id)
    (: certificates 'revoke-certificate cert-id))
   ((post cert-id contents #:optional (now (current-time/ms)))
    (: (partition-for-time now) 'post cert-id now contents))
   ((edit cert-id msg-id created contents #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'edit cert-id msg-id now contents))
   ((delete cert-id msg-id created #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'delete cert-id msg-id now))
   ((react cert-id msg-id created emoji #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'react cert-id msg-id now emoji))
   ((unreact cert-id msg-id created emoji #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'unreact cert-id msg-id now emoji))))
