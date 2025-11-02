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
;;; Certificates and profiles
;;;

(define-record-type <certificate>
  (make-certificate id parent signer controllers predicate revoked?)
  certificate?
  (id certificate-id)                   ; SHA-256 hash
  (parent certificate-parent)           ; SHA-256 hash
  (signer certificate-signer)           ; ed25519 public key
  (controllers certificate-controllers) ; list of ed25519 public key
  (predicate certificate-predicate)     ; procedure
  (revoked? certificate-revoked?))      ; boolean

(define-record-type <profile>
  (%make-profile public-key self-proposed-name)
  profile?
  (public-key profile-public-key)
  (self-proposed-name profile-self-proposed-name))

(define (make-profile timestamp public-key name)
  (%make-profile public-key (make-lww-register timestamp name)))

(define-record-type <group>
  (%make-group certificates profiles)
  group?
  (certificates group-certificates)
  (profiles group-profiles))

(define (make-group)
  (%make-group (make-hashmap) (make-hashmap)))

(define (set-group-profile-self-proposed-name group timestamp public-key name)
  (match group
    (($ <group> certificates profiles)
     (match (hashmap-ref profiles public-key)
       (#f
        (let ((profile (make-profile timestamp public-key name)))
          (%make-group certificates (hashmap-set profiles public-key profile))))
       (($ <profile> public-key spn)
        (let ((new (lww-register-set spn timestamp name)))
          (if (eq? spn new)
              group
              (let ((profile (%make-profile public-key new)))
                (%make-group certificates
                             (hashmap-set profiles public-key profile))))))))))

(define (add-group-certificate group root-signer id parent signer
                               controllers exp)
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
         (equal? author who)))))
  (match group
    (($ <group> certificates profiles)
     (define (valid parent)
       (let* ((pred (compile-predicate exp))
              (cert (make-certificate id parent signer controllers pred #f)))
         (%make-group (hashmap-set certificates id cert) profiles)))
     (if parent
         (match (hashmap-ref certificates parent)
           ;; No such parent; invalid.
           (#f group)
           ((and parent ($ <certificate> _ _ _ parent-controllers))
            (if (member signer parent-controllers)
                ;; Valid certificate.
                (valid parent)
                ;; Parent capability was not for the signer of this
                ;; capability; invalid.
                group)))
         ;; Root certificates have no parent.  The signer must be the
         ;; root signer in this case.
         (if (equal? signer root-signer)
             (valid #f)
             group)))))

(define (revoke-group-certificate group cert-id who)
  (match group
    (($ <group> certificates profiles)
     (match (hashmap-ref certificates cert-id)
       ;; No such cert; no-op.
       (#f group)
       (($ <certificate> id parent signer controllers predicate revoked?)
        (cond
         ;; Already revoked; no-op.
         (revoked? group)
         ;; Revoker is the signer; revoke!
         ((equal? signer who)
          (let ((revoked-cert (make-certificate id parent signer controllers
                                                predicate #t)))
            (%make-group (hashmap-set certificates cert-id revoked-cert)
                         profiles)))
         ;; Revoker is not the signer; rejected.
         ;;
         ;; TODO: Record this incident for auditing purposes.
         (else group)))))))


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
  (make-react id timestamp reactor certificate reacted-at char reacted?)
  react?
  (id react-id)                   ; SHA-256 hash
  (timestamp react-timestamp)     ; HLC
  (reactor react-reactor)         ; ed25519 public key
  (certificate react-certificate) ; SHA-256 hash
  (reacted-at react-reacted-at)   ; epoch time (ms)
  (char react-char)               ; char
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

(define (make-message id timestamp author certificate when contents)
  (%make-message id timestamp author certificate when contents '() '() '()))

(define* (message-edit msg id timestamp editor certificate when new)
  (match msg
    (($ <message> id* timestamp* author certificate* created-at contents
                  reacts edits deletes)
     (let ((edit (make-edit id timestamp editor certificate when new)))
       (%make-message id* timestamp* author certificate* created-at contents
                      reacts (cons edit edits) deletes)))))

(define* (message-delete msg id timestamp deleter certificate when)
  (match msg
    (($ <message> id* author timestamp* certificate* created-at contents
                  reacts edits deletes)
     (let ((delete (make-delete id timestamp deleter certificate when)))
       (%make-message id* author timestamp* certificate* created-at contents
                      reacts edits (cons delete deletes))))))

(define (%message-react msg id timestamp reactor certificate when char value)
  (match msg
    (($ <message> id* author timestamp* certificate* created-at contents
                  reacts edits deletes)
     (let ((react (make-react id timestamp reactor certificate when char value)))
       (%make-message id* author timestamp* certificate* created-at contents
                      (cons react reacts) edits deletes)))))

(define (message-react msg id timestamp reactor certificate when char)
  (%message-react msg id timestamp reactor certificate when char #t))

(define (message-unreact msg id timestamp reactor certificate when char)
  (%message-react msg id timestamp reactor certificate when char #f))

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

;; The group CRDT accumulates certificate and user profile data.
;;
;; TODO: Petnames
(define-actor (^group become replica-id root-signer private-key)
  (define (query group)
    (match group
      (($ <group> certificates profiles)
       (list certificates
             (hashmap-fold
              (lambda (public-key profile memo)
                (match profile
                  (($ <profile> _ spn)
                   (hashmap-set memo public-key (lww-register-value spn)))))
              (make-hashmap) profiles)))))
  (define (effect id timestamp who exp group)
    (match exp
      (('set-spn name)
       (set-group-profile-self-proposed-name group timestamp who name))
      (('add-certificate parent controllers pred)
       (add-group-certificate group root-signer id parent who controllers pred))
      (('revoke-certificate cert-id)
       (revoke-group-certificate group cert-id who))
      (_ group)))
  (define crdt
    (spawn ^crdt replica-id private-key
           #:init (make-group)
           #:query query
           #:effect effect))
  (methods
   ((add-replica replica) (: crdt 'add-replica replica))
   ((ref) (: crdt 'ref))
   ((missing event-ids) (: crdt 'missing event-ids))
   ((push events) (: crdt 'push events))
   ((set-spn name) (: crdt 'commit `(set-spn ,name)))
   ((add-certificate parent controllers pred)
    (: crdt 'commit `(add-certificate ,parent ,controllers ,pred)))
   ((revoke-certificate cert-id)
    (: crdt 'commit `(revoke-certificate ,cert-id)))))

;; A chat log holds one chunk of a chat room's history.
(define-actor (^chat-log become replica-id private-key)
  (define (query messages)
    (map (match-lambda
           (($ <message> id timestamp author cert created-at contents
                         reacts edits deletes)
            (list id author cert created-at contents
                  ;; Reacts.
                  (map (match-lambda
                         (($ <react> _ _ reactor cert when char reacted?)
                          (list reactor cert when char reacted?)))
                       (sort reacts react<?))
                  ;; Edits, in descending total order.
                  (map (match-lambda
                         (($ <edit> _ _ editor cert when contents)
                          (list editor cert when contents)))
                       (sort edits edit>?))
                  ;; Deletes, in descending total order.
                  (map (match-lambda
                         (($ <delete> _ _ deleter cert when)
                          (list deleter cert when)))
                       (sort deletes delete>?)))))
         ;; Return messages in chronological order.
         (sort (hashmap-fold (lambda (id msg memo) (cons msg memo)) '() messages)
               message<?)))
  ;; TODO: Ignore events referring to messages that are not from
  ;; causal predecessors.
  (define (effect id timestamp who exp messages)
    (match exp
      (('post cert-id created contents)
       (hashmap-set messages id
                    (make-message id timestamp who cert-id created contents)))
      (('edit cert-id msg-id when contents)
       (let ((msg (hashmap-ref messages msg-id)))
         (hashmap-set messages msg-id
                      (message-edit msg id timestamp who cert-id
                                    when contents))))
      (('delete cert-id msg-id when)
       (let ((msg (hashmap-ref messages msg-id)))
         (hashmap-set messages msg-id
                      (message-delete msg id timestamp who cert-id when))))
      (('react cert-id msg-id when char)
       (hashmap-set messages msg-id
                    (message-react (hashmap-ref messages msg-id)
                                   id timestamp who cert-id when char)))
      (('unreact cert-id msg-id when char)
       (hashmap-set messages msg-id
                    (message-unreact (hashmap-ref messages msg-id)
                                     id timestamp who cert-id when char)))
      (_ messages)))
  (define crdt
    (spawn ^crdt replica-id private-key
           #:init (make-hashmap)
           #:query query
           #:effect effect))
  (methods
   ((add-replica replica) (: crdt 'add-replica replica))
   ((ref) (: crdt 'ref))
   ((missing event-ids) (: crdt 'missing event-ids))
   ((push events) (: crdt 'push events))
   ((post cert-id when contents)
    (: crdt 'commit `(post ,cert-id ,when ,contents)))
   ((edit cert-id msgid when contents)
    (: crdt 'commit `(edit ,cert-id ,msgid ,when ,contents)))
   ((delete cert-id msgid when)
    (: crdt 'commit `(delete ,cert-id ,msgid ,when)))
   ((react cert-id msgid when char)
    (: crdt 'commit `(react ,cert-id ,msgid ,when ,char)))
   ((unreact cert-id msgid when char)
    (: crdt 'commit `(unreact ,cert-id ,msgid ,when ,char)))))

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

(define-actor (^chat-room become id root-signer #:optional (period (* 30 60)))
  (define (^partition-replica become replica key)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((missing event-ids) (<- replica 'missing/messages event-ids key))
     ((push events) (<- replica 'push/messages events key))))
  (define (^group-replica become replica)
    (methods
     ((replica-id) (<- replica 'replica-id))
     ((missing event-ids) (<- replica 'missing/group event-ids))
     ((push events) (<- replica 'push/group events))))
  (define (^chat-room-replica become)
    (methods
     ((replica-id) replica-id)
     ((missing/messages heads key)
      (: (partition-ref key) 'missing heads))
     ((push/messages events key)
      (: (partition-ref key) 'push events))
     ((missing/group heads)
      (: group 'missing heads))
     ((push/group events)
      (: group 'push events))))
  (define spn (: id 'spn))
  (define private-key (: id 'private-key))
  (define public-key (: id 'public-key))
  ;; Generate a random replica ID.
  (define replica-id (base64-encode (strong-random-bytes 32) #:padding? #f))
  ;; Our replica interface.
  (define replica (spawn ^chat-room-replica))
  ;; The group stores user profile information.
  (define group (spawn ^group replica-id root-signer private-key))
  ;; Tell the group our self-proposed name.
  (: group 'set-spn spn)
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
   ;; Spawn a revokable proxy to our replica that we can share with
   ;; someone else.
   ((fresh-replica)
    (spawn-revokable-and-revoker replica))
   ((add-replica replica)
    (: replicas (cons replica (: replicas)))
    (: group 'add-replica (spawn ^group-replica replica))
    (hashmap-for-each
     (lambda (key log)
       (let ((replica* (spawn ^partition-replica replica key)))
         (: log 'add-replica replica*)))
     (: partitions)))
   ((ref time)
    (: (partition-for-time time) 'ref))
   ;; Mainly for testing.
   ((ref-all)
    (append-map (match-lambda ((_ . log) (: log 'ref)))
                (sort (hashmap-fold (lambda (k v memo) (cons (cons k v) memo))
                                    '() (: partitions))
                      (lambda (a b) (< (car a) (car b))))))
   ((group) (: group 'ref))
   ((set-spn name) (: group 'set-spn name))
   ((add-certificate #:key parent (controllers '()) (predicate #t))
    (: group 'add-certificate parent controllers predicate))
   ((revoke-certificate cert-id)
    (: group 'revoke-certificate cert-id))
   ((post cert-id contents #:optional (now (current-time/ms)))
    (: (partition-for-time now) 'post cert-id now contents))
   ((edit cert-id msg-id created contents #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'edit cert-id msg-id now contents))
   ((delete cert-id msg-id created #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'delete cert-id msg-id now))
   ((react cert-id msg-id created char #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'react cert-id msg-id now char))
   ((unreact cert-id msg-id created char #:optional (now (current-time/ms)))
    (: (partition-for-time created) 'unreact cert-id msg-id now char))))
