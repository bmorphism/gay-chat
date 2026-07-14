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

(use-modules (brassica chat)
             (fibers)
             (fibers channels)
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer fake)
             (goblins utils hashmap)
             (ice-9 match)
             (srfi srfi-1))

;; Alice, Bob, and Carol are going to have a little chat.
(define vat-alice (spawn-vat))
(define vat-bob (spawn-vat))
(define vat-carol (spawn-vat))
;; One more vat for our fake network.
(define fake-network-vat (spawn-vat))

;; Alice, Bob, and Carol create cryptographic identities for
;; themselves.
(define id-alice (with-vat vat-alice (spawn ^identity "Alice")))
(define id-bob (with-vat vat-bob (spawn ^identity "Bob")))
(define id-carol (with-vat vat-carol (spawn ^identity "Carol")))

(define pubkey-alice (with-vat vat-alice (: id-alice 'public-key)))
(define pubkey-bob (with-vat vat-bob (: id-bob 'public-key)))
(define pubkey-carol (with-vat vat-carol (: id-carol 'public-key)))

;; Alice initiates the chat room and thus will be the root signer for
;; all certificates.
(define chat-alice (with-vat vat-alice (spawn ^chat-room id-alice pubkey-alice)))
(define chat-bob (with-vat vat-bob (spawn ^chat-room id-bob pubkey-alice)))
(define chat-carol (with-vat vat-carol (spawn ^chat-room id-carol pubkey-alice)))

;; Alice, using her root certificate powers, creates a certificate for
;; herself that grants full privileges.
(define cert-alice
  (with-vat vat-alice
    (: chat-alice 'add-certificate #:controllers (list pubkey-alice))))
;; Alice then creates a certificate for both Bob and Carol that lets
;; them post new messages, react to any message, and edit/delete their
;; own messages.
(define cert-bob
  (with-vat vat-alice
    (: chat-alice 'add-certificate
       #:controllers (list pubkey-bob)
       #:predicate '(when-op (edit delete) (allow-self)))))
(define cert-carol
  (with-vat vat-alice
    (: chat-alice 'add-certificate
       #:controllers (list pubkey-carol)
       #:predicate '(when-op (edit delete) (allow-self)))))

;; Alice generates revokable proxies to her chat room presence to give
;; to Bob and Carol.
(define-values (chat-alice<-bob revoke-alice<-bob)
  (apply values (with-vat vat-alice (: chat-alice 'fresh-replica))))
(define-values (chat-alice<-carol revoke-alice<-carol)
  (apply values (with-vat vat-alice (: chat-alice 'fresh-replica))))
;; Bob and Carol reciprocate.
(define-values (chat-bob<-alice revoke-bob<-alice)
  (apply values (with-vat vat-bob (: chat-bob 'fresh-replica))))
(define-values (chat-carol<-alice revoke-carol<-alice)
  (apply values (with-vat vat-carol (: chat-carol 'fresh-replica))))

;; And now we hook everything up to OCapN!
(define fake-network (with-vat fake-network-vat (spawn ^fake-network)))
(define (spawn-fake-netlayer name)
  (let* ((new-conn-ch (make-channel))
         (netlayer (spawn ^fake-netlayer name fake-network new-conn-ch)))
    (<-np fake-network 'register name new-conn-ch)
    netlayer))
(define netlayer-alice (with-vat vat-alice (spawn-fake-netlayer "alice")))
(define netlayer-bob (with-vat vat-bob (spawn-fake-netlayer "bob")))
(define netlayer-carol (with-vat vat-carol (spawn-fake-netlayer "carol")))

(define mycapn-alice (with-vat vat-alice (spawn-mycapn netlayer-alice)))
(define mycapn-bob (with-vat vat-bob (spawn-mycapn netlayer-bob)))
(define mycapn-carol (with-vat vat-carol (spawn-mycapn netlayer-carol)))

(define sturdyref-alice<-bob
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-bob 'fake)))
(define sturdyref-alice<-carol
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-carol 'fake)))
(define sturdyref-bob<-alice
  (with-vat vat-bob (: mycapn-bob 'register chat-bob<-alice 'fake)))
(define sturdyref-carol<-alice
  (with-vat vat-carol (: mycapn-carol 'register chat-carol<-alice 'fake)))

;; Some helper procedures to print out the results of this world.
(define (render-chat title names messages)
  (format #t "# Chat log for ~a\n" title)
  (newline)
  (for-each
   (match-lambda
     ((id author cert-id created-at modified-at deleted-at contents reacts)
      (let ((name (or (assoc-ref names author) "?????"))
            (reacts (sort reacts (lambda (a b) (char<? (car a) (car b))))))
        (cond
         (deleted-at
          (format #t "<~a>\t--- deleted ---\n" name))
         (else
          (format #t "<~a>\t~a" name contents)
          (unless (null? reacts)
            (display "\t[")
            (for-each (match-lambda
                        ((char . whos)
                         (format #t " ~ax~a" char (length whos))))
                      reacts)
            (display " ]"))
          (newline))))))
   messages)
  (newline))

(define (render-chats)
  (with-vat vat-alice
    (render-chat "Alice"
                 (: chat-alice 'profiles)
                 (: chat-alice 'all-messages)))
  (with-vat vat-bob
    (render-chat "Bob"
                 (: chat-bob 'profiles)
                 (: chat-bob 'all-messages)))
  (with-vat vat-carol
    (render-chat "Carol"
                 (: chat-carol 'profiles)
                 (: chat-carol 'all-messages))))

(define (for-each-message proc chat)
  (let lp ((messages (: chat 'all-messages)))
    (match messages
      (() #t)
      ((msg . messages)
       (proc msg)
       (lp messages)))))

;; Alice is connected to Bob and Carol, but Bob and Carol do not have
;; a direct connection to each other.  Nonetheless, all messages will
;; propagate to all three eventually.
(with-vat vat-alice
  (: chat-alice 'add-replica (: mycapn-alice 'enliven sturdyref-bob<-alice))
  (: chat-alice 'add-replica (: mycapn-alice 'enliven sturdyref-carol<-alice)))
;; Sleep a bit to avoid crossed hello exceptions in the output.
(sleep .5)
(with-vat vat-bob
  (: chat-bob 'add-replica (: mycapn-bob 'enliven sturdyref-alice<-bob)))
(with-vat vat-carol
 (: chat-carol 'add-replica (: mycapn-carol 'enliven sturdyref-alice<-carol)))

;; Post some messages to populate the chat log.
(with-vat vat-alice (<-np chat-alice 'post cert-alice "Hello"))
(sleep .01)
(with-vat vat-bob (<-np chat-bob 'post cert-bob "Hey, Alice!"))
(sleep .01)
(with-vat vat-carol (<-np chat-carol 'post cert-carol "Hey everyone!"))
(sleep .01)
;; Bob's cat walks on his keyboard and posts nonsense.
(with-vat vat-bob (<-np chat-bob 'post cert-bob "asdf"))
(sleep .01)
(with-vat vat-alice (<-np chat-alice 'post cert-alice "This is a neat chat world!"))
(sleep .01)
;; Carol makes a typo.
(with-vat vat-carol (<-np chat-carol 'post cert-carol "Yeah, it's so grood."))

;; Give the group time to converge.
(sleep 1)
(render-chats)

;; Edit, delete, and react to previously sent messages.
(with-vat vat-alice
  (let ((names (: chat-alice 'profiles)))
    (for-each-message
     (match-lambda
       ((id author cert-id created-at modified-at deleted-at contents reacts)
        (cond
         ((and (equal? (assoc-ref names author) "Bob")
               (equal? contents "Hey, Alice!"))
          ;; Alice reacts to Bob's greeting.
          (<-np chat-alice 'react cert-alice id created-at #\🌊)))))
     chat-alice)))

(with-vat vat-bob
  (let ((names (: chat-bob 'profiles)))
    (for-each-message
     (match-lambda
       ((id author cert-id created-at modified-at deleted-at contents reacts)
        (cond
         ((and (equal? (assoc-ref names author) "Alice")
               (equal? contents "Hello"))
          ;; Bob reacts to Carol's greeting.
          (<-np chat-bob 'react cert-bob id created-at "👋")
          ;; Bob cannot edit Carol's message.
          (<-np chat-bob 'edit cert-bob id created-at "owo"))
         ((and (equal? (assoc-ref names author) "Bob")
               (equal? contents "asdf"))
          ;; Bob deletes his cat's post.
          (<-np chat-bob 'delete cert-bob id created-at))
         ((and (equal? (assoc-ref names author) "Alice")
               (equal? contents "This is a neat chat world!"))
          ;; Bob agrees that this world is neat.
          (<-np chat-bob 'react cert-bob id created-at "💯"))
         ((and (equal? (assoc-ref names author) "Carol")
               (equal? contents "Yeah, it's so grood."))
          ;; Bob cannot delete Carol's message.
          (<-np chat-bob 'delete cert-bob id created-at)))))
     chat-bob)))

(with-vat vat-carol
  (let ((names (: chat-carol 'profiles)))
    (for-each-message
     (match-lambda
       ((id author cert-id created-at modified-at deleted-at contents reacts)
        (cond
         ((and (equal? (assoc-ref names author) "Alice")
               (equal? contents "Hello"))
          ;; Carol reacts to Alice's greeting.
          (<-np chat-carol 'react cert-carol id created-at "👋"))
         ((and (equal? (assoc-ref names author) "Alice")
               (equal? contents "This is a neat chat world!"))
          ;; Carol accidentally reacts with a thumbs down emoji and
          ;; quickly unreacts.
          (<-np chat-carol 'react cert-carol id created-at "👎")
          (<-np chat-carol 'unreact cert-carol id created-at "👎"))
         ((and (equal? (assoc-ref names author) "Carol")
               (equal? contents "Yeah, it's so grood."))
          ;; Carol edits her previous message to fix a typo.
          (<-np chat-carol 'edit cert-carol id created-at "Yeah, it's so good!")))))
     chat-carol)))

;; Give the group time to converge again.
(sleep 1)
(display "\nSome time later...\n\n\n")

(render-chats)
