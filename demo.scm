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

(use-modules (chat)
             (fibers)
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer tcp-tls)
             (goblins utils hashmap)
             (ice-9 match)
             (srfi srfi-1))

;; Alice, Bob, and Carol are going to have a little chat.
(define vat-alice (spawn-vat))
(define vat-bob (spawn-vat))
(define vat-carol (spawn-vat))

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
(define netlayer-alice (with-vat vat-alice (spawn ^tcp-tls-netlayer "localhost")))
(define netlayer-bob (with-vat vat-bob (spawn ^tcp-tls-netlayer "localhost")))
(define netlayer-carol (with-vat vat-carol (spawn ^tcp-tls-netlayer "localhost")))

(define mycapn-alice (with-vat vat-alice (spawn-mycapn netlayer-alice)))
(define mycapn-bob (with-vat vat-bob (spawn-mycapn netlayer-bob)))
(define mycapn-carol (with-vat vat-carol (spawn-mycapn netlayer-carol)))

(define sturdyref-alice<-bob
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-bob 'tcp-tls)))
(define sturdyref-alice<-carol
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-carol 'tcp-tls)))
(define sturdyref-bob<-alice
  (with-vat vat-bob (: mycapn-bob 'register chat-bob<-alice 'tcp-tls)))
(define sturdyref-carol<-alice
  (with-vat vat-carol (: mycapn-carol 'register chat-carol<-alice 'tcp-tls)))

;; Some helper procedures to print out the results of this demo.
;;
;; TODO: Move all the filtering based on certificate to the ^chat-room
;; actor.
(define (render-chat title group messages)
  (match group
    ((certs names)
     (define (allowed? cert-id op author who)
       (match (hashmap-ref certs cert-id)
         (#f #f)
         (cert
          (certificate-allows? cert op author who))))
     (format #t "# Chat log for ~a\n" title)
     (newline)
     (for-each
      (match-lambda
        ((id author cert-id created-at contents reacts edits deletes)
         (when (allowed? cert-id 'post author author)
           (let ((name (hashmap-ref names author "?????"))
                 (reacts
                  (sort
                   (hashmap-fold
                    (lambda (char whos reacts)
                      (if (null? whos)
                          reacts
                          (cons (cons char whos) reacts)))
                    '()
                    (fold (lambda (react reacts)
                            (match react
                              ((who cert-id when char reacted?)
                               (if (allowed? cert-id 'react author who)
                                   (let ((whos (hashmap-ref reacts char '())))
                                     (hashmap-set reacts char
                                                  (if reacted?
                                                      (lset-adjoin equal? whos who)
                                                      (delete who whos))))
                                   reacts))))
                          (make-hashvmap) reacts))
                   (lambda (a b)
                     (char<? (car a) (car b)))))
                 (edit (find (match-lambda
                               ((who cert when contents)
                                (allowed? cert-id 'edit author who)))
                             edits))
                 (delete (find (match-lambda
                                 ((who cert when)
                                  (allowed? cert-id 'delete author who)))
                               deletes)))
             (cond
              (delete
               (format #t "<~a>\t--- deleted ---\n" name))
              (else
               (let ((contents (match edit
                                 (#f contents)
                                 ((_ _ _ contents) contents))))
                 (format #t "<~a>\t~a" name contents)
                 (unless (null? reacts)
                   (display "\t[")
                   (for-each (match-lambda
                               ((char . whos)
                                (format #t " ~ax~a" char (length whos))))
                             reacts)
                   (display " ]"))
                 (newline))))))))
      messages)
     (newline))))

(define (render-chats)
  (with-vat vat-alice
    (render-chat "Alice" (: chat-alice 'group) (: chat-alice 'ref-all)))
  (with-vat vat-bob
    (render-chat "Bob" (: chat-bob 'group) (: chat-bob 'ref-all)))
  (with-vat vat-carol
    (render-chat "Carol" (: chat-carol 'group) (: chat-carol 'ref-all))))

(define (for-each-message proc chat)
  (let lp ((messages (: chat 'ref-all)))
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
(with-vat vat-alice (<-np chat-alice 'post cert-alice "This is a neat chat demo!"))
(sleep .01)
;; Carol makes a typo.
(with-vat vat-carol (<-np chat-carol 'post cert-carol "Yeah, it's so grood."))

;; Give the group time to converge.
(sleep 1)
(render-chats)

;; Edit, delete, and react to previously sent messages.
(with-vat vat-alice
  (match (: chat-alice 'group)
    ((certs names)
     (for-each-message
      (match-lambda
        ((id author cert created-at contents reacts edits deletes)
         (cond
          ((and (equal? (hashmap-ref names author) "Bob")
                (equal? contents "Hey, Alice!"))
           ;; Alice reacts to Bob's greeting.
           (<-np chat-alice 'react cert-alice id created-at #\🌊))
          ((and (equal? (hashmap-ref names author) "Carol")
                (equal? contents "Yeah, it's so grood."))
           ;; Alice cannot delete Carol's message.
           (<-np chat-alice 'delete cert-alice id created-at)))))
      chat-alice))))

(with-vat vat-bob
  (match (: chat-bob 'group)
    ((certs names)
     (for-each-message
      (match-lambda
        ((id author cert created-at contents reacts edits deletes)
         (cond
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "Hello"))
           ;; Bob reacts to Carol's greeting.
           (<-np chat-bob 'react cert-bob id created-at #\👋)
           ;; Bob cannot edit Carol's message.
           (<-np chat-bob 'edit cert-bob id created-at "owo"))
          ((and (equal? (hashmap-ref names author) "Bob")
                (equal? contents "asdf"))
           ;; Bob deletes his cat's post.
           (<-np chat-bob 'delete cert-bob id created-at))
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "This is a neat chat demo!"))
           ;; Bob agrees that this demo is neat.
           (<-np chat-bob 'react cert-bob id created-at #\💯)))))
      chat-bob))))

(with-vat vat-carol
  (match (: chat-carol 'group)
    ((certs names)
     (for-each-message
      (match-lambda
        ((id author cert created-at contents reacts edits deletes)
         (cond
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "Hello"))
           ;; Carol reacts to Alice's greeting.
           (<-np chat-carol 'react cert-carol id created-at #\👋))
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "This is a neat chat demo!"))
           ;; Carol accidentally reacts with a thumbs down emoji and
           ;; quickly unreacts.
           (<-np chat-carol 'react cert-carol id created-at #\👎)
           (<-np chat-carol 'unreact cert-carol id created-at #\👎))
          ((and (equal? (hashmap-ref names author) "Carol")
                (equal? contents "Yeah, it's so grood."))
           ;; Carol edits her previous message to fix a typo.
           (<-np chat-carol 'edit cert-carol id created-at "Yeah, it's so good!")))))
      chat-carol))))

;; Give the group time to converge again.
(sleep 1)
(display "\nSome time later...\n\n\n")

(render-chats)
