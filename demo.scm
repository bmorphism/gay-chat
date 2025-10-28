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
             (ice-9 match))

(define vat-alice (spawn-vat))
(define vat-bob (spawn-vat))
(define vat-carol (spawn-vat))

(define id-alice (with-vat vat-alice (spawn ^identity "Alice")))
(define id-bob (with-vat vat-bob (spawn ^identity "Bob")))
(define id-carol (with-vat vat-carol (spawn ^identity "Carol")))

(define pubkey-alice (with-vat vat-alice (: id-alice 'public-key)))
(define pubkey-bob (with-vat vat-bob (: id-bob 'public-key)))
(define pubkey-carol (with-vat vat-carol (: id-carol 'public-key)))

(define chat-alice (with-vat vat-alice (spawn ^chat-room id-alice)))
(define chat-bob (with-vat vat-bob (spawn ^chat-room id-bob)))
(define chat-carol (with-vat vat-carol (spawn ^chat-room id-carol)))

(define netlayer-alice (with-vat vat-alice (spawn ^tcp-tls-netlayer "localhost")))
(define netlayer-bob (with-vat vat-bob (spawn ^tcp-tls-netlayer "localhost")))
(define netlayer-carol (with-vat vat-carol (spawn ^tcp-tls-netlayer "localhost")))

(define mycapn-alice (with-vat vat-alice (spawn-mycapn netlayer-alice)))
(define mycapn-bob (with-vat vat-bob (spawn-mycapn netlayer-bob)))
(define mycapn-carol (with-vat vat-carol (spawn-mycapn netlayer-carol)))

(define sturdyref-alice (with-vat vat-alice (: mycapn-alice 'register chat-alice 'tcp-tls)))
(define sturdyref-bob (with-vat vat-bob (: mycapn-bob 'register chat-bob 'tcp-tls)))
(define sturdyref-carol (with-vat vat-carol (: mycapn-carol 'register chat-carol 'tcp-tls)))

(define (render-chat title names messages)
  (format #t "# Chat log for ~a\n" title)
  (newline)
  (for-each (match-lambda
              ((id author created modified deleted contents reacts)
               (let ((name (hashmap-ref names author "?????")))
                 (cond
                  (deleted
                   (format #t "<~a>\t--- deleted ---\n" name))
                  (else
                   (format #t "<~a>\t~a" name contents)
                   (unless (null? reacts)
                     (display "\t[")
                     (for-each (match-lambda
                                 ((char . who)
                                  (format #t " ~ax~a" char (length who))))
                               reacts)
                     (display " ]"))
                   (newline))))))
            messages)
  (newline))

(define (render-chats)
  (with-vat vat-alice
    (render-chat "Alice" (: chat-alice 'names) (: chat-alice 'ref-all)))
  (with-vat vat-bob
    (render-chat "Bob" (: chat-bob 'names) (: chat-bob 'ref-all)))
  (with-vat vat-carol
    (render-chat "Carol" (: chat-carol 'names) (: chat-carol 'ref-all))))

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
  (: chat-alice 'add-replica (: mycapn-alice 'enliven sturdyref-bob))
  (: chat-alice 'add-replica (: mycapn-alice 'enliven sturdyref-carol)))
(sleep .5) ; sleep a bit to avoid crossed hellos
(with-vat vat-bob
  (: chat-bob 'add-replica (: mycapn-bob 'enliven sturdyref-alice)))
(with-vat vat-carol
 (: chat-carol 'add-replica (: mycapn-carol 'enliven sturdyref-alice)))

;; Post some messages to populate the chat log.
(with-vat vat-alice (<-np chat-alice 'post "Hello"))
(with-vat vat-bob (<-np chat-bob 'post "Hey, Alice!"))
(with-vat vat-carol (<-np chat-carol 'post "Hey everyone!"))
;; Bob's cat walks on his keyboard and posts nonsense.
(with-vat vat-bob (<-np chat-bob 'post "asdf"))
(with-vat vat-alice (<-np chat-alice 'post "This is a neat chat demo!"))
;; Carol makes a typo.
(with-vat vat-carol (<-np chat-carol 'post "Yeah, it's so grood."))

;; Give the group time to converge.
(sleep 1)
(render-chats)

;; Edit, delete, and react to previously sent messages.
(with-vat vat-alice
  (let ((names (: chat-alice 'names)))
    (for-each-message
     (match-lambda
       ((id author created modified deleted contents reacts)
        (cond
         ((and (equal? (hashmap-ref names author) "Bob")
               (equal? contents "Hey, Alice!"))
          ;; Alice reacts to Bob's greeting.
          (<-np chat-alice 'react id created #\🌊))
         ((and (equal? (hashmap-ref names author) "Carol")
               (equal? contents "Yeah, it's so grood."))
          ;; Alice cannot delete Carol's message.
          (<-np chat-alice 'delete id created)))))
     chat-alice)))

(with-vat vat-bob
  (let ((names (: chat-bob 'names)))
    (for-each-message
     (match-lambda
       ((id author created modified deleted contents reacts)
        (cond
         ((and (equal? (hashmap-ref names author) "Alice")
               (equal? contents "Hello"))
          ;; Bob reacts to Carol's greeting.
          (<-np chat-bob 'react id created #\👋)
          ;; Bob cannot edit Carol's message.
          (<-np chat-bob 'edit id created "owo"))
         ((and (equal? (hashmap-ref names author) "Bob")
               (equal? contents "asdf"))
          ;; Bob deletes his cat's post.
          (<-np chat-bob 'delete id created))
         ((and (equal? (hashmap-ref names author) "Alice")
               (equal? contents "This is a neat chat demo!"))
          ;; Bob agrees that this demo is neat.
          (<-np chat-bob 'react id created #\💯)))))
     chat-bob)))

(with-vat vat-carol
  (let ((names (: chat-carol 'names)))
    (for-each-message
     (match-lambda
       ((id author created modified deleted contents reacts)
        (cond
         ((and (equal? (hashmap-ref names author) "Alice")
               (equal? contents "Hello"))
          ;; Carol reacts to Alice's greeting.
          (<-np chat-carol 'react id created #\👋))
         ((and (equal? (hashmap-ref names author) "Alice")
               (equal? contents "This is a neat chat demo!"))
          ;; Carol accidentally reacts with a thumbs down emoji and
          ;; quickly unreacts.
          (<-np chat-carol 'react id created #\👎)
          (<-np chat-carol 'unreact id created #\👎))
         ((and (equal? (hashmap-ref names author) "Carol")
               (equal? contents "Yeah, it's so grood."))
          ;; Carol edits her previous message to fix a typo.
          (<-np chat-carol 'edit id created "Yeah, it's so good!")))))
     chat-carol)))

;; Give the group time to converge again.
(sleep 1)
(display "\nSome time later...\n\n\n")

(render-chats)
