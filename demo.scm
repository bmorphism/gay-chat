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
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins utils hashmap)
             (ice-9 match))

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

(define vat (spawn-vat))

(define id-alice (with-vat vat (spawn ^identity "Alice")))
(define id-bob (with-vat vat (spawn ^identity "Bob")))
(define id-carol (with-vat vat (spawn ^identity "Carol")))

(define pubkey-alice (with-vat vat (: id-alice 'public-key)))
(define pubkey-bob (with-vat vat (: id-bob 'public-key)))
(define pubkey-carol (with-vat vat (: id-carol 'public-key)))

(define chat-alice (with-vat vat (spawn ^chat-room id-alice)))
(define chat-bob (with-vat vat (spawn ^chat-room id-bob)))
(define chat-carol (with-vat vat (spawn ^chat-room id-carol)))

(define (render-chats)
  (with-vat vat
    (render-chat "Alice" (: chat-alice 'names) (: chat-alice 'ref-all))
    (render-chat "Bob" (: chat-alice 'names) (: chat-bob 'ref-all))
    (render-chat "Carol" (: chat-alice 'names) (: chat-carol 'ref-all))))

;; Alice is connected Bob and Carol.  Bob and Carol are not connected
;; to each other.
(with-vat vat
  (: chat-alice 'add-replica chat-bob)
  (: chat-alice 'add-replica chat-carol)

  (: chat-bob 'add-replica chat-alice)
  ;; (: chat-bob 'add-replica "baz" chat-carol)

  (: chat-carol 'add-replica chat-alice)
  ;; (: chat-carol 'add-replica "bar" chat-bob)
  )

;; Initial messages.
(with-vat vat (<-np chat-alice 'post "Hello"))
(with-vat vat
  (<-np chat-bob 'post "Hey, Alice!")
  (<-np chat-carol 'post "Hey everyone!")
  (<-np chat-bob 'post "asdf"))
(with-vat vat (<-np chat-alice 'post "This is a neat chat demo!"))
(with-vat vat (<-np chat-carol 'post "Yeah, it's so grood."))

(sleep 1)
(render-chats)

;; Edit, delete, and react.
(with-vat vat
  (let ((names (: chat-bob 'names)))
    (let lp ((messages (: chat-bob 'ref-all)))
      (match messages
        (() #t)
        (((id author created modified deleted contents reacts) . messages)
         (cond
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "Hello"))
           (<-np chat-bob 'react id created #\👋)
           (<-np chat-carol 'react id created #\👋)
           ;; Bob cannot edit Carol's message
           (<-np chat-bob 'edit id created "owo"))
          ((and (equal? (hashmap-ref names author) "Bob")
                (equal? contents "asdf"))
           (<-np chat-bob 'delete id created))
          ((and (equal? (hashmap-ref names author) "Alice")
                (equal? contents "This is a neat chat demo!"))
           (<-np chat-bob 'react id created #\💯)
           (<-np chat-carol 'react id created #\👎)
           (<-np chat-carol 'unreact id created #\👎))
          ((and (equal? (hashmap-ref names author) "Carol")
                (equal? contents "Yeah, it's so grood."))
           (<-np chat-carol 'edit id created "Yeah, it's so good!")
           ;; Alice cannot delete Carol's message.
           (<-np chat-alice 'delete id created)))
         (lp messages))))))

(display "\nSome time later...\n\n\n")
(sleep 1)
(render-chats)
