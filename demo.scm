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
             (ice-9 match))

(define* (render-chat title messages #:key debug?)
  (format #t "# Chat log for ~a\n" title)
  (newline)
  (for-each (match-lambda
              ((id author created modified deleted contents reacts)
               (let ((name (author-name author)))
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

(define (author-name pubkey)
  (cond
   ((equal? pubkey pubkey-alice) "Alice")
   ((equal? pubkey pubkey-bob) "Bob")
   ((equal? pubkey pubkey-carol) "Carol")
   (else "?????")))

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

;; Render result for each peer.
(with-vat vat
  (render-chat "Alice" (: chat-alice 'ref-all))
  (render-chat "Bob" (: chat-bob 'ref-all))
  (render-chat "Carol" (: chat-carol 'ref-all)))

;; Edit, delete, and react.
(with-vat vat
  (let lp ((messages (: chat-bob 'ref-all)))
    (match messages
      (() #t)
      (((id author created modified deleted contents reacts) . messages)
       (cond
        ((and (equal? (author-name author) "Alice")
              (equal? contents "Hello"))
         (<-np chat-bob 'react id created #\👋)
         (<-np chat-carol 'react id created #\👋))
        ((and (equal? (author-name author) "Bob")
              (equal? contents "asdf"))
         (<-np chat-bob 'delete id created))
        ((and (equal? (author-name author) "Alice")
              (equal? contents "This is a neat chat demo!"))
         (<-np chat-bob 'react id created #\💯)
         (<-np chat-carol 'react id created #\👎)
         (<-np chat-carol 'unreact id created #\👎))
        ((and (equal? (author-name author) "Carol")
              (equal? contents "Yeah, it's so grood."))
         (<-np chat-carol 'edit id created "Yeah, it's so good!")))
       (lp messages)))))

(display "\nSome time later...\n\n\n")
(sleep 1)

;; Render result for each peer.
(with-vat vat
  (render-chat "Alice" (: chat-alice 'ref-all))
  (render-chat "Bob" (: chat-bob 'ref-all))
  (render-chat "Carol" (: chat-carol 'ref-all)))
