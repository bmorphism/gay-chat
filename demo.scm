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
              ((id created modified deleted from contents reacts)
               (cond
                (deleted
                 (format #t "<~a>\t--- deleted ---\n" from))
                (else
                 (format #t "<~a>\t~a" from contents)
                 (unless (null? reacts)
                   (display "\t[")
                   (for-each (match-lambda
                               ((char . who)
                                (format #t " ~ax~a" char (length who))))
                             reacts)
                   (display " ]"))
                 (newline)))))
            messages)
  (newline))

(define vat (spawn-vat))

(define chat-a (with-vat vat (spawn ^chat-room "Alice")))
(define chat-b (with-vat vat (spawn ^chat-room "Bob")))
(define chat-c (with-vat vat (spawn ^chat-room "Carol")))

;; Alice is connected Bob and Carol.  Bob and Carol are not connected
;; to each other.
(with-vat vat
  (: chat-a 'add-replica "Bob" chat-b)
  (: chat-a 'add-replica "Carol" chat-c)

  (: chat-b 'add-replica "Alice" chat-a)
  ;; (: chat-b 'add-replica "baz" chat-c)

  (: chat-c 'add-replica "Alice" chat-a)
  ;; (: chat-c 'add-replica "bar" chat-b)
  )

;; Initial messages.
(with-vat vat (<-np chat-a 'post "Alice" "Hello"))
(with-vat vat
  (<-np chat-b 'post "Bob" "Hey, Alice!")
  (<-np chat-c 'post "Carol" "Hey everyone!")
  (<-np chat-b 'post "Bob" "asdf"))
(with-vat vat (<-np chat-a 'post "Alice" "This is a neat chat demo!"))
(with-vat vat (<-np chat-c 'post "Carol" "Yeah, it's so grood."))

(sleep 1)

;; Render result for each peer.
(with-vat vat
  (render-chat "Alice" (: chat-a 'ref-all))
  (render-chat "Bob" (: chat-b 'ref-all))
  (render-chat "Carol" (: chat-c 'ref-all)))

;; Edit, delete, and react.
(with-vat vat
  (let lp ((messages (: chat-b 'ref-all)))
    (match messages
      (() #t)
      (((id created modified deleted from contents reacts) . messages)
       (cond
        ((and (equal? from "Alice") (equal? contents "Hello"))
         (<-np chat-b 'react id created #\👋)
         (<-np chat-c 'react id created #\👋))
        ((and (equal? from "Bob") (equal? contents "asdf"))
         (<-np chat-b 'delete id created))
        ((and (equal? from "Alice") (equal? contents "This is a neat chat demo!"))
         (<-np chat-b 'react id created #\💯)
         (<-np chat-c 'react id created #\👎)
         (<-np chat-c 'unreact id created #\👎))
        ((and (equal? from "Carol") (equal? contents "Yeah, it's so grood."))
         (<-np chat-c 'edit id created "Yeah, it's so good!")))
       (lp messages)))))

(display "\nSome time later...\n\n\n")
(sleep 1)

;; Render result for each peer.
(with-vat vat
  (render-chat "Alice" (: chat-a 'ref-all))
  (render-chat "Bob" (: chat-b 'ref-all))
  (render-chat "Carol" (: chat-c 'ref-all)))
