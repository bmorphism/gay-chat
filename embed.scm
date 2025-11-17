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
             (brassica dom document)
             (brassica dom element)
             (brassica dom event)
             (brassica dom)
             (fibers)
             (fibers channels)
             (fibers scheduler)
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins actor-lib cell)
             (goblins actor-lib joiners)
             (goblins actor-lib methods)
             (goblins actor-lib on)
             (goblins actor-lib timers)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer fake)
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

;; Setup OCapN.
(define fake-network (with-vat fake-network-vat (spawn ^fake-network)))
;; A mycapn wrapper that allows us to simulate disconnecting and
;; reconnecting from the fake network whilst preserving registered
;; sturdyrefs.
(define-actor (^meta-mycapn become id)
  (define (spawn-fake-netlayer)
    (let* ((new-conn-ch (make-channel))
           (name (: id 'spn))
           (netlayer (spawn ^fake-netlayer name fake-network new-conn-ch)))
      (<-np fake-network 'register name new-conn-ch)
      netlayer))
  (define netlayer (spawn ^cell (spawn-fake-netlayer)))
  (define mycapn (spawn ^cell (spawn-mycapn (: netlayer))))
  (define sturdyrefs (spawn ^cell '()))
  (define connected
    (methods
     ((register obj)
      (let-on ((sturdyref (<- (: mycapn) 'register obj 'fake)))
        (: sturdyrefs (cons (cons obj sturdyref) (: sturdyrefs)))
        sturdyref))
     ((enliven sturdyref)
      (: (: mycapn) 'enliven sturdyref))
     ((disconnect)
      (: (: netlayer) 'halt)
      (: netlayer #f)
      (: mycapn #f)
      (become disconnected))))
  (define disconnected
    (methods
     ((register obj)
      (error "disconnected"))
     ((enliven sturdyref)
      (error "disconnected"))
     ((connect)
      (: netlayer (spawn-fake-netlayer))
      (: mycapn (spawn-mycapn (: netlayer)))
      (let ((registry (: (: mycapn) 'get-registry)))
        (map (match-lambda
               ((obj . sturdyref)
                (<-np registry 'register obj
                      (ocapn-sturdyref-swiss-num sturdyref))))
             (: sturdyrefs)))
      (become connected))))
  connected)
(define mycapn-alice (with-vat vat-alice (spawn ^meta-mycapn id-alice)))
(define mycapn-bob (with-vat vat-bob (spawn ^meta-mycapn id-bob)))
(define mycapn-carol (with-vat vat-carol (spawn ^meta-mycapn id-carol)))

(define sturdyref-alice<-bob
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-bob)))
(define sturdyref-alice<-carol
  (with-vat vat-alice (: mycapn-alice 'register chat-alice<-carol)))
(define sturdyref-bob<-alice
  (with-vat vat-bob (: mycapn-bob 'register chat-bob<-alice)))
(define sturdyref-carol<-alice
  (with-vat vat-carol (: mycapn-carol 'register chat-carol<-alice)))

(define (chat-window vat mycapn peers id cert-id room)
  (define peer-names (map car peers))
  (define peer-sturdyrefs (map cdr peers))
  (define connected? #t)
  (define name (with-vat vat (: id 'spn)))
  (define pubkey (with-vat vat (: id 'public-key)))
  (define room-connection-id (string-append "message-connection-" name))
  (define room-log-id (string-append "room-log-" name))
  (define room-compose-id (string-append "room-compose-" name))
  (define room-editing-id (string-append "message-editing-" name))
  (define editing #f)
  (define (^refresher become)
    (lambda args
      (let ((messages (: room 'all-messages))
            (names (: room 'profiles)))
        (schedule-task
         (lambda ()
           (refresh-messages messages names)))
        #t)))
  (define (toggle-connection event)
    (prevent-default! event)
    (cond
     (connected?
      (set! connected? #f)
      (set-inner-shtml! (get-element-by-id room-connection-id) "disconnected")
      (with-vat vat (: mycapn 'disconnect)))
     (else
      (set! connected? #t)
      (set-inner-shtml! (get-element-by-id room-connection-id) "connected")
      (with-vat vat (: mycapn 'connect)))))
  (define (send-message event)
    (let* ((textarea (get-element-by-id room-compose-id))
           (str (element-value textarea)))
      (prevent-default! event)
      (unless (string-null? str)
        (match editing
          ;; New message.
          (#f
           (with-vat vat
             (: room 'post cert-id str)))
          ;; Edit message.
          ((msg-id _ _ created-at . _)
           (with-vat vat
             (: room 'edit cert-id msg-id created-at str))
           (set! editing #f)
           (set-inner-shtml! (get-element-by-id room-editing-id) "")))
        (set-element-value! textarea ""))))
  (define (compose-keydown event)
    (match (keyboard-event-key event)
      ("Enter"
       (unless (keyboard-event-shift? event)
         (send-message event)))
      ("Escape"
       (when editing
         (prevent-default! event)
         (set! editing #f)
         (set-element-value! (get-element-by-id room-compose-id) "")
         (set-inner-shtml! (get-element-by-id room-editing-id) "")))
      (_ (values))))
  (define (refresh-messages messages names)
    (define (render-message message)
      (match message
        ((msg-id author cert-id* created-at modified-at deleted-at contents reacts)
         (define (reaction emoji)
           (lambda (event)
             (with-vat vat
               (: room 'react cert-id msg-id created-at emoji))))
         (define (unreaction emoji)
           (lambda (event)
             (with-vat vat
               (: room 'unreact cert-id msg-id created-at emoji))))
         (define (react-button emoji)
           `(a (@ (href "#")
                  (click ,(reaction emoji)))
               ,emoji))
         (define (edit-message event)
           (set! editing message)
           (set-inner-shtml!
            (get-element-by-id room-editing-id)
            `(p (strong "Editing: ") ,contents))
           (let ((textarea (get-element-by-id room-compose-id)))
             (focus! textarea)
             (set-element-value! textarea contents)))
         (define (remove-message event)
           (with-vat vat
             (: room 'delete cert-id msg-id created-at)))
         (define show-controls? #f)
         (define (toggle-controls event)
           (cond
            (show-controls?
             ;; Defer hiding the controls in case the user tapped on
             ;; an edit control.  We want the click handler to fire
             ;; for that.
             (set-element-class! (event-current-target event) "message")
             (set! show-controls? #f))
            (else
             (set-element-class! (event-current-target event)
                                 "message message-tapped")
             (set! show-controls? #t))))
         `(div (@ (class "message-block"))
               (p (cite ,(assoc-ref names author)))
               ,(if deleted-at
                    '(p (@ (class "message-removed")) "message removed")
                    `(div (@ (class "message")
                             (click ,toggle-controls))
                          (p ,contents)
                          ,@(if modified-at
                                '((small "(edited)"))
                                '())
                          (aside (@ (class "message-toolbar"))
                                 ,(react-button "❤️")
                                 ,(react-button "👍")
                                 ,(react-button "🤣")
                                 ,(react-button "👋")
                                 ,(react-button "👀")
                                 (a (@ (href "#") (click ,edit-message)) "edit")
                                 (a (@ (href "#") (click ,remove-message)) "remove"))
                          ,@(match reacts
                              (() '())
                              (reacts
                               `((ul (@ (class "message-reactions"))
                                     ,@(map (match-lambda
                                              ((emoji . whos)
                                               `(li ,(if (member pubkey whos)
                                                         `(@ (class "our-reaction")
                                                             (click ,(unreaction emoji)))
                                                         `(@ (click ,(reaction emoji))))
                                                    ,emoji " " ,(length whos))))
                                            reacts)))))))))))
    (define log-elem (get-element-by-id room-log-id))
    (define scrolled-to-bottom?
      (= (element-scroll-top log-elem) (element-scroll-top-max log-elem)))
    (set-inner-shtml! log-elem (map render-message messages))
    (when scrolled-to-bottom?
      (set-element-scroll-top! log-elem (element-scroll-top-max log-elem))))
  (with-vat vat
    (: room 'subscribe (spawn ^refresher))
    ;; Attempt to connect to peers, retrying if we can't connect or if
    ;; we lose connection once established.
    (let ((retry-delay 1))
      (let-on ((sturdyrefs (all-of* peer-sturdyrefs)))
        (for-each
         (lambda (sturdyref)
           (let try-again ()
             (on (race (<- mycapn 'enliven sturdyref) (timeout retry-delay))
                 (match-lambda
                   (#t (try-again))
                   (replica
                    (: room 'add-replica replica)
                    (on-sever replica
                              (lambda (type reason)
                                (try-again)))))
                 #:catch
                 (lambda (err)
                   (on (timeout retry-delay) (lambda (_) (try-again)))))))
         sturdyrefs))))
  `(article (@ (class "chat-window"))
            (header
             (p (strong ,name) ": "
                (span (@ (id ,room-connection-id)) "connected")
                " — peers: " ,(string-join peer-names ", "))
             (form
              (button (@ (click ,toggle-connection))
                      "Toggle connection")))
            (section (@ (id ,room-log-id)
                        (class "room-log")))
            (section (@ (id ,room-editing-id)))
            (section (@ (class "room-compose"))
                     (textarea (@ (id ,room-compose-id)
                                  (placeholder "Type a message...")
                                  (keydown ,compose-keydown)))
                     (button (@ (click ,send-message)) "➤"))))

(set-inner-shtml!
 (document-body)
 `(main (@ (class "chat-main"))
        ,(chat-window vat-alice mycapn-alice
                      `(("Bob" . ,sturdyref-bob<-alice)
                        ("Carol" . ,sturdyref-carol<-alice))
                      id-alice cert-alice chat-alice)
        ,(chat-window vat-bob mycapn-bob
                      `(("Alice" . ,sturdyref-alice<-bob))
                      id-bob cert-bob chat-bob)
        ,(chat-window vat-carol mycapn-carol
                      `(("Alice" . ,sturdyref-alice<-carol))
                      id-carol cert-carol chat-carol)))
