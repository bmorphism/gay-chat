(use-modules (brassica dom)
             (brassica dom document)
             (brassica dom element)
             (brassica dom event)
             (ice-9 match)
             (srfi srfi-9)
             (srfi srfi-11))

(define-record-type <connection>
  (make-connection name status actor)
  connection?
  (name connection-name)
  ;; invited | active | revoked
  (status connection-status set-connection-status!)
  (actor connection-actor))

(define-record-type <room>
  (make-room name actor certificate-id connections)
  room?
  (name room-name)
  (actor room-actor)
  (certificate-id room-certificate-id)
  (connections room-connections set-room-connections!))

(define (add-room-connection! room connection)
  (set-room-connections! room (cons connection (room-connections room))))

(define (value-for-id id)
  (element-value (get-element-by-id id)))

(define *backend* #f)

(define (setup make-backend)
  (define (start event)
    (prevent-default! event)
    (let* ((spn (value-for-id "self-proposed-name"))
           (relay (value-for-id "relay-uri"))
           (id (*backend* 'make-identity spn)))
      (*backend* 'connect-to-relay relay)
      (chat id)))
  (set! *backend* (make-backend))
  (set-inner-shtml!
   (document-body)
   `(main
     (dialog (@ (open ""))
             (article
              (header (h1 "Brassica Chat"))
              (p "Brassica chat is a local-first chat application built with
Goblins!")
              (p "To get started, enter your desired name and the address of your OCapN
relay.")
              (form (@ (submit ,start))
                    (fieldset
                     (label "Display name"
                            (input (@ (id "self-proposed-name")
                                      (placeholder "Alice"))))
                     (label "Relay URI"
                            (input (@ (id "relay-uri")
                                      (placeholder "ocapn://...")))))
                    (button (@ (type "submit")) "Start")))))))

(define (chat id)
  (define rooms '())
  (define current-room #f)
  (define our-name (*backend* 'send id 'spn))
  (define (add-room! room)
    (define (on-change event)
      (when (eq? room current-room)
        (refresh-room-log room)))
    (let ((notifier (*backend* 'make-notifier on-change)))
      (*backend* 'send (room-actor room) 'subscribe notifier)
      (set! rooms (cons room rooms))))
  (define (start-create-room event)
    (prevent-default! event)
    (set! current-room #f)
    (set-inner-shtml!
     (get-element-by-id "context-header")
     `(span (@ (class "room-title")) "Create room"))
    (set-inner-shtml!
     (get-element-by-id "main-pane")
     `(article
       (form (@ (submit ,finish-create-room))
             (fieldset
              (label "Display name" (input (@ (id "room-name")))))
             (button (@ (type "submit")) "Create")))))
  (define (finish-create-room event)
    (prevent-default! event)
    (let* ((name (value-for-id "room-name"))
           (pubkey (*backend* 'send id 'public-key))
           (actor (*backend* 'make-room id pubkey))
           (cert-id (*backend* 'send actor 'add-certificate
                               #:controllers (list pubkey)))
           (room (make-room name actor cert-id '())))
      (add-room! room)
      (switch-to-room room)))
  (define (start-join-room event)
    (prevent-default! event)
    (set! current-room #f)
    (set-inner-shtml!
     (get-element-by-id "context-header")
     `(span (@ (class "room-title")) "Join room"))
    (set-inner-shtml!
     (get-element-by-id "main-pane")
     `(article
       (form (@ (submit ,finish-join-room))
             (fieldset
              (label "Connection name" (input (@ (id "join-connection-name"))))
              (label "Invite URI" (input (@ (id "join-invite-sturdyref")))))
             (button (@ (type "submit")) "Join")))))
  (define (finish-join-room event)
    (prevent-default! event)
    (let* ((connection-name (value-for-id "join-connection-name"))
           (sturdyref (value-for-id "join-invite-sturdyref"))
           (invite (*backend* 'enliven sturdyref)))
      (match (*backend* 'send invite 'redeem (*backend* 'send id 'public-key))
        ((name root-signer cert-id connector)
         (let* ((actor (*backend* 'make-room id root-signer))
                (our-replica (car (*backend* 'send actor 'fresh-replica)))
                (their-replica (*backend* 'send connector 'connect our-replica))
                (connection (make-connection connection-name 'connected
                                             our-replica))
                (room (make-room name actor cert-id (list connection))))
           (*backend* 'send actor 'add-replica their-replica)
           (add-room! room)
           (switch-to-room room))))))
  (define (refresh-room-list)
    (define (make-switcher room)
      (lambda (event)
        (prevent-default! event)
        (switch-to-room room)))
    (set-inner-shtml!
     (get-element-by-id "aux-pane")
     `((article
        ,(match rooms
           (()
            '(section
              (p "No chat rooms available! 😭")
              (p "Create a join or a room with the buttons below!")))
           (_
            `(ul (@ (class "room-list"))
                 ,@(map (lambda (room)
                          `(li (@ (class ,(if (eq? room current-room)
                                              "current-room"
                                              "room-name")))
                               (a (@ (href "#")
                                     (click ,(make-switcher room)))
                                  ,(room-name room))))
                        rooms)))))
       (article (@ (class "room-list-buttons"))
                (button (@ (click ,start-create-room)) "Create room")
                (button (@ (click ,start-join-room)) "Join room")))))
  (define (render-message msg)
    (match msg
      ((id author cert-id created-at modified-at deleted-at contents reacts)
       (let ((reacts (sort reacts (lambda (a b) (char<? (car a) (car b))))))
         (cond
          (deleted-at
           `(p (em "messaged deleted")))
          (else
           `(p ,contents
               ;; (ul (li (button "edit"))
               ;;     (li (button "delete")))
               ;; ,@(match reacts
               ;;     (() '())
               ;;     (reacts
               ;;      `((ul
               ;;         ,@(map (match-lambda
               ;;                  ((char . whos)
               ;;                   `(li ,(format #t " ~ax~a" char (length whos)))))
               ;;                reacts)))))
               )))))))
  (define (render-message-block messages names)
    (match messages
      (((_ author . _) . _)
       `(section (@ (class "message-block"))
                 (header
                  (cite ,(or (assoc-ref names author) "<unknown>")))
                 ,@(map render-message messages)))))
  (define (get-all-messages room)
    (*backend* 'send (room-actor room) 'all-messages))
  (define (chunkify pred lst)
    (define (next-chunk lst)
      (match lst
        ((_) (values lst '()))
        ((prev . lst)
         (let lp ((lst lst) (prev prev))
           (match lst
             (() (values (list prev) '()))
             ((next . rest)
              (if (pred prev next)
                  (let-values (((chunk rest) (lp rest next)))
                    (values (cons prev chunk) rest))
                  (values (list prev) lst))))))))
    (if (null? lst)
        '()
        (let-values (((chunk lst) (next-chunk lst)))
          (cons chunk (chunkify pred lst)))))
  (define (refresh-room-log room)
    (let* ((actor (room-actor room))
           (names (*backend* 'send actor 'profiles)))
      (set-inner-shtml!
       (get-element-by-id "room-log")
       (map (lambda (msg)
              (render-message-block msg names))
            (chunkify (lambda (a b)
                        (match-let (((_ author-a . _) a)
                                    ((_ author-b . _) b))
                          (equal? author-a author-b)))
                      (get-all-messages room))))))
  (define (switch-to-room room)
    (define (send-message)
      (let* ((textarea (get-element-by-id "room-message"))
             (str (element-value textarea)))
        (unless (string-null? str)
          (*backend* 'send (room-actor room) 'post
                     (room-certificate-id room)
                     str)
          (set-element-value! textarea "")
          (refresh-room-log room))))
    (define (send-message/click event)
      (prevent-default! event)
      (send-message))
    (define (send-message/enter event)
      (when (string=? (keyboard-event-key event) "Enter")
        (send-message)))
    (define (create-invite event)
      (prevent-default! event)
      (let* ((name-elem (get-element-by-id "invite-name"))
             (name (element-value name-elem)))
        (unless (string-null? name)
          (let* ((invite (*backend* 'make-invite
                                    (room-name room)
                                    (room-actor room)
                                    (room-certificate-id room)))
                 (connection (make-connection name 'invited invite)))
            (add-room-connection! room connection)
            (set-element-value! name-elem "")
            (refresh-connections)))))
    (define (refresh-connections)
      (set-inner-shtml!
       (get-element-by-id "room-connections")
       (match (room-connections room)
         (()
          '((p "No connections! 😭")
            (p "Invite someone below.")))
         (connections
          `(table
            ,@(map (lambda (conn)
                     `(tr (td ,(connection-name conn))
                          (td ,(symbol->string (connection-status conn)))
                          (td
                           ,@(match (connection-status conn)
                               ('invited
                                `((a (@ (href ,(*backend* 'make-sturdyref
                                                          (connection-actor conn))))
                                     "Invite URI")))
                               (_ '())))))
                   connections))))))
    (define (open-room-settings event)
      (prevent-default! event)
      (set-inner-shtml!
       (get-element-by-id "context-header")
       `(span (@ (class "room-title"))
              "Settings: ",(room-name room)))
      (set-inner-shtml!
       (get-element-by-id "main-pane")
       `(article
         (header (h2 "Connections"))
         (section (@ (id "room-connections")))
         (section
          (h3 "Invite someone")
          (form (@ (submit ,create-invite))
                (fieldset
                 (label "Name" (input (@ (id "invite-name")))))
                (button (@ (type "submit")) "Invite")))))
      (refresh-connections))
    (define (open-room-log)
      (set-inner-shtml!
       (get-element-by-id "main-pane")
       `((article (@ (id "room-log")))
         (article (@ (class "room-compose"))
                  (textarea (@ (id "room-message")
                               (placeholder "Type a message...")
                               (keydown ,send-message/enter)))
                  (button (@ (click ,send-message/click)) "➤"))))
      (refresh-room-log room))
    (unless (eq? room current-room)
      (set! current-room room)
      (refresh-room-list))
    (set-inner-shtml!
     (get-element-by-id "context-header")
     `((span (@ (class "room-title")) ,(room-name room))
       (a (@ (class "room-settings")
             (href "#")
             (click ,open-room-settings))
          "⚙")))
    (open-room-log))
  (set-inner-shtml!
   (document-body)
   `(main (@ (class "chat-container"))
          (div (@ (id "profile"))
               (a (@ (class "profile-icon")
                     (href "#"))
                  ,(string (char-upcase (string-ref our-name 0)))))
          (div (@ (id "context-header")))
          (div (@ (id "aux-pane")))
          (div (@ (id "main-pane")))))
  (refresh-room-list))

setup
