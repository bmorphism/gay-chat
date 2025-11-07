(use-modules (brassica chat)
             (brassica relay)
             (fibers)
             (fibers channels)
             (fibers operations)
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins actor-lib methods)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer prelay)
             (goblins ocapn netlayer prelay-utils)
             (goblins ocapn netlayer websocket)
             (goblins utils hashmap)
             (goblins vat)
             (ice-9 match)
             (srfi srfi-1))

(define (await-promise-operation vat promise)
  (define (try-fn) #f)
  (define (block-fn state resume)
    (with-vat vat
      (on promise
          (lambda (x)
            (when (op-state-complete! state)
              (resume (lambda () x))
              #t))
          #:catch
          (lambda (err)
            (when (op-state-complete! state)
              (resume (lambda () (error "promise was rejected" err)))
              #f))))
    (values))
  (make-base-operation #f try-fn block-fn))

(define (await vat promise)
  (perform-operation (await-promise-operation vat promise)))

(define (call-with-vat* vat thunk)
  (call-with-values (lambda () (call-with-vat vat thunk))
    (case-lambda
      (() (values))
      ((x)
       (if (promise-refr? x)
           (await vat x)
           x))
      (vals (apply values vals)))))

(define-syntax-rule (with-vat* vat body ...)
  (call-with-vat* vat (lambda () body ...)))

(define-actor (^invite become name room parent-cert-id)
  (define (redeemed . args) (error "already redeemed"))
  (methods
   ((redeem who)
    (become redeemed
            (list name
                  (: room 'root-signer)
                  (: room 'add-certificate
                     #:parent parent-cert-id
                     #:controllers (list who)
                     #:predicate '(when-op (edit delete) (allow-self)))
                  (spawn ^connector room))))))

;; TODO: Add revocation support.
(define-actor (^connector become room)
  (methods
   ((connect their-replica)
    (match (: room 'fresh-replica)
      ((our-replica revoker)
       (: room 'add-replica their-replica)
       our-replica)))))

(define-actor (^notifier become proc)
  (lambda args
    (syscaller-free-fiber
     (lambda () (apply proc args)))
    #t))

(lambda ()
  (define vat (spawn-vat))
  (define netlayer (with-vat vat (spawn ^websocket-netlayer)))
  (define mycapn (with-vat vat (spawn-mycapn netlayer)))
  (methods
   ((send obj . args)
    (if (or (promise-refr? obj) (not (local-refr? obj)))
        (with-vat* vat (apply <- obj args))
        (with-vat vat (apply : obj args))))
   ((make-identity spn)
    (with-vat vat (spawn ^identity spn)))
   ((make-room id root-signer)
    (with-vat vat (spawn ^chat-room id root-signer)))
   ((make-invite name room parent-cert-id)
    (with-vat vat (spawn ^invite name room parent-cert-id)))
   ((make-notifier proc)
    (with-vat vat (spawn ^notifier proc)))
   ((connect-to-relay uri)
    (with-vat vat
      (define hub (<- mycapn 'enliven (string->ocapn-id uri)))
      ;; TODO: Save registered device to re-use.
      (define device (<- hub 'register))
      (define relay (connect-device device mycapn))
      (: mycapn 'install-netlayer relay)
      #t))
   ((make-sturdyref obj)
    (with-vat* vat
               (on (<- mycapn 'register obj 'prelay)
                   ocapn-id->string
                   #:promise? #t)))
   ((enliven sturdyref)
    (with-vat vat
      (: mycapn 'enliven (string->ocapn-id sturdyref))))))
