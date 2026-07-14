;;; gay://chat structured event helpers
;;; This module is intentionally list-based so events can be stored in
;;; Brassica's existing Syrup-encoded message `contents` without adding
;;; new record marshallers.

(define-module (brassica gay event)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-gay-event
            gay-event?
            gay-event-version
            gay-event-kind
            gay-event-color
            gay-event-refs
            gay-event-body
            gay-event-feedback
            gay-event-ref
            gay-event-color-ref
            valid-gay-event?))

(define* (make-gay-event kind #:key
                         (version 1)
                         (color '())
                         (refs '())
                         (body '())
                         (feedback '()))
  `(gay-event
    (version ,version)
    (kind ,kind)
    (color ,color)
    (refs ,refs)
    (body ,body)
    (feedback ,feedback)))

(define (gay-event? x)
  (match x
    (('gay-event . _) #t)
    (_ #f)))

(define (section event key #:optional (default #f))
  (match event
    (('gay-event sections ...)
     (match (assoc key sections)
       ((_ value) value)
       (#f default)))
    (_ default)))

(define (gay-event-version event) (section event 'version))
(define (gay-event-kind event) (section event 'kind))
(define (gay-event-color event) (section event 'color '()))
(define (gay-event-refs event) (section event 'refs '()))
(define (gay-event-body event) (section event 'body '()))
(define (gay-event-feedback event) (section event 'feedback '()))

(define (alist-ref* key alist #:optional (default #f))
  (match (assoc key alist)
    ((_ value) value)
    (#f default)))

(define (gay-event-ref event key #:optional (default #f))
  (alist-ref* key (gay-event-refs event) default))

(define (gay-event-color-ref event key #:optional (default #f))
  (alist-ref* key (gay-event-color event) default))

(define valid-kinds
  '(observation protention feedback obstruction experiment result
    decision retrospective petname port grant revoke score))

(define valid-color-keys
  '(domain phase role sensitivity glue))

(define (valid-gay-event? event)
  (and (gay-event? event)
       (equal? (gay-event-version event) 1)
       (memq (gay-event-kind event) valid-kinds)
       (every (match-lambda
                ((key _) (memq key valid-color-keys))
                (_ #f))
              (gay-event-color event))))
