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

;;; Commentary:
;;;
;;; Relay helpers.
;;;
;;; Code:

(define-module (brassica relay)
  #:use-module ((goblins) #:hide ($))
  #:use-module ((goblins) #:select (($ . :)))
  #:use-module (goblins actor-lib cell)
  #:use-module (goblins actor-lib common)
  #:use-module (goblins actor-lib joiners)
  #:use-module (goblins actor-lib methods)
  #:use-module (goblins actor-lib on)
  #:use-module (goblins ocapn ids)
  #:use-module (goblins ocapn netlayer prelay)
  #:use-module (goblins utils crypto)
  #:use-module (ice-9 match)
  #:export (^relay-admin
            connect-device))

(define-actor (^enliven-facet become mycapn)
  (methods
   ((enliven sturdyref)
    (: mycapn 'enliven sturdyref))))

(define-actor (^register-facet become mycapn netlayer)
  (define netlayer-name (: netlayer 'netlayer-name))
  (methods
   ((register obj)
    (: mycapn 'register obj netlayer-name))))

;; Multi-device hub.
(define-actor (^device-hub become enliven register)
  (define devices (spawn ^ghash))
  (define online (spawn ^seteq))
  (define (^device become)
    (define token (list 'disconnect))
    (define online? (spawn ^cell #f))
    (define-values (prelay-endpoint prelay-controller)
      (spawn-prelay-pair enliven))
    (define (^controller-proxy become)
      (lambda args
        (if (: online?)
            (apply : prelay-controller args)
            (error "device is offline" args))))
    (define prelay-controller* (spawn ^controller-proxy))
    (define endpoint-sturdyref (<- register 'register prelay-endpoint))
    (define controller-sturdyref (<- register 'register prelay-controller*))
    (methods
     ((connect presence)
      (cond
       ((: online?)
        (error "device claimed"))
       (else
        (: online? #t)
        (on-sever presence
                  (lambda (type reason)
                    (: online? #f)))
        (all-of endpoint-sturdyref controller-sturdyref))))))
  (methods
   ((register)
    (define device (spawn ^device))
    (define sturdyref (<- register 'register device))
    (: devices 'set sturdyref device)
    device)))

(define-actor (^relay-admin become mycapn netlayer)
  (define enliven (spawn ^enliven-facet mycapn))
  (define register (spawn ^register-facet mycapn netlayer))
  (define accounts (spawn ^cell '()))
  (methods
   ((add-account)
    (define hub (spawn ^device-hub enliven register))
    (: accounts (cons hub (: accounts)))
    (: register 'register hub))))

(define (connect-device device mycapn)
  (define (^presence become)
    (lambda args (error "I am but a simple presence actor")))
  (define relay-info (<- device 'connect (spawn ^presence)))
  (define endpoint-sturdyref
    (on-match relay-info ((endpoint _) endpoint)))
  (define controller-sturdyref
    (on-match relay-info ((_ controller) controller)))
  (spawn ^prelay-netlayer
         (spawn ^enliven-facet mycapn)
         endpoint-sturdyref
         controller-sturdyref))
