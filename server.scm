;;; Copyright (C) 2025 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;; http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(use-modules (brassica relay)
             (fibers)
             (fibers conditions)
             ((goblins) #:hide ($))
             ((goblins) #:select (($ . :)))
             (goblins default-vat-scheduler)
             (goblins vat)
             (goblins ocapn ids)
             (goblins ocapn captp)
             (goblins ocapn netlayer websocket)
             (goblins actor-lib facet)
             (goblins actor-lib joiners)
             (goblins actor-lib methods)
             (goblins actor-lib on)
             (goblins utils crypto)
             (hoot web-server)
             (ice-9 match)
             (ice-9 threads))

(define vat (spawn-vat))

(with-vat vat
  (define netlayer
    (spawn ^websocket-netlayer
           #:url "ws://localhost:8889" ; "ws://108.20.181.127:8889"
           #:host "0.0.0.0"
           #:port 8889
           #:verify-certificates? #f
           #:encrypted? #f))
  (define mycapn (spawn-mycapn netlayer))
  (define relay-admin (spawn ^relay-admin mycapn netlayer))

  (define (fresh-account)
    (<- relay-admin 'add-account))

  (define users '("Alice" "Bob" "Carol"))

  (let-on ((srefs (all-of* (map (lambda (_) (fresh-account)) users))))
    (display "Relay sturdyrefs:\n\n")
    (for-each (lambda (user sref)
                (format #t "~a:\n~a\n\n" user (ocapn-id->string sref)))
              users srefs)
    (signal-condition! start-web-server)))

(define start-web-server (make-condition))

;; Wait for the relay to boot up, then start the web server.
(wait start-web-server)
(serve)
