;;; Hybrid logical clock
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
;;; Hybrid logical clocks are a tuple of real time, logical time, and
;;; a string ID.  Despite incorporating real time, this clock is
;;; monotonic.  A partial ordering can be formed by comparing the
;;; clock's real and logical components, in that order.  A total
;;; ordering can be formed by incorporating the clock ID as a
;;; tie-breaker.
;;;
;;; Code:

(define-module (hlc)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (current-time/ms

            <clock>
            %make-clock
            make-clock
            clock?
            clock-real
            clock-logical
            clock-id
            clock-tick
            clock-join
            clock-compare
            clock-compare-partial
            clock<?))

(define (current-time/ms)
  (match (gettimeofday)
    ((sec . usec)
     (+ (* sec 1000) (floor/ usec 1000)))))

(define-record-type <clock>
  (%make-clock real logical id)
  clock?
  (real clock-real)                     ; int
  (logical clock-logical)               ; int
  (id clock-id))          ; string

(define (make-clock id)
  (%make-clock (current-time/ms) 0 id))

(define* (clock-tick clock #:optional (now (current-time/ms)))
  (match clock
    (($ <clock> real logical id)
     ;; The real time component can never go backwards, even if the
     ;; system clock does.
     (if (> now real)
         (%make-clock now 0 id)
         (%make-clock real (1+ logical) id)))))

(define* (clock-join clock other #:optional (now (current-time/ms)))
  (match-let ((($ <clock> real logical id) clock)
              (($ <clock> real* logical* _) other))
    (cond
     ;; System time is ahead of both clocks.
     ((and (> now real) (> now real*))
      (%make-clock now 0 id))
     ;; Both clocks have the same real time; join on logical time.
     ((= real real*)
      (%make-clock real (1+ (max logical logical*)) id))
     ;; Our clock is behind the other clock.
     ((< real real*)
      (%make-clock real* (1+ logical*) id))
     ;; Our clock is ahead of the other clock.
     (else
      (%make-clock real (1+ logical) id)))))

;; Total order comparison, using ids to break ties.
(define (clock-compare a b)
  (match-let ((($ <clock> real logical id) a)
              (($ <clock> real* logical* id*) b))
    ;; Order of priority: real time, logical time, and then finally
    ;; the ID string as a last resort to determine a winner in the
    ;; concurrent case.
    (if (= real real*)
        (if (= logical logical*)
            (if (string=? id id*)
                0
                (if (string<? id id*) -1 1))
            (- logical logical*))
        (- real real*))))

;; Partial order comparison.
(define (clock-compare-partial a b)
  (match-let ((($ <clock> real logical id) a)
              (($ <clock> real* logical* id*) b))
    (if (= real real*)
        (- logical logical*)
        (- real real*))))

(define (clock<? a b)
  (negative? (clock-compare a b)))
