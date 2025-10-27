;;; Commentary:
;;
;;
;; Hybrid logical clocks are a tuple of real time, logical time, and a
;; node ID.  Despite incorporating real time, this clock is monotonic.
(define-module (hlc)
  #:use-module (ice-9 match)
  #:export (make-clock
            clock-real
            clock-logical
            clock-id
            clock-tick
            clock-join
            clock-compare
            clock-compare-partial
            clock<?))

;; HLCs are tuples of real time (non-negative int), logical time
;; (non-negative int), and machine ID (string).
(define (%make-clock real logical id) (vector real logical id))
(define (clock-real ts) (vector-ref ts 0))
(define (clock-logical ts) (vector-ref ts 1))
(define (clock-id ts) (vector-ref ts 2))

(define (make-clock id)
  (%make-clock (current-time) 0 id))

(define* (clock-tick clock #:optional (now (current-time)))
  (match clock
    (#(real logical id)
     ;; The real time can never go backwards, even if the system clock
     ;; does.
     (if (> now real)
         (%make-clock now 0 id)
         (%make-clock real (1+ logical) id)))))

(define* (clock-join clock other #:optional (now (current-time)))
  (match-let ((#(real logical id) clock)
              (#(real* logical* _) other))
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
  (match-let ((#(real logical id) a)
              (#(real* logical* id*) b))
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
  (match-let ((#(real logical id) a)
              (#(real* logical* id*) b))
    (if (= real real*)
        (- logical logical*)
        (- real real*))))

(define (clock<? a b)
  (negative? (clock-compare a b)))
