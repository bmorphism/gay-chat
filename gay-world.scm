(use-modules (brassica gay event)
             (ice-9 match)
             (srfi srfi-1))

(define events
  (call-with-input-file "gay-world-events.scm"
    (lambda (port)
      (let lp ((memo '()))
        (match (read port)
          ((? eof-object?) (reverse memo))
          (event (lp (cons event memo))))))))

(unless (every valid-gay-event? events)
  (error "invalid event in gay-world-events.scm"))

(format #t "gay://chat world events: ~a\n" (length events))
(for-each
 (lambda (event)
   (format #t "- ~a ~a\n"
           (gay-event-kind event)
           (or (assoc-ref (gay-event-body event) 'claim)
               (assoc-ref (gay-event-body event) 'summary)
               (assoc-ref (gay-event-body event) 'conflict)
               "")))
 events)

(let ((status (system* "guile" "-L" "." "export-worldview.scm" "world" "worldview" "gay-world-events.scm")))
  (unless (zero? status)
    (error "export-worldview failed" status)))
