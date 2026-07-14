;;; Export gay://chat events to a consensus-topos JSON and Markdown view.
;;; Usage:
;;;   guile -L . export-worldview.scm ROOM-NAME OUTPUT-DIR EVENT-FILE...
;;;
;;; EVENT-FILE is a Scheme datum stream containing gay-event forms, one per read.

(use-modules (brassica gay event)
             (ice-9 format)
             (ice-9 match)
             (ice-9 pretty-print)
             (srfi srfi-1))

(define (usage)
  (display "usage: guile -L . export-worldview.scm ROOM-NAME OUTPUT-DIR EVENT-FILE...\n"
           (current-error-port))
  (exit 64))

(define (read-all file)
  (call-with-input-file file
    (lambda (port)
      (let lp ((memo '()))
        (match (read port)
          ((? eof-object?) (reverse memo))
          (datum (lp (cons datum memo))))))))

(define (json-escape str)
  (call-with-output-string
    (lambda (port)
      (string-for-each
       (lambda (ch)
         (match ch
           (#\\ (display "\\\\" port))
           (#\" (display "\\\"" port))
           (#\newline (display "\\n" port))
           (#\return (display "\\r" port))
           (#\tab (display "\\t" port))
           (_ (display ch port))))
       str))))

(define (->string x)
  (cond
   ((string? x) x)
   ((symbol? x) (symbol->string x))
   ((number? x) (number->string x))
   ((boolean? x) (if x "true" "false"))
   (else
    (call-with-output-string
      (lambda (port) (write x port))))))

(define (write-json x port)
  (cond
   ((string? x) (format port "\"~a\"" (json-escape x)))
   ((symbol? x) (write-json (symbol->string x) port))
   ((number? x) (display x port))
   ((boolean? x) (display (if x "true" "false") port))
   ((null? x) (display "[]" port))
   ((and (list? x)
         (every (match-lambda (((? symbol?) _) #t) (_ #f)) x))
    (display "{" port)
    (let lp ((items x) (first? #t))
      (match items
        (() #t)
        (((key value) . rest)
         (unless first? (display "," port))
         (write-json (symbol->string key) port)
         (display ":" port)
         (write-json value port)
         (lp rest #f))))
    (display "}" port))
   ((list? x)
    (display "[" port)
    (let lp ((items x) (first? #t))
      (match items
        (() #t)
        ((item . rest)
         (unless first? (display "," port))
         (write-json item port)
         (lp rest #f))))
    (display "]" port))
   (else (write-json (->string x) port))))

(define* (body-ref event key #:optional (default #f))
  (match (assoc key (gay-event-body event))
    ((_ value) value)
    (#f default)))

(define (event-summary event)
  `((kind ,(gay-event-kind event))
    (color ,(gay-event-color event))
    (refs ,(gay-event-refs event))
    (body ,(gay-event-body event))
    (feedback ,(gay-event-feedback event))))

(define (kind=? kind)
  (lambda (event) (eq? (gay-event-kind event) kind)))

(define (write-section title events port)
  (format port "\n## ~a\n\n" title)
  (match events
    (() (display "None.\n" port))
    (_
     (for-each
      (lambda (event)
        (format port "- **~a** — ~a\n"
                (gay-event-kind event)
                (or (body-ref event 'claim)
                    (body-ref event 'summary)
                    (body-ref event 'conflict)
                    (body-ref event 'result)
                    "(no claim/summary)")))
      events))))

(define (main args)
  (match args
    ((_ room out-dir files ...)
     (when (or (string-null? room) (string-null? out-dir) (null? files))
       (usage))
     (let* ((events (filter valid-gay-event? (append-map read-all files)))
            (observations (filter (kind=? 'observation) events))
            (protentions (filter (kind=? 'protention) events))
            (feedback (filter (kind=? 'feedback) events))
            (obstructions (filter (kind=? 'obstruction) events))
            (experiments (filter (kind=? 'experiment) events))
            (results (filter (kind=? 'result) events))
            (decisions (filter (kind=? 'decision) events))
            (topos `((room ,room)
                     (event_count ,(length events))
                     (strong_beliefs ,(map event-summary decisions))
                     (active_protentions ,(map event-summary protentions))
                     (open_obstructions ,(map event-summary obstructions))
                     (experiments ,(map event-summary experiments))
                     (results ,(map event-summary results))
                     (feedback ,(map event-summary feedback))
                     (observations ,(map event-summary observations)))))
       (unless (file-exists? out-dir) (mkdir out-dir))
       (call-with-output-file (string-append out-dir "/consensus-topos.json")
         (lambda (port)
           (write-json topos port)
           (newline port)))
       (call-with-output-file (string-append out-dir "/current.md")
         (lambda (port)
           (format port "# gay://chat worldview — ~a\n" room)
           (format port "\nEvents: ~a\n" (length events))
           (write-section "Strong beliefs / decisions" decisions port)
           (write-section "Active protentions" protentions port)
           (write-section "Open obstructions" obstructions port)
           (write-section "Experiments" experiments port)
           (write-section "Results" results port)
           (write-section "Feedback" feedback port)
           (write-section "Observations" observations port)))
       (format #t "exported ~a events to ~a\n" (length events) out-dir)))
    (_ (usage))))

(main (command-line))
