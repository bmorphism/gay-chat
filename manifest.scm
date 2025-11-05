(use-modules (guix gexp)
             (guix packages)
             (gnu packages autotools)
             (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz))

(define guile-goblins*
  (package
    (inherit guile-goblins)
    (arguments
     (list #:tests? #f
           #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
           ;; Copy sources to a directory just for Hoot, so we aren't
           ;; mixing modules for the host and target together which
           ;; causes problems.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-hoot
                 (lambda* _
                   (let ((src (string-append #$output "/share/guile/site/3.0"))
                         (dst (string-append #$output "/share/guile-hoot/site")))
                     (mkdir-p dst)
                     (copy-recursively src dst)
                     #t))))))
    (native-inputs
     (modify-inputs (package-native-inputs guile-goblins)
       (append autoconf automake)))))

(packages->manifest (list gnu-make guile-next guile-goblins* guile-hoot))
