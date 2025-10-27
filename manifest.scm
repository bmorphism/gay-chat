(use-modules (gnu packages base)
             (gnu packages guile)
             (gnu packages guile-xyz))

(packages->manifest (list gnu-make guile-next guile-goblins guile-hoot))
