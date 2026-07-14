#!/bin/sh
set -eu

guile -L . -c '(use-modules (brassica gay event)) (unless (valid-gay-event? (make-gay-event (quote protention) #:color (quote ((domain strategy))))) (error "event validation failed"))'
guile -L . tests/gay-event-test.scm
guile -L . gay-demo.scm
test -s worldview/current.md
test -s worldview/consensus-topos.json
python3 -m json.tool worldview/consensus-topos.json >/dev/null
printf 'gay://chat tests ok\n'
