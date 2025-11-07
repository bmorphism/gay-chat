backend_modules = \
  brassica/hlc.scm \
  brassica/crdt.scm \
  brassica/chat.scm \
  brassica/relay.scm

ui_modules = \
  brassica/dom/document.scm \
  brassica/dom/element.scm \
  brassica/dom/event.scm \
  brassica/dom.scm

demo: $(modules)
	guile -L . demo.scm

app-backend.wasm: app-backend.scm $(backend_modules)
	guild compile-wasm -L . -o app-backend.wasm app-backend.scm

app-ui.wasm: app-ui.scm $(ui_modules)
	guild compile-wasm --bundle --mode=secondary -L . -o app-ui.wasm app-ui.scm

server: app-backend.wasm app-ui.wasm server.scm
	guile -L . server.scm

.PHONY: demo app server
