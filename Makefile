backend_modules = \
  brassica/gay/event.scm \
  brassica/hlc.scm \
  brassica/crdt.scm \
  brassica/chat.scm \
  brassica/relay.scm

ui_modules = \
  brassica/gay/event.scm \
  brassica/dom/document.scm \
  brassica/dom/element.scm \
  brassica/dom/event.scm \
  brassica/dom.scm

world: $(backend_modules)
	guile -L . world.scm

gay-world: $(backend_modules) export-worldview.scm gay-world.scm
	guile -L . gay-world.scm

app-backend.wasm: app-backend.scm $(backend_modules)
	guild compile-wasm -L . -o app-backend.wasm app-backend.scm

app-ui.wasm: app-ui.scm $(ui_modules)
	guild compile-wasm --bundle --mode=secondary -L . -o app-ui.wasm app-ui.scm

server: app-backend.wasm app-ui.wasm server.scm
	guile -L . server.scm

embed.wasm: embed.scm $(backend_modules) $(ui_modules)
	guild compile-wasm --bundle -L . -o embed.wasm embed.scm

.PHONY: world gay-world app server
