function toUint8Array(obj) {
    return new Uint8Array(obj);
}

const subtle = globalThis.crypto.subtle;
const algorithm = { name: "Ed25519" };
const wasmOpts = {
  user_imports: {
    crypto: {
      digest: (algorithm, data) => {
        return subtle.digest(algorithm, data).then(toUint8Array);
      },
      randomValues: (length) => {
        const array = new Uint8Array(length);
        crypto.getRandomValues(array);
        return array;
      },
      generateEd25519KeyPair: () => {
        return subtle.generateKey(algorithm, true, ["sign", "verify"]);
      },
      keyPairPrivateKey: (keyPair) => keyPair.privateKey,
      keyPairPublicKey: (keyPair) => keyPair.publicKey,
      exportKey: (key) => {
        return subtle.exportKey("raw", key).then(toUint8Array);
      },
      importPublicKey: (key) => {
        return subtle.importKey("raw", key, algorithm, true, ["verify"]);
      },
      signEd25519: (data, key) => {
        return subtle.sign(algorithm, key?.privateKey || key, data).then(toUint8Array);
      },
      verifyEd25519: (signature, data, publicKey) => {
        return subtle.verify(algorithm, publicKey, signature, data);
      }
    },
    document: {
      get: () => document,
      body: () => document.body,
      getElementById: (id) => document.getElementById(id),
      createTextNode: (text) => document.createTextNode(text),
      createElement: (tag) => document.createElement(tag)
    },
    element: {
      value: (elem) => elem.value,
      setValue: (elem, value) => elem.value = value,
      class: (elem) => elem.className,
      setClass: (elem, cls) => elem.className = cls,
      width: (elem) => elem.width,
      height: (elem) => elem.height,
      setWidth: (elem, width) => elem.width = width,
      setHeight: (elem, height) => elem.height = height,
      scrollTop: (elem) => elem.scrollTop,
      setScrollTop: (elem, y) => elem.scrollTop = y,
      scrollTopMax: (elem) => elem.scrollTopMax,
      parent: (elem) => elem.parentElement,
      firstChild: (elem) => elem.firstChild,
      nextSibling: (elem) => elem.nextSibling,
      appendChild: (parent, child) => parent.appendChild(child),
      setAttribute: (elem, name, value) => elem.setAttribute(name, value),
      removeAttribute: (elem, name) => elem.removeAttribute(name),
      remove: (elem) => elem.remove(),
      replaceWith: (oldElem, newElem) => oldElem.replaceWith(newElem),
      clone: (elem) => elem.cloneNode(),
      focus: (elem) => elem.focus()
    },
    event: {
      addEventListener: (target, type, listener) => target.addEventListener(type, listener),
      removeEventListener: (target, type, listener) => target.removeEventListener(type, listener),
      target: (event) => event.target,
      currentTarget: (event) => event.currentTarget,
      preventDefault: (event) => event.preventDefault(),
      keyboardKey: (event) => event.key,
      keyboardShiftKey: (event) => event.shiftKey
    },
    uint8Array: {
      new: (length) => new Uint8Array(length),
      fromArrayBuffer: (buffer) => new Uint8Array(buffer),
      length: (array) => array.length,
      ref: (array, index) => array[index],
      set: (array, index, value) => array[index] = value
    },
    webSocket: {
      close: (ws) => ws.close(),
      new(url) {
        ws = new WebSocket(url);
        ws.binaryType = "arraybuffer";
        return ws;
      },
      send: (ws, data) => ws.send(data),
      setOnOpen(ws, f) {
        ws.onopen = (e) => {
          f();
        };
      },
      setOnError(ws, f) {
        ws.onerror = (e) => f();
      },
      setOnMessage(ws, f) {
        ws.onmessage = (e) => {
          f(e.data);
        };
      },
      setOnClose(ws, f) {
        ws.onclose = (e) => {
          f(e.code, e.reason);
        };
      }
    }
  }
};

window.addEventListener("load", async () => {
  const [makeBackend] = await Scheme.load_main("app-backend.wasm", wasmOpts);
  const [spawnGui] = await makeBackend.reflector.load_extension("app-ui.wasm", wasmOpts);
  spawnGui.call(makeBackend);
});
