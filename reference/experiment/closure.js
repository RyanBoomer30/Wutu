const memory = new WebAssembly.Memory({
  initial: 10,
});

const tbl = new WebAssembly.Table({
  initial: 2,
  element: "anyfunc"
});

const importObject = {
  js: { mem: memory, tbl: tbl }
};

const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('closure.wasm');

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  try {
    console.log(obj.instance.exports.ocsh());
  } catch (e) {
    console.log(e instanceof WebAssembly.RuntimeError); // true
    console.log(e.message); // "Hello"
    console.log(e.name); // "RuntimeError"
    console.log(e.fileName); // "someFile"
    console.log(e.lineNumber); // 10
    console.log(e.columnNumber); // 0
    console.log(e.stack); // returns the location where the code was run
  }
});