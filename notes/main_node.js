const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('simple.wasm');

const importObject = {
  imports: { imported_func: (arg) => console.log((arg >> 0n).toString()) },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  const { exported_func } = obj.instance.exports;
  exported_func();
});