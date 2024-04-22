const tbl = new WebAssembly.Table({
  initial: 0,
  element: "anyfunc"
});

console.log(tbl.length);

const importObject = {
  js: { tbl }
};

const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('table_grow.wasm');

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  obj.instance.exports.ocsh(),
  
  console.log(tbl.length);
  console.log(tbl.get(0)());
  console.log(tbl.get(1)());
});