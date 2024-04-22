const memory = new WebAssembly.Memory({
  initial: 10,
});

const tbl = new WebAssembly.Table({
  initial: 1,
  element: "anyfunc"
});

const callClosure = (clos, val) => {
  const untagged = clos - 5n;
  const mem = new BigInt64Array(memory.buffer);  
  const tblptr = mem[(untagged / 8n) + 1n];

  return tbl.get(Number(tblptr))(clos, val);
}

const doStuff = (clos, val) => {
  return callClosure(clos, val);
}

const importObject = {
  js: { mem: memory, tbl: tbl },
  imports: { doStuff: doStuff }
};

const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('interop.wasm');

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  console.log(obj.instance.exports.ocsh());
});