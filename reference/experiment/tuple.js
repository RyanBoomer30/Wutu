const memory = new WebAssembly.Memory({
  initial: 1,
});

const importObject = {
  runtime: { memory: memory }
};

const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('tuple.wasm');

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  let tagged_ptr = obj.instance.exports.our_code_starts_here();
  
  // signed? i'm assuming
  const mem = new BigInt64Array(memory.buffer);
  
  // we untag, then shift because we access with words but ptr is in bytes
  for (i = 0; i < mem[(tagged_ptr - 3n) / 8n]; i++) {
    console.log(mem[i + 1]);
  }
});