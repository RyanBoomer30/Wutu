const fs = require('node:fs');

const MAX_I64 = 9223372036854775807n
const MIN_I64 = -9223372036854775808n

const wasmBuffer = fs.readFileSync('overflow.wasm');

const safeAdd = (x, y) => {
  const result = x + y;
  if (result > MAX_I64 || result < MIN_I64) {
    throw new Error("Overflow boooo");
  }
  return result;
}

const importObject = {
  imports: { imported_func: (arg) => console.log((arg).toString()),
             safeAdd: safeAdd },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  const { exported_func } = obj.instance.exports;
  try {
    exported_func();
  } catch(e) {
    console.error(e);
  }
});