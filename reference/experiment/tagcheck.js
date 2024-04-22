const fs = require('node:fs');

const wasmBuffer = fs.readFileSync('tagcheck.wasm');

const importObject = {
  imports: { error: (ec, val) => { throw new Error("Wrong type booo") }}
};

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  const { exported_func } = obj.instance.exports;
  try {
    console.log(exported_func());
  } catch(e) {
    // in reality, we would prob not just print to console,
    // rather report to user wherever they expect their answer
    console.error(e.message);
  }
});