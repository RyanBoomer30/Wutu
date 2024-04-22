const fs = require('node:fs');

if (process.argv.length !== 3) {
  console.error('Expected an input wasm file, and nothing more!');
  process.exit(1);
}

const wasmBuffer = fs.readFileSync(process.argv[2]);

const importObject = {
  imports: { imported_func: (arg) => console.log((arg >> 1n).toString()) },
};

WebAssembly.instantiate(wasmBuffer, importObject).then(obj => {
  const { exported_func } = obj.instance.exports;
  exported_func();
});