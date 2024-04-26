const runtime = require('../compiler/runtime');

const fs = require('node:fs');

if (process.argv.length !== 3) {
  console.error('Expected an input wasm file, and nothing more!');
  process.exit(1);
}

const wasmBuffer = fs.readFileSync(process.argv[2]);

WebAssembly.instantiate(wasmBuffer, runtime.importObject).then(obj => {
  // try {
    let answer = obj.instance.exports.our_code_starts_here();
    // console.log(answer);
    // runtime.print_table();
    runtime.print(answer);
  // } catch (e) {
  //   console.error(e.message);
  //   process.exit(e.cause);  // for our test runner to detect
  // }
});