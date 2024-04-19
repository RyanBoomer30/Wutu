const importObject = {
  imports: { imported_func: (arg) => console.log((arg >> 0n).toString()) },
};

WebAssembly.instantiateStreaming(fetch("simple.wasm"), importObject).then(
  (obj) => obj.instance.exports.exported_func(),
);