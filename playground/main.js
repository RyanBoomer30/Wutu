/* Web dev code */
// The parse function that processes the input data
function parse(input) {
  console.log("Received input:", input);
  handleData(input);
}

async function handleData(input) {
  try {
    const response = await fetch('http://localhost:8080/submit', {
      method: 'POST',
      body: input
    });

    if (response.ok) {
      const data = await response.json();
      console.log('Result: ', data.result);
      console.log('Value: ', data.value);

      // Assuming data.value is a base64-encoded string of the WASM binary
      const base64 = data.value;
      
      // Convert base64 to a Uint8Array
      const binaryString = atob(base64);
      const bytes = new Uint8Array(binaryString.length);
      for (let i = 0; i < binaryString.length; i++) {
        bytes[i] = binaryString.charCodeAt(i);
      }

      // Below are copy paste from the compiler code below that worked in the browser
      const imports = {
        imports: {
          imported_func: (arg) => {
            console.log((arg >> 0n).toString());
          },
        },
      };

      const wasmModule = await WebAssembly.compile(bytes);
      console.log(wasmModule);
      const wasmInstance = await WebAssembly.instantiate(wasmModule, imports)

      console.log('WASM instance:', wasmInstance);

      if (wasmInstance.exports.exportedFunction) {
        wasmInstance.exports.exported_func();
      }
    } else {
      throw new Error('HTTP error: ' + response.status);
    }
  } catch (error) {
    console.error('Error:', error);
  }
}

/* Compiler code */
const importObject = {
  imports: {
    imported_func: (arg) => {
      console.log((arg >> 0n).toString());
    },
  },
};

WebAssembly.instantiateStreaming(fetch("simple.wasm"), importObject)
  .then((obj) => {
    obj.instance.exports.exported_func();
  })
  .catch((error) => {
    console.error("Error instantiating the WebAssembly module:", error);
  });