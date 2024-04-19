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
      // const data = await response.text(); // Use json() or text() based on your response content type
      console.log('Success:', response.status);
    } else {
      throw new Error('HTTP error: ' + response.status);
    }
  } catch (error) {
    console.error('Error:', error);
  }
}


/* Compiler code */
const importObject = {
  imports: { imported_func: (arg) => console.log((arg >> 0n).toString()) },
};

WebAssembly.instantiateStreaming(fetch("simple.wasm"), importObject).then(
  (obj) => obj.instance.exports.exported_func(),
);