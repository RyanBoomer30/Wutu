import { importObject, print } from "../compiler/runtime.js";

const form = document.getElementById("runButton");
form.addEventListener("click", handle_submit);

function handle_submit() {
  const code = document.getElementById('code').value;
  handleData(code);
};

function foo(code) {
  console.log(code);
}

async function handleData(input) {
  try {
    const response = await fetch('http://localhost:8080/submit', {
      method: 'POST',
      body: input
    });

    if (response.ok) {
      const data = await response.json();
      // console.log('Result: ', data.result);
      // console.log('Value: ', data.value);

      // Assuming data.value is a base64-encoded string of the WASM binary
      const base64 = data.value;
      
      // Convert base64 to a Uint8Array
      const binaryString = atob(base64);
      const bytes = new Uint8Array(binaryString.length);
      for (let i = 0; i < binaryString.length; i++) {
        bytes[i] = binaryString.charCodeAt(i);
      }

      WebAssembly.instantiate(bytes, importObject).then(obj => {
        let answer = obj.instance.exports.our_code_starts_here();
        print(answer);
      });
    } else {
      throw new Error('HTTP error: ' + response.status);
    }
  } catch (error) {
    console.error('Error:', error);
  }
}