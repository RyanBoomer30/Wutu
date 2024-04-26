import "../compiler/runtime.js";

const form = document.getElementById("runButton");
form.addEventListener("click", handle_submit);

function handle_submit() {
  const code = document.getElementById("code").value;
  handleData(code);
}

const output = document.getElementById("output");

async function handleData(input) {
  const response = await fetch("http://localhost:8080/submit", {
    method: "POST",
    body: input,
  });

  if (response.ok) {
    const data = await response.json();

    if (!data.did_compile) {
      output.textContent += "Compile Errors:\n" + data.value + "\n";
      return;
    }

    // Assuming data.value is a base64-encoded string of the WASM binary
    const base64 = data.value;

    // Convert base64 to a Uint8Array
    const binaryString = atob(base64);
    const bytes = new Uint8Array(binaryString.length);
    for (let i = 0; i < binaryString.length; i++) {
      bytes[i] = binaryString.charCodeAt(i);
    }

    // before we instantiate, reset the trigger callback
    // by cloning and replacing the button (which doesn't take event listeners)
    const old_trigger = document.getElementById("triggerButton");
    const new_trigger = old_trigger.cloneNode(true);
    old_trigger.parentNode.replaceChild(new_trigger, old_trigger);

    WebAssembly.instantiate(bytes, runtime.importObject).then((obj) => {
      try {
        let answer = obj.instance.exports.our_code_starts_here();
        let mem = new BigInt64Array(runtime.memory.buffer);
        output.textContent += runtime.snake_to_string(answer, mem) + "\n";
      } catch (err) {
        console.log(err);
        output.textContent += "Runtime " + err.message + "\n";
      }
    });
  } else {
    throw new Error("HTTP error: " + response.status);
  }
}
