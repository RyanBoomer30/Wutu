## Baby steps
- [x] Get a JS harness working that can instantiate and call a Wasm module from a file
- [x] Experiment with interoperability, calling JS from Wutu and vice-versa
  - Specifically, play around with BigInt64Array
  - Try calling with indirect pointer into function table from both Wasm and JS
    - Use this to guide extern markings. Can we export the whole table?
- [x] Experiment with memory stores and loads, referencing [this](https://rsms.me/wasm-intro)
  - Think hard about bits vs bytes vs words: I think things should actually work nicely?
- [x] Experiment with handling traps
  - We need to be able to raise our own errors, as well as handle Wasm errors (like overflow)
    - We can call some `error()` function in JS, but how would that halt executions? Check options
  - We can't halt, but what happens if we just throw? What even happens?
- Think about how we would compile each operation, etc: experiment as you go!
  - Focus on which things get kept on the stack / popped off, and how we can save/dup if needed

- [x] Make our own AST + pretty-printer
  - [x] Make AST
    - We may need memory.grow later, depending on how GC works ...
    - Maybe expand to make numeric operations not just i64, if necessary
