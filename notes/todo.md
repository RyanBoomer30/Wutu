## Baby steps
- Get a JS harness working that can instantiate and call a Wasm module from a file
- Experiment with memory stores and loads, referencing [this](https://rsms.me/wasm-intro)
  - Think hard about bits vs bytes vs words: I think things should actually work nicely?
- Experiment with interoperability, calling JS from Wutu and vice-versa
  - Specifically, play around with BigInt64Array
- Experiment with handling traps
- Think about how we would compile each operation, etc: experiment as you go!
  - Focus on which things get kept on the stack / popped off, and how we can save/dup
- Figure out an AST that we can target: make your own or use pre-existing (check resources)
- Decide whether ANF is needed -> start to change compilation
  - Probably some accumulated list of functions that we can add to when we see lambdas,
    as well as some list of instructions to stick in the current function body


## Think
- We need to figure out how to make tests ergonomic
  - We can change our Makefile to give us Wasm files -> Selenium tests, ran in browser
  - Look at Jest: maybe we can load some folder of Wasm files and run them from JS?
    - We can't even use the spec interpreter locally, since we need to test our runtime
- We may not even need to ANF, based on how the stack machine works
  - Shunting yard algorithm could be helpful?
  - Passive think about whether this would break garbage collection with shadow stack
    - Especially with things that are just on the stack machine stack that aren't necessarily locals
- Read more about how LLVM uses the shadow stack
  - Maintain invariant that everything that is heap allocated on our stack is on our shadow stack too,
    and make stack point to shadow stack. Then use the same copying collector we use currently
  - We don't have to worry about "RSP" and "RBP": we can surgically skip, or compare against stack top