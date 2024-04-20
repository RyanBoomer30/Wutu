## Baby steps
- [x] Get a JS harness working that can instantiate and call a Wasm module from a file
- [ ] Experiment with interoperability, calling JS from Wutu and vice-versa
  - Specifically, play around with BigInt64Array
  - Try calling with indirect pointer into function table from both Wasm and JS
    - Use this to guide extern markings. Can we export the whole table?
- [ ] Experiment with memory stores and loads, referencing [this](https://rsms.me/wasm-intro)
  - Think hard about bits vs bytes vs words: I think things should actually work nicely?
- [ ] Experiment with handling traps
  - We need to be able to raise our own errors, as well as handle Wasm errors (like overflow)
    - We can call some `error()` function in JS, but how would that halt executions? Check options
  - We can't halt, but what happens if we just throw? What even happens?
- Think about how we would compile each operation, etc: experiment as you go!
  - Focus on which things get kept on the stack / popped off, and how we can save/dup if needed

- [ ] Start getting playground stuff working
  - [x] Web server working!
  - [x] Start with a simple function from `string` to `.wasm` file, and send that back
  - [ ] Then, `string` to EITHER `.wasm` or `error`
  - [ ] Actually use the compiler + pretty-printer + converter
    - [ ] Do we need to change any of the deprecated stuff? Hopefully no

- [ ] Make our own AST + pretty-printer
  - [ ] Pipe through `wat2wasm` before we run, which is annoying but oh well
        (might actually fit in our current infrastructure easily)
  - We should experiment first and sketch out cases to see what we will have to include,
    or maybe just start with the easy stuff and go from there? Probably better to sketch
  - [ ] Can we use `[@@deriving sexp]` to make the pretty-printer easy? IDK how it works...
    - Look at `.wat` format resources to see what is actually needed and go from there
    - Speaking of `.wat` format details, look at 64-bit literals and reserved symbols

- Decide whether ANF is needed -> start to change compilation
  - Probably some accumulated list of functions that we can add to when we see lambdas,
    as well as some list of instructions to stick in the current function body

- [ ] We should be able to adapt current test runners, using `node` as appropriate
  - Turn to this once we have working tiny compiler
  - Think how test will be able to run files with either...
    maybe a new flag? Probably needs new command

## Think
- We need to figure out how to make tests ergonomic
- We may not even need to ANF, based on how the stack machine works
  - Shunting yard algorithm could be helpful?
  - Passive think about whether this would break garbage collection with shadow stack
    - Especially with things that are just on the stack machine stack that aren't necessarily locals

- Read more about how LLVM uses the shadow stack
  - Maintain invariant that everything that is heap allocated on our stack is on our shadow stack too,
    and make stack point to shadow stack. Then use the same copying collector we use currently
  - We don't have to worry about "RSP" and "RBP": we can surgically skip, or compare against stack top
- Tail calls might be annoying with the shadow stack, but at least we have control over the calls
  - It should be a matter of slipping the relevant bookkeeping in where we can effectively