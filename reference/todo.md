- [x] `.wasm` test helpers
  - [x] Figure out how to run `node` with the relevant `.wasm` file
  - [x] Make new `runner` helpers, maybe make `wasmrunner` or something
  - [x] Maybe update `Makefile` as well to make the proper `.wasm` files
  - [ ] Implement file test: add arg to `input_file_test_suite`
- [x] Check out how to pass in arguments to `node` test harness,
      look at `runner` to see how they do it

- [x] Update naive_stack_allocation to first return indices, then map them to slots
- [ ] Code generation (AKA the hard part...): test as you go!
  - [ ] Lambda
  - [ ] LetRec
  - [ ] EApp
- [ ] Runtime functions
  - [ ] Printing tuples
  - [ ] Equal, input
  - [ ] Other interesting stuff
- [ ] FIGURE OUT: currently, `nil` doesn't work the way we want it to, since 0 is a valid
      slot in the linear memory. We can offset by 16, or the shadow stack will do that for us
      (although I am not optimistic about being able to get to GC...)
- [ ] GC
  - [ ] Interactions with tail calls

- [ ] Figure out how to serve `.wasm` files
  1. We can write our `.wat` on the server, invoke `wat2wasm`, and then read the resulting `.wasm` file
  2. We could try to parse into the `Wasm` ast, then turn that into bytes
  3. We could go in two steps: write `.wasm` to the server, and return. THEN, fetch `.wasm` directly
  - Depending on what we choose, we will have to update `.server` accordingly,
    (probably fix the deprecated stuff too), and maybe extra `compile` handlers
    - Remember, don't mix Core with the standard library stuff
- [ ] Make file rule for server? Phony exec? Actual build? There were some compilation troubles…


## Think
- Read more about how LLVM uses the shadow stack
  - Maintain invariant that everything that is heap allocated on our stack is on our shadow stack too,
    and make stack point to shadow stack. Then use the same copying collector we use currently
  - We don't have to worry about "RSP" and "RBP": we can surgically skip, or compare against stack top
- Tail calls might be annoying with the shadow stack, but at least we have control over the calls
  - It should be a matter of slipping the relevant bookkeeping in where we can effectively

Interop:
- For arbitrary interop, how would we be able to call with non-SNAKEVAL values?
  Ex: tuples, etc. Try to get that working later
  - Could also get extern keyword or something.
    Play around and see what cool things we can do!
- We can turn SNAKE closures and tuples into JS lambdas and arrays.
  - Test out how these are printed, especially for cyclic data, maybe we can use some of it for print