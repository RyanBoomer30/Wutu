- [ ] Figure out how to serve `.wasm` files
  1. We can write our `.wat` on the server, invoke `wat2wasm`, and then read the resulting `.wasm` file
  2. We could try to parse into the `Wasm` ast, then turn that into bytes
  3. We could go in two steps: write `.wasm` to the server, and return. THEN, fetch `.wasm` directly
  - Depending on what we choose, we will have to update `.server` accordingly,
    (probably fix the deprecated stuff too), and maybe extra `compile` handlers
    - Remember, don't mix Core with the standard library stuff

## Think
- Read more about how LLVM uses the shadow stack
  - Maintain invariant that everything that is heap allocated on our stack is on our shadow stack too,
    and make stack point to shadow stack. Then use the same copying collector we use currently
  - We don't have to worry about "RSP" and "RBP": we can surgically skip, or compare against stack top
- Tail calls might be annoying with the shadow stack, but at least we have control over the calls
  - It should be a matter of slipping the relevant bookkeeping in where we can effectively