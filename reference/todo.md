- [ ] Remove duplication in pipeline (parameterized by input functions and by the backend)
  - [ ] Clean up `no_js_builtins`
- [ ] Implement file test: add arg to `input_file_test_suite`

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