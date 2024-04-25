Contributions:
- Designed a new AST for (a portion of) WebAssembly, which was our compilation target
  - Had to write a pretty printer, tried to make AST relatively extensible while not having
    a ton of overhead (which pre-existing options all had)
- Completely rewrote the codegen pass, targeting this AST
  - Required us to do lambda lifting and a little extra book-keeping
- Completely rewrote the runtime in JavaScript
  - New features: JS can call back into compiled Wasm, not just Wasm calling into JS
  - Ex: button click -> triggers Wasm function, `bigBang`, etc
- Wrote a web playground and server
- Test runner and infrastructure
  - Adapted pre-existing test infrastructure to work for Wasm:
  - Added command-line flag to `main`
  - Updated Makefile and `runner` execution functions
  - Generally refactored compiler and test suite, some `dune` updates too

Design decisions:
- Still using ANF
  - Order of arguments to `indirect_call` instruction has the function table index come last.
    However, our evaluation order has function being evaluated first
    We would like the function to be an immediate... if only we had a way to do that (AKA ANF!)
  - We could try to manually ANF in just these cases (which probably would work) but that feels
    like reinventing the wheel. Using our ANF code also lets re-use `naive_stack_allocation`
  - Generally makes compilation more convenient, and probably would be nice for GC / shadow stack
- Lambda lifting + bookkeeping
  - We don't have lambda lifting as its own phase, because we would have to make some new
    intermediate data structure for the lifted functions (since `wfunc` stores `winstrs` but also
    has extra information from the `CLambda` itself like arity)
    - We also wouldn't just be able to map `compile` over the list of function bodies, since we
      still need to unpack closures (maybe could refactor some helpers but not worth it)
  - We thread `wfuns_so_far` through, since we need to know an offset into the function table
    This is essentially accumulating the function table as we go: can just get length of list
    - We store function table offset `* 2`, for GC purposes (similar to how we `align` in `asm`)
  - We could have also done a sort / normalize solution, but it would have relied on the fact
    that our `envt`s are lists that can be sorted. Accumulating turned out to be cleanes
- Runtime operations: overflow
  - Since Wasm doesn't support overflow, we delegate to the runtime for addition, subtraction,
    and multiplication (since we can check for overflow there)
    - We also check tags in the runtime here, since it's just more convenient
- For now, we just set a `MAX_TABLE` size in our runtime. There are other options as well, but
  this was the simplest and doesn't change how things work too much
  - When compiling, go in two steps: first, send over file and server returns function table
    size for the runtime to use
  - Alternatively, start with table size 0 and dynamically populate it. This is slower and 
    makes resulting `wasm` binary harder to read / reason about
- Tag checking now uses Wasm functions
  - Wasm functions are very easy to call, compared to functions in x86_64, so we abstracted
    our tag-checking code and called functions instead of inlining all of it
  - It is unclear which is faster, since JS engines do crazy things. We have not benchmarked
  - This would be easy to go either way, but we wanted to try out a new / different option
- Locals:
  - We don't name things and instead we use indices (AKA same logic from `naive_stack_allocation`)
  - Our old calling convention very closely aligned with how locals / function parameters work
    in Wasm (first arguments, then locals (which we can turn into closure -> locals)), which meant
    we did not have to change anything at all!

GC:
- GC is _not_ currently implemented, but we have thought lots about it and made designed
  decisions with the goal of making it easier to implement in the future
  - Ex: storing function table pointers `* 2`, keeping ANF can help, etc
- We would use a shadow stack, since we cannot walk the Wasm stack:
  - Instead of tagged pointers to the linear memory, we reserve a section of the linear
    memory to serve as the "stack" (1 MB is what LLVM uses) and store pointers into there
  - The shadow stack itself has pointers into the rest of the linear memory ("heap" almost)
  - This lets us walk the stack and update pointers: extra layer of indirection!
- Potential roadblocks:
  - Tricky to test, since we grow our memory in page increments (vs the word level, which
    let us write fine-tuned test)
    - Could try to remedy with more test helpers, but these would be more intricate
  - Tail calls could be tricky, since we would have to basically redo our stack frame logic
    in the shadow stack (which was hard to get right the first time)
    - Alternatively, we could try to over-approximate and not replace the shadow stack frames,
      but this runs into shadow stack overflow (the whole thing we are trying to solve with TCE)
  - Generally takes lots of careful book-keeping (sneaking it into the right places, to make
    sure that tail calls stay tail calls, etc) and tracking tags carefully
  - If we extended with GC, we would have to be careful not to trigger a GC
    while tagged pointers to the allocated but not yet populated closures are
    on our stack. They temporary point to part of the linear memory that are filled with junks