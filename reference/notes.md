## Relevant notes
- Indices into the linear memory / function table must be 32-bit integers
  - We probably want SNAKEVALs to stay 64-bit (always), so we should extend and truncate as appropriate
  - We could use the upper bits to store some information (think: tags), but this hopefully isn't needed
- Memory can be accessed with 8, 16, 32, or 64 bit wide loads and stores
- We will need to put all of our functions into one global function table (do lambda lifting)
    - We then use indirect_call with an index into that table
    - Check out the Elm closure post for more ideas, but we probably will just stick with our current closure
      conversion: pass in the closure itself as the first argument and unpack things first into locals
- A function internally has zero or more local variables, which are named mutable slots that live for the duration of the function call
    - Parameters are just locals that get initialized
- Probably need some global R15-like variable to keep track of the top of our linear memory


## Requirements
Sandbox page: enter code and have it compile
Interoperability is particularly interesting:
- Call standard runtime functions like `print`, `input`, `alert`, etc
- We should have support for JS calling our Wutu functions in interesting ways
  - First order: `addEventListener` and get a button that calls Wutu function on press
  - Next order: `bigBang` that takes a starting SNAKEVAL, an on-tick function, and a
    `SNAKEVAL -> SNAKEVAL` render function (print is called on this repeatedly)
  - Fully capable: arbitrary interoperability between the two
- Get things working without GC / shadow stack, then try to add in if we can
  - Keep this in the back of your mind when making design decisions, however


## One day
- Get Wutu resume ready!
- Publish Wutu playground
- Include example programs (keep the sample `.wat` on the web) to serve as documentation
    - Ex: look at Pyret website examples maybe?
- Talk about class, include compiler pipeline information and other technical details
  - This could also be more like a blog post documenting decisions and ideas
- Syntax highlighting one day?

- Probably actually add in the `tailcall` flag, since Safari doesn't support tail calls
  - Maybe later we can pass that with the request (along with other flags)
  - Some of the current flags (like allocation strategy) are not applicable with Wasm compiler