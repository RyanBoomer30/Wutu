## Learn Wasm
[Understanding WAT](https://developer.mozilla.org/en-US/docs/WebAssembly/Understanding_the_text_format)
- Read this first, if you are a beginner
- [WAT -> Wasm](https://developer.mozilla.org/en-US/docs/WebAssembly/Text_format_to_wasm)

Will Crichton on Wasm (Stanford CS 242):
- Read this next
- [Mutability / intro](https://stanford-cs242.github.io/f19/lectures/05-1-mutability)
- [Control flow](https://stanford-cs242.github.io/f19/lectures/05-2-control-flow)

[Spec paper](https://dl.acm.org/doi/pdf/10.1145/3062341.3062363)
- Read this maybe never
- The spec online is detailed, but not the most useful...
  - The page on [memory instructions](https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions)
    is likely going to be useful as we figure load stuff out

[Another useful tutorial](https://rsms.me/wasm-intro#addressing-memory)
- This one is especially useful for figuring out memory addressing stuff

Specific Wasm behavior:
- [JS API for exported functions](https://webassembly.github.io/spec/js-api/#exported-function-exotic-objects)
  - `ToWebAssemblyValue` which coerces JS to Wasm value, which may be useful for rich interoperability!
- [BigInt](https://github.com/WebAssembly/JS-BigInt-integration)
  - [JS Documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt)
  - BigInts can be read or written from WebAssembly memory using `BigInt64Array` or `BigUint64Array`
- Traps throw [RuntimeErrors](https://developer.mozilla.org/en-US/docs/WebAssembly/JavaScript_interface/RuntimeError)
  - This also links to JS interface and API at the bottom


## Compile to Wasm
Learning lessons from Elm:
- [Overview](https://dev.to/briancarroll/porting-elm-to-webassembly-2lp4) (talks about runtime and GC a tiny bit)
- [Closure compilation](https://dev.to/briancarroll/elm-functions-in-webassembly-50ak)
- [Byte-level representations](https://dev.to/briancarroll/elm-in-wasm-built-in-typeclasses-2kcb)

[JS API](https://developer.mozilla.org/en-US/docs/WebAssembly/Using_the_JavaScript_API): this will be necessary to call between both!

[V8 on Tail Calls](https://v8.dev/blog/wasm-tail-call)
- Tail calls... might exist, depending on the browser
- [Proposal](https://github.com/WebAssembly/tail-call/blob/master/proposals/tail-call/Overview.md)

[GC Proposal Overview](https://github.com/WebAssembly/gc/blob/main/proposals/gc/Overview.md)
- Contains some potential ways for compiling closures, structs
- [V8 on GC Porting](https://v8.dev/blog/wasm-gc-porting)
- [LLVM uses shadow stack](https://hackmd.io/@pepyakin/SkmPKGhiq)

Existing materials:
- [Wasm spec repo](https://github.com/WebAssembly/spec/tree/main/interpreter)
    - Interpreter written in OCaml, including an AST (that we could consider using) that can be converted to `.wat` or `.wasm` directly
    - This looks to have some testing info, but I don't think it's directly applicable
      (although we could try to use their interpreter for our tests, but that feels weird)
- [Wasm_of_ocaml](https://github.com/ocaml-wasm/wasm_of_ocaml/tree/main)