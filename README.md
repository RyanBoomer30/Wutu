# Wutu
Compiler for a Python-inspired functional language with A-Normal Form IR, targeting both x86_64 and WebAssembly. Uses OCaml for compiler passes, with C/JavaScript for runtime support.

# Features
The language supports:
- Algebraic operations for 63-bit signed integers
- Anonymous, first-class, mutually-recursive functions
- Arbitrary tail-call elimination
- Mutable tuples with destructuring
- Compile-time scope/shadowing analysis
- Runtime type (tag) checking
- I/O primitives

Platform-specific features:
| Features | x86_64  | Wasm |
| ---------| ------------- | ------------- |
| Platform Interoperability  |  ✅  |  ✅  |
| Register Allocation  |  ✅  |  ❌  |
| Garbage Collection  |  ✅  |  ❌  |
| DOM Integration  |  ❌  |  ✅  |
| Interactive Textual Programs   |  ❌  |  ✅  |

# Installation
First, you would want to install the `opam` library for your library. Then, use the following commands to install the required libraries (We suggest that you create an environment for this)
```
sudo apt install opam
opam init
opam switch create 4.14.1
opam install extlib ounit batteries
```

Next, you need to install Clang and NodeJS
```
sudo apt install clang=15.0.0
sudo apt install nodejs=18.12.1
```

Now, clone this repository and use Dune to build the project
```
git clone https://github.com/RyanBoomer30/Wutu.git
cd Wutu
make main
make test
dune build
```

To make sure that the compiler is correctly installed, run the test to validate (all tests should pass)
```
./test
```
