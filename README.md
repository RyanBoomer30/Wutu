# Wutu
Compiler with A-Normal Form IR for non-trivial expression code generation targetting x64 assembly and WASM. All compiled time is written in Ocaml and C/Javascript for runtime.

# Features
The language supports:
- Basic algebraic operations up to 62-bit signed integers
- Tail call
- Sequences, Tuples
- Lambda
- Function Recursion
- Shadow
- Register Allocation
- Printing

Platform-specific features:
| Features | X86  | WASM |
| ---------| ------------- | ------------- |
| Garbage Collection  |  ✅  | ❌  |
| Trigger   |  ❌  | ✅  |
| Alert   |  ❌  | ✅  |
| BigBang   |  ❌  | ✅  |

# Installation
First, you would want to install the `opam` library for your library. Then, use the following commands to install the required libraries (We suggest that you create an environment for this)
```
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
