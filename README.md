An implementation of the compiler from Andrew W. Appel's Modern Compiler Implementation in ML
https://www.cs.princeton.edu/~appel/modern/ml/

Builds a 32-bit compiler, so need to have gcc-multilib installed, as well as a recent version of OCaml.

To build `make` should be enough, and then to compile a program:

`./main.native < program_source` or type it directly into stdin

`./test_all.sh` runs some tests

