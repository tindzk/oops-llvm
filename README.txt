OOPS-LLVM
=========
Compiler for the OOPS language using LLVM, written in Scala.

Requirements
------------
LLVM 3.4 must be installed on the target machine.

Installation
------------
- Compile the project with Maven:
  mvn clean dependency:copy-dependencies compile
- To execute the test case for if-statements, run:
  ./oopsc.sh tests/if.oops

Usage
-----
Passing only the path pointing to a source file compiles and executes it
directly using LLVM's JIT. If you want to have a look at the generated LLVM
bytecode instead, run the following commands:

$ ./oopsc.sh --norun tests/strings.oops strings.bc
$ llvm-dis strings.bc

A readable version of the generated LLVM bytecode will be created in
``strings.ll``.

See ``./oopsc.sh --help`` for a list of all supported parameters.