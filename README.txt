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
