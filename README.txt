OOPS-LLVM
=========
Compiler for the OOPS language using LLVM, written in Scala.

Requirements
------------
- Scala
- LLVM 3.4
- ANTLR4

Libraries
--------
- Scala libraries (http://www.scala-lang.org/)
- llvm-j (http://github.com/tindzk/llvm-j)
- bridj (https://code.google.com/p/bridj/)
- JUnit 4 (http://junit.org/)
- scalalogging (https://github.com/typesafehub/scalalogging)
- SLF4J (http://www.slf4j.org/)
- scallop (https://github.com/scallop/scallop)
- ANTLR4 (http://www.antlr.org/)

Alternatively, you can also check out the repository git@github.com:tindzk/oops-llvm-libs.git
to libs/. It will contain all required libraries OOPS-LLVM depends on.

Installation
------------
- Generate the grammar by executing ./grm.sh.
- Use ./build.py to obtain the package oopsc.jar. The build script
  requires the Scala libraries to be stored in /usr/share/scala/lib/.
- To execute the test case for if-statements, run:
  java -jar oopsc.jar tests/if.oops
