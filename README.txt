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

License
-------
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
