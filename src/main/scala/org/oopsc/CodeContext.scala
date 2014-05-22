package org.oopsc

import org.llvm._
import scala.collection.mutable.Stack

class CodeContext(jit: Boolean = false) {
  var module = Module.createWithName("module")

  var sem: SemanticAnalysis = null

  var isUnwindBlock = false
  var unwindBlocks = new Stack[BasicBlock]

  var engine: ExecutionEngine = null

  var retBlock: BasicBlock = null
  var retValue: Value = null

  if (jit) {
    Target.initialiseNativeTarget()
    ExecutionEngine.linkInJIT()

    engine = ExecutionEngine.createForModule(module)

    /* Requires that the native JIT target was initialised beforehand. */
    engine.createJITCompilerForModule(module, 2)
  }

  var builder: Builder = Builder.createBuilder()

  var currentFunction : Value = null

  def writeBitcode(path: String) {
    module.writeBitcodeToFile(path)
  }

  def dump() {
    module.dumpModule()
  }

  def verify() {
    module.verify()
  }

  def run() {
    if (engine == null) {
      throw new CompileException("JIT must be enabled during instantiation.")
    }

    val pass = PassManager.create()

    pass.addConstantPropagationPass()
    pass.addInstructionCombiningPass()
    pass.addPromoteMemoryToRegisterPass()
    pass.addGVNPass()
    pass.addCFGSimplificationPass()

    pass.runForModule(module)

    engine.addTargetData(pass)
    engine.runFunction(this.module.getNamedFunction("main"))
  }
}
