package org.oopsc.symbol

import org.oopsc.{CodeContext, Identifier}
import org.llvm.Value

class SelfVariableSymbol(classSymbol: ScopedType) extends VariableSymbol(new Identifier("SELF"), classSymbol) {
  this.accessLevel = AccessLevel.Private
  this.declaringContext = Some(classSymbol)

  override def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) =
    code.currentFunction.getFirstParam
}
