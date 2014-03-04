package org.oopsc.symbol

import org.oopsc.{Identifier, TreeStream, CodeContext}
import org.llvm.Value

class ParameterSymbol(ident: Identifier, typeIdent: Identifier) extends VariableSymbol(ident, typeIdent) {
  /* Set during the contextual analysis. */
  var id = 0

  override def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) = {
    if (deref) {
      code.currentFunction.getParam(this.id)
    } else {
      /* TODO Store in instance table, i.e. code.instances. */
      val llvmValue = code.builder.buildAlloca(
        this.getResolvedType.getLLVMType(), this.name())
      code.builder.buildStore(code.currentFunction.getParam(this.id), llvmValue)
      llvmValue
    }
  }

  override def print(tree: TreeStream) {
    tree.println(s"${this.accessLevel} ${this.name()} (${this.id}): " +
      this.resolvedType.map(_.name()).getOrElse("<unresolved>"))
  }
}
