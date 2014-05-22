package org.oopsc.symbol

import org.oopsc._
import org.llvm.Value

class FieldSymbol(ident: Identifier) extends VariableSymbol(ident) {
  def this(ident: Identifier, typeSymbol: Type) {
    this(ident)
    this.resolvedType = Some(typeSymbol)
    this.typeIdent = typeSymbol.ident()
  }

  def this(ident: Identifier, typeIdent: Identifier) {
    this(ident)
    this.typeIdent = typeIdent
  }

  /**
   * TODO Move
   * Position of the variable in the stack frame, or the position of the attribute
   * in the object. Set during the contextual analysis.
   */
  var offset = 0

  override def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) = {
    val ctx = this.declaringContext.get

    if (ctx.mustEmbed() && !ctx.isComposite()) {
      if (deref) {
        val load = code.builder.buildLoad(context, "")
        load
      } else {
        context
      }
    } else {
      if (deref) {
        val load = code.builder.buildLoad(context, "")
        code.builder.buildExtractValue(load, this.offset, "")
      } else {
        LLVMHelpers.getPointer(code.builder, context, 0, this.offset)
      }
    }
  }

  override def print(tree: TreeStream) {
    tree.println(s"${this.accessLevel} ${this.name()} (${this.offset}): " +
      this.resolvedType.map(_.name()).getOrElse("<unresolved>"))
  }
}