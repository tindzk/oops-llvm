package org.oopsc.symbol

import org.llvm.{Value, TypeRef}
import org.oopsc.{TreeStream, Identifier, CodeContext}

class PrimitiveType(identifier: String, llvmType : TypeRef) extends Type {
  override def ident() = new Identifier(identifier)
  override def mustEmbed() = true
  override def getLLVMType() = llvmType

  def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) = {
    if (deref) {
      val load = code.builder.buildLoad(context, "")
      load
    } else {
      context
    }
  }

  def print(tree: TreeStream) {
    // TODO
  }
}
