package org.oopsc.symbol

import org.oopsc._
import org.llvm.Builder
import org.llvm.Value
import org.oopsc.expression.BinaryExpression.Operator

/* TODO This class must implement its methods properly. For now it's only meant for internal usage. */
/* `eval' has the following parameters: Builder, left value and right value. */
class OperatorSymbol(var op: Operator, var resultType: Type, var eval: (Builder, Value, Value) => Value) extends ScopedType {
  override def ident() = new Identifier(op.toString)
  override def getLLVMType() = null
  override def instantiate(code: CodeContext, arguments: Value*) = null

  override def defPass(sem: SemanticAnalysis) {

  }

  override def refPass(sem: SemanticAnalysis) {

  }

  def print(tree: TreeStream) {

  }
}
