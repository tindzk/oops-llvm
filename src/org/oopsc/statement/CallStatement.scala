package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression.Expression

class CallStatement(var call: Expression) extends Statement {
  override def refPass(sem: SemanticAnalysis) {
    this.call.refPass(sem)
  }

  override def print(tree: TreeStream) {
    tree.println("CALL")
    tree.indent
    this.call.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext) {
    this.call.generateCode(code, true)
  }
}