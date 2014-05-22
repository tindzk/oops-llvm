package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression.Expression

/**
 * Statement for triggering an exception.
 */
class ThrowStatement(var value: Expression, var position: Position = new Position()) extends Statement {
  override def returns() = true

  override def refPass(sem: SemanticAnalysis) {

  }

  override def optimPass() : Statement = {
    this
  }

  override def print(tree: TreeStream) {
    tree.println("THROW")
    tree.indent
    this.value.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext) {

  }
}