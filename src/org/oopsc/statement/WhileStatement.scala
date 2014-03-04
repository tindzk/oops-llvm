package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression.{BooleanLiteralExpression, Expression}
import scala.collection.mutable.ListBuffer
import org.llvm.binding.LLVMLibrary.LLVMIntPredicate
import org.llvm.TypeRef
import org.oopsc.symbol.Block

class WhileStatement(var condition: Expression, var statements: Block) extends Statement {
  override def refPass(sem: SemanticAnalysis) {
    this.condition.refPass(sem)
    this.condition.resolvedType.check(sem, sem.types.boolWrapper, this.condition.position)
    this.statements.refPass(sem)
  }

  override def optimPass() : Statement = {
    this.condition = this.condition.optimPass()

    this.condition match {
      case BooleanLiteralExpression(false, _) =>
        /* If the condition evaluates to false, return a NullStatement. */
        return new NullStatement

      case _ =>
    }

    this.statements = this.statements.optimPass()
    this
  }

  override def print(tree: TreeStream) {
    tree.println("WHILE")
    tree.indent
    this.condition.print(tree)

    tree.println("DO")
    this.statements.print(tree)

    tree.unindent
  }

  override def generateCode(code: CodeContext) {
    val end = code.currentFunction.appendBasicBlock("while.end")
    val cond = code.currentFunction.appendBasicBlock("while.cond")
    val body = code.currentFunction.appendBasicBlock("while.body")

    code.builder.buildBr(cond)

    code.builder.positionBuilderAtEnd(cond)
    code.builder.buildCondBr(condition.generateCode(code, true), body, end)

    code.builder.positionBuilderAtEnd(body)
    this.statements.generateCode(code, cond)

    code.builder.positionBuilderAtEnd(end)
  }
}