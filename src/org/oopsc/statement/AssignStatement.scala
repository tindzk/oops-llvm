package org.oopsc.statement

import org.oopsc._
import org.oopsc.expression.{EvaluateExpression, Expression}

class AssignStatement(var leftOperand: Expression, var rightOperand: Expression) extends Statement {
  override def refPass(sem: SemanticAnalysis) {
    this.leftOperand.refPass(sem)
    this.rightOperand.refPass(sem)

    if (!this.leftOperand.isVariable()) {
      throw new CompileException("Variable reference expected.", this.leftOperand.position)
    } else {
      this.leftOperand match {
        case left: EvaluateExpression =>
          if (left.ref.declaration.get.name() == "BASE" ||
              left.ref.declaration.get.name() == "SELF")
          {
            throw new CompileException("Cannot assign to BASE or SELF.", this.leftOperand.position)
          }

        case _ =>
      }
    }

    this.rightOperand.resolvedType.check(sem, this.leftOperand.resolvedType, this.rightOperand.position)
  }

  override def optimPass(): Statement = {
    this.leftOperand = this.leftOperand.optimPass()
    this.rightOperand = this.rightOperand.optimPass()
    this
  }

  override def print(tree: TreeStream) {
    tree.println("ASSIGN")
    tree.indent
    this.leftOperand.print(tree)
    this.rightOperand.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext) {
    val left = this.leftOperand.resolvedType()

    val leftValue = this.leftOperand.generateCode(code, false)
    val rightValue = this.rightOperand.generateCode(code, true)

    val castRightValue =
      if (!rightOperand.resolvedType().mustEmbed() && !(this.leftOperand.resolvedType() eq this.rightOperand.resolvedType())) {
        /* Down-cast the right object if necessary. */
        code.builder.buildBitCast(rightValue, left.getLLVMType().pointerType(), "")
      } else {
        rightValue
      }

    left.assign(code, leftValue, castRightValue)
  }
}