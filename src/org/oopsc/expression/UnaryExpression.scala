package org.oopsc.expression

import org.oopsc._
import org.oopsc.symbol._
import org.oopsc.symbol.ClassType
import org.llvm.binding.LLVMLibrary.LLVMIntPredicate
import org.llvm._

object UnaryExpression extends Enumeration {
  type Operator = Value
  val MINUS, NOT = Value
}

case class UnaryExpression(var operator: UnaryExpression.Operator, var operand: Expression, _position: Position) extends Expression(_position) {
  import UnaryExpression._

  override def refPass(sem: SemanticAnalysis) {
    this.operand.refPass(sem)

    this.operator match {
      case NOT =>
        this.operand.resolvedType().check(sem, sem.types.boolWrapper, this.operand.position)

      case MINUS =>
        this.operand.resolvedType().check(sem, sem.types.intWrapper, this.operand.position)
    }
  }

  override def optimPass() : Expression = {
    this.operand = this.operand.optimPass()
    this.operand match {
      case o: BooleanLiteralExpression =>
        this.operator match {
          case NOT =>
            val value = !o.value
            BooleanLiteralExpression(value, this.position)
        }

      case o: IntegerLiteralExpression =>
        this.operator match {
          case MINUS =>
            val value = -o.value
            IntegerLiteralExpression(value, this.position)
        }

      case o: UnaryExpression =>
        if (o.operator == this.operator == MINUS) {
          /* -(-x) → x */
          o.operand
        } else if (o.operator == this.operator == NOT) {
          /* NOT (NOT x) → x */
          o.operand
        } else {
          this
        }

      case _ => this
    }
  }

  override def resolvedType() : Type =
    this.operand.resolvedType()

  def print(tree: TreeStream) {
    tree.println(this.operator.toString + " : " + this.resolvedType().name)
    tree.indent
    this.operand.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext, deref: Boolean) = {
    val value = this.operator match {
      case NOT =>
        code.builder.buildICmp(LLVMIntPredicate.LLVMIntEQ, this.operand.generateCode(code, true), TypeRef.int1Type().constNull(), "")
      case MINUS =>
        code.builder.buildSub(TypeRef.int1Type().constNull(), this.operand.generateCode(code, true), "")
    }

    if (deref) {
      value
    } else {
      val prom = this.resolvedType().promote(code.sem)
      prom.instantiate(code, value)
    }
  }
}