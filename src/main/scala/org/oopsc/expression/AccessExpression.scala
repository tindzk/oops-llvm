package org.oopsc.expression

import org.oopsc.{CodeContext, TreeStream, SemanticAnalysis}
import org.oopsc.symbol.ScopedType

/**
 * Represents a method or attribute access.
 */
class AccessExpression(var leftOperand: Expression, rightOperand: EvaluateExpression) extends Expression(leftOperand.position) {
  private var isBase = false

  override def refPass(sem: SemanticAnalysis) {
    this.leftOperand.refPass(sem)

    /* Deal with accesses to methods or attributes in the base class. */
    this.leftOperand match {
      case call: EvaluateExpression =>
        if (call.ref.identifier.name == "BASE") {
          this.isBase = true
        }

      case _ =>
    }

    this.rightOperand.setStaticAccess(this.isBase)

    /* The left operand denotes the scope of the right operand. */
    this.rightOperand.setScope(this.leftOperand.resolvedType().promote(sem))
    this.rightOperand.refPass(sem)
  }

  override def optimPass() : Expression = {
    this.leftOperand = this.leftOperand.optimPass()
    this
  }

  /* The type of this expression is always the type of the right operand. */
  override def resolvedType() =
    this.rightOperand.resolvedType()

  override def isVariable() =
    this.rightOperand.isVariable()

  def print(tree: TreeStream) {
    tree.println("ACCESS " + (if (this.isVariable()) "REF " else "") + ": " + this.resolvedType().name)
    tree.indent
    this.leftOperand.print(tree)
    this.rightOperand.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext, deref: Boolean) = {
    if (!this.isBase) {
      if (this.leftOperand.resolvedType().mustEmbed()) {
        /* Pointer to a primitive data type. */
        val context = this.leftOperand.generateCode(code, false)
        this.rightOperand.setAccessContext(context)
      } else {
        /* Ordinary objects. */
        val context = this.leftOperand.generateCode(code, true)
        this.rightOperand.setAccessContext(context)
      }
    }

    this.rightOperand.generateCode(code, deref)
  }
}