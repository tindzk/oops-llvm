package org.oopsc.expression

import org.oopsc._
import org.oopsc.symbol._
import scala.Some

/**
 * Class instantiation.
 */
class NewExpression(var newType: ResolvableType) extends Expression(newType.identifier.position) {
  override def refPass(sem: SemanticAnalysis) {
    this.newType.declaration = Some(sem.currentScope.get.resolveClass(sem, this.newType.identifier))
  }

  override def resolvedType() = this.newType.declaration.get

  def print(tree: TreeStream) {
    tree.println(s"NEW ${this.newType.identifier.name} : ${this.resolvedType().name}")
  }

  override def generateCode(code: CodeContext, deref: Boolean) =
    this.resolvedType().asInstanceOf[ClassType].instantiate(code)
}