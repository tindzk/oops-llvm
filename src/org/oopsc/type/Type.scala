package org.oopsc.symbol

import org.oopsc._
import org.llvm._
import scala.Some
import org.oopsc.expression.BinaryExpression

object Type {
  /**
   * Throws an exception for a type mismatch, converting the type names into a string.
   *
   * @param expected Expected type.
   * @param position Position in the source code.
   */
  def typeError(expected: Type, given: Type, position: Position) {
    throw new CompileException(s"Type mismatch: ${expected.name()} expected, ${given.name()} given.", position)
  }
}

trait Type {
  var accessLevel = AccessLevel.Public
  var declaringContext: Option[ScopedType] = None

  def name() = ident().name
  def pos() = ident().position
  def ident(): Identifier
  def mustEmbed() = false
  def isComposite() = false
  def getLLVMType() : TypeRef
  def op(op: BinaryExpression.Operator): Option[OperatorSymbol] = None

  /* If the current type is a native type, then this method resolves
   * its wrapper class which supports instantiation on the heap.
   */
  def promote(sem: SemanticAnalysis) : ScopedType = {
    if (this eq Types.int32Type) {
      sem.types.intWrapper
    } else if (this eq Types.charType) {
      sem.types.charWrapper
    } else if (this eq Types.boolType) {
      sem.types.boolWrapper
    } else if (this eq Types.stringType) {
      sem.types.stringWrapper
    } else {
      this.asInstanceOf[ScopedType]
    }
  }

  /**
   * Checks the compatibility to the given type. Throws an exception upon type mismatch.
   *
   * @param expected Expected type.
   * @param position Position in the source code.
   */
  def check(sem: SemanticAnalysis, expected: Type, position: Position) {
    if (!this.isA(sem, expected)) {
      Type.typeError(expected, this, position)
    }
  }

  /**
   * Checks the compatibility to the given type.
   *
   * @param expected Expected type.
   */
  def isA(sem: SemanticAnalysis, expected: Type): Boolean = {
    val exp = expected.promote(sem)
    val cmp = this.promote(sem)

    val compatClasses = List((sem.types.charWrapper, Types.int32Type))
    if (compatClasses.contains((cmp, exp)) || compatClasses.contains((exp, cmp))) {
      return true
    }

    /* TODO
    while (cmp ne expected) {
      cmp.getSuperClass() match {
        case Some(c) => cmp = c
        case None => return false
      }
    }
    */

    /* TODO For now, all type compatibility checks are disabled. */
    true
  }

  /* Default behaviour may be overwritten internally by base types. */
  def assign(code: CodeContext, leftOperand: Value, rightOperand: Value) {
    code.builder.buildStore(rightOperand, leftOperand)
  }

  /**
   * Performs the definition pass of the semantic analysis.
   *
   * @param sem Context of the semantic analysis.
   */
  def defPass(sem: SemanticAnalysis) {

  }

  def llvmDeclPass(code: CodeContext) {

  }

  /**
   * Performs the reference pass of the semantic analysis.
   *
   * @param sem Context of the semantic analysis.
   */
  def refPass(sem: SemanticAnalysis) {

  }

  def optimPass() {

  }

  def generateCode(code: CodeContext) {

  }

  /**
   * Prints declaration in a tree structure.
   *
   * @param tree Output stream.
   */
  def print(tree: TreeStream)

  def availableFor(sem: SemanticAnalysis, clazz: Option[ScopedType]) = {
    this.accessLevel match {
      case AccessLevel.Public => true
      case AccessLevel.Private =>
        (clazz, this.declaringContext) match {
          case (Some(c), Some(c2)) => c eq c2
          case _ => false
        }
      case AccessLevel.Protected =>
        (clazz, this.declaringContext) match {
          /* isA() checks the class hierarchy (as opposed to a simple reference check with `eq'). */
          case (Some(c), Some(c2)) => c.isA(sem, c2)
          case _ => false
        }
    }
  }
}
