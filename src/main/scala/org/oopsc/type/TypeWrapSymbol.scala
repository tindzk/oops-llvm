package org.oopsc.symbol

import org.oopsc._
import scala.collection.mutable.ListBuffer
import org.oopsc.expression.{BinaryExpression}
import org.llvm.{TypeRef, Value}
import scala.Some

/** Wraps any type, extending it with functionality such as operands or operators. */
class TypeWrapSymbol(identifier: Identifier) extends ScopedType {
  var coveringType : ResolvableType = null

  override def ident() = identifier
  override def mustEmbed() = true
  override def isComposite() = this.coveringType.declaration.get.isComposite()

  /* TODO Merge `OperatorSymbol' and `MethodSymbol' (-> FunctionSymbol). */
  var operators = new ListBuffer[OperatorSymbol]
  var operations = new ListBuffer[MethodSymbol]

  def this(ident: Identifier, coveringType: Type) {
    this(ident)
    this.coveringType = new ResolvableType(ident, Some(coveringType))
  }

  def this(ident: Identifier, coveringType: ResolvableType) {
    this(ident)
    this.coveringType = coveringType
  }

  override def getLLVMType(): TypeRef =
    this.coveringType.declaration.get.getLLVMType()

  override def llvmDeclPass(code: CodeContext) {
    this.operations.foreach(_.llvmDeclPass(code))
  }

  override def defPass(sem: SemanticAnalysis) {
    sem.defineSymbol(this)
    sem.enter(this)
    this.operations.foreach(_.defPass(sem))
    sem.leave
  }

  override def refPass(sem: SemanticAnalysis) {
    sem.enter(this)
    this.operations.foreach(_.refPass(sem))
    sem.leave
  }

  override def generateCode(code: CodeContext) {
    this.operations.foreach(_.generateCode(code))
  }

  override def op(op: BinaryExpression.Operator) =
    this.operators.find(_.op == op)

  override def instantiate(code: CodeContext, arguments: Value*) = {
    /* TODO Figure out when it is sufficient to allocate the object on the stack. */
    val inst = code.builder.buildMalloc(this.getLLVMType(), this.name())

    if (arguments.length > 0) {
      this.assign(code, inst, arguments(0))
    }

    inst
  }

  def print(tree: TreeStream) {
    // TODO
  }
}