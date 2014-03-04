package org.oopsc.symbol

import org.oopsc.{SemanticAnalysis, TreeStream, CodeContext, Identifier}
import org.llvm.{TypeRef, Value}
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConverters._

class RecordType(identifier: Identifier, fields: FieldSymbol*) extends ScopedType {
  override def ident() = identifier
  override def isComposite() = true

  /** Attributes declared by this record. */
  var attributes : ListBuffer[FieldSymbol] = ListBuffer.concat(fields)
  attributes.foreach(_.declaringContext = Some(this))

  private var llvmType: TypeRef = null

  override def getLLVMType(): TypeRef = {
    if (llvmType == null) {
      llvmType = TypeRef.structTypeNamed(this.name())
      TypeRef.structSetBody(llvmType, this.attributes.map(_.getLLVMType()).toList.asJava, false)
    }

    this.llvmType
  }

  override def refPass(sem: SemanticAnalysis) {
    sem.enter(this)

    /* Resolve attribute types and assign indices. */
    for ((a, i) <- this.attributes.zipWithIndex) {
      a.refPass(sem)
      a.offset = i
    }

    sem.leave()
  }

  def attr(name: String) =
    this.attributes.find(_.name() == name).get

  def getLLVMValue()(context: Value, code: CodeContext, deref: Boolean) = {
    if (deref) {
      val load = code.builder.buildLoad(context, "")
      load
    } else {
      context
    }
  }

  override def instantiate(code: CodeContext, arguments: Value*) : Value = {
    /* TODO Figure out when it is sufficient to allocate the object on the stack. */
    val inst = code.builder.buildMalloc(this.getLLVMType(), this.name())

    if (arguments.length > 0) {
      /* TODO For more parameters, don't use assign but call constructor. */
      this.assign(code, inst, arguments(0))
    }

    inst
  }

  def print(tree: TreeStream) {
    // TODO
  }
}
