package org.oopsc.expression

import org.oopsc._
import org.oopsc.symbol.{ClassType, ResolvableType}
import org.oopsc.{CodeContext, TreeStream, SemanticAnalysis}
import org.llvm.binding.LLVMLibrary.LLVMIntPredicate
import org.llvm.{Value, TypeRef}

class TypeCheckExpression(var oper: Expression, className: ResolvableType) extends Expression(oper.position) {
  override def refPass(sem: SemanticAnalysis) {
    this.oper.refPass(sem)
    this.className.declaration = Some(sem.currentScope.get.resolveClass(sem, className.identifier))
  }

  override def optimPass(): Expression = {
    this.oper = this.oper.optimPass()
    this
  }

  override def resolvedType() =
    Types.boolType

  def print(tree: TreeStream) {
    tree.println("ISA " + this.className.identifier.name)
    tree.indent
    this.oper.print(tree)
    tree.unindent
  }

  override def generateCode(code: CodeContext, deref: Boolean) : Value = {
    val result = code.builder.buildAlloca(TypeRef.int1Type(), "typeCheck.result")

    val value = this.oper.generateCode(code, true)

    if (value.isNull) {
      /* Any check against NULL must return `false' as NULL is not a
       * valid object and is not an instance of any class.
       */
      return TypeRef.int1Type().constInt(0, false)
    }

    val expected = this.className.declaration.get.asInstanceOf[ClassType]
    val expectedVmt = code.builder.buildBitCast(expected.getVirtualMethodTable(code), TypeRef.int64Type().pointerType().pointerType(), "cast")

    /* Dereference the object to get the VMT (offset 0 of the object). */
    val refVMT = LLVMHelpers.getPointer(code.builder, value, 0, 0)
    val vmtValue = code.builder.buildLoad(refVMT, "vtable")
    val vmt = code.builder.buildAlloca(TypeRef.int64Type().pointerType().pointerType(), "vmt")
    code.builder.buildStore(vmtValue, vmt)

    val end = code.currentFunction.appendBasicBlock("typeCheck.end")
    val cont = code.currentFunction.appendBasicBlock("typeCheck.cont")
    val cond = code.currentFunction.appendBasicBlock("typeCheck.cond")
    val isTrue = code.currentFunction.appendBasicBlock("typeCheck.isTrue")
    val isFalse = code.currentFunction.appendBasicBlock("typeCheck.isFalse")

    code.builder.buildBr(cond)

    code.builder.positionBuilderAtEnd(cond)
    val derefVMT = code.builder.buildLoad(vmt, "derefVMT")
    val isEq = code.builder.buildICmp(LLVMIntPredicate.LLVMIntEQ, derefVMT, expectedVmt, "isEq")
    code.builder.buildCondBr(isEq, isTrue, isFalse)

    /* VMT matches. */
    code.builder.positionBuilderAtEnd(isTrue)
    code.builder.buildStore(TypeRef.int1Type().constInt(1, false), result)
    code.builder.buildBr(end)

    /* VMT does not match. Load VMT of the super class (offset 0 in VMT). */
    code.builder.positionBuilderAtEnd(isFalse)
    val refVTableEntry = LLVMHelpers.getPointer(code.builder, derefVMT, 0)
    val superVTable = code.builder.buildLoad(refVTableEntry, "superVTable")

    /* Stop if VMT does not have any parent. */
    code.builder.buildCondBr(code.builder.buildIsNull(superVTable, ""), end, cont)

    code.builder.positionBuilderAtEnd(cont)
    val superVTableCast = code.builder.buildBitCast(superVTable, TypeRef.int64Type().pointerType().pointerType(), "superVTableCast")
    code.builder.buildStore(superVTableCast, vmt)
    code.builder.buildBr(cond)

    code.builder.positionBuilderAtEnd(end)

    if (deref) {
      code.builder.buildLoad(result, "")
    } else {
      result
    }
  }
}