package org.oopsc.expression

import org.oopsc._
import org.oopsc.symbol._
import org.llvm._
import org.bridj.Pointer
import org.llvm.binding.LLVMLibrary.LLVMValueRef
import org.llvm.binding.LLVMLibrary

abstract class LiteralExpression(`type`: Type, position: Position = new Position()) extends Expression(position) {
  override def resolvedType() : Type = `type`
}

case class BooleanLiteralExpression(value: Boolean, var _position: Position = new Position()) extends LiteralExpression(Types.boolType, _position) {
  def print(tree: TreeStream) {
    tree.println(this.value)
  }

  override def generateCode(code: CodeContext, deref: Boolean) =
    if (deref) {
      TypeRef.int1Type().constInt(if (value) 1 else 0, false)
    } else {
      this.resolvedType().promote(code.sem).instantiate(code, this.generateCode(code, true))
    }
}

case class IntegerLiteralExpression(var value: Int, var _position: Position = new Position()) extends LiteralExpression(Types.int32Type, _position) {
  def print(tree: TreeStream) {
    tree.println(this.value)
  }

  override def generateCode(code: CodeContext, deref: Boolean) = {
    if (deref) {
      TypeRef.int32Type().constInt(value, false)
    } else {
      this.resolvedType().promote(code.sem).instantiate(code, this.generateCode(code, true))
    }
  }
}

case class CharacterLiteralExpression(value: Char, var _position: Position = new Position()) extends LiteralExpression(Types.charType, _position) {
  def print(tree: TreeStream) {
    tree.println(this.value)
  }

  override def generateCode(code: CodeContext, deref: Boolean) = {
    if (deref) {
      TypeRef.int32Type().constInt(value, false)
    } else {
      this.resolvedType().promote(code.sem).instantiate(code, this.generateCode(code, true))
    }
  }
}

case class StringLiteralExpression(value: String, var _position: Position = new Position()) extends LiteralExpression(Types.stringType, _position) {
  def print(tree: TreeStream) {
    tree.println(this.value)
  }

  override def generateCode(code: CodeContext, deref: Boolean) =
    LLVMHelpers.buildConstStringVariable(code.module,
      LLVMHelpers.getLLVMString(code.sem, code.module, value))
}

case class NullLiteralExpression(var _position: Position = new Position()) extends LiteralExpression(Types.nullType, _position) {
  def print(tree: TreeStream) {
    tree.println("NULL")
  }

  override def generateCode(code: CodeContext, deref: Boolean) =
    TypeRef.int32Type().constNull()
}