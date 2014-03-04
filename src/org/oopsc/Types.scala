package org.oopsc

import scala.collection.JavaConverters._

import org.oopsc.symbol._
import org.llvm.{Value, TypeRef}
import org.oopsc.statement.NativeInvokeStatement
import org.oopsc.expression.BinaryExpression
import org.llvm.binding.LLVMLibrary.LLVMIntPredicate

object Types {
  /** Internal type for methods without result. */
  final val voidType = new PrimitiveType("monty.core.Void", TypeRef.voidType())

  /** Internal type for null. Compatible to all classes. */
  final val nullType = new PrimitiveType("monty.core.Null", TypeRef.voidType())

  /** Internal base types for numbers. */
  final val int1Type = new PrimitiveType("monty.core.Int1", TypeRef.int1Type())
  final val int8Type = new PrimitiveType("monty.core.Int8", TypeRef.int8Type())
  final val int16Type = new PrimitiveType("monty.core.Int16", TypeRef.int16Type())
  final val int32Type = new PrimitiveType("monty.core.Int32", TypeRef.int32Type())
  final val int64Type = new PrimitiveType("monty.core.Int64", TypeRef.int64Type())
  final val int128Type = new PrimitiveType("monty.core.Int128", TypeRef.intType(128))

  final val uint8Type = new PrimitiveType("monty.core.UInt8", TypeRef.int8Type())
  final val uint16Type = new PrimitiveType("monty.core.UInt16", TypeRef.int16Type())
  final val uint32Type = new PrimitiveType("monty.core.UInt32", TypeRef.int32Type())
  final val uint64Type = new PrimitiveType("monty.core.UInt64", TypeRef.int64Type())
  final val uint128Type = new PrimitiveType("monty.core.UInt128", TypeRef.intType(128))

  final val doubleType = new PrimitiveType("monty.core.Double", TypeRef.doubleType())
  final val floatType = new PrimitiveType("monty.core.Float", TypeRef.floatType())

  final val boolType = new PrimitiveType("monty.core.Boolean", TypeRef.int1Type())
  final val charType = new PrimitiveType("monty.core.Character", TypeRef.int32Type())

  /** Internal base type for strings. */
  final val stringType = new PrimitiveType("monty.core.String", TypeRef.int8Type().pointerType())
}

class Types {
  final val strStruct = new RecordType(new Identifier("Str"),
    new FieldSymbol(new Identifier("length"), Types.int32Type),
    new FieldSymbol(new Identifier("data"), Types.stringType))

  final val objectClass = new ClassType(new Identifier("monty.stdlib.Obj"))
  final val intWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Int"), Types.int32Type)
  final val octWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Oct"), Types.int8Type)
  final val hexWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Hex"), Types.int16Type)
  final val charWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Char"), Types.int32Type)
  final val byteWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Byte"), Types.int8Type)
  final val boolWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Bool"), Types.int1Type)
  final val stringWrapper = new TypeWrapSymbol(new Identifier("monty.stdlib.Str"), strStruct) {
    override def assign(code: CodeContext, leftOperand: Value, rightOperand: Value) {
      val right = code.builder.buildLoad(rightOperand, "")
      code.builder.buildStore(right, leftOperand)
    }
  }

  intWrapper.operations += {
    val x = new MethodSymbol(new Identifier("print"))
    val p = List(TypeRef.int8Type().pointerType()).asJava
    x.statements.statements += new NativeInvokeStatement(
      TypeRef.functionType(TypeRef.int32Type(), true, p),
      "printf",
      List(
        (value: Value, code: CodeContext, deref: Boolean) => LLVMHelpers.getCString(code, "%i"),
        (value: Value, code: CodeContext, deref: Boolean) => intWrapper.coveringType.declaration.get.asInstanceOf[PrimitiveType].getLLVMValue()(value, code, deref)))  // TODO ugly

    x
  }

  intWrapper.operators += new OperatorSymbol(BinaryExpression.PLUS, intWrapper,
    (builder, left, right) =>
      builder.buildAdd(left, right, ""))

  intWrapper.operators += new OperatorSymbol(BinaryExpression.MINUS, intWrapper,
    (builder, left, right) =>
      builder.buildSub(left, right, ""))

  intWrapper.operators += new OperatorSymbol(BinaryExpression.LT, boolWrapper,
    (builder, left, right) =>
      builder.buildICmp(LLVMIntPredicate.LLVMIntSLT, left, right, ""))

  intWrapper.operators += new OperatorSymbol(BinaryExpression.GT, boolWrapper,
    (builder, left, right) =>
      builder.buildICmp(LLVMIntPredicate.LLVMIntSGT, left, right, ""))

  intWrapper.operators += new OperatorSymbol(BinaryExpression.EQ, boolWrapper,
    (builder, left, right) =>
      builder.buildICmp(LLVMIntPredicate.LLVMIntEQ, left, right, ""))

  charWrapper.operators += new OperatorSymbol(BinaryExpression.PLUS, charWrapper,
    (builder, left, right) =>
      builder.buildAdd(left, right, ""))

  charWrapper.operators += new OperatorSymbol(BinaryExpression.MINUS, charWrapper,
    (builder, left, right) =>
      builder.buildSub(left, right, ""))

  charWrapper.operations += {
    val x = new MethodSymbol(new Identifier("print"))
    val p = List(TypeRef.int8Type().pointerType()).asJava
    x.statements.statements += new NativeInvokeStatement(
      TypeRef.functionType(TypeRef.int32Type(), true, p),
      "printf",
      List(
        (value: Value, code: CodeContext, deref: Boolean) => LLVMHelpers.getCString(code, "%c"),
        (value: Value, code: CodeContext, deref: Boolean) => intWrapper.coveringType.declaration.get.asInstanceOf[PrimitiveType].getLLVMValue()(value, code, deref)))

    x
  }

  stringWrapper.operations += {
    val x = new MethodSymbol(new Identifier("print"))

    import scala.collection.JavaConverters._
    val p = List(TypeRef.int8Type().pointerType()).asJava
    x.statements.statements += new NativeInvokeStatement(
      TypeRef.functionType(TypeRef.int32Type(), true, p),
      "printf",
      List(
        (value: Value, code: CodeContext, deref: Boolean) => LLVMHelpers.getCString(code, "%.*s"),
        (value: Value, code: CodeContext, deref: Boolean) => stringWrapper.coveringType.declaration.get.asInstanceOf[RecordType].attr("length").getLLVMValue()(value, code, deref),
        (value: Value, code: CodeContext, deref: Boolean) => stringWrapper.coveringType.declaration.get.asInstanceOf[RecordType].attr("data").getLLVMValue()(value, code, deref)))
    x
  }
}