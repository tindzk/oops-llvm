package org.oopsc.expression

import org.oopsc._
import org.oopsc.scope._
import org.oopsc.symbol._
import scala.collection.mutable.ArrayBuffer
import org.llvm.{TypeRef, Value}
import org.llvm.binding.LLVMLibrary.{LLVMIntPredicate, LLVMCallConv}
import org.oopsc.statement.NativeInvokeStatement
import scala.Some

/**
 * Represents a variable/attribute access or a method call.
 */
class EvaluateExpression(var ref: ResolvableType) extends Expression(ref.identifier.position) {
  protected var scope: ScopedType = null
  protected var isStaticAccess = false
  protected var accessContext : Value = null
  protected var arguments = new ArrayBuffer[Expression]

  /**
   * By default, methods are called dynamically by resolving the target index in the VMT which the object points to. A
   * static context bypasses the VMT and calls the method directly. A static access is required when calling methods in
   * the base class.
   */
  def setStaticAccess(value: Boolean) {
    this.isStaticAccess = value
  }

  def setScope(scope: ScopedType) {
    this.scope = scope
  }

  def setAccessContext(value: Value = null) {
    this.accessContext = value
  }

  def addArgument(value: Expression) {
    this.arguments += value
  }

  var sem: SemanticAnalysis = null

  override def refPass(sem: SemanticAnalysis) {
    this.sem = sem
    val resolveScope = if (this.scope == null) sem.currentScope.get else this.scope

    /* Resolve variable or method. */
    this.ref.declaration = Some(resolveScope.resolveSymbol(sem, this.ref.identifier, Some(sem.currentScopedType)))

    /* Check arguments. */
    if (this.ref.declaration.get.isInstanceOf[ClassType]) {
      if (this.arguments.size != 1) {
        throw new CompileException("A type cast expects exactly one argument.", this.ref.identifier.position)
      }
    } else if (!this.ref.declaration.get.isInstanceOf[MethodSymbol]) {
      if (this.arguments.size != 0) {
        throw new CompileException("Arguments cannot be passed to a variable.", this.ref.identifier.position)
      }
    }

    /* Resolve method or attribute context. */
    if (this.ref.declaration.get.isInstanceOf[MethodSymbol] || this.ref.declaration.get.isInstanceOf[FieldSymbol]) {
      if (this.scope == null && sem.currentMethod != null) {
        this.scope = sem.currentMethod.declaringContext.get
      }
    }

    this.ref.declaration.get match {
      case m: MethodSymbol =>
        /* Verify that the passed arguments match the expected parameters. */
        if (this.arguments.size != m.parameters.size) {
          throw new CompileException(s"Parameter count mismatch: ${m.parameters.size} expected, ${this.arguments.size} given.", this.ref.identifier.position)
        }

        for (((arg, param), num) <- this.arguments.zip(m.parameters).zipWithIndex) {
          arg.refPass(sem)

          if (!arg.resolvedType().isA(sem, param.getResolvedType)) {
            throw new CompileException(
              s"Argument ${num + 1} mismatches: ${param.resolvedType.get.name()} expected, ${arg.resolvedType().name} given.", this.ref.identifier.position)
          }
        }

      case c: ClassType =>
        /* Cast. */
        this.arguments(0).refPass(sem)

      case _ =>
    }
  }

  /**
   * Override as to propagate the resolved type.
   */
  override def resolvedType(): Type =
    this.ref.declaration.get match {
      case v: VariableSymbol => v.getResolvedType
      case m: MethodSymbol => m.getResolvedReturnType(this.sem)
      case c: ClassType => c
      case _ => super.resolvedType()
    }

  override def isVariable() =
    this.ref.declaration.get match {
      case sym: VariableSymbol => true
      case _ => false
    }

  def print(tree: TreeStream) {
    tree.println("EVALUATE")
    tree.indent
    tree.println(this.ref.identifier.name + " : " + (if (this.isVariable()) "REF " else "") + this.resolvedType().name)
    tree.unindent
  }

  override def generateCode(code: CodeContext, deref: Boolean) : Value = {
    if (this.accessContext == null) {
      /* Insert BASE or SELF. */
      this.accessContext = code.currentFunction.getFirstParam
    }

    this.ref.declaration.get match {
      case sym: VariableSymbol =>
        /* Variable or attribute. */
        sym.getLLVMValue()(this.accessContext, code, deref)

      case m: MethodSymbol =>
        var ctx = this.accessContext

        val fun =
          if (this.isStaticAccess || this.scope.mustEmbed()) {
            /* Static method call */
            if (!this.scope.mustEmbed()) {
              ctx = code.builder.buildBitCast(ctx, this.scope.getLLVMType().pointerType(), "")
            }

            m.getLLVMFunction()
          } else {
            /* Dynamic method call. */
            val refVMT = LLVMHelpers.getPointer(code.builder, this.accessContext, 0, 0)
            val vmt = code.builder.buildLoad(refVMT, "vtable")

            val refVTableEntry = LLVMHelpers.getPointer(code.builder, vmt, m.vmtIndex)
            val vtableElem = code.builder.buildLoad(refVTableEntry, "vtableEntry")

            code.builder.buildBitCast(vtableElem, m.getLLVMFunctionType(this.scope.asInstanceOf[Type]).pointerType(), "cast")
          }

        val args = List(ctx) ++ this.arguments.map(_.generateCode(code, true))
        import scala.collection.JavaConverters._

        val value = if (code.unwindBlocks.nonEmpty) {
          val blkThen = code.currentFunction.appendBasicBlock("continue")
          val invoke = code.builder.buildInvoke(fun, args.asJava, blkThen, code.unwindBlocks.top, "")
          code.builder.positionBuilderAtEnd(blkThen)
          invoke
        } else {
          code.builder.buildCall(fun, "", args.asJava)
        }

        if (deref) {
          value
        } else {
          val prom = this.resolvedType().promote(this.sem)

          if (prom.mustEmbed()) {
            prom.instantiate(code, value)
          } else {
            value
          }
        }

      case sym: ClassType =>
        /* TODO What is supposed to happen for casts such as Object(NULL) or Main(NULL)? */
        val result = code.builder.buildAlloca(sym.getLLVMType().pointerType(), "typeCast.result")

        /* Get object reference. */
        val firstArg = this.arguments(0)

        /* Check type compatibility first. */
        /* TODO firstArg is evaluated twice */
        val check = new TypeCheckExpression(firstArg, new ResolvableType(sym.ident(), Some(sym)))

        val end = code.currentFunction.appendBasicBlock("typeCast.end")
        val isTrue = code.currentFunction.appendBasicBlock("typeCast.isTrue")
        val isFalse = code.currentFunction.appendBasicBlock("typeCast.isFalse")

        val isEq = code.builder.buildICmp(LLVMIntPredicate.LLVMIntEQ, check.generateCode(code, true), TypeRef.int1Type().constInt(1, false), "isEq")
        code.builder.buildCondBr(isEq, isTrue, isFalse)

        code.builder.positionBuilderAtEnd(isTrue)
        code.builder.buildStore(code.builder.buildBitCast(firstArg.generateCode(code, true), sym.getLLVMType().pointerType(), ""), result)
        code.builder.buildBr(end)

        code.builder.positionBuilderAtEnd(isFalse)
        /* TODO Throw exception in this case */
        code.builder.buildStore(code.builder.buildBitCast(firstArg.generateCode(code, true), sym.getLLVMType().pointerType(), ""), result)
        code.builder.buildBr(end)

        code.builder.positionBuilderAtEnd(end)

        code.builder.buildLoad(result, "")
    }
  }
}