package org.oopsc.statement

import org.oopsc._
import org.llvm.{BasicBlock, TypeRef, Value}
import scala.collection.JavaConverters._

/* TODO This should be an expression. It makes sense to merge this into `EvaluateExpression'. */
class NativeInvokeStatement(var funType: TypeRef, var funName: String, var args: List[(Value, CodeContext, Boolean) => Value] = List()) extends Statement {
  override def print(tree: TreeStream) {
    tree.println("NATIVE-INVOKE")
    tree.indent
    tree.println(funName)
    tree.unindent
  }

  def callFunc(code: CodeContext, appliedArgs: List[Value]) = {
    val fun = LLVMHelpers.getNativeFunction(code, this.funType, this.funName)

    if (this.funType.getReturnType() == TypeRef.voidType()) {
      code.builder.buildCall(fun, "", appliedArgs.asJava)
    } else {
      code.builder.buildCall(fun, "call_" + this.funName, appliedArgs.asJava)
    }
  }

  def invokeFunc(code: CodeContext, appliedArgs: List[Value], then: BasicBlock, _catch: BasicBlock) = {
    val fun = LLVMHelpers.getNativeFunction(code, this.funType, this.funName)

    if (this.funType.getReturnType() == TypeRef.voidType()) {
      code.builder.buildInvoke(fun, appliedArgs.asJava, then, _catch, "")
    } else {
      code.builder.buildInvoke(fun, appliedArgs.asJava, then, _catch, "invoke_" + this.funName)
    }
  }

  override def generateCode(code: CodeContext) {
    /* TODO code.currentFunction.getFirstParam is probably wrong. */
    val appliedArgs = args.map(_(code.currentFunction.getFirstParam, code, true))
    callFunc(code, appliedArgs)
  }
}